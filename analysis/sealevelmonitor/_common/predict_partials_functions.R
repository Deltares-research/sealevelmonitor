
library(dplyr)
library(purrr)
library(tidyr)


#' Component-wise predictions with SE and CI for lm() with offset + trend + nodal cycle
#'
#' @param fit   An "lm" object.
#' @param data  A data.frame/tibble with the variables in the model.
#'              If NULL, uses the original model frame stored in `fit`.
#' @param parts Character vector of components to return. Any of:
#'              - "trend" : (Intercept) + I(year - epoch) [+ from1993 if present]
#'              - "nodal" : the cos/sin pair that encodes the nodal cycle
#' @param level Confidence level for two-sided CI (default 0.95).
#' @return A tibble/data.frame = `data` augmented with columns:
#'         trend_fit, trend_se, trend_lwr, trend_upr,
#'         nodal_fit, nodal_se, nodal_lwr, nodal_upr,
#'         offset, fitted_total
predict_partial_se <- function(fit, data = NULL,
                               parts = c("trend", "nodal"),
                               level = 0.95) {
  stopifnot(inherits(fit, "lm"))
  parts <- match.arg(parts, c("trend", "nodal"), several.ok = TRUE)
  
  # --- build a model frame and matrix that align with the fitted terms ---
  if (is.null(data)) {
    mf <- model.frame(fit, na.action = na.pass)
  } else {
    # use the terms/env stored in the fit, so constants like `epoch` still work
    mf <- model.frame(formula(fit), data = data, xlev = fit$xlevels, na.action = na.pass)
  }
  X  <- model.matrix(terms(fit), mf)
  cn <- colnames(X)
  
  # Reorder coef/cov to match the column order in X (safer)
  b_full <- coef(fit)[cn]
  V_full <- vcov(fit)[cn, cn, drop = FALSE]
  
  # Identify column indices for each component
  idx_trend <- which(cn %in% c("(Intercept)", "I(year - epoch)", "from1993"))
  idx_nodal <- grep("^I\\((cos|sin)\\(", cn)  # cos(...) and sin(...)
  
  # Helper: contribution + SE for a subset of columns
  contrib_se <- function(idx) {
    if (length(idx) == 0L) {
      n <- nrow(X)
      list(fit = rep(NA_real_, n), se = rep(NA_real_, n))
    } else {
      Xp <- X[, idx, drop = FALSE]
      bp <- b_full[idx]
      Vp <- V_full[idx, idx, drop = FALSE]
      fit <- as.numeric(Xp %*% bp)
      # diag(X V X^T) efficiently:
      se  <- sqrt(rowSums((Xp %*% Vp) * Xp))
      list(fit = fit, se = se)
    }
  }
  
  out_trend <- contrib_se(idx_trend)
  out_nodal <- contrib_se(idx_nodal)
  
  # t critical value for CI on mean function
  crit <- stats::qt(0.5 + level/2, df = df.residual(fit))
  
  # Offsets (can be NULL -> treat as 0)
  off <- model.offset(mf)
  if (is.null(off)) off <- rep(0, nrow(mf))
  
  # Build output data (preserve input rows)
  res <- as.data.frame(mf)
  # Add requested parts
  if ("trend" %in% parts) {
    res$trend_fit <- out_trend$fit
    res$trend_se  <- out_trend$se
    res$trend_lwr <- res$trend_fit - crit * res$trend_se
    res$trend_upr <- res$trend_fit + crit * res$trend_se
  }
  if ("nodal" %in% parts) {
    res$nodal_fit <- out_nodal$fit
    res$nodal_se  <- out_nodal$se
    res$nodal_lwr <- res$nodal_fit - crit * res$nodal_se
    res$nodal_upr <- res$nodal_fit + crit * res$nodal_se
  }
  
  # Always useful to return offset and total fit for checking
  res$offset       <- off
  # total = offset + sum of all columns' contribution (trend + nodal + anything else in the model)
  # Here your model only has trend+nodal (plus offset), so:
  total_fit <- rep(0, nrow(X))
  if (length(idx_trend)) total_fit <- total_fit + out_trend$fit
  if (length(idx_nodal)) total_fit <- total_fit + out_nodal$fit
  res$fitted_total <- total_fit + off
  
  # If original `data` was supplied, bind these columns back onto it (safer for row order)
  if (!is.null(data)) {
    # keep original data columns and bind new ones in the same row order
    res <- cbind(data, res[, setdiff(names(res), names(data)), drop = FALSE])
  }
  res
}