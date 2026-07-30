# Summary: Comparing AIC Between GLM/GAM and LM Models in R

This document summarizes how and when you can compare AIC values across **linear models (lm)**, **generalized linear models (GLM)**, and **generalized additive models (GAM, mgcv)**.

## 1. Can you compare AIC between GLM and GAM?

Yes—AIC values can be compared **if** the models:

-   use the **same response**, **same data**, and **same likelihood family**, and
-   are fitted using **maximum likelihood (ML)** and not REML.

`mgcv` experts explicitly state:

-   Do **not** compare AIC for models fitted using REML when fixed-effects differ. Use `method = "ML"` for comparisons.
-   AIC for `gam` objects is computed using `logLik.gam()`, which applies essential corrections for penalized smooths.

## 2. Why you should not compare AIC between lm() and gam() directly

`lm()` fits a Gaussian model via OLS and computes likelihood differently than `mgcv::gam()`. This means:

-   The two likelihoods are **not guaranteed to be on the same scale**.

-   `AIC(gam)` includes **Wood–Pya–Säfken (2016)** smoothing-parameter corrections, while `lm()` does not.

Thus, directly comparing:

```         
AIC(lm_model) AIC(gam_model) 
```

is **not recommended**.

## 3. The safe method: Fit the linear model via gam()

To ensure true comparability:

```         
m_lm <- gam(y ~ x, family = gaussian(), method = "ML", data = dat) 

m_gam <- gam(y ~ s(x), family = gaussian(), method = "ML", data = dat) AIC(m_lm, m_gam) 
```

Why this works:

-   Both models use **identical likelihood machinery**.

-   Both use ML.

-   Both use `logLik.gam()` corrections.

This is explicitly recommended by mgcv experts.

## 4. When AIC comparison is invalid

AIC cannot be compared when:

-   One or both models use **REML** (for different fixed effects).

-   The family is **quasi** (e.g., quasibinomial, quasipoisson); AIC is **not defined** for quasi models.

-   Models use different distributions (Gaussian vs Poisson/binomial, etc.).

If you must compare quasi-likelihood models, use QAIC from packages like **MuMIn**.

## 5. Summary Checklist

-   ✔ Same data, same response.

-   ✔ Same family and link.

-   ✔ Fit GAM with **ML**, not REML.

-   ✔ Fit LM using `gam()` (not `lm()`) for AIC comparison.

-   ✔ Use `AIC()` (not `$aic`).

-   ❌ Do not compare AIC for quasi-family models.

## 6. Recommended Code Template

```         
library(mgcv)  

# Linear model via gam() 
m_lm <- gam(y ~ x, family = gaussian(), method = "ML", data = dat)  

# GAM model 
m_gam <- gam(y ~ s(x), family = gaussian(), method = "ML", data = dat)  

# Compare AIC(m_lm, m_gam) 
```

If you want, I can generate a version tailored to your exact models or data structure.

## 7. Why use gam() also for linear (lm-like) models?

When comparing AIC values between a GAM and a simpler linear model, both models must use **the same likelihood framework**.

Using `lm()` is not recommended because:

-   `lm()` and `gam()` compute likelihoods differently. `lm()` uses OLS-based Gaussian likelihood, while `mgcv::gam()` uses penalized likelihood machinery with EDF corrections.

-   `AIC(gam)` incorporates smoothing-parameter uncertainty corrections (Wood–Pya–Säfken 2016), but `lm()` does not.

-   Therefore, AIC values are **not on the same scale** when comparing `lm()` and `gam()`.

Using `gam()` without smooths ensures:

-   identical likelihood definitions for both models,

-   consistent ML fitting (`method = "ML"`),

-   AIC values that can be validly compared.

This approach is explicitly recommended by mgcv experts, who state that fitting the linear model via `gam()` ensures comparable AIC computation.
