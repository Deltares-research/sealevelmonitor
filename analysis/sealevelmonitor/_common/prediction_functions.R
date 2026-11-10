
## Functions to calculate predictions for the three models with confidence interval and possibility to include future years
## 
## Belangrijke interpretatie
# Dit is een confidence interval voor de verwachte modelwaarde:
#   
#   Xβ^±z XVHACXTX\hat{\beta} \pm z\,\sqrt{X V_{\mathrm{HAC}}X^\mathsf{T}}Xβ^​±zXVHAC​XT​
# 
# Het interval omvat:
#   
# onzekerheid in de geschatte regressiecoëfficiënten;
# de covarianties tussen die coëfficiënten;
# de HAC-correctie voor heteroskedasticiteit en autocorrelatie.
#
# Het predictie-interval bevat bovendien
# residuele jaar-tot-jaarvariatie;
#
# Het omvat niet:
#   
# onzekerheid over de toekomstige stormopzet;
# structurele onzekerheid tussen de drie modeltypen;
# onzekerheid of de historische versnelling na 2040 doorzet.
# 
# De verschillen tussen linear, broken_linear en broken_squared moeten daarom afzonderlijk worden gezien als modelstructurele onzekerheid.

makePredictionTableCI <- function(
    models,
    future_years = NULL,
    level = 0.95
) {
  
  # ============================================================
  # 1. Invoer controleren
  # ============================================================
  
  required_columns <- c(
    "station",
    "modeltype",
    "data",
    "model",
    "vcov.HAC"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(models)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "De volgende vereiste kolommen ontbreken in models: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (
    !is.numeric(level) ||
    length(level) != 1L ||
    is.na(level) ||
    level <= 0 ||
    level >= 1
  ) {
    stop("level moet één numerieke waarde tussen 0 en 1 zijn.")
  }
  
  
  # ============================================================
  # 2. Helper: toekomstige jaren toevoegen
  # ============================================================
  
  add_future_years <- function(df, future_years) {
    
    required_data_columns <- c(
      "year",
      "height",
      "surge_anomaly"
    )
    
    missing_data_columns <- setdiff(
      required_data_columns,
      names(df)
    )
    
    if (length(missing_data_columns) > 0L) {
      stop(
        "De volgende kolommen ontbreken in een data-tabel: ",
        paste(missing_data_columns, collapse = ", ")
      )
    }
    
    # Zorg dat beide gebroken-trendpredictoren beschikbaar zijn.
    if (!"from1993" %in% names(df)) {
      df$from1993 <- pmax(df$year - 1993, 0)
    }
    
    if (!"from1960_square" %in% names(df)) {
      df$from1960_square <- pmax(df$year - 1960, 0)^2
    }
    
    # Bestaande rijen behoren tot de oorspronkelijke modeldata.
    df <- df |>
      dplyr::mutate(
        is_future = FALSE
      )
    
    if (
      is.null(future_years) ||
      length(future_years) == 0L
    ) {
      return(df)
    }
    
    future_years <- sort(
      unique(
        as.numeric(future_years)
      )
    )
    
    if (anyNA(future_years)) {
      stop("future_years mag geen ontbrekende waarden bevatten.")
    }
    
    # Alleen nog ontbrekende jaren toevoegen.
    years_to_add <- setdiff(
      future_years,
      df$year
    )
    
    if (length(years_to_add) == 0L) {
      return(df)
    }
    
    future_df <- tibble::tibble(
      year = years_to_add,
      
      # Geen waarnemingen voor toekomstige jaren.
      height = NA_real_,
      
      # Surge anomaly kan niet worden geëxtrapoleerd.
      surge_anomaly = 0,
      
      # Gebroken lineaire predictor.
      from1993 = pmax(
        years_to_add - 1993,
        0
      ),
      
      # Gebroken kwadratische predictor.
      from1960_square = pmax(
        years_to_add - 1960,
        0
      )^2,
      
      is_future = TRUE
    )
    
    dplyr::bind_rows(
      df,
      future_df
    ) |>
      dplyr::arrange(year)
  }
  
  
  # ============================================================
  # 3. Helper: diagonaal van X V X'
  # ============================================================
  
  calculate_component_variance <- function(
    X_component,
    V
  ) {
    
    component_variance <- rowSums(
      (X_component %*% V) * X_component
    )
    
    # Door numerieke afronding kunnen zeer kleine negatieve
    # waarden ontstaan.
    scale_value <- max(
      1,
      max(abs(component_variance), na.rm = TRUE)
    )
    
    tolerance <- 100 *
      .Machine$double.eps *
      scale_value
    
    materially_negative <-
      component_variance < -tolerance
    
    if (any(materially_negative, na.rm = TRUE)) {
      warning(
        "Negatieve varianties gevonden die groter zijn dan ",
        "verwachte numerieke afrondingsfouten. Controleer vcov.HAC.",
        call. = FALSE
      )
    }
    
    pmax(
      component_variance,
      0
    )
  }
  
  
  # ============================================================
  # 4. Helper: voorspellingen voor één model
  #
  # Berekent:
  #
  # A. volledige modelvoorspelling
  #
  # B. langetermijntrend:
  #    - HAC-confidence interval;
  #    - predictie-interval voor één gecorrigeerde jaarwaarde
  #
  # C. nodale component:
  #    - uitsluitend HAC-confidence interval
  # ============================================================
  
  predict_one_model <- function(
    df,
    model,
    vcov_hac,
    level
  ) {
    
    # ----------------------------------------------------------
    # HAC-covariantiematrix controleren
    # ----------------------------------------------------------
    
    if (is.null(vcov_hac)) {
      stop("vcov.HAC ontbreekt voor één of meer modellen.")
    }
    
    vcov_hac <- as.matrix(vcov_hac)
    
    if (
      is.null(rownames(vcov_hac)) ||
      is.null(colnames(vcov_hac))
    ) {
      stop(
        "vcov.HAC moet rij- en kolomnamen hebben die ",
        "overeenkomen met coef(model)."
      )
    }
    
    
    # ----------------------------------------------------------
    # Volledige modelvoorspelling
    #
    # Inclusief:
    # - langetermijntrend;
    # - nodale cyclus;
    # - offset(surge_anomaly).
    # ----------------------------------------------------------
    
    full_prediction <- stats::predict(
      object = model,
      newdata = df
    )
    
    
    # ----------------------------------------------------------
    # Designmatrix maken
    # ----------------------------------------------------------
    
    model_terms <- stats::delete.response(
      stats::terms(model)
    )
    
    X <- stats::model.matrix(
      object = model_terms,
      data = df,
      contrasts.arg = model$contrasts,
      xlev = model$xlevels
    )
    
    beta <- stats::coef(model)
    
    
    # ----------------------------------------------------------
    # Coëfficiënten, designmatrix en HAC-matrix uitlijnen
    #
    # offset(surge_anomaly) heeft geen geschatte coëfficiënt en
    # valt daardoor automatisch buiten common_terms.
    # ----------------------------------------------------------
    
    common_terms <- Reduce(
      intersect,
      list(
        names(beta),
        colnames(X),
        rownames(vcov_hac),
        colnames(vcov_hac)
      )
    )
    
    # Niet-geschatte of aliasede coëfficiënten verwijderen.
    common_terms <- common_terms[
      !is.na(beta[common_terms]) &
        is.finite(beta[common_terms])
    ]
    
    if (length(common_terms) == 0L) {
      stop(
        paste0(
          "Geen overeenkomstige termen gevonden tussen ",
          "coef(model), model.matrix() en vcov.HAC."
        )
      )
    }
    
    X_use <- X[
      ,
      common_terms,
      drop = FALSE
    ]
    
    beta_use <- beta[
      common_terms
    ]
    
    V_use <- vcov_hac[
      common_terms,
      common_terms,
      drop = FALSE
    ]
    
    
    # ----------------------------------------------------------
    # Nodale modeltermen herkennen
    # ----------------------------------------------------------
    
    nodal_columns <- grepl(
      pattern = "cos\\(|sin\\(",
      x = colnames(X_use)
    )
    
    if (!any(nodal_columns)) {
      warning(
        "Geen sin- of cos-termen gevonden voor model: ",
        paste(
          deparse(stats::formula(model)),
          collapse = " "
        ),
        call. = FALSE
      )
    }
    
    
    # ----------------------------------------------------------
    # Kritieke waarde
    #
    # HAC-inferentie is asymptotisch, daarom wordt qnorm()
    # gebruikt.
    # ----------------------------------------------------------
    
    critical_value <- stats::qnorm(
      1 - (1 - level) / 2
    )
    
    
    # ==========================================================
    # 4A. Langetermijntrend
    #
    # De nodale predictoren worden op nul gezet.
    # ==========================================================
    
    X_longterm <- X_use
    
    if (any(nodal_columns)) {
      X_longterm[
        ,
        nodal_columns
      ] <- 0
    }
    
    prediction_recalc <- as.numeric(
      X_longterm %*% beta_use
    )
    
    prediction_recalc_variance <-
      calculate_component_variance(
        X_component = X_longterm,
        V = V_use
      )
    
    prediction_recalc_se <- sqrt(
      prediction_recalc_variance
    )
    
    
    # ----------------------------------------------------------
    # Confidence interval van de gemiddelde langetermijntrend
    # ----------------------------------------------------------
    
    prediction_recalc_lwr <-
      prediction_recalc -
      critical_value * prediction_recalc_se
    
    prediction_recalc_upr <-
      prediction_recalc +
      critical_value * prediction_recalc_se
    
    
    # ----------------------------------------------------------
    # Predictie-interval van één toekomstige, voor surge en
    # nodaal getij gecorrigeerde jaarwaarde
    #
    # Var(nieuwe jaarwaarde) =
    #   HAC-parameteronzekerheid + residuele variantie
    # ----------------------------------------------------------
    
    residual_sigma <- stats::sigma(
      model
    )
    
    if (
      length(residual_sigma) != 1L ||
      is.na(residual_sigma) ||
      !is.finite(residual_sigma)
    ) {
      stop(
        "De residuele standaardafwijking kon niet worden ",
        "bepaald voor één of meer modellen."
      )
    }
    
    prediction_recalc_pred_variance <-
      prediction_recalc_variance +
      residual_sigma^2
    
    prediction_recalc_pred_se <- sqrt(
      pmax(
        prediction_recalc_pred_variance,
        0
      )
    )
    
    prediction_recalc_pred_lwr <-
      prediction_recalc -
      critical_value * prediction_recalc_pred_se
    
    prediction_recalc_pred_upr <-
      prediction_recalc +
      critical_value * prediction_recalc_pred_se
    
    
    # ==========================================================
    # 4B. Nodale component
    #
    # Alleen de sinus- en cosinustermen blijven behouden.
    # Hiervoor wordt alleen een HAC-confidence interval berekend.
    # ==========================================================
    
    X_nodal <- X_use
    
    X_nodal[
      ,
      !nodal_columns
    ] <- 0
    
    nodal_tide <- as.numeric(
      X_nodal %*% beta_use
    )
    
    nodal_tide_variance <-
      calculate_component_variance(
        X_component = X_nodal,
        V = V_use
      )
    
    nodal_tide_se <- sqrt(
      nodal_tide_variance
    )
    
    nodal_tide_lwr <-
      nodal_tide -
      critical_value * nodal_tide_se
    
    nodal_tide_upr <-
      nodal_tide +
      critical_value * nodal_tide_se
    
    
    # ==========================================================
    # 4C. Resultaat voor dit model
    # ==========================================================
    
    df |>
      dplyr::mutate(
        
        # Volledige voorspelling:
        # trend + nodaal getij + surge-offset
        prediction =
          as.numeric(full_prediction),
        
        # ------------------------------------------------------
        # Langetermijntrend
        # ------------------------------------------------------
        
        prediction_recalc =
          prediction_recalc,
        
        # HAC-confidence interval
        prediction_recalc_se =
          prediction_recalc_se,
        
        prediction_recalc_lwr =
          prediction_recalc_lwr,
        
        prediction_recalc_upr =
          prediction_recalc_upr,
        
        # Predictie-interval
        residual_sigma =
          residual_sigma,
        
        prediction_recalc_pred_se =
          prediction_recalc_pred_se,
        
        prediction_recalc_pred_lwr =
          prediction_recalc_pred_lwr,
        
        prediction_recalc_pred_upr =
          prediction_recalc_pred_upr,
        
        # ------------------------------------------------------
        # Nodale component
        #
        # Alleen een HAC-confidence interval.
        # ------------------------------------------------------
        
        nodal_tide =
          nodal_tide,
        
        nodal_tide_se =
          nodal_tide_se,
        
        nodal_tide_lwr =
          nodal_tide_lwr,
        
        nodal_tide_upr =
          nodal_tide_upr
      )
  }
  
  
  # ============================================================
  # 5. Eventueel toekomstige jaren toevoegen
  # ============================================================
  
  models_extended <- models |>
    dplyr::mutate(
      prediction_data = purrr::map(
        data,
        add_future_years,
        future_years = future_years
      )
    )
  
  
  # ============================================================
  # 6. Voorspellingen voor ieder station en model berekenen
  # ============================================================
  
  results <- models_extended |>
    dplyr::mutate(
      prediction_result = purrr::pmap(
        list(
          df = prediction_data,
          model = model,
          vcov_hac = vcov.HAC
        ),
        function(df, model, vcov_hac) {
          
          predict_one_model(
            df = df,
            model = model,
            vcov_hac = vcov_hac,
            level = level
          )
        }
      )
    ) |>
    dplyr::select(
      station,
      modeltype,
      prediction_result
    ) |>
    tidyr::unnest(
      prediction_result
    )
  
  
  # ============================================================
  # 7. Gecorrigeerde waarnemingen en diagnostiek
  # ============================================================
  
  results <- results |>
    dplyr::mutate(
      
      # Meting gecorrigeerd voor alleen surge anomaly.
      `data_height-surge_anomaly` =
        height -
        surge_anomaly,
      
      # Meting gecorrigeerd voor surge en nodaal getij.
      height_corrected =
        height -
        surge_anomaly -
        nodal_tide,
      
      # Volledige voorspelling zonder surge-offset.
      `preds_height-surge_anomaly` =
        prediction -
        surge_anomaly,
      
      # Controle van de decompositie:
      #
      # prediction =
      #   langetermijntrend + nodaal getij + surge anomaly
      reconstruction_error =
        prediction -
        (
          prediction_recalc +
            nodal_tide +
            surge_anomaly
        )
    )
  
  
  # ============================================================
  # 8. Uitvoerkolommen selecteren
  # ============================================================
  
  results <- results |>
    dplyr::select(
      station,
      modeltype,
      
      year,
      is_future,
      
      height,
      surge_anomaly,
      height_corrected,
      
      # --------------------------------------------------------
      # Langetermijntrend
      # --------------------------------------------------------
      
      prediction_recalc,
      
      # HAC-confidence interval
      prediction_recalc_se,
      prediction_recalc_lwr,
      prediction_recalc_upr,
      
      # Predictie-interval
      residual_sigma,
      prediction_recalc_pred_se,
      prediction_recalc_pred_lwr,
      prediction_recalc_pred_upr,
      
      # --------------------------------------------------------
      # Nodale component
      #
      # Alleen HAC-confidence interval.
      # --------------------------------------------------------
      
      nodal_tide,
      nodal_tide_se,
      nodal_tide_lwr,
      nodal_tide_upr,
      
      # --------------------------------------------------------
      # Volledige voorspelling en diagnostiek
      # --------------------------------------------------------
      
      prediction,
      
      `data_height-surge_anomaly`,
      `preds_height-surge_anomaly`,
      
      reconstruction_error,
      
      # Modelpredictoren behouden indien aanwezig.
      dplyr::any_of(
        c(
          "from1993",
          "from1960_square"
        )
      )
    ) |>
    dplyr::arrange(
      station,
      modeltype,
      year
    )
  
  return(results)
}
plotLongtermTrends <- function(
    predictions,
    station_name = NULL,
    facet_models = FALSE,
    show_future_line = TRUE,
    show_confidence_interval = TRUE,
    show_prediction_interval = TRUE,
    point_size = 2,
    line_width = 0.9
) {
  
  # ============================================================
  # 1. Invoer controleren
  # ============================================================
  
  required_columns <- c(
    "station",
    "modeltype",
    "year",
    "is_future",
    "height",
    "surge_anomaly",
    "nodal_tide",
    "prediction_recalc",
    "prediction_recalc_lwr",
    "prediction_recalc_upr",
    "prediction_recalc_pred_lwr",
    "prediction_recalc_pred_upr"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(predictions)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      "De volgende kolommen ontbreken in predictions: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  
  # ============================================================
  # 2. Eventueel één station selecteren
  # ============================================================
  
  plot_data <- predictions
  
  if (!is.null(station_name)) {
    
    plot_data <- plot_data |>
      dplyr::filter(
        station == station_name
      )
    
    if (nrow(plot_data) == 0L) {
      stop(
        "Station '",
        station_name,
        "' komt niet voor in predictions."
      )
    }
  }
  
  
  # ============================================================
  # 3. Gecorrigeerde metingen berekenen
  #
  # Gecorrigeerde zeespiegel =
  #   meting - surge anomaly - nodaal getij
  #
  # Als height_corrected al door makePredictionTableCI() is
  # gemaakt, wordt de bestaande kolom gebruikt.
  # ============================================================
  
  if (!"height_corrected" %in% names(plot_data)) {
    
    plot_data <- plot_data |>
      dplyr::mutate(
        height_corrected =
          height -
          surge_anomaly -
          nodal_tide
      )
  }
  
  
  # ============================================================
  # 4. Historische metingen voorbereiden
  #
  # De oorspronkelijke waarneming staat eenmaal per modeltype in
  # de tabel. In een niet-gefacetteerde figuur worden daarom alleen
  # de observaties van één modeltype gebruikt om dubbele punten te
  # voorkomen.
  #
  # De nodale correctie kan per model enigszins verschillen.
  # Daarom worden bij facet_models = TRUE de modelspecifieke
  # gecorrigeerde metingen per facet gebruikt.
  # ============================================================
  
  if (isTRUE(facet_models)) {
    
    observation_data <- plot_data |>
      dplyr::filter(
        !is_future,
        !is.na(height_corrected)
      )
    
  } else {
    
    first_modeltype <- unique(
      plot_data$modeltype
    )[1]
    
    observation_data <- plot_data |>
      dplyr::filter(
        modeltype == first_modeltype,
        !is_future,
        !is.na(height_corrected)
      ) |>
      dplyr::distinct(
        station,
        year,
        .keep_all = TRUE
      )
  }
  
  
  # ============================================================
  # 5. Basispalet
  #
  # Dezelfde kleuren worden gebruikt voor:
  # - langetermijntrend;
  # - confidence interval;
  # - predictie-interval.
  # ============================================================
  
  modeltypes <- unique(
    plot_data$modeltype
  )
  
  default_colours <- c(
    linear = "#0072B2",
    broken_linear = "#D55E00",
    broken_squared = "#009E73"
  )
  
  missing_colour_names <- setdiff(
    modeltypes,
    names(default_colours)
  )
  
  if (length(missing_colour_names) > 0L) {
    
    extra_colours <- scales::hue_pal()(
      length(missing_colour_names)
    )
    
    names(extra_colours) <- missing_colour_names
    
    model_colours <- c(
      default_colours,
      extra_colours
    )
    
  } else {
    
    model_colours <- default_colours
  }
  
  model_colours <- model_colours[
    modeltypes
  ]
  
  
  # ============================================================
  # 6. Basisfiguur
  # ============================================================
  
  p <- ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = year
    )
  )
  
  
  # ============================================================
  # 7. Predictie-interval
  #
  # Dit is de buitenste, lichtste band.
  #
  # De band omvat:
  # - parameteronzekerheid van de trend;
  # - residuele spreiding van één individuele jaarwaarde.
  # ============================================================
  
  if (isTRUE(show_prediction_interval)) {
    
    p <- p +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(
          ymin = prediction_recalc_pred_lwr / 10,
          ymax = prediction_recalc_pred_upr / 10,
          fill = modeltype
        ),
        alpha = 0.10,
        colour = NA,
        na.rm = TRUE
      )
  }
  
  
  # ============================================================
  # 8. Confidence interval
  #
  # Dit is de binnenste, donkerdere band.
  #
  # De band beschrijft de HAC-onzekerheid van de geschatte
  # gemiddelde langetermijntrend.
  # ============================================================
  
  if (isTRUE(show_confidence_interval)) {
    
    p <- p +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(
          ymin = prediction_recalc_lwr  / 10,
          ymax = prediction_recalc_upr / 10,
          fill = modeltype
        ),
        alpha = 0.28,
        colour = NA,
        na.rm = TRUE
      )
  }
  
  
  # ============================================================
  # 9. Langetermijntrend
  #
  # Historisch deel: doorgetrokken lijn
  # Toekomstig deel: gestreepte lijn
  # ============================================================
  
  p <- p +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        y = prediction_recalc / 10,
        colour = modeltype,
        linetype = is_future,
        group = interaction(
          modeltype,
          is_future
        )
      ),
      linewidth = line_width,
      na.rm = TRUE
    )
  
  
  # ============================================================
  # 10. Gecorrigeerde metingen
  # ============================================================
  
  p <- p +
    ggplot2::geom_point(
      data = observation_data,
      mapping = ggplot2::aes(
        x = year,
        y = height_corrected / 10
      ),
      inherit.aes = FALSE,
      shape = 21,
      size = point_size,
      stroke = 0.4,
      colour = "grey20",
      fill = "white",
      alpha = 0.8,
      na.rm = TRUE
    )
  
  
  # ============================================================
  # 11. Overgang tussen historische periode en toekomst
  # ============================================================
  
  if (
    isTRUE(show_future_line) &&
    any(plot_data$is_future, na.rm = TRUE)
  ) {
    
    first_future_year <- min(
      plot_data$year[
        plot_data$is_future
      ],
      na.rm = TRUE
    )
    
    p <- p +
      ggplot2::geom_vline(
        xintercept = first_future_year - 0.5,
        linetype = "dotted",
        colour = "grey35",
        linewidth = 0.6
      )
  }
  
  
  # ============================================================
  # 12. Schalen en labels
  # ============================================================
  
  p <- p +
    ggplot2::scale_colour_manual(
      values = model_colours,
      name = "Model"
    ) +
    
    ggplot2::scale_fill_manual(
      values = model_colours,
      name = "Model"
    ) +
    
    ggplot2::scale_linetype_manual(
      values = c(
        `FALSE` = "solid",
        `TRUE` = "dashed"
      ),
      breaks = c(FALSE, TRUE),
      labels = c(
        `FALSE` = "Modelperiode",
        `TRUE` = "Extrapolatie"
      ),
      name = NULL
    ) +
    
    ggplot2::labs(
      title = if (is.null(station_name)) {
        "Langetermijntrend in zeespiegelniveau"
      } else {
        paste(
          "Langetermijntrend:",
          station_name
        )
      },
      
      subtitle = paste0(
        if (isTRUE(show_confidence_interval)) {
          "Donkere band: confidence interval. "
        } else {
          ""
        },
        if (isTRUE(show_prediction_interval)) {
          "Lichte band: predictie-interval. "
        } else {
          ""
        },
        "Metingen zijn gecorrigeerd voor surge anomaly ",
        "en het 18,6-jarige nodale getij."
      ),
      
      x = "Jaar",
      
      y = paste0(
        "Zeespiegel gecorrigeerd voor windopzetanomalie ", "\n",
        "en nodaal getij (cm t.o.v. NAP (2005))"
      ),
      
      colour = "Model",
      fill = "Model"
    ) +
    
    ggplot2::theme_bw() +
    
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      legend.position = "right"
    ) +
    
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  
  
  # ============================================================
  # 13. Optioneel aparte panelen per modeltype
  # ============================================================
  
  if (isTRUE(facet_models)) {
    
    p <- p +
      ggplot2::facet_wrap(
        ~ modeltype,
        ncol = 1
      ) +
      
      # Modeltype staat al in de facetkop
      ggplot2::guides(
        colour = "none",
        fill = "none"
      )
  }
  
  return(p)
}