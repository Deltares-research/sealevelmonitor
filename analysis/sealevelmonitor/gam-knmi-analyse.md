Comparison between GAM and GLM (broken linear) model for Dutch Sea Level
================
Willem Stolte
2026-11-09

## Introduction

The Dutch Sea Level Monitor uses General Linearized Model variants to
describe the sea level at the Dutch coastal tidal stations. In the
literature, also other models are used. For example, GAM was used by
Keizer et al. (2023) to detect changes in sea level rise by comparing
different periods with respect to GAM fitted sea level changes. GAM is
at the moment not included as one of the alternative models in the
official product.

The aim of this notebook is:

- To implement a first version of GAM analyses for the Dutch main tidal
  station using the Sea Level Monitor data
- To compare the performance of GAM, and GLM broken linear model
  (preferred model for the Sea Level Monitor 2026), in terms of e.g. AIC

<details>

<summary>

Show code for data preparation
</summary>

``` r
# load necessary library
library(mgcv)    # for GAMs
library(tidyverse)
library(gratia)

epoch = 1970
datapath = "../../data/deltares/input/psmsl_gtsm_yr-latest.csv"
nstations <- length(params$selected_station)

df <- read_delim(file.path(datapath), delim = ";", col_types = cols()) %>%
  # filter(station %in% params$station) %>%
  filter(year >= 1890)

df$year_c = df$year - epoch

df <- df %>% 
  mutate(
    station = factor(
      station, levels = c(
        "Delfzijl",
        "Harlingen",
        "Den Helder",
        "IJmuiden",
        "Hoek van Holland",
        "Vlissingen",
        "Netherlands",
        "Netherlands (without Delfzijl)"
      )
    )
  ) %>%
  addBreakPoints()
```

</details>

## Define GAM and GLM

The GAM model used here is using 4 components:

- smooth term
- nodal cosine term
- nodal sine term
- surge anomaly term

The GLM broken linear model is using the following components:

- linear component
- extra trend from 1993
- nodal cosine term
- nodal sine term
- surge anomaly term

### Add helper variables

``` r
prepare_nodal <- function(df, epoch = 1970){
  df %>%
    mutate(
      nodal_cos = cos(2 * pi * (year - epoch) / 18.613),
      nodal_sin = sin(2 * pi * (year - epoch) / 18.613)
    )
}
```

### Comparing GAM and GLM

The aim is to compare the model fits from GAM and GLM in terms of AIC.
This can only be done when using the same model formulations for both
models. The broken linear model is therefore formulated using the mgcv
package (like the GAM) without the smoothing term, so only using linear
terms.

Be aware that the exact outcomes of the GLM model in this notebook may
not necessarily match exactly with the GLM outcomes in the Sea Level
Monitor script.

In the GAM formulation, $k$ denotes the basis dimension of a smooth
term, defining the maximum flexibility available to the fitted spline.
It sets an upper bound on the effective degrees of freedom, while the
actual smoothness is determined by penalization during model fitting. It
is set to 50 for this analysis.

``` r
slm_k = 50

gam_model <- function(df, epoch = 1970, k = slm_k){

  mgcv::gam(
    height ~ s(year, k = k)   # smooth term on year
    +     nodal_cos
    +     nodal_sin
    + surge_anomaly,
    family = gaussian(),
    data = df,
    method = "REML"
  )
}

# GLM only using linear terms, no smoother
glm_model <- function(df){

  mgcv::gam(
    height ~ year_c
    +        from1993
    +        nodal_cos
    +        nodal_sin
    +        surge_anomaly,
    family = gaussian(),
    data = df,
    method = "REML"
  )
}

glm2_model <- function(df){
    mgcv::gam(
    height ~ year_c
    +        from1960_square
    +        nodal_cos
    +        nodal_sin
    +        surge_anomaly,
    family = gaussian(),
    data = df,
    method = "REML"
  )

} 
```

Functions to extract predictions, standard errors and derivatives

<details>

<summary>

Show code for helper functions
</summary>

``` r
## use for plotting - vertically neutral compared to measurements
get_height_predictions <- function(model, newdata){
  
  preddata <- newdata
  
  preddata$nodal_cos     <- 0
  preddata$nodal_sin     <- 0
  preddata$surge_anomaly <- mean(newdata$surge_anomaly, na.rm = TRUE)
  
  p <- predict(
    model,
    newdata = preddata,
    type = "response",
    se.fit = TRUE
  )
  
  tibble(
    fit_height = p$fit,
    se_height  = p$se.fit
  )
}

get_derivatives <- function(model, epoch = 1970) {

  # =====================================================
  # CASE 1: GAM met smooth term
  # =====================================================
  if (length(model$smooth) > 0) {

    der <- gratia::derivatives(
      model,
      term = "s(year)",
      type = "central",
      interval = "confidence",
      unconditional = TRUE
    )

    return(
      tibble::tibble(
        x          = der$year,
        derivative = der$.derivative,
        se         = der$.se,
        lower      = der$.lower_ci,
        upper      = der$.upper_ci
      )
    )
  }

  coefs <- coef(model)
  V <- vcov(model)

  # model bevat year_c, niet year
  x_c <- seq(
    floor(min(model$model$year_c, na.rm = TRUE)),
    ceiling(max(model$model$year_c, na.rm = TRUE)),
    by = 1
  )

  year <- x_c + epoch

  # =====================================================
  # CASE 2: Gebroken lineair model
  # year_c + from1993
  # =====================================================
  if ("from1993" %in% names(coefs)) {

    b1 <- coefs["year_c"]
    b2 <- coefs["from1993"]

    h <- as.numeric(year >= 1993)

    derivative <- b1 + b2 * h

    V11 <- V["year_c", "year_c"]
    V22 <- V["from1993", "from1993"]
    V12 <- V["year_c", "from1993"]

    se <- sqrt(
      V11 +
        h^2 * V22 +
        2 * h * V12
    )

    return(
      tibble::tibble(
        x = year,
        derivative = derivative,
        se = se,
        lower = derivative - 1.96 * se,
        upper = derivative + 1.96 * se
      )
    )
  }

  # =====================================================
  # CASE 3: Gebroken kwadratisch model
  # year_c + from1960_square
  # met:
  # from1960_square = pmax(year - 1960, 0)^2
  # =====================================================
  if ("from1960_square" %in% names(coefs)) {

    b1 <- coefs["year_c"]
    b2 <- coefs["from1960_square"]

    h <- pmax(year - 1960, 0)

    derivative <- b1 + 2 * b2 * h

    V11 <- V["year_c", "year_c"]
    V22 <- V["from1960_square", "from1960_square"]
    V12 <- V["year_c", "from1960_square"]

    se <- sqrt(
      V11 +
        (2 * h)^2 * V22 +
        2 * (2 * h) * V12
    )

    return(
      tibble::tibble(
        x = year,
        derivative = derivative,
        se = se,
        lower = derivative - 1.96 * se,
        upper = derivative + 1.96 * se
      )
    )
  }

  stop("Unsupported model type")
}
```

</details>

## Apply GAM and GLM models to sea level data for all stations

The GAM and GLM models was applied to all six main stations and the
composite stations according to the code below.

<details>

<summary>

Show code for model generation
</summary>

``` r
selected_model = c("glm", "glm2", "gam")

by_station_model_compared = df %>%
  prepare_nodal() %>%
  addBreakPoints() %>%
  group_by(station) %>%
  nest() %>%
  ungroup() %>%
  expand_grid(modeltype = selected_model) %>% # in case multiple models are used
  #### parse function names based on selected model()
  mutate(modelfunctionname = paste(modeltype, "model", sep = "_")) %>%
  #### add functions for model calculation
  mutate(modelfunctions = map(modelfunctionname, get)) %>%
  #### add models based on data and functions
  mutate(model = pmap(
    list(
      data,
      modelfunctions
    ),
    \(.d, .f) .f(.d)
  )) %>%
  #### add output parameters and predictions
  mutate(
    glance = map(model, broom::glance),
    # rsq    = glance %>% map_dbl("r.squared"),
    adj.rsq = glance %>% map_dbl("adj.r.squared"),
    AIC    = glance %>% map_dbl("AIC"),
    npar = glance %>% map_dbl("npar"),
    tidy   = map(model, ~ broom::tidy(.x, parametric = TRUE, smooth = TRUE)),
    augment = map(model, broom::augment)#,
    # equation = map(model, function(x) equatiomatic::extract_eq(x))
  ) %>%
  mutate(
  pred_height = map2(
    model,
    data,
    get_height_predictions
  ),
  derivatives = map(model, get_derivatives)
)
```

</details>

In general, the resulting predictions from GAM and GLM models are very
similar for most stations. The GAM model follows, as can be expected,
better the variability of the measurements. Especially for individual
stations like Delfzijl and Harlingen, the GLM model seems to miss much
of the variation present in the signal.

``` r
plot_comparison <- function(df) {
  df %>%
    select(data, pred_height, modeltype, station) %>%
    unnest(c(data, pred_height)) %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = height, color = modeltype), alpha = 0.3) +
    geom_line(aes(y = fit_height, color = modeltype), linewidth = 0.7) +
    geom_ribbon(
    aes(
      ymin = fit_height - 1.96 * se_height,
      ymax = fit_height + 1.96 * se_height,
      fill = modeltype
    ),
    alpha = 0.5
  ) +
    facet_wrap("station") +
    theme_minimal()
}

plot_comparison(by_station_model_compared %>%
                  filter(!grepl("Netherlands", station)))
```

![Comparison between GAM and GLM model fits for all individual Dutch
tidal stations.](gam-knmi-analyse_files/figure-gfm/gam-glm-all-1.png)
For the combined station “Netherlands (without Delfzijl)” GLM and GAM
overlap to a very high extend. In fact, the difference is hard to see

## Results for the average sea level at the Dutch coast

``` r
plot_comparison(by_station_model_compared %>%
                  filter(station %in% params$selected_station )
)
```

<figure>
<img src="gam-knmi-analyse_files/figure-gfm/gam-glm-combined-1.png"
alt="Comparison between GAM and GLM model fits for all Dutch stations, including the combined stations for the Dutch coast without Delfzijl." />
<figcaption aria-hidden="true">Comparison between GAM and GLM model fits
for all Dutch stations, including the combined stations for the Dutch
coast without Delfzijl.</figcaption>
</figure>

## Estimating sea level rate along the Dutch coast

The plot below shows the sea level rate in time as estimated using GAM
and GLM (broken linear) for the combined station “Netherlands (without
Delfzijl)”. The sea level rate change over time is of course smoother
for the GAM model. However, the two signals are surprisingly alike. It
is furthermore evident that the confidence interval of the GAM model
increase towards the end of the time series

<details>

<summary>

Show code for plot generation
</summary>

``` r
plot_derivatives <- function(by_station_model_compared) {

  # unnest derivatives én metadata
  df_plot <- by_station_model_compared %>%
    select(station, modeltype, derivatives) %>%
    unnest(c(derivatives))

  ggplot(df_plot, aes(x = x, y = derivative, color = modeltype)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = modeltype),
                alpha = 0.2) +
    geom_line(size = 0.7) +
    # facet_wrap(~ station, scales = "free_y") +
    theme_bw() +
    labs(
      title = "Afgeleiden d(height)/d(year)",
      subtitle = "Vergelijking stations",
      x = "Jaar",
      y = "Afgeleide (trend in mm per jaar)",
      color = "modeltype"
    ) +
    coord_cartesian(ylim = c(0,NA))
}
```

</details>

``` r
by_station_model_compared %>%
  filter(station %in% params$selected_station ) %>%
  plot_derivatives() +
  facet_wrap("station")
```

<figure>
<img src="gam-knmi-analyse_files/figure-gfm/plot-derivatives-1.png"
alt="Derivatives plus confidence intervals." />
<figcaption aria-hidden="true">Derivatives plus confidence
intervals.</figcaption>
</figure>

## AIC comparison

AIC can now be compared between the models. It appears that for all
individual stations the GAM model mostly gives a lower AIC (and
therefore a better fitting model). This is most notably for stations
Harlingen, Hoek van Holland and Vlissingen. However, for the average
station “Netherlands (without Delfzijl)”, the GLM model performs
slightly better (lower AIC).

``` r
by_station_model_compared %>%
  select(station, modeltype, adj.rsq, AIC) %>%
  ggplot(aes(station, AIC, color = modeltype)) +
  geom_point(shape = "|", size = 6) +
  coord_flip()
```

![](gam-knmi-analyse_files/figure-gfm/aic-1.png)<!-- -->

## Conclusions

A first trial to compare GAM and GLM reveals that both models describe
the sea level change at the Dutch coast well, and the difference between
them in terms of AIC is smalll. For individual stations, the differences
are larger, where GAM better describes the variation for stations
Vlissingen, Hoek van Holland, IJmuiden, and Harlingen.

## Compare to analyses by other institutes

In het volgende deel worden ook de berekende trendschattingen van KNMI
en PBL geintegreerd. Zowel KNMI als PBL rekenden dit jaar, op basis van
hun eigen analysemethoden, de zeespiegelstijging uit over de jaren.
Onderstaande figuur geeft hier een beeld van. Voor de duidelijkheid
hebben we hier alleen de GAM van KNMi getoond.

``` r
# read knmi slr data (produced 2026)
knmi_gam <- read_delim("../../data/knmi/gam/NetherlandsNoDelfzijl/1000/rates_quantiles_NearestPointAverage2.csv", delim = ",") %>%
  select(
    x = time,
    derivative = `0.5`,
    lower = `0.05`,
    upper = `0.95`
  ) %>%
  mutate(
    modeltype = "knmi_gam",
    station = "Netherlands (without Delfzijl)"
  )
```

``` r
# read pbl slr data (produced 2026)
pbl_link <- "../../data/pbl/slm_pbl_2026/0229_001g_clo_11_nlv5schoon.xlsx"
readxl::excel_sheets(pbl_link)
```

    FALSE [1] "INFO"                           "Zeespiegel voor kust Nederland"
    FALSE [3] "versnelling"

``` r
pbl_irw <- readxl::read_xlsx(pbl_link, sheet = "versnelling", skip = 8, col_names = c("year", "rate", "lower", "upper")) %>%
    select(
    x = year,
    derivative = rate,
    lower = lower,
    upper = upper
  ) %>%
  mutate(
    modeltype = "pbl_irw",
    station = "Netherlands (without Delfzijl)"
  )
```

``` r
  df_plot <- by_station_model_compared %>%
    select(station, modeltype, derivatives) %>%
    unnest(c(derivatives)) %>%
  filter(
    station %in% params$selected_station,
    modeltype %in% c("glm", "glm2")) %>%
  select(
    x,
    station,
    modeltype,
    derivative, 
    lower,
    upper
  ) %>%
  bind_rows(knmi_gam) %>%
  bind_rows(pbl_irw) 

  ggplot(df_plot, aes(x = x, y = derivative, color = modeltype)) +
    geom_ribbon(
      aes(
        ymin = lower, 
        ymax = upper, 
        fill = modeltype
      ),
      alpha = 0.1,
      linewidth = 0.3
    ) +
    geom_line(size = 1.2) +
    # facet_wrap(~ station, scales = "free_y") +
    theme_bw() +
    labs(
      title = "trends inclusief 95% betrouwbaarheid",
      subtitle = "Vergelijking methodieken",
      x = "Jaar",
      y = "trend in mm per jaar",
      color = "modeltype"
    ) +
    coord_cartesian(xlim = c(1960, NA), ylim = c(0,NA))
```

<figure>
<img src="gam-knmi-analyse_files/figure-gfm/plot-derivatives_all-1.png"
alt="Sea level rates" />
<figcaption aria-hidden="true">Sea level rates</figcaption>
</figure>

``` r
ggsave("../../rapportage/slm2026/images/vergelijking_alle_modellen.png", 
       width = 7, height = 5)
```

## References

Keizer, Iris, Dewi Le Bars, Cees De Valk, André Jüling, Roderik Van De
Wal, and Sybren Drijfhout. 2023. “The Acceleration of Sea-Level Rise
along the Coast of the Netherlands Started in the 1960s.” Ocean Science
19 (4): 991–1007. <https://doi.org/10.5194/os-19-991-2023>.

## Additinal analysis

``` r
by_station_model_compared %>% 
  unnest(data, pred_height) %>% 
  select(-modelfunctions, - model, - glance, -tidy, - augment) %>% 
  unnest(fit_height, se_height) %>% 
  ggplot(aes(x = year)) + 
  geom_point(aes(y = height), alpha = 0.1) + 
  geom_line(aes(y = fit_height, color = modeltype), linewidth = 1) +
  facet_wrap("station")
```

## Appendix

``` r
by_station_model_compared %>%
  filter(!grepl("Netherlands", station)) %>%
  plot_derivatives() +
  facet_wrap("station")
```

<figure>
<img src="gam-knmi-analyse_files/figure-gfm/plot-derivatives-all-1.png"
alt="Derivatives plus confidence intervals for individual stations." />
<figcaption aria-hidden="true">Derivatives plus confidence intervals for
individual stations.</figcaption>
</figure>
