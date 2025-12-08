Zeespiegelmonitor analysis
================
Willem Stolte, Nathalie Dees
08 December, 2025

# Sea Level Monitor analysis

The Sea Level Monitor methodology is described in detail in Deltares
(2023). Principles are:

- Sea Level is based on yearly averaged sea levels that are reported by
  Rijkswaterstaat to [PSMSL](https://www.psmsl.org) for the six main
  stations.
- The mean of these stations is used to estimate the “current sea-level
  rise”. The measurements since 1890 are taken into account.
  Measurements before that are considered less valid because the
  Amsterdam Ordnance Datum was not yet normalized.
- Sea Level is corrected for yearly fluctuations in *surge* using
  independently modelled surge values from the Global Surge and Tide
  Model (GTSM).
- The trend of Sea Level over the years is fitted with various models
  including nodal tide, with anD without an acceleration term. For the
  models with accelleration terms, the significance of the accelleration
  is tested.
- From the different models a preferred model is chosen based on:
  - lowest AIC (Akaike Information Criteria)
  - significant better fit than the simplest model (linear model)

This document is executed with the following parameters:

``` r
data.frame(
  "name" = names(unlist(params)),
  "value" = unlist(params),
  row.names = NULL
  ) %>%
  knitr::kable(caption = "Values of document parameters")
```

| name               | value                          |
|:-------------------|:-------------------------------|
| monitoryear        | 2025                           |
| startyear          | 1890                           |
| wind_or_surge_type | GTSM                           |
| overwrite          | FALSE                          |
| station1           | Delfzijl                       |
| station2           | Harlingen                      |
| station3           | Den Helder                     |
| station4           | IJmuiden                       |
| station5           | Hoek van Holland               |
| station6           | Vlissingen                     |
| station7           | Netherlands                    |
| station8           | Netherlands (without Delfzijl) |
| modeltype1         | linear                         |
| modeltype2         | broken_linear                  |
| modeltype3         | broken_squared                 |

Values of document parameters

## Get latest data from file

In an other script, annual average sea level data for the Dutch main
stations is downloaded from the [Permanent Service for Mean Sea Level
site](http://www.psmsl.org) and combined with the Global Tide and Surge
Model (GTSM) annual average surge values.

``` r
current_df <-   read_delim(
  "../../data/deltares/results/dutch-sea-level-monitor-export-stations-latest.csv", 
  delim = ";") %>%
  filter(year >= params$startyear)
```

In this analysis, measurements over the period 1890 to 2024 are
considered.

## Locations of the main stations

This document analyses the sea level trend of the main stations in the
Netherlands using different models. Based on geographical coverage and
available time series length six stations are considered to be “main
tide gauge stations”.

Additionally, to calculate the average sea level and sea level trend
along the Dutch Coast, the main station sea level is averaged for each
year (virtual station “Netherlands”). Because the station “Delfzijl” has
a considerable gap in vertical adjustment for subsidence, we also
consider a variant in which Delfzijl is omitted from the main analysis
selection (“Netherlands (without Delfzijl)”).

``` r
map_stations(df = current_df, mainstations_df = mainstations_df, mainstations_locs = mainstations_locs)
```

<figure>
<img src="sealevelanalysis_files/figure-gfm/selected-stations-1.png"
alt="Hoofdgetijstations in Nederland. Er is aangegeven welke stations zijn meegenomen in dit rekendocument." />
<figcaption aria-hidden="true">Hoofdgetijstations in Nederland. Er is
aangegeven welke stations zijn meegenomen in dit
rekendocument.</figcaption>
</figure>

### Sea level measurements

In this section we look at sea level measurements. The global collection
of tide gauge records at the PSMSL was used to access the data. There
are two types of datasets the “Revised Local Reference” and “Metric”.
For the Netherlands the difference is that the “Revised Local Reference”
undoes the corrections from the NAP correction in 2005, to get a
consistent dataset. Here we transform the RLR back to NAP (without
undoing the correction).

The rlrnap computes the rlr back to latest NAP (ignoring the undoing of
the NAP correction). Id’s are the station ids in the PSMSL dataset. They
may change from year to year as the PSMSL 0 point is arbitary. You can
lookup the relevant parameters in the schematic diagram like this [LRL
diagram for station
Vlissingen](https://www.psmsl.org/data/obtaining/rlr.diagrams/20.php)

``` r
knitr::kable(mainstations_df[,c('name', 'psmsl_id', 'msl-rlr', 'msl-nap', 'nap-rlr')], caption = "PSMSL information on the six mains tide gauge stations in the Netherlands.")
```

| name             | psmsl_id | msl-rlr | msl-nap | nap-rlr |
|:-----------------|:---------|:--------|:--------|:--------|
| Vlissingen       | 20       | 6976    | 46      | 6930    |
| Hoek van Holland | 22       | 6987    | 114     | 6873    |
| Den Helder       | 23       | 6962    | 16      | 6946    |
| Delfzijl         | 24       | 6953    | 130     | 6823    |
| Harlingen        | 25       | 7024    | 110     | 6914    |
| IJmuiden         | 32       | 7014    | 64      | 6950    |

PSMSL information on the six mains tide gauge stations in the
Netherlands.

Sea level data for all six main station are shown below in an
interactive plot.

``` r
p <- current_df %>%
  dplyr::filter(!grepl("Netherlands", station)) %>%
ggplot(aes(year, height)) +
  geom_point(alpha = 1, aes(color = station), shape = 21, fill = "white", size = 1) +
  geom_line(alpha = 0.5, aes(color = station), linewidth = 0.5) +
  xlab("jaar") + ylab("gemeten zeespiegel in mm") +
  theme_light() +
  theme(legend.position = "bottom")

p
```

<figure>
<img src="sealevelanalysis_files/figure-gfm/zeespiegelmetingen-1.png"
alt="Yearly averaged sea level for the six main tidal stations in the Netherlands." />
<figcaption aria-hidden="true">Yearly averaged sea level for the six
main tidal stations in the Netherlands.</figcaption>
</figure>

``` r
# ggplotly(p) %>% layout(legend = list(x = 0.05, y = 0.95))
```

Sea level measurements for the six main stations (yearly average) are
shown in figure @ref(fig:zeespiegelmetingen) as deviations from the mean
for each year. This way, the characteristic patterns of the stations are
easier to see.

A few things become clear

- Harlingen show a lower trend as compared to the average of the
  stations from roughly 1930 to 1990.
- IJmuiden shows strong fluctuations in the first decades of the series
  that are not seen at the other stations. in 2018, the signal drops a
  few cm.
- In all series, the two most recent years show very high sea levels.

``` r
current_df %>%
  dplyr::filter(!grepl("Netherlands", station)) %>%
  group_by(year) %>%
  mutate(mean = mean(height)) %>% ungroup() %>%
  mutate(norm_station_height = height - mean) %>%
  ggplot(aes(year, norm_station_height, color = station)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.2) +
  facet_wrap(facets = "station", ncol = 2) +
  theme(legend.position = "bottom")
```

<figure>
<img src="sealevelanalysis_files/figure-gfm/zeespiegelanomalieen-1.png"
alt="Afwijking van het gemiddelde voor elk jaar voor de zes hoofdstations langs de Nederlandse kust." />
<figcaption aria-hidden="true">Afwijking van het gemiddelde voor elk
jaar voor de zes hoofdstations langs de Nederlandse kust.</figcaption>
</figure>

``` r
# p
```

``` r
p <- current_df %>%
  dplyr::filter(grepl("Netherlands", station)) %>%
ggplot(aes(year, height)) +
  geom_point(alpha = 1, aes(color = station), shape = 21, fill = "white", size = 1) +
  geom_line(alpha = 0.5, aes(color = station), linewidth = 0.75) +
  xlab("jaar") + ylab("gemeten zeespiegel in mm") +
  theme_light() +
  theme(legend.position = "bottom")

# ggplotly(p) %>% layout(legend = list(x = 0.05, y = 0.95))
p
```

<figure>
<img
src="sealevelanalysis_files/figure-gfm/zeespiegelmetingenGemiddeld-1.png"
alt="Jaarlijks gemiddelde zeespiegel voor gemiddelde van stations langs de Nederlandse kust." />
<figcaption aria-hidden="true">Jaarlijks gemiddelde zeespiegel voor
gemiddelde van stations langs de Nederlandse kust.</figcaption>
</figure>

### Sea level high years

The 5 years with highest sea levels are shown in table
@ref(tab:highest5years).

``` r
current_df %>%
  dplyr::filter(station == "Netherlands (without Delfzijl)") %>%
  dplyr::arrange(-height) %>%
  dplyr::select(year, station, height_mm = height) %>%
  # dplyr::group_by(station) %>%
  dplyr::slice(c(1:5)) %>%
  knitr::kable(caption = "Overview of the five highest yearly average water levels for the combined station Netherlands (without Delfzijl) in mm during the considered period. ")
```

| year | station                        | height_mm |
|-----:|:-------------------------------|----------:|
| 2024 | Netherlands (without Delfzijl) |     161.4 |
| 2023 | Netherlands (without Delfzijl) |     152.8 |
| 2020 | Netherlands (without Delfzijl) |      96.6 |
| 2022 | Netherlands (without Delfzijl) |      94.6 |
| 2017 | Netherlands (without Delfzijl) |      94.2 |

Overview of the five highest yearly average water levels for the
combined station Netherlands (without Delfzijl) in mm during the
considered period.

### Storm surge

The expected storm surge per year is determined using output of the
Global Tide and Surge Model (GTSM) (ref). This calculates the surge
given the bathymetry and climatic conditions. Model results are
available from 1950 onwards (figure @ref(fig:gtsm-surge)). The model is
run each successive year to calculate the annual average wind and air
pressure for use in the Sea Level Monitor. The calculated variation in
surge (surge anomaly) is subtracted from the measured sea level before
the sea level rise is calculated. For the years before 1950, no runs are
available and sea level is corrected for average surge only. This
correction reduces the variation due to differences in surge per year
and allows a more precise estimate of the long-term trend to be made.

``` r
p <- current_df %>%
  # filter(!grepl("Netherlands", station)) %>%
  # filter(station == "IJmuiden") %>%
ggplot(aes(year, surge_anomaly)) +
  geom_point(alpha = 1, aes(color = station), shape = 21, fill = "white", size = 1) +
  geom_line(alpha = 0.5, aes(color = station), linewidth = 0.75) +
  xlab("jaar") + ylab("windopzet in mm") +
  theme_light() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(1945, params$monitoryear))

ggplotly(p) # %>% layout(legend = list(x = 0.05, y = 0.95))
```

<figure>
<img src="sealevelanalysis_files/figure-gfm/gtsm-surge-anomalie-1.png"
alt="Modelled surge anomaly (deviation of storm surge from long year avearge). The yearly averages surge is calculated for 1950 - now. For earlier years, an average surge is assumed." />
<figcaption aria-hidden="true">Modelled surge anomaly (deviation of
storm surge from long year avearge). The yearly averages surge is
calculated for 1950 - now. For earlier years, an average surge is
assumed.</figcaption>
</figure>

``` r
# p
```

## Trend analysis

The sea level trend is calculated using generalized linear regression.
the following models are tested (Deltares 2018 and Deltares 2023):

- linear model
- broken linear model (with breakpoint at 1993)
- broken quadratic model (breakpoint at 1960)

The trend is calculated for all stations individually, for the mean of
all stations (“Netherlands”), and for means of all stations minus
station Delfzijl (“Netherlands without Delfzijl”).

In the regression equation, nodal tide is one of the components and is
estimated as a sinusoid curve with a period of 18.6 years.

``` r
byStation <- current_df %>%
  filter(year > params$startyear) %>%
  dplyr::group_by(station) %>%
  tidyr::nest() %>%
  dplyr::ungroup()
```

``` r
selectedmodel <- params$modeltype

models <- byStation %>%
  expand_grid(modeltype = selectedmodel) %>%

  mutate(modelfunctionname = paste(modeltype, "model", sep = "_")) %>%
  # add functions for model calculation
  mutate(modelfunctions = map(modelfunctionname, get)) %>%
  # add models based on data and functions
  mutate(model = pmap(
    list(
      data,
      modelfunctions
    ),
    \(.d, .f) .f(.d)
  )) %>%
  mutate(
    glance = map(model, broom::glance),
    rsq    = glance %>% map_dbl("r.squared"),
    adj.rsq = glance %>% map_dbl("adj.r.squared"),
    AIC    = glance %>% map_dbl("AIC"),
    tidy   = map(model, broom::tidy),
    augment = map(model, broom::augment),
    equation = map(model, function(x) equatiomatic::extract_eq(x, ital_vars = TRUE))
  )
```

``` r
eq <- models %>% 
  distinct(modeltype, equation) %>%
  mutate(equation = paste0("$`", equation, "`$"))

knitr::kable(eq, escape = F)
```

| modeltype | equation |
|:---|:---|
| linear | $``height = \alpha + \beta_{1}(year\ -\ epoch) + \beta_{2}(cos(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \beta_{3}(sin(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \epsilon``$ |
| broken_linear | $``height = \alpha + \beta_{1}(year\ -\ epoch) + \beta_{2}(from1993) + \beta_{3}(cos(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \beta_{4}(sin(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \epsilon``$ |
| broken_squared | $``height = \alpha + \beta_{1}(year\ -\ epoch) + \beta_{2}(from1960\_square) + \beta_{3}(cos(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \beta_{4}(sin(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \epsilon``$ |

## Autocorrelation

Autocorrelation with previous year(s) can sometimes explain part of the
otherwise unexplained variance. Especially when a trend is detected in
the data, autocorrelation with relatively short lags (1 or few years)
often occurs. In case of autocorrelation, we recalculate standard errors
of the estimated parameters accordingly.

``` r
plot_ACF(models)
```

<figure>
<img src="sealevelanalysis_files/figure-gfm/acf-plot-1.png"
alt="Autocorrelation plot for selected stations and models." />
<figcaption aria-hidden="true">Autocorrelation plot for selected
stations and models.</figcaption>
</figure>

There appears to be a consistent autocorrelation for all stations and
models with a ‘lag’ of one year. The autocorrelation does not influence
the value of the calculated trend parameters, but needs to be taken into
account when calculating standard errors. The [Newey West autocorrelatie
term](https://search.r-project.org/CRAN/refmans/sandwich/html/NeweyWest.html)
is used to correctly calculate the standard errors.

At station ‘Vlissingen’ there is autocorrelation with a ‘lag’ of 10
years. This could indicate an effect of the 8.8 year ‘lunar perigee
cycle’. Because this only occurs at Vlissingen, this tidal component is
not further accounted for in the analysis.

There is no apparent autocorrelation with a ‘lag’ of 18.6 years, the
‘nodal tide’ cycle. This is because the nodal tide is already
incorporated in the three models.

``` r
require(sandwich)

models <- addHACterms(models)
```

## Heteroskedasticity

### Residuals distribution

The distribution of residuals resembles a normal distribution for most
stations and model variants. Station Harlingen is an example where the
distribution is out of centre when using the linear model. The width of
the residuals distribution is narrower when measurements are corrected
for surge prior to application of the models.

``` r
plotResidualDistribution(models)
```

<img src="sealevelanalysis_files/figure-gfm/unnamed-chunk-6-1.png" width="100%" />

### Variation of residuals over time

Distribution of residuals should not reveal a clear deviation from a
horizontal line.

``` r
models %>%
  unnest(c(data, augment), names_sep = "_") %>%
  ggplot(aes(data_year, augment_.resid)) +
  geom_point(alpha = 0.4) +
  facet_grid(station ~ modeltype) +
  theme(strip.text.y = element_text(angle = 0))
```

![](sealevelanalysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Sea level rise

``` r
lookup <- c(
  Constant = "(Intercept)",
  Trend = "I(year - epoch)",
  u_nodal = "I(cos(2 * pi * (year - epoch)/(18.613)))",
  v_nodal = "I(sin(2 * pi * (year - epoch)/(18.613)))",
  `+ trend 1993` = "from1993",
  `+ square_trend 1960` = "from1960_square",
  AR_term = "previousYearHeight"
)

all_predictions <- makePredictionTable(models, lookup)

if(params$overwrite){
  write_csv(all_predictions, file = paste0("../../results/analysis_output/predictions_", today(), ".csv"))
  write_csv(all_predictions, file = "../../results/analysis_output/predictions_latest.csv")
}
```

``` r
ggplot(
  all_predictions,
  aes(x = data_year)
) +
  geom_point(aes(y = data_height, color = "observed"), alpha = 0.5, size = 1) +
  geom_line(aes(y = prediction_recalc, color = "predicted"), linewidth = 1) +
  facet_grid(station ~ modeltype) +
  theme(strip.text.y = element_text(angle = 0))
```

<figure>
<img src="sealevelanalysis_files/figure-gfm/prediction-plot-1.png"
alt="Observed and predicted sea level for selected stations and models." />
<figcaption aria-hidden="true">Observed and predicted sea level for
selected stations and models.</figcaption>
</figure>

``` r
  p <- plot_station( 
    predictions_all = all_predictions,
    stationi = unique(all_predictions$station),
    correctionVariant = "GTSM", 
    modelVariant = unique(all_predictions$modeltype), 
    printNumbers = F, 
    startyear = 1890
  ) +
  facet_grid(station ~ modeltype) +
  theme(
    # legend.direction = "horizontal",
    # legend.box = "horizontal",
    legend.position = "bottom", #c(0.975, 0.025),
    # legend.justification = c(1, 0),
    legend.title = element_blank()
  ) +
  theme(strip.text.y = element_text(angle = 90)) +
  theme_bw()

  
  # ggplotly(p) %>% layout(legend = list(x = 0.05, y = 0.95))
  p
```

## Parameters

``` r
## gebruik DT in plaats van kableextra
# library(DT)

lookup.df <- data.frame(long_term = unname(lookup),
                        short_term = names(lookup))

parametertable <- models %>%
  select(station, modeltype, tidy) %>% 
  unnest(tidy) %>%
  left_join(models %>%
              select(station, modeltype, tidy.HAC) %>%
              unnest(tidy.HAC),
            by = c(
              station = "station",
              modeltype = "modeltype",
              term = "term.HAC"
            )
  ) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  left_join(lookup.df, by = c(term = "long_term")) %>%
  select(-term, term = short_term) %>%
  relocate(term, .after = modeltype)

# parametertable %>%
#   DT::datatable(
#     options = list(
#       "digits" = 3
#     )
#   )

if(params$overwrite){
  write_csv(parametertable, file = paste0("../../results/analysis_output/parameters_", today(), ".csv"))
  write_csv(parametertable, file = "../../results/analysis_output/parameters_latest.csv")
}
  kableExtra::kable(parametertable,
    caption = "Coefficients for all models and stations.",digits = 2
    ) %>%
  kableExtra::scroll_box(height = "500px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:500px; ">

<table>

<caption>

Coefficients for all models and stations.
</caption>

<thead>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

station
</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

modeltype
</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">

term
</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

estimate
</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

std.error
</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

statistic
</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

p.value
</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">

st.err.HAC
</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Vlissingen
</td>

<td style="text-align:left;">

linear
</td>

<td style="text-align:left;">

Constant
</td>

<td style="text-align:right;">

-62.78
</td>

<td style="text-align:right;">

2.26
</td>

<td style="text-align:right;">

-27.76
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

2.42
</td>

</tr>

<tr>

<td style="text-align:left;">

Vlissingen
</td>

<td style="text-align:left;">

linear
</td>

<td style="text-align:left;">

Trend
</td>

<td style="text-align:right;">

2.42
</td>

<td style="text-align:right;">

0.06
</td>

<td style="text-align:right;">

43.47
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

0.07
</td>

</tr>

<tr>

<td style="text-align:left;">

Vlissingen
</td>

<td style="text-align:left;">

linear
</td>

<td style="text-align:left;">

u_nodal
</td>

<td style="text-align:right;">

5.85
</td>

<td style="text-align:right;">

3.07
</td>

<td style="text-align:right;">

1.91
</td>

<td style="text-align:right;">

0.06
</td>

<td style="text-align:right;">

3.51
</td>

</tr>

<tr>

<td style="text-align:left;">

Vlissingen
</td>

<td style="text-align:left;">

linear
</td>

<td style="text-align:left;">

v_nodal
</td>

<td style="text-align:right;">

-13.18
</td>

<td style="text-align:right;">

3.03
</td>

<td style="text-align:right;">

-4.35
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

3.76
</td>

</tr>

<tr>

<td style="text-align:left;">

Vlissingen
</td>

<td style="text-align:left;">

broken_linear
</td>

<td style="text-align:left;">

Constant
</td>

<td style="text-align:right;">

-66.69
</td>

<td style="text-align:right;">

2.98
</td>

<td style="text-align:right;">

-22.41
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

3.02
</td>

</tr>

<tr>

<td style="text-align:left;">

Vlissingen
</td>

<td style="text-align:left;">

broken_linear
</td>

<td style="text-align:left;">

Trend
</td>

<td style="text-align:right;">

2.32
</td>

<td style="text-align:right;">

0.07
</td>

<td style="text-align:right;">

31.10
</td>

<td style="text-align:right;">

0.00
</td>

<td style="text-align:right;">

0.10
</td>

</tr>

<tr>

<td style="text-align:left;">

Vlissingen
</td>

<td style="text-align:left;">

broken_linear
</td>

<td style="text-align:left;">

- trend 1993
  </td>

  <td style="text-align:right;">

  0.72
  </td>

  <td style="text-align:right;">

  0.36
  </td>

  <td style="text-align:right;">

  1.99
  </td>

  <td style="text-align:right;">

  0.05
  </td>

  <td style="text-align:right;">

  0.36
  </td>

  </tr>

  <tr>

  <td style="text-align:left;">

  Vlissingen
  </td>

  <td style="text-align:left;">

  broken_linear
  </td>

  <td style="text-align:left;">

  u_nodal
  </td>

  <td style="text-align:right;">

  5.88
  </td>

  <td style="text-align:right;">

  3.03
  </td>

  <td style="text-align:right;">

  1.94
  </td>

  <td style="text-align:right;">

  0.06
  </td>

  <td style="text-align:right;">

  3.41
  </td>

  </tr>

  <tr>

  <td style="text-align:left;">

  Vlissingen
  </td>

  <td style="text-align:left;">

  broken_linear
  </td>

  <td style="text-align:left;">

  v_nodal
  </td>

  <td style="text-align:right;">

  -12.48
  </td>

  <td style="text-align:right;">

  3.01
  </td>

  <td style="text-align:right;">

  -4.14
  </td>

  <td style="text-align:right;">

  0.00
  </td>

  <td style="text-align:right;">

  3.79
  </td>

  </tr>

  <tr>

  <td style="text-align:left;">

  Vlissingen
  </td>

  <td style="text-align:left;">

  broken_squared
  </td>

  <td style="text-align:left;">

  Constant
  </td>

  <td style="text-align:right;">

  -65.56
  </td>

  <td style="text-align:right;">

  3.80
  </td>

  <td style="text-align:right;">

  -17.24
  </td>

  <td style="text-align:right;">

  0.00
  </td>

  <td style="text-align:right;">

  4.36
  </td>

  </tr>

  <tr>

  <td style="text-align:left;">

  Vlissingen
  </td>

  <td style="text-align:left;">

  broken_squared
  </td>

  <td style="text-align:left;">

  Trend
  </td>

  <td style="text-align:right;">

  2.36
  </td>

  <td style="text-align:right;">

  0.09
  </td>

  <td style="text-align:right;">

  25.67
  </td>

  <td style="text-align:right;">

  0.00
  </td>

  <td style="text-align:right;">

  0.13
  </td>

  </tr>

  <tr>

  <td style="text-align:left;">

  Vlissingen
  </td>

  <td style="text-align:left;">

  broken_squared
  </td>

  <td style="text-align:left;">

  - square_trend 1960
    </td>

    <td style="text-align:right;">

    0.00
    </td>

    <td style="text-align:right;">

    0.00
    </td>

    <td style="text-align:right;">

    0.91
    </td>

    <td style="text-align:right;">

    0.36
    </td>

    <td style="text-align:right;">

    0.00
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Vlissingen
    </td>

    <td style="text-align:left;">

    broken_squared
    </td>

    <td style="text-align:left;">

    u_nodal
    </td>

    <td style="text-align:right;">

    5.82
    </td>

    <td style="text-align:right;">

    3.07
    </td>

    <td style="text-align:right;">

    1.90
    </td>

    <td style="text-align:right;">

    0.06
    </td>

    <td style="text-align:right;">

    3.50
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Vlissingen
    </td>

    <td style="text-align:left;">

    broken_squared
    </td>

    <td style="text-align:left;">

    v_nodal
    </td>

    <td style="text-align:right;">

    -12.91
    </td>

    <td style="text-align:right;">

    3.04
    </td>

    <td style="text-align:right;">

    -4.24
    </td>

    <td style="text-align:right;">

    0.00
    </td>

    <td style="text-align:right;">

    3.79
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Hoek van Holland
    </td>

    <td style="text-align:left;">

    linear
    </td>

    <td style="text-align:left;">

    Constant
    </td>

    <td style="text-align:right;">

    1.76
    </td>

    <td style="text-align:right;">

    2.27
    </td>

    <td style="text-align:right;">

    0.78
    </td>

    <td style="text-align:right;">

    0.44
    </td>

    <td style="text-align:right;">

    2.37
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Hoek van Holland
    </td>

    <td style="text-align:left;">

    linear
    </td>

    <td style="text-align:left;">

    Trend
    </td>

    <td style="text-align:right;">

    2.55
    </td>

    <td style="text-align:right;">

    0.06
    </td>

    <td style="text-align:right;">

    45.55
    </td>

    <td style="text-align:right;">

    0.00
    </td>

    <td style="text-align:right;">

    0.06
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Hoek van Holland
    </td>

    <td style="text-align:left;">

    linear
    </td>

    <td style="text-align:left;">

    u_nodal
    </td>

    <td style="text-align:right;">

    1.24
    </td>

    <td style="text-align:right;">

    3.08
    </td>

    <td style="text-align:right;">

    0.40
    </td>

    <td style="text-align:right;">

    0.69
    </td>

    <td style="text-align:right;">

    3.88
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Hoek van Holland
    </td>

    <td style="text-align:left;">

    linear
    </td>

    <td style="text-align:left;">

    v_nodal
    </td>

    <td style="text-align:right;">

    -9.56
    </td>

    <td style="text-align:right;">

    3.03
    </td>

    <td style="text-align:right;">

    -3.15
    </td>

    <td style="text-align:right;">

    0.00
    </td>

    <td style="text-align:right;">

    3.08
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Hoek van Holland
    </td>

    <td style="text-align:left;">

    broken_linear
    </td>

    <td style="text-align:left;">

    Constant
    </td>

    <td style="text-align:right;">

    -3.42
    </td>

    <td style="text-align:right;">

    2.95
    </td>

    <td style="text-align:right;">

    -1.16
    </td>

    <td style="text-align:right;">

    0.25
    </td>

    <td style="text-align:right;">

    3.23
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Hoek van Holland
    </td>

    <td style="text-align:left;">

    broken_linear
    </td>

    <td style="text-align:left;">

    Trend
    </td>

    <td style="text-align:right;">

    2.41
    </td>

    <td style="text-align:right;">

    0.07
    </td>

    <td style="text-align:right;">

    32.60
    </td>

    <td style="text-align:right;">

    0.00
    </td>

    <td style="text-align:right;">

    0.09
    </td>

    </tr>

    <tr>

    <td style="text-align:left;">

    Hoek van Holland
    </td>

    <td style="text-align:left;">

    broken_linear
    </td>

    <td style="text-align:left;">

    - trend 1993
      </td>

      <td style="text-align:right;">

      0.96
      </td>

      <td style="text-align:right;">

      0.36
      </td>

      <td style="text-align:right;">

      2.66
      </td>

      <td style="text-align:right;">

      0.01
      </td>

      <td style="text-align:right;">

      0.36
      </td>

      </tr>

      <tr>

      <td style="text-align:left;">

      Hoek van Holland
      </td>

      <td style="text-align:left;">

      broken_linear
      </td>

      <td style="text-align:left;">

      u_nodal
      </td>

      <td style="text-align:right;">

      1.28
      </td>

      <td style="text-align:right;">

      3.00
      </td>

      <td style="text-align:right;">

      0.42
      </td>

      <td style="text-align:right;">

      0.67
      </td>

      <td style="text-align:right;">

      3.75
      </td>

      </tr>

      <tr>

      <td style="text-align:left;">

      Hoek van Holland
      </td>

      <td style="text-align:left;">

      broken_linear
      </td>

      <td style="text-align:left;">

      v_nodal
      </td>

      <td style="text-align:right;">

      -8.64
      </td>

      <td style="text-align:right;">

      2.98
      </td>

      <td style="text-align:right;">

      -2.90
      </td>

      <td style="text-align:right;">

      0.00
      </td>

      <td style="text-align:right;">

      3.08
      </td>

      </tr>

      <tr>

      <td style="text-align:left;">

      Hoek van Holland
      </td>

      <td style="text-align:left;">

      broken_squared
      </td>

      <td style="text-align:left;">

      Constant
      </td>

      <td style="text-align:right;">

      -7.45
      </td>

      <td style="text-align:right;">

      3.69
      </td>

      <td style="text-align:right;">

      -2.02
      </td>

      <td style="text-align:right;">

      0.04
      </td>

      <td style="text-align:right;">

      4.26
      </td>

      </tr>

      <tr>

      <td style="text-align:left;">

      Hoek van Holland
      </td>

      <td style="text-align:left;">

      broken_squared
      </td>

      <td style="text-align:left;">

      Trend
      </td>

      <td style="text-align:right;">

      2.33
      </td>

      <td style="text-align:right;">

      0.09
      </td>

      <td style="text-align:right;">

      26.11
      </td>

      <td style="text-align:right;">

      0.00
      </td>

      <td style="text-align:right;">

      0.11
      </td>

      </tr>

      <tr>

      <td style="text-align:left;">

      Hoek van Holland
      </td>

      <td style="text-align:left;">

      broken_squared
      </td>

      <td style="text-align:left;">

      - square_trend 1960
        </td>

        <td style="text-align:right;">

        0.01
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        3.11
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Hoek van Holland
        </td>

        <td style="text-align:left;">

        broken_squared
        </td>

        <td style="text-align:left;">

        u_nodal
        </td>

        <td style="text-align:right;">

        1.14
        </td>

        <td style="text-align:right;">

        2.98
        </td>

        <td style="text-align:right;">

        0.38
        </td>

        <td style="text-align:right;">

        0.70
        </td>

        <td style="text-align:right;">

        3.65
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Hoek van Holland
        </td>

        <td style="text-align:left;">

        broken_squared
        </td>

        <td style="text-align:left;">

        v_nodal
        </td>

        <td style="text-align:right;">

        -8.69
        </td>

        <td style="text-align:right;">

        2.95
        </td>

        <td style="text-align:right;">

        -2.94
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        3.05
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Den Helder
        </td>

        <td style="text-align:left;">

        linear
        </td>

        <td style="text-align:left;">

        Constant
        </td>

        <td style="text-align:right;">

        -56.04
        </td>

        <td style="text-align:right;">

        2.44
        </td>

        <td style="text-align:right;">

        -22.92
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        2.74
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Den Helder
        </td>

        <td style="text-align:left;">

        linear
        </td>

        <td style="text-align:left;">

        Trend
        </td>

        <td style="text-align:right;">

        1.59
        </td>

        <td style="text-align:right;">

        0.06
        </td>

        <td style="text-align:right;">

        26.45
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        0.07
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Den Helder
        </td>

        <td style="text-align:left;">

        linear
        </td>

        <td style="text-align:left;">

        u_nodal
        </td>

        <td style="text-align:right;">

        5.69
        </td>

        <td style="text-align:right;">

        3.32
        </td>

        <td style="text-align:right;">

        1.72
        </td>

        <td style="text-align:right;">

        0.09
        </td>

        <td style="text-align:right;">

        3.44
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Den Helder
        </td>

        <td style="text-align:left;">

        linear
        </td>

        <td style="text-align:left;">

        v_nodal
        </td>

        <td style="text-align:right;">

        -14.35
        </td>

        <td style="text-align:right;">

        3.27
        </td>

        <td style="text-align:right;">

        -4.38
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        4.09
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Den Helder
        </td>

        <td style="text-align:left;">

        broken_linear
        </td>

        <td style="text-align:left;">

        Constant
        </td>

        <td style="text-align:right;">

        -65.05
        </td>

        <td style="text-align:right;">

        3.04
        </td>

        <td style="text-align:right;">

        -21.41
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        3.12
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Den Helder
        </td>

        <td style="text-align:left;">

        broken_linear
        </td>

        <td style="text-align:left;">

        Trend
        </td>

        <td style="text-align:right;">

        1.36
        </td>

        <td style="text-align:right;">

        0.08
        </td>

        <td style="text-align:right;">

        17.88
        </td>

        <td style="text-align:right;">

        0.00
        </td>

        <td style="text-align:right;">

        0.09
        </td>

        </tr>

        <tr>

        <td style="text-align:left;">

        Den Helder
        </td>

        <td style="text-align:left;">

        broken_linear
        </td>

        <td style="text-align:left;">

        - trend 1993
          </td>

          <td style="text-align:right;">

          1.66
          </td>

          <td style="text-align:right;">

          0.37
          </td>

          <td style="text-align:right;">

          4.49
          </td>

          <td style="text-align:right;">

          0.00
          </td>

          <td style="text-align:right;">

          0.37
          </td>

          </tr>

          <tr>

          <td style="text-align:left;">

          Den Helder
          </td>

          <td style="text-align:left;">

          broken_linear
          </td>

          <td style="text-align:left;">

          u_nodal
          </td>

          <td style="text-align:right;">

          5.76
          </td>

          <td style="text-align:right;">

          3.10
          </td>

          <td style="text-align:right;">

          1.86
          </td>

          <td style="text-align:right;">

          0.06
          </td>

          <td style="text-align:right;">

          3.06
          </td>

          </tr>

          <tr>

          <td style="text-align:left;">

          Den Helder
          </td>

          <td style="text-align:left;">

          broken_linear
          </td>

          <td style="text-align:left;">

          v_nodal
          </td>

          <td style="text-align:right;">

          -12.75
          </td>

          <td style="text-align:right;">

          3.08
          </td>

          <td style="text-align:right;">

          -4.14
          </td>

          <td style="text-align:right;">

          0.00
          </td>

          <td style="text-align:right;">

          3.86
          </td>

          </tr>

          <tr>

          <td style="text-align:left;">

          Den Helder
          </td>

          <td style="text-align:left;">

          broken_squared
          </td>

          <td style="text-align:left;">

          Constant
          </td>

          <td style="text-align:right;">

          -70.91
          </td>

          <td style="text-align:right;">

          3.79
          </td>

          <td style="text-align:right;">

          -18.72
          </td>

          <td style="text-align:right;">

          0.00
          </td>

          <td style="text-align:right;">

          4.10
          </td>

          </tr>

          <tr>

          <td style="text-align:left;">

          Den Helder
          </td>

          <td style="text-align:left;">

          broken_squared
          </td>

          <td style="text-align:left;">

          Trend
          </td>

          <td style="text-align:right;">

          1.24
          </td>

          <td style="text-align:right;">

          0.09
          </td>

          <td style="text-align:right;">

          13.55
          </td>

          <td style="text-align:right;">

          0.00
          </td>

          <td style="text-align:right;">

          0.11
          </td>

          </tr>

          <tr>

          <td style="text-align:left;">

          Den Helder
          </td>

          <td style="text-align:left;">

          broken_squared
          </td>

          <td style="text-align:left;">

          - square_trend 1960
            </td>

            <td style="text-align:right;">

            0.02
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            4.88
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Den Helder
            </td>

            <td style="text-align:left;">

            broken_squared
            </td>

            <td style="text-align:left;">

            u_nodal
            </td>

            <td style="text-align:right;">

            5.53
            </td>

            <td style="text-align:right;">

            3.06
            </td>

            <td style="text-align:right;">

            1.81
            </td>

            <td style="text-align:right;">

            0.07
            </td>

            <td style="text-align:right;">

            2.99
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Den Helder
            </td>

            <td style="text-align:left;">

            broken_squared
            </td>

            <td style="text-align:left;">

            v_nodal
            </td>

            <td style="text-align:right;">

            -12.93
            </td>

            <td style="text-align:right;">

            3.03
            </td>

            <td style="text-align:right;">

            -4.26
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            3.81
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Delfzijl
            </td>

            <td style="text-align:left;">

            linear
            </td>

            <td style="text-align:left;">

            Constant
            </td>

            <td style="text-align:right;">

            24.13
            </td>

            <td style="text-align:right;">

            2.95
            </td>

            <td style="text-align:right;">

            8.19
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            3.31
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Delfzijl
            </td>

            <td style="text-align:left;">

            linear
            </td>

            <td style="text-align:left;">

            Trend
            </td>

            <td style="text-align:right;">

            1.93
            </td>

            <td style="text-align:right;">

            0.07
            </td>

            <td style="text-align:right;">

            26.57
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            0.09
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Delfzijl
            </td>

            <td style="text-align:left;">

            linear
            </td>

            <td style="text-align:left;">

            u_nodal
            </td>

            <td style="text-align:right;">

            2.82
            </td>

            <td style="text-align:right;">

            4.00
            </td>

            <td style="text-align:right;">

            0.70
            </td>

            <td style="text-align:right;">

            0.48
            </td>

            <td style="text-align:right;">

            4.77
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Delfzijl
            </td>

            <td style="text-align:left;">

            linear
            </td>

            <td style="text-align:left;">

            v_nodal
            </td>

            <td style="text-align:right;">

            -14.93
            </td>

            <td style="text-align:right;">

            3.94
            </td>

            <td style="text-align:right;">

            -3.79
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            4.32
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Delfzijl
            </td>

            <td style="text-align:left;">

            broken_linear
            </td>

            <td style="text-align:left;">

            Constant
            </td>

            <td style="text-align:right;">

            12.09
            </td>

            <td style="text-align:right;">

            3.60
            </td>

            <td style="text-align:right;">

            3.36
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            3.29
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Delfzijl
            </td>

            <td style="text-align:left;">

            broken_linear
            </td>

            <td style="text-align:left;">

            Trend
            </td>

            <td style="text-align:right;">

            1.62
            </td>

            <td style="text-align:right;">

            0.09
            </td>

            <td style="text-align:right;">

            17.97
            </td>

            <td style="text-align:right;">

            0.00
            </td>

            <td style="text-align:right;">

            0.10
            </td>

            </tr>

            <tr>

            <td style="text-align:left;">

            Delfzijl
            </td>

            <td style="text-align:left;">

            broken_linear
            </td>

            <td style="text-align:left;">

            - trend 1993
              </td>

              <td style="text-align:right;">

              2.22
              </td>

              <td style="text-align:right;">

              0.44
              </td>

              <td style="text-align:right;">

              5.07
              </td>

              <td style="text-align:right;">

              0.00
              </td>

              <td style="text-align:right;">

              0.36
              </td>

              </tr>

              <tr>

              <td style="text-align:left;">

              Delfzijl
              </td>

              <td style="text-align:left;">

              broken_linear
              </td>

              <td style="text-align:left;">

              u_nodal
              </td>

              <td style="text-align:right;">

              2.91
              </td>

              <td style="text-align:right;">

              3.66
              </td>

              <td style="text-align:right;">

              0.79
              </td>

              <td style="text-align:right;">

              0.43
              </td>

              <td style="text-align:right;">

              4.43
              </td>

              </tr>

              <tr>

              <td style="text-align:left;">

              Delfzijl
              </td>

              <td style="text-align:left;">

              broken_linear
              </td>

              <td style="text-align:left;">

              v_nodal
              </td>

              <td style="text-align:right;">

              -12.80
              </td>

              <td style="text-align:right;">

              3.64
              </td>

              <td style="text-align:right;">

              -3.52
              </td>

              <td style="text-align:right;">

              0.00
              </td>

              <td style="text-align:right;">

              3.73
              </td>

              </tr>

              <tr>

              <td style="text-align:left;">

              Delfzijl
              </td>

              <td style="text-align:left;">

              broken_squared
              </td>

              <td style="text-align:left;">

              Constant
              </td>

              <td style="text-align:right;">

              6.14
              </td>

              <td style="text-align:right;">

              4.56
              </td>

              <td style="text-align:right;">

              1.34
              </td>

              <td style="text-align:right;">

              0.18
              </td>

              <td style="text-align:right;">

              4.33
              </td>

              </tr>

              <tr>

              <td style="text-align:left;">

              Delfzijl
              </td>

              <td style="text-align:left;">

              broken_squared
              </td>

              <td style="text-align:left;">

              Trend
              </td>

              <td style="text-align:right;">

              1.50
              </td>

              <td style="text-align:right;">

              0.11
              </td>

              <td style="text-align:right;">

              13.62
              </td>

              <td style="text-align:right;">

              0.00
              </td>

              <td style="text-align:right;">

              0.13
              </td>

              </tr>

              <tr>

              <td style="text-align:left;">

              Delfzijl
              </td>

              <td style="text-align:left;">

              broken_squared
              </td>

              <td style="text-align:left;">

              - square_trend 1960
                </td>

                <td style="text-align:right;">

                0.02
                </td>

                <td style="text-align:right;">

                0.00
                </td>

                <td style="text-align:right;">

                4.90
                </td>

                <td style="text-align:right;">

                0.00
                </td>

                <td style="text-align:right;">

                0.00
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Delfzijl
                </td>

                <td style="text-align:left;">

                broken_squared
                </td>

                <td style="text-align:left;">

                u_nodal
                </td>

                <td style="text-align:right;">

                2.62
                </td>

                <td style="text-align:right;">

                3.68
                </td>

                <td style="text-align:right;">

                0.71
                </td>

                <td style="text-align:right;">

                0.48
                </td>

                <td style="text-align:right;">

                4.53
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Delfzijl
                </td>

                <td style="text-align:left;">

                broken_squared
                </td>

                <td style="text-align:left;">

                v_nodal
                </td>

                <td style="text-align:right;">

                -13.22
                </td>

                <td style="text-align:right;">

                3.65
                </td>

                <td style="text-align:right;">

                -3.62
                </td>

                <td style="text-align:right;">

                0.00
                </td>

                <td style="text-align:right;">

                3.68
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Harlingen
                </td>

                <td style="text-align:left;">

                linear
                </td>

                <td style="text-align:left;">

                Constant
                </td>

                <td style="text-align:right;">

                6.03
                </td>

                <td style="text-align:right;">

                2.80
                </td>

                <td style="text-align:right;">

                2.15
                </td>

                <td style="text-align:right;">

                0.03
                </td>

                <td style="text-align:right;">

                3.54
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Harlingen
                </td>

                <td style="text-align:left;">

                linear
                </td>

                <td style="text-align:left;">

                Trend
                </td>

                <td style="text-align:right;">

                1.25
                </td>

                <td style="text-align:right;">

                0.07
                </td>

                <td style="text-align:right;">

                18.03
                </td>

                <td style="text-align:right;">

                0.00
                </td>

                <td style="text-align:right;">

                0.09
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Harlingen
                </td>

                <td style="text-align:left;">

                linear
                </td>

                <td style="text-align:left;">

                u_nodal
                </td>

                <td style="text-align:right;">

                1.95
                </td>

                <td style="text-align:right;">

                3.80
                </td>

                <td style="text-align:right;">

                0.51
                </td>

                <td style="text-align:right;">

                0.61
                </td>

                <td style="text-align:right;">

                4.48
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Harlingen
                </td>

                <td style="text-align:left;">

                linear
                </td>

                <td style="text-align:left;">

                v_nodal
                </td>

                <td style="text-align:right;">

                -12.30
                </td>

                <td style="text-align:right;">

                3.75
                </td>

                <td style="text-align:right;">

                -3.28
                </td>

                <td style="text-align:right;">

                0.00
                </td>

                <td style="text-align:right;">

                4.50
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Harlingen
                </td>

                <td style="text-align:left;">

                broken_linear
                </td>

                <td style="text-align:left;">

                Constant
                </td>

                <td style="text-align:right;">

                -8.82
                </td>

                <td style="text-align:right;">

                3.18
                </td>

                <td style="text-align:right;">

                -2.78
                </td>

                <td style="text-align:right;">

                0.01
                </td>

                <td style="text-align:right;">

                3.22
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Harlingen
                </td>

                <td style="text-align:left;">

                broken_linear
                </td>

                <td style="text-align:left;">

                Trend
                </td>

                <td style="text-align:right;">

                0.86
                </td>

                <td style="text-align:right;">

                0.08
                </td>

                <td style="text-align:right;">

                10.84
                </td>

                <td style="text-align:right;">

                0.00
                </td>

                <td style="text-align:right;">

                0.09
                </td>

                </tr>

                <tr>

                <td style="text-align:left;">

                Harlingen
                </td>

                <td style="text-align:left;">

                broken_linear
                </td>

                <td style="text-align:left;">

                - trend 1993
                  </td>

                  <td style="text-align:right;">

                  2.74
                  </td>

                  <td style="text-align:right;">

                  0.39
                  </td>

                  <td style="text-align:right;">

                  7.08
                  </td>

                  <td style="text-align:right;">

                  0.00
                  </td>

                  <td style="text-align:right;">

                  0.38
                  </td>

                  </tr>

                  <tr>

                  <td style="text-align:left;">

                  Harlingen
                  </td>

                  <td style="text-align:left;">

                  broken_linear
                  </td>

                  <td style="text-align:left;">

                  u_nodal
                  </td>

                  <td style="text-align:right;">

                  2.07
                  </td>

                  <td style="text-align:right;">

                  3.24
                  </td>

                  <td style="text-align:right;">

                  0.64
                  </td>

                  <td style="text-align:right;">

                  0.52
                  </td>

                  <td style="text-align:right;">

                  3.69
                  </td>

                  </tr>

                  <tr>

                  <td style="text-align:left;">

                  Harlingen
                  </td>

                  <td style="text-align:left;">

                  broken_linear
                  </td>

                  <td style="text-align:left;">

                  v_nodal
                  </td>

                  <td style="text-align:right;">

                  -9.67
                  </td>

                  <td style="text-align:right;">

                  3.22
                  </td>

                  <td style="text-align:right;">

                  -3.01
                  </td>

                  <td style="text-align:right;">

                  0.00
                  </td>

                  <td style="text-align:right;">

                  3.63
                  </td>

                  </tr>

                  <tr>

                  <td style="text-align:left;">

                  Harlingen
                  </td>

                  <td style="text-align:left;">

                  broken_squared
                  </td>

                  <td style="text-align:left;">

                  Constant
                  </td>

                  <td style="text-align:right;">

                  -14.79
                  </td>

                  <td style="text-align:right;">

                  4.14
                  </td>

                  <td style="text-align:right;">

                  -3.58
                  </td>

                  <td style="text-align:right;">

                  0.00
                  </td>

                  <td style="text-align:right;">

                  4.57
                  </td>

                  </tr>

                  <tr>

                  <td style="text-align:left;">

                  Harlingen
                  </td>

                  <td style="text-align:left;">

                  broken_squared
                  </td>

                  <td style="text-align:left;">

                  Trend
                  </td>

                  <td style="text-align:right;">

                  0.75
                  </td>

                  <td style="text-align:right;">

                  0.10
                  </td>

                  <td style="text-align:right;">

                  7.49
                  </td>

                  <td style="text-align:right;">

                  0.00
                  </td>

                  <td style="text-align:right;">

                  0.12
                  </td>

                  </tr>

                  <tr>

                  <td style="text-align:left;">

                  Harlingen
                  </td>

                  <td style="text-align:left;">

                  broken_squared
                  </td>

                  <td style="text-align:left;">

                  - square_trend 1960
                    </td>

                    <td style="text-align:right;">

                    0.02
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    6.26
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    Harlingen
                    </td>

                    <td style="text-align:left;">

                    broken_squared
                    </td>

                    <td style="text-align:left;">

                    u_nodal
                    </td>

                    <td style="text-align:right;">

                    1.73
                    </td>

                    <td style="text-align:right;">

                    3.34
                    </td>

                    <td style="text-align:right;">

                    0.52
                    </td>

                    <td style="text-align:right;">

                    0.61
                    </td>

                    <td style="text-align:right;">

                    3.87
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    Harlingen
                    </td>

                    <td style="text-align:left;">

                    broken_squared
                    </td>

                    <td style="text-align:left;">

                    v_nodal
                    </td>

                    <td style="text-align:right;">

                    -10.32
                    </td>

                    <td style="text-align:right;">

                    3.31
                    </td>

                    <td style="text-align:right;">

                    -3.12
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    3.76
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    IJmuiden
                    </td>

                    <td style="text-align:left;">

                    linear
                    </td>

                    <td style="text-align:left;">

                    Constant
                    </td>

                    <td style="text-align:right;">

                    -42.15
                    </td>

                    <td style="text-align:right;">

                    2.61
                    </td>

                    <td style="text-align:right;">

                    -16.15
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    2.63
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    IJmuiden
                    </td>

                    <td style="text-align:left;">

                    linear
                    </td>

                    <td style="text-align:left;">

                    Trend
                    </td>

                    <td style="text-align:right;">

                    2.15
                    </td>

                    <td style="text-align:right;">

                    0.06
                    </td>

                    <td style="text-align:right;">

                    33.33
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    0.08
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    IJmuiden
                    </td>

                    <td style="text-align:left;">

                    linear
                    </td>

                    <td style="text-align:left;">

                    u_nodal
                    </td>

                    <td style="text-align:right;">

                    10.23
                    </td>

                    <td style="text-align:right;">

                    3.54
                    </td>

                    <td style="text-align:right;">

                    2.89
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    4.32
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    IJmuiden
                    </td>

                    <td style="text-align:left;">

                    linear
                    </td>

                    <td style="text-align:left;">

                    v_nodal
                    </td>

                    <td style="text-align:right;">

                    -12.57
                    </td>

                    <td style="text-align:right;">

                    3.49
                    </td>

                    <td style="text-align:right;">

                    -3.60
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    4.07
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    IJmuiden
                    </td>

                    <td style="text-align:left;">

                    broken_linear
                    </td>

                    <td style="text-align:left;">

                    Constant
                    </td>

                    <td style="text-align:right;">

                    -43.98
                    </td>

                    <td style="text-align:right;">

                    3.48
                    </td>

                    <td style="text-align:right;">

                    -12.64
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    3.41
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    IJmuiden
                    </td>

                    <td style="text-align:left;">

                    broken_linear
                    </td>

                    <td style="text-align:left;">

                    Trend
                    </td>

                    <td style="text-align:right;">

                    2.10
                    </td>

                    <td style="text-align:right;">

                    0.09
                    </td>

                    <td style="text-align:right;">

                    24.03
                    </td>

                    <td style="text-align:right;">

                    0.00
                    </td>

                    <td style="text-align:right;">

                    0.11
                    </td>

                    </tr>

                    <tr>

                    <td style="text-align:left;">

                    IJmuiden
                    </td>

                    <td style="text-align:left;">

                    broken_linear
                    </td>

                    <td style="text-align:left;">

                    - trend 1993
                      </td>

                      <td style="text-align:right;">

                      0.34
                      </td>

                      <td style="text-align:right;">

                      0.42
                      </td>

                      <td style="text-align:right;">

                      0.80
                      </td>

                      <td style="text-align:right;">

                      0.43
                      </td>

                      <td style="text-align:right;">

                      0.38
                      </td>

                      </tr>

                      <tr>

                      <td style="text-align:left;">

                      IJmuiden
                      </td>

                      <td style="text-align:left;">

                      broken_linear
                      </td>

                      <td style="text-align:left;">

                      u_nodal
                      </td>

                      <td style="text-align:right;">

                      10.25
                      </td>

                      <td style="text-align:right;">

                      3.55
                      </td>

                      <td style="text-align:right;">

                      2.89
                      </td>

                      <td style="text-align:right;">

                      0.00
                      </td>

                      <td style="text-align:right;">

                      4.34
                      </td>

                      </tr>

                      <tr>

                      <td style="text-align:left;">

                      IJmuiden
                      </td>

                      <td style="text-align:left;">

                      broken_linear
                      </td>

                      <td style="text-align:left;">

                      v_nodal
                      </td>

                      <td style="text-align:right;">

                      -12.25
                      </td>

                      <td style="text-align:right;">

                      3.52
                      </td>

                      <td style="text-align:right;">

                      -3.48
                      </td>

                      <td style="text-align:right;">

                      0.00
                      </td>

                      <td style="text-align:right;">

                      4.06
                      </td>

                      </tr>

                      <tr>

                      <td style="text-align:left;">

                      IJmuiden
                      </td>

                      <td style="text-align:left;">

                      broken_squared
                      </td>

                      <td style="text-align:left;">

                      Constant
                      </td>

                      <td style="text-align:right;">

                      -44.11
                      </td>

                      <td style="text-align:right;">

                      4.40
                      </td>

                      <td style="text-align:right;">

                      -10.03
                      </td>

                      <td style="text-align:right;">

                      0.00
                      </td>

                      <td style="text-align:right;">

                      4.70
                      </td>

                      </tr>

                      <tr>

                      <td style="text-align:left;">

                      IJmuiden
                      </td>

                      <td style="text-align:left;">

                      broken_squared
                      </td>

                      <td style="text-align:left;">

                      Trend
                      </td>

                      <td style="text-align:right;">

                      2.10
                      </td>

                      <td style="text-align:right;">

                      0.11
                      </td>

                      <td style="text-align:right;">

                      19.76
                      </td>

                      <td style="text-align:right;">

                      0.00
                      </td>

                      <td style="text-align:right;">

                      0.14
                      </td>

                      </tr>

                      <tr>

                      <td style="text-align:left;">

                      IJmuiden
                      </td>

                      <td style="text-align:left;">

                      broken_squared
                      </td>

                      <td style="text-align:left;">

                      - square_trend 1960
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        0.55
                        </td>

                        <td style="text-align:right;">

                        0.58
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        IJmuiden
                        </td>

                        <td style="text-align:left;">

                        broken_squared
                        </td>

                        <td style="text-align:left;">

                        u_nodal
                        </td>

                        <td style="text-align:right;">

                        10.21
                        </td>

                        <td style="text-align:right;">

                        3.55
                        </td>

                        <td style="text-align:right;">

                        2.88
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        4.35
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        IJmuiden
                        </td>

                        <td style="text-align:left;">

                        broken_squared
                        </td>

                        <td style="text-align:left;">

                        v_nodal
                        </td>

                        <td style="text-align:right;">

                        -12.39
                        </td>

                        <td style="text-align:right;">

                        3.52
                        </td>

                        <td style="text-align:right;">

                        -3.52
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        4.04
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        Netherlands
                        </td>

                        <td style="text-align:left;">

                        linear
                        </td>

                        <td style="text-align:left;">

                        Constant
                        </td>

                        <td style="text-align:right;">

                        -21.51
                        </td>

                        <td style="text-align:right;">

                        2.04
                        </td>

                        <td style="text-align:right;">

                        -10.54
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        2.26
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        Netherlands
                        </td>

                        <td style="text-align:left;">

                        linear
                        </td>

                        <td style="text-align:left;">

                        Trend
                        </td>

                        <td style="text-align:right;">

                        1.98
                        </td>

                        <td style="text-align:right;">

                        0.05
                        </td>

                        <td style="text-align:right;">

                        39.38
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        0.06
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        Netherlands
                        </td>

                        <td style="text-align:left;">

                        linear
                        </td>

                        <td style="text-align:left;">

                        u_nodal
                        </td>

                        <td style="text-align:right;">

                        4.63
                        </td>

                        <td style="text-align:right;">

                        2.77
                        </td>

                        <td style="text-align:right;">

                        1.67
                        </td>

                        <td style="text-align:right;">

                        0.10
                        </td>

                        <td style="text-align:right;">

                        3.06
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        Netherlands
                        </td>

                        <td style="text-align:left;">

                        linear
                        </td>

                        <td style="text-align:left;">

                        v_nodal
                        </td>

                        <td style="text-align:right;">

                        -12.82
                        </td>

                        <td style="text-align:right;">

                        2.73
                        </td>

                        <td style="text-align:right;">

                        -4.69
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        3.10
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        Netherlands
                        </td>

                        <td style="text-align:left;">

                        broken_linear
                        </td>

                        <td style="text-align:left;">

                        Constant
                        </td>

                        <td style="text-align:right;">

                        -29.31
                        </td>

                        <td style="text-align:right;">

                        2.52
                        </td>

                        <td style="text-align:right;">

                        -11.63
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        2.34
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        Netherlands
                        </td>

                        <td style="text-align:left;">

                        broken_linear
                        </td>

                        <td style="text-align:left;">

                        Trend
                        </td>

                        <td style="text-align:right;">

                        1.78
                        </td>

                        <td style="text-align:right;">

                        0.06
                        </td>

                        <td style="text-align:right;">

                        28.16
                        </td>

                        <td style="text-align:right;">

                        0.00
                        </td>

                        <td style="text-align:right;">

                        0.07
                        </td>

                        </tr>

                        <tr>

                        <td style="text-align:left;">

                        Netherlands
                        </td>

                        <td style="text-align:left;">

                        broken_linear
                        </td>

                        <td style="text-align:left;">

                        - trend 1993
                          </td>

                          <td style="text-align:right;">

                          1.44
                          </td>

                          <td style="text-align:right;">

                          0.31
                          </td>

                          <td style="text-align:right;">

                          4.69
                          </td>

                          <td style="text-align:right;">

                          0.00
                          </td>

                          <td style="text-align:right;">

                          0.29
                          </td>

                          </tr>

                          <tr>

                          <td style="text-align:left;">

                          Netherlands
                          </td>

                          <td style="text-align:left;">

                          broken_linear
                          </td>

                          <td style="text-align:left;">

                          u_nodal
                          </td>

                          <td style="text-align:right;">

                          4.69
                          </td>

                          <td style="text-align:right;">

                          2.57
                          </td>

                          <td style="text-align:right;">

                          1.83
                          </td>

                          <td style="text-align:right;">

                          0.07
                          </td>

                          <td style="text-align:right;">

                          2.76
                          </td>

                          </tr>

                          <tr>

                          <td style="text-align:left;">

                          Netherlands
                          </td>

                          <td style="text-align:left;">

                          broken_linear
                          </td>

                          <td style="text-align:left;">

                          v_nodal
                          </td>

                          <td style="text-align:right;">

                          -11.43
                          </td>

                          <td style="text-align:right;">

                          2.55
                          </td>

                          <td style="text-align:right;">

                          -4.48
                          </td>

                          <td style="text-align:right;">

                          0.00
                          </td>

                          <td style="text-align:right;">

                          2.85
                          </td>

                          </tr>

                          <tr>

                          <td style="text-align:left;">

                          Netherlands
                          </td>

                          <td style="text-align:left;">

                          broken_squared
                          </td>

                          <td style="text-align:left;">

                          Constant
                          </td>

                          <td style="text-align:right;">

                          -32.78
                          </td>

                          <td style="text-align:right;">

                          3.21
                          </td>

                          <td style="text-align:right;">

                          -10.21
                          </td>

                          <td style="text-align:right;">

                          0.00
                          </td>

                          <td style="text-align:right;">

                          3.15
                          </td>

                          </tr>

                          <tr>

                          <td style="text-align:left;">

                          Netherlands
                          </td>

                          <td style="text-align:left;">

                          broken_squared
                          </td>

                          <td style="text-align:left;">

                          Trend
                          </td>

                          <td style="text-align:right;">

                          1.71
                          </td>

                          <td style="text-align:right;">

                          0.08
                          </td>

                          <td style="text-align:right;">

                          22.07
                          </td>

                          <td style="text-align:right;">

                          0.00
                          </td>

                          <td style="text-align:right;">

                          0.09
                          </td>

                          </tr>

                          <tr>

                          <td style="text-align:left;">

                          Netherlands
                          </td>

                          <td style="text-align:left;">

                          broken_squared
                          </td>

                          <td style="text-align:left;">

                          - square_trend 1960
                            </td>

                            <td style="text-align:right;">

                            0.01
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            4.37
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands
                            </td>

                            <td style="text-align:left;">

                            broken_squared
                            </td>

                            <td style="text-align:left;">

                            u_nodal
                            </td>

                            <td style="text-align:right;">

                            4.51
                            </td>

                            <td style="text-align:right;">

                            2.59
                            </td>

                            <td style="text-align:right;">

                            1.74
                            </td>

                            <td style="text-align:right;">

                            0.09
                            </td>

                            <td style="text-align:right;">

                            2.82
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands
                            </td>

                            <td style="text-align:left;">

                            broken_squared
                            </td>

                            <td style="text-align:left;">

                            v_nodal
                            </td>

                            <td style="text-align:right;">

                            -11.74
                            </td>

                            <td style="text-align:right;">

                            2.57
                            </td>

                            <td style="text-align:right;">

                            -4.57
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            2.86
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands (without Delfzijl)
                            </td>

                            <td style="text-align:left;">

                            linear
                            </td>

                            <td style="text-align:left;">

                            Constant
                            </td>

                            <td style="text-align:right;">

                            -30.64
                            </td>

                            <td style="text-align:right;">

                            1.97
                            </td>

                            <td style="text-align:right;">

                            -15.56
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            2.16
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands (without Delfzijl)
                            </td>

                            <td style="text-align:left;">

                            linear
                            </td>

                            <td style="text-align:left;">

                            Trend
                            </td>

                            <td style="text-align:right;">

                            1.99
                            </td>

                            <td style="text-align:right;">

                            0.05
                            </td>

                            <td style="text-align:right;">

                            41.01
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            0.06
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands (without Delfzijl)
                            </td>

                            <td style="text-align:left;">

                            linear
                            </td>

                            <td style="text-align:left;">

                            u_nodal
                            </td>

                            <td style="text-align:right;">

                            4.99
                            </td>

                            <td style="text-align:right;">

                            2.67
                            </td>

                            <td style="text-align:right;">

                            1.87
                            </td>

                            <td style="text-align:right;">

                            0.06
                            </td>

                            <td style="text-align:right;">

                            2.97
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands (without Delfzijl)
                            </td>

                            <td style="text-align:left;">

                            linear
                            </td>

                            <td style="text-align:left;">

                            v_nodal
                            </td>

                            <td style="text-align:right;">

                            -12.39
                            </td>

                            <td style="text-align:right;">

                            2.63
                            </td>

                            <td style="text-align:right;">

                            -4.70
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            3.01
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands (without Delfzijl)
                            </td>

                            <td style="text-align:left;">

                            broken_linear
                            </td>

                            <td style="text-align:left;">

                            Constant
                            </td>

                            <td style="text-align:right;">

                            -37.59
                            </td>

                            <td style="text-align:right;">

                            2.46
                            </td>

                            <td style="text-align:right;">

                            -15.27
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            2.32
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands (without Delfzijl)
                            </td>

                            <td style="text-align:left;">

                            broken_linear
                            </td>

                            <td style="text-align:left;">

                            Trend
                            </td>

                            <td style="text-align:right;">

                            1.81
                            </td>

                            <td style="text-align:right;">

                            0.06
                            </td>

                            <td style="text-align:right;">

                            29.33
                            </td>

                            <td style="text-align:right;">

                            0.00
                            </td>

                            <td style="text-align:right;">

                            0.07
                            </td>

                            </tr>

                            <tr>

                            <td style="text-align:left;">

                            Netherlands (without Delfzijl)
                            </td>

                            <td style="text-align:left;">

                            broken_linear
                            </td>

                            <td style="text-align:left;">

                            - trend 1993
                              </td>

                              <td style="text-align:right;">

                              1.28
                              </td>

                              <td style="text-align:right;">

                              0.30
                              </td>

                              <td style="text-align:right;">

                              4.28
                              </td>

                              <td style="text-align:right;">

                              0.00
                              </td>

                              <td style="text-align:right;">

                              0.30
                              </td>

                              </tr>

                              <tr>

                              <td style="text-align:left;">

                              Netherlands (without Delfzijl)
                              </td>

                              <td style="text-align:left;">

                              broken_linear
                              </td>

                              <td style="text-align:left;">

                              u_nodal
                              </td>

                              <td style="text-align:right;">

                              5.05
                              </td>

                              <td style="text-align:right;">

                              2.51
                              </td>

                              <td style="text-align:right;">

                              2.01
                              </td>

                              <td style="text-align:right;">

                              0.05
                              </td>

                              <td style="text-align:right;">

                              2.70
                              </td>

                              </tr>

                              <tr>

                              <td style="text-align:left;">

                              Netherlands (without Delfzijl)
                              </td>

                              <td style="text-align:left;">

                              broken_linear
                              </td>

                              <td style="text-align:left;">

                              v_nodal
                              </td>

                              <td style="text-align:right;">

                              -11.16
                              </td>

                              <td style="text-align:right;">

                              2.49
                              </td>

                              <td style="text-align:right;">

                              -4.48
                              </td>

                              <td style="text-align:right;">

                              0.00
                              </td>

                              <td style="text-align:right;">

                              2.82
                              </td>

                              </tr>

                              <tr>

                              <td style="text-align:left;">

                              Netherlands (without Delfzijl)
                              </td>

                              <td style="text-align:left;">

                              broken_squared
                              </td>

                              <td style="text-align:left;">

                              Constant
                              </td>

                              <td style="text-align:right;">

                              -40.56
                              </td>

                              <td style="text-align:right;">

                              3.14
                              </td>

                              <td style="text-align:right;">

                              -12.93
                              </td>

                              <td style="text-align:right;">

                              0.00
                              </td>

                              <td style="text-align:right;">

                              3.18
                              </td>

                              </tr>

                              <tr>

                              <td style="text-align:left;">

                              Netherlands (without Delfzijl)
                              </td>

                              <td style="text-align:left;">

                              broken_squared
                              </td>

                              <td style="text-align:left;">

                              Trend
                              </td>

                              <td style="text-align:right;">

                              1.75
                              </td>

                              <td style="text-align:right;">

                              0.08
                              </td>

                              <td style="text-align:right;">

                              23.15
                              </td>

                              <td style="text-align:right;">

                              0.00
                              </td>

                              <td style="text-align:right;">

                              0.09
                              </td>

                              </tr>

                              <tr>

                              <td style="text-align:left;">

                              Netherlands (without Delfzijl)
                              </td>

                              <td style="text-align:left;">

                              broken_squared
                              </td>

                              <td style="text-align:left;">

                              - square_trend 1960
                                </td>

                                <td style="text-align:right;">

                                0.01
                                </td>

                                <td style="text-align:right;">

                                0.00
                                </td>

                                <td style="text-align:right;">

                                3.94
                                </td>

                                <td style="text-align:right;">

                                0.00
                                </td>

                                <td style="text-align:right;">

                                0.00
                                </td>

                                </tr>

                                <tr>

                                <td style="text-align:left;">

                                Netherlands (without Delfzijl)
                                </td>

                                <td style="text-align:left;">

                                broken_squared
                                </td>

                                <td style="text-align:left;">

                                u_nodal
                                </td>

                                <td style="text-align:right;">

                                4.89
                                </td>

                                <td style="text-align:right;">

                                2.53
                                </td>

                                <td style="text-align:right;">

                                1.93
                                </td>

                                <td style="text-align:right;">

                                0.06
                                </td>

                                <td style="text-align:right;">

                                2.74
                                </td>

                                </tr>

                                <tr>

                                <td style="text-align:left;">

                                Netherlands (without Delfzijl)
                                </td>

                                <td style="text-align:left;">

                                broken_squared
                                </td>

                                <td style="text-align:left;">

                                v_nodal
                                </td>

                                <td style="text-align:right;">

                                -11.45
                                </td>

                                <td style="text-align:right;">

                                2.51
                                </td>

                                <td style="text-align:right;">

                                -4.56
                                </td>

                                <td style="text-align:right;">

                                0.00
                                </td>

                                <td style="text-align:right;">

                                2.84
                                </td>

                                </tr>

                                </tbody>

                                </table>

                                </div>

## Which model is the preferred model?

### Is there a significant acceleration?

``` r
acc_broken_linear <- parametertable %>%
  filter(modeltype == "broken_linear") %>%
  filter(term == "+ trend 1993") %>%
  select(station, estimate, p.value )
knitr::kable(acc_broken_linear, caption = "p-values for the acceleration term in the broken linear model for all stations. ")
```

| station                        | estimate | p.value |
|:-------------------------------|---------:|--------:|
| Vlissingen                     |    0.723 |   0.048 |
| Hoek van Holland               |    0.956 |   0.009 |
| Den Helder                     |    1.664 |   0.000 |
| Delfzijl                       |    2.222 |   0.000 |
| Harlingen                      |    2.741 |   0.000 |
| IJmuiden                       |    0.337 |   0.428 |
| Netherlands                    |    1.440 |   0.000 |
| Netherlands (without Delfzijl) |    1.284 |   0.000 |

p-values for the acceleration term in the broken linear model for all
stations.

For the broken linear model, there is a significant acceleration
starting in the year 1993 when fitting the average sea level combined
for all stations without Delfzijl. For individual stations, the
acceleration is not significant for the station IJmuiden.

``` r
acc_broken_squared <- parametertable %>%
  filter(modeltype == "broken_squared") %>%
  filter(term == "+ square_trend 1960") %>%
  select(station, p.value )
knitr::kable(acc_broken_squared, caption = "p-values for the acceleration term in the broken squared model for all stations. ")
```

| station                        | p.value |
|:-------------------------------|--------:|
| Vlissingen                     |   0.364 |
| Hoek van Holland               |   0.002 |
| Den Helder                     |   0.000 |
| Delfzijl                       |   0.000 |
| Harlingen                      |   0.000 |
| IJmuiden                       |   0.580 |
| Netherlands                    |   0.000 |
| Netherlands (without Delfzijl) |   0.000 |

p-values for the acceleration term in the broken squared model for all
stations.

For the broken squared model, there is a significant acceleration
starting in the year 1960 when fitting the average sea level combined
for all stations without Delfzijl. For individual stations, the
acceleration is not significant for the stations Vlissingen and
IJmuiden.

### Which model has the lowest Akaike Information Criterion (AIC)?

Of the two models with an acceleration term, the model with lowest AIC
is the preferred model.

``` r
colors <- c(lowest = "orange")

AIC_df <- models %>%
  mutate(station = as.character(station)) %>%
  select(station, modeltype, AIC) %>%
  arrange(-AIC) %>% 
  mutate(modeltype = factor(modeltype, levels=config$runparameters$modeltype)) %>%
  mutate(station = factor(station, levels=config$runparameters$station)) %>%
  group_by(station) %>%
  mutate(AIC_score = ifelse(AIC == min(AIC), "lowest", ""))

AIC_df %>%
  ggplot(aes(x = modeltype, y = AIC, color = AIC_score)) +
  geom_point(size = 6, shape = "|") +
  coord_flip() +
  scale_color_manual(values = colors) +
  facet_wrap("station")
```

![](sealevelanalysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
preferred_model = AIC_df %>% 
  ungroup() %>% 
  filter(AIC_score == "lowest", station == "Netherlands (without Delfzijl)") %>% 
  select(modeltype) %>% 
  unlist() %>% 
  as.character() %>% 
  unname()

AIC_df %>% ungroup() %>%
  filter(AIC_score == "lowest") %>% 
  arrange(modeltype) %>% 
  select(station, `lowest AIC model` = modeltype) %>%
  knitr::kable(caption = "Model with lowest AIC for each station or station combination. ")
```

| station                        | lowest AIC model |
|:-------------------------------|:-----------------|
| IJmuiden                       | linear           |
| Delfzijl                       | broken_linear    |
| Harlingen                      | broken_linear    |
| Vlissingen                     | broken_linear    |
| Netherlands                    | broken_linear    |
| Netherlands (without Delfzijl) | broken_linear    |
| Den Helder                     | broken_squared   |
| Hoek van Holland               | broken_squared   |

Model with lowest AIC for each station or station combination.

For the combined stations Netherlands and Netherlands (without
Delfzijl), the model has the lowest AIC, and is therefore the first
candidate for the preferred model. In the next section, it is tested
whether the broken-linear model explains the observed variation
significantly better than the simplest model, the linear model.

For stations Den Helder and Hoek van Holland, the broken squared model
is the model with lowest AIC.

At station IJmuiden, all three models have similar AIC.

Considering all stations, the broken linear and the broken squared model
describe the observed variation approximately equally well.

### Is the preferred model significantly better than the linear model?

The broken linear model is chosen as the preferred candidate because it
gave a better explanation of the observation, corrected for the degrees
of freedom of the model (AIC criterion). Here, it is tested whether the
broken linear model is *significantly* better model that the most simple
model, the broken linear model.

``` r
# extract models to compare
bl <- models %>% 
  filter(
    station == "Netherlands (without Delfzijl)",
    modeltype == "broken_linear"
  ) %>%
  select(model) %>% unlist(recursive = F) %>% unname()

l <- models %>% 
  filter(
    station == "Netherlands (without Delfzijl)",
    modeltype == "linear"
  ) %>%
  select(model) %>% unlist(recursive = F) %>% unname()

# create anova table
t <- anova(l[[1]], bl[[1]])
# extract p value from table
p_value <- t$`Pr(>F)`[2]

if(p_value<0.01) {
  alternativemodelaccepted = TRUE
  alternativemodelrefused = FALSE
}
```

The Anova table for model comparison is shown below.

``` r
makePrettyAnovaTable(t, 3)
```

| Res.Df |   RSS |  Df | Sum of Sq |    F |        p |
|-------:|------:|----:|----------:|-----:|---------:|
|    130 | 61000 |     |           |      |          |
|    129 | 53400 |   1 |      7580 | 18.3 | 3.62e-05 |

The acceleration model (broken linear) has one more degree of freedom
than the linear model. The broken linear model is significantly better
than the linear model (p \< 0.001).

## Conclusions

Based on the above analysis, the following conclusions are drawn:

Based on variance analysis the broken linear model is significantly
better than the linear model. the broken linear model is accepted as the
preferred model.

</div>
