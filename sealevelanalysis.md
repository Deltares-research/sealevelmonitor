Zeespiegelmonitor analysis
================
Willem Stolte, Nathalie Dees
18 February, 2026

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

## Introduction

The Dutch Sea Level Monitor produces and communicates the mean sea level
along the Dutch Coast and a best estimate of the sea level change over
the past 15 years. Every 4 years, a report is produced. More information
and a link to the latest report can be found
[here](https://www.deltares.nl/expertise/onze-expertises/zeespiegelstijging/zeespiegelmonitor).

Every year, Dutch mean sea level and sea level change is calculated.
This document is a first analyses, producing an overview of all main
tide gauge stations, and an average for the Dutch coast. Furthermore, a
choice of regression model is made to describe sea level change over the
last 15 years. In an accompanying document the results are presented in
a more user-friendly way for the Dutch coast and the preferred
regression model.

## Read needed data

In the script
[updateSLMinput.R](https://github.com/Deltares-research/sealevelmonitor/blob/main/analysis/sealevelmonitor/updateSLMinput.R),
annual average sea level data for the Dutch main stations is downloaded
from the [Permanent Service for Mean Sea Level
site](http://www.psmsl.org) and combined with the Global Tide and Surge
Model (GTSM) annual average surge values.

``` r
current_df <-   read_delim(
  "../../data/deltares/input/psmsl_gtsm_yr-latest.csv", 
  delim = ";") %>%
  filter(year >= params$startyear)
```

In this analysis, measurements over the period 1890 to 2024 are
considered.

## Locations of the main tide gauge stations

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

<div class="figure">

<div class="leaflet html-widget html-fill-item" id="htmlwidget-3ee7e2ce785fd63221a5" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-3ee7e2ce785fd63221a5">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addCircleMarkers","args":[[51.442,51.978,52.964,53.326,53.176,52.462],[3.596,4.12,4.745,6.933,5.409,4.555],10,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.2},null,null,null,null,["Vlissingen","Hoek van Holland","Den Helder","Delfzijl","Harlingen","IJmuiden"],{"interactive":false,"permanent":true,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[51.442,53.326],"lng":[3.596,6.933]},"setView":[[52.558,4.893],6,[]]},"evals":[],"jsHooks":[]}</script>

<p class="caption">

Hoofdgetijstations in Nederland. Er is aangegeven welke stations zijn
meegenomen in dit rekendocument.
</p>

</div>

## Sea level measurements

In this section we look at sea level measurements. The global collection
of tide gauge records at the PSMSL was used to access the data. There
are two types of datasets the “Revised Local Reference” and “Metric”.
For the Netherlands the difference is that the “Revised Local Reference”
undoes the NAP correction in 2005, to get a consistent dataset. Here we
transform the RLR back to post-2005 NAP (without undoing the
correction).

In the below table, the nap-LRL computes the rlr back to latest NAP
(ignoring the undoing of the NAP correction). Id’s are the station ids
in the PSMSL dataset. You can lookup the relevant parameters in the
schematic diagram like this [LRL diagram for station
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

Sea level data for all six main station are shown below in a time plot.

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
<img
src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/zeespiegelmetingen-1.png"
alt="Yearly averaged sea level for the six main tidal stations in the Netherlands." />
<figcaption aria-hidden="true">Yearly averaged sea level for the six
main tidal stations in the Netherlands.</figcaption>
</figure>

``` r
# ggplotly(p) %>% layout(legend = list(x = 0.05, y = 0.95))
```

Sea level measurements for the six main stations (yearly average) are
shown in figure @ref(fig:zeespiegelanomalieen) as deviations from the
mean for each year. This way, the characteristic patterns of the
stations are easier to see.

A few things become clear

- Harlingen shows a lower trend as compared to the average of the
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
<img
src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/zeespiegelanomalieen-1.png"
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
src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/zeespiegelmetingenGemiddeld-1.png"
alt="Jaarlijks gemiddelde zeespiegel voor gemiddelde van stations langs de Nederlandse kust." />
<figcaption aria-hidden="true">Jaarlijks gemiddelde zeespiegel voor
gemiddelde van stations langs de Nederlandse kust.</figcaption>
</figure>

### Sea level highest years

The 5 years with highest sea levels are shown below in table
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

## Storm surge corrections

The expected storm surge per year is determined using output of the
[Global Tide and Surge Model
(GTSM)](https://www.deltares.nl/en/expertise/projects/global-modelling-of-tides-and-storm-surges).
GTSM calculates the surge given the bathymetry and climatic conditions.
Since it is a global model, it does not need boundary conditions. Model
results are available from 1950 onwards (figure @ref(fig:gtsm-surge)).
The calculated yearly averaged variation in surge (surge anomaly) for
each main station is subtracted from the measured sea level before sea
level rise is calculated. For the years before 1950, no runs are
available and sea level is corrected for average surge only. This
correction reduces the variation due to differences in surge per year
(for 1950 onward) and allows a more precise estimate of the long-term
trend.

``` r
q <- current_df %>%
  filter(station == "Netherlands (without Delfzijl)") %>%
  # filter(station == "IJmuiden") %>%
ggplot(aes(year, surge_anomaly-200)) +
  # geom_point(alpha = 1, aes(color = station), shape = 21, fill = "white", size = 1) +
  geom_line(alpha = 0.5, aes(color = "surge anomaly - 200"), linewidth = 0.75) +
  geom_line(aes(year, height, color = "height")) +
  geom_line(aes(year, height-surge_anomaly, color = "height-surge anomaly")) +
  xlab("jaar") + ylab("mm") +
  theme_light() +
  theme(legend.position = "bottom") +
  coord_cartesian(xlim = c(1890, params$monitoryear)) +
  scale_x_continuous(n.breaks = 10)


ggplotly(q) # %>% layout(legend = list(x = 0.05, y = 0.95))
```

<div class="figure">

<div class="plotly html-widget html-fill-item" id="htmlwidget-d747c07a7beedc96520d" style="width:768px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-d747c07a7beedc96520d">{"x":{"data":[{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-200,-182.62676087734249,-198.25482797824787,-191.06195918139957,-232.15208030536934,-179.22743353479095,-196.16885477603523,-217.1963309111708,-203.48037804148299,-197.48550343387717,-244.04582429629303,-205.77820439289252,-180.37937182706534,-196.21879607393686,-230.44509278720281,-234.71657452496706,-175.19219657970643,-182.96178460558008,-157.76212657205278,-210.30515376092299,-223.19888647215029,-186.83834717264421,-224.43588862709009,-245.7668183505844,-187.58052073702854,-165.27683255878136,-221.73253351060112,-235.61004899281738,-183.1633983337795,-201.9133539959995,-204.96173652811785,-211.46604950772056,-179.09582959967952,-205.04306122351514,-170.20422205803573,-214.52351463084622,-210.90762335102198,-203.64888961244665,-232.60207765556993,-175.23889162614682,-197.25156031447403,-151.97055732521372,-225.0761385985617,-200.89218505302108,-213.04688975005757,-178.86493444652879,-198.47943418220004,-273.76648388907023,-236.25515947156993,-165.47778147639193,-171.93594074526845,-180.59484311340358,-193.32619966573978,-220.23813047144938,-234.82730368288827,-189.61961075597188,-206.72026385690691,-201.88638445967953,-176.97684701288497,-183.4731147059446,-209.59847521809047,-225.46215111524117,-193.62942026836444,-202.94237851012491,-228.45940379787129,-204.24827182667676,-184.3854268126658,-222.99111508690086,-183.62221199293975,-242.26455816926855,-194.34602462565212,-194.03881709178611,-217.09776529913159,-211.8746006085961,-176.36532474414352,-183.15122653725771],"text":["year: 1890<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1891<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1892<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1893<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1894<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1895<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1896<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1897<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1898<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1899<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1900<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1901<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1902<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1903<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1904<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1905<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1906<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1907<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1908<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1909<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1910<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1911<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1912<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1913<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1914<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1915<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1916<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1917<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1918<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1919<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1920<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1921<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1922<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1923<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1924<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1925<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1926<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1927<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1928<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1929<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1930<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1931<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1932<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1933<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1934<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1935<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1936<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1937<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1938<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1939<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1940<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1941<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1942<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1943<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1944<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1945<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1946<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1947<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1948<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1949<br />surge_anomaly - 200: -200.0000<br />colour: surge anomaly - 200","year: 1950<br />surge_anomaly - 200: -182.6268<br />colour: surge anomaly - 200","year: 1951<br />surge_anomaly - 200: -198.2548<br />colour: surge anomaly - 200","year: 1952<br />surge_anomaly - 200: -191.0620<br />colour: surge anomaly - 200","year: 1953<br />surge_anomaly - 200: -232.1521<br />colour: surge anomaly - 200","year: 1954<br />surge_anomaly - 200: -179.2274<br />colour: surge anomaly - 200","year: 1955<br />surge_anomaly - 200: -196.1689<br />colour: surge anomaly - 200","year: 1956<br />surge_anomaly - 200: -217.1963<br />colour: surge anomaly - 200","year: 1957<br />surge_anomaly - 200: -203.4804<br />colour: surge anomaly - 200","year: 1958<br />surge_anomaly - 200: -197.4855<br />colour: surge anomaly - 200","year: 1959<br />surge_anomaly - 200: -244.0458<br />colour: surge anomaly - 200","year: 1960<br />surge_anomaly - 200: -205.7782<br />colour: surge anomaly - 200","year: 1961<br />surge_anomaly - 200: -180.3794<br />colour: surge anomaly - 200","year: 1962<br />surge_anomaly - 200: -196.2188<br />colour: surge anomaly - 200","year: 1963<br />surge_anomaly - 200: -230.4451<br />colour: surge anomaly - 200","year: 1964<br />surge_anomaly - 200: -234.7166<br />colour: surge anomaly - 200","year: 1965<br />surge_anomaly - 200: -175.1922<br />colour: surge anomaly - 200","year: 1966<br />surge_anomaly - 200: -182.9618<br />colour: surge anomaly - 200","year: 1967<br />surge_anomaly - 200: -157.7621<br />colour: surge anomaly - 200","year: 1968<br />surge_anomaly - 200: -210.3052<br />colour: surge anomaly - 200","year: 1969<br />surge_anomaly - 200: -223.1989<br />colour: surge anomaly - 200","year: 1970<br />surge_anomaly - 200: -186.8383<br />colour: surge anomaly - 200","year: 1971<br />surge_anomaly - 200: -224.4359<br />colour: surge anomaly - 200","year: 1972<br />surge_anomaly - 200: -245.7668<br />colour: surge anomaly - 200","year: 1973<br />surge_anomaly - 200: -187.5805<br />colour: surge anomaly - 200","year: 1974<br />surge_anomaly - 200: -165.2768<br />colour: surge anomaly - 200","year: 1975<br />surge_anomaly - 200: -221.7325<br />colour: surge anomaly - 200","year: 1976<br />surge_anomaly - 200: -235.6100<br />colour: surge anomaly - 200","year: 1977<br />surge_anomaly - 200: -183.1634<br />colour: surge anomaly - 200","year: 1978<br />surge_anomaly - 200: -201.9134<br />colour: surge anomaly - 200","year: 1979<br />surge_anomaly - 200: -204.9617<br />colour: surge anomaly - 200","year: 1980<br />surge_anomaly - 200: -211.4660<br />colour: surge anomaly - 200","year: 1981<br />surge_anomaly - 200: -179.0958<br />colour: surge anomaly - 200","year: 1982<br />surge_anomaly - 200: -205.0431<br />colour: surge anomaly - 200","year: 1983<br />surge_anomaly - 200: -170.2042<br />colour: surge anomaly - 200","year: 1984<br />surge_anomaly - 200: -214.5235<br />colour: surge anomaly - 200","year: 1985<br />surge_anomaly - 200: -210.9076<br />colour: surge anomaly - 200","year: 1986<br />surge_anomaly - 200: -203.6489<br />colour: surge anomaly - 200","year: 1987<br />surge_anomaly - 200: -232.6021<br />colour: surge anomaly - 200","year: 1988<br />surge_anomaly - 200: -175.2389<br />colour: surge anomaly - 200","year: 1989<br />surge_anomaly - 200: -197.2516<br />colour: surge anomaly - 200","year: 1990<br />surge_anomaly - 200: -151.9706<br />colour: surge anomaly - 200","year: 1991<br />surge_anomaly - 200: -225.0761<br />colour: surge anomaly - 200","year: 1992<br />surge_anomaly - 200: -200.8922<br />colour: surge anomaly - 200","year: 1993<br />surge_anomaly - 200: -213.0469<br />colour: surge anomaly - 200","year: 1994<br />surge_anomaly - 200: -178.8649<br />colour: surge anomaly - 200","year: 1995<br />surge_anomaly - 200: -198.4794<br />colour: surge anomaly - 200","year: 1996<br />surge_anomaly - 200: -273.7665<br />colour: surge anomaly - 200","year: 1997<br />surge_anomaly - 200: -236.2552<br />colour: surge anomaly - 200","year: 1998<br />surge_anomaly - 200: -165.4778<br />colour: surge anomaly - 200","year: 1999<br />surge_anomaly - 200: -171.9359<br />colour: surge anomaly - 200","year: 2000<br />surge_anomaly - 200: -180.5948<br />colour: surge anomaly - 200","year: 2001<br />surge_anomaly - 200: -193.3262<br />colour: surge anomaly - 200","year: 2002<br />surge_anomaly - 200: -220.2381<br />colour: surge anomaly - 200","year: 2003<br />surge_anomaly - 200: -234.8273<br />colour: surge anomaly - 200","year: 2004<br />surge_anomaly - 200: -189.6196<br />colour: surge anomaly - 200","year: 2005<br />surge_anomaly - 200: -206.7203<br />colour: surge anomaly - 200","year: 2006<br />surge_anomaly - 200: -201.8864<br />colour: surge anomaly - 200","year: 2007<br />surge_anomaly - 200: -176.9768<br />colour: surge anomaly - 200","year: 2008<br />surge_anomaly - 200: -183.4731<br />colour: surge anomaly - 200","year: 2009<br />surge_anomaly - 200: -209.5985<br />colour: surge anomaly - 200","year: 2010<br />surge_anomaly - 200: -225.4622<br />colour: surge anomaly - 200","year: 2011<br />surge_anomaly - 200: -193.6294<br />colour: surge anomaly - 200","year: 2012<br />surge_anomaly - 200: -202.9424<br />colour: surge anomaly - 200","year: 2013<br />surge_anomaly - 200: -228.4594<br />colour: surge anomaly - 200","year: 2014<br />surge_anomaly - 200: -204.2483<br />colour: surge anomaly - 200","year: 2015<br />surge_anomaly - 200: -184.3854<br />colour: surge anomaly - 200","year: 2016<br />surge_anomaly - 200: -222.9911<br />colour: surge anomaly - 200","year: 2017<br />surge_anomaly - 200: -183.6222<br />colour: surge anomaly - 200","year: 2018<br />surge_anomaly - 200: -242.2646<br />colour: surge anomaly - 200","year: 2019<br />surge_anomaly - 200: -194.3460<br />colour: surge anomaly - 200","year: 2020<br />surge_anomaly - 200: -194.0388<br />colour: surge anomaly - 200","year: 2021<br />surge_anomaly - 200: -217.0978<br />colour: surge anomaly - 200","year: 2022<br />surge_anomaly - 200: -211.8746<br />colour: surge anomaly - 200","year: 2023<br />surge_anomaly - 200: -176.3653<br />colour: surge anomaly - 200","year: 2024<br />surge_anomaly - 200: -183.1512<br />colour: surge anomaly - 200"],"type":"scatter","mode":"lines","line":{"width":2.8346456692913389,"color":"rgba(97,156,255,0.5)","dash":"solid"},"hoveron":"points","name":"surge anomaly - 200","legendgroup":"surge anomaly - 200","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[-203.40000000000001,-189.40000000000001,-174.40000000000001,-154,-147,-182,-197.59999999999999,-190.80000000000001,-153,-151,-189.40000000000001,-210.19999999999999,-238,-129.40000000000001,-155.40000000000001,-172.80000000000001,-144.80000000000001,-169.19999999999999,-179.19999999999999,-128.19999999999999,-119.2,-148,-87,-109.59999999999999,-95.799999999999997,-133.19999999999999,-86.200000000000003,-138.80000000000001,-140.40000000000001,-162,-162,-139.80000000000001,-136.40000000000001,-114.40000000000001,-138.59999999999999,-89.200000000000003,-108.2,-102,-89.200000000000003,-144.40000000000001,-90.599999999999994,-86.400000000000006,-101.2,-147,-133.80000000000001,-85.599999999999994,-68.200000000000003,-79.400000000000006,-72.400000000000006,-102.59999999999999,-98,-130.19999999999999,-115.8,-69.200000000000003,-63.399999999999999,-73.599999999999994,-81.200000000000003,-133.80000000000001,-65.200000000000003,-53.200000000000003,-47.600000000000001,-60.799999999999997,-41.399999999999999,-82.400000000000006,-50.799999999999997,-44.399999999999999,-104.59999999999999,-69,-54.799999999999997,-99.200000000000003,-62.799999999999997,-1.8,-50.799999999999997,-110.59999999999999,-89,-26.199999999999999,3.2000000000000002,21.600000000000001,-42.600000000000001,-54,-24.199999999999999,-77.400000000000006,-94.599999999999994,-31.600000000000001,-27,-63.799999999999997,-86.200000000000003,-32,-41.200000000000003,-27,-3,25.399999999999999,2.6000000000000001,37.799999999999997,-17.600000000000001,-17.199999999999999,-19.399999999999999,-29.199999999999999,44.799999999999997,6.4000000000000004,40.399999999999999,-59.399999999999999,-14.6,-17,23.800000000000001,26.600000000000001,-81.400000000000006,-27.800000000000001,51,45.799999999999997,41.399999999999999,48,34.200000000000003,21.199999999999999,52.200000000000003,37.799999999999997,33.600000000000001,82.799999999999997,56.200000000000003,24.199999999999999,17,47.799999999999997,39.600000000000001,14.4,54.799999999999997,75.200000000000003,63.600000000000001,94.200000000000003,26.199999999999999,92,96.599999999999994,75.799999999999997,94.599999999999994,152.80000000000001,161.40000000000001],"text":["year: 1890<br />height: -203.4<br />colour: height","year: 1891<br />height: -189.4<br />colour: height","year: 1892<br />height: -174.4<br />colour: height","year: 1893<br />height: -154.0<br />colour: height","year: 1894<br />height: -147.0<br />colour: height","year: 1895<br />height: -182.0<br />colour: height","year: 1896<br />height: -197.6<br />colour: height","year: 1897<br />height: -190.8<br />colour: height","year: 1898<br />height: -153.0<br />colour: height","year: 1899<br />height: -151.0<br />colour: height","year: 1900<br />height: -189.4<br />colour: height","year: 1901<br />height: -210.2<br />colour: height","year: 1902<br />height: -238.0<br />colour: height","year: 1903<br />height: -129.4<br />colour: height","year: 1904<br />height: -155.4<br />colour: height","year: 1905<br />height: -172.8<br />colour: height","year: 1906<br />height: -144.8<br />colour: height","year: 1907<br />height: -169.2<br />colour: height","year: 1908<br />height: -179.2<br />colour: height","year: 1909<br />height: -128.2<br />colour: height","year: 1910<br />height: -119.2<br />colour: height","year: 1911<br />height: -148.0<br />colour: height","year: 1912<br />height:  -87.0<br />colour: height","year: 1913<br />height: -109.6<br />colour: height","year: 1914<br />height:  -95.8<br />colour: height","year: 1915<br />height: -133.2<br />colour: height","year: 1916<br />height:  -86.2<br />colour: height","year: 1917<br />height: -138.8<br />colour: height","year: 1918<br />height: -140.4<br />colour: height","year: 1919<br />height: -162.0<br />colour: height","year: 1920<br />height: -162.0<br />colour: height","year: 1921<br />height: -139.8<br />colour: height","year: 1922<br />height: -136.4<br />colour: height","year: 1923<br />height: -114.4<br />colour: height","year: 1924<br />height: -138.6<br />colour: height","year: 1925<br />height:  -89.2<br />colour: height","year: 1926<br />height: -108.2<br />colour: height","year: 1927<br />height: -102.0<br />colour: height","year: 1928<br />height:  -89.2<br />colour: height","year: 1929<br />height: -144.4<br />colour: height","year: 1930<br />height:  -90.6<br />colour: height","year: 1931<br />height:  -86.4<br />colour: height","year: 1932<br />height: -101.2<br />colour: height","year: 1933<br />height: -147.0<br />colour: height","year: 1934<br />height: -133.8<br />colour: height","year: 1935<br />height:  -85.6<br />colour: height","year: 1936<br />height:  -68.2<br />colour: height","year: 1937<br />height:  -79.4<br />colour: height","year: 1938<br />height:  -72.4<br />colour: height","year: 1939<br />height: -102.6<br />colour: height","year: 1940<br />height:  -98.0<br />colour: height","year: 1941<br />height: -130.2<br />colour: height","year: 1942<br />height: -115.8<br />colour: height","year: 1943<br />height:  -69.2<br />colour: height","year: 1944<br />height:  -63.4<br />colour: height","year: 1945<br />height:  -73.6<br />colour: height","year: 1946<br />height:  -81.2<br />colour: height","year: 1947<br />height: -133.8<br />colour: height","year: 1948<br />height:  -65.2<br />colour: height","year: 1949<br />height:  -53.2<br />colour: height","year: 1950<br />height:  -47.6<br />colour: height","year: 1951<br />height:  -60.8<br />colour: height","year: 1952<br />height:  -41.4<br />colour: height","year: 1953<br />height:  -82.4<br />colour: height","year: 1954<br />height:  -50.8<br />colour: height","year: 1955<br />height:  -44.4<br />colour: height","year: 1956<br />height: -104.6<br />colour: height","year: 1957<br />height:  -69.0<br />colour: height","year: 1958<br />height:  -54.8<br />colour: height","year: 1959<br />height:  -99.2<br />colour: height","year: 1960<br />height:  -62.8<br />colour: height","year: 1961<br />height:   -1.8<br />colour: height","year: 1962<br />height:  -50.8<br />colour: height","year: 1963<br />height: -110.6<br />colour: height","year: 1964<br />height:  -89.0<br />colour: height","year: 1965<br />height:  -26.2<br />colour: height","year: 1966<br />height:    3.2<br />colour: height","year: 1967<br />height:   21.6<br />colour: height","year: 1968<br />height:  -42.6<br />colour: height","year: 1969<br />height:  -54.0<br />colour: height","year: 1970<br />height:  -24.2<br />colour: height","year: 1971<br />height:  -77.4<br />colour: height","year: 1972<br />height:  -94.6<br />colour: height","year: 1973<br />height:  -31.6<br />colour: height","year: 1974<br />height:  -27.0<br />colour: height","year: 1975<br />height:  -63.8<br />colour: height","year: 1976<br />height:  -86.2<br />colour: height","year: 1977<br />height:  -32.0<br />colour: height","year: 1978<br />height:  -41.2<br />colour: height","year: 1979<br />height:  -27.0<br />colour: height","year: 1980<br />height:   -3.0<br />colour: height","year: 1981<br />height:   25.4<br />colour: height","year: 1982<br />height:    2.6<br />colour: height","year: 1983<br />height:   37.8<br />colour: height","year: 1984<br />height:  -17.6<br />colour: height","year: 1985<br />height:  -17.2<br />colour: height","year: 1986<br />height:  -19.4<br />colour: height","year: 1987<br />height:  -29.2<br />colour: height","year: 1988<br />height:   44.8<br />colour: height","year: 1989<br />height:    6.4<br />colour: height","year: 1990<br />height:   40.4<br />colour: height","year: 1991<br />height:  -59.4<br />colour: height","year: 1992<br />height:  -14.6<br />colour: height","year: 1993<br />height:  -17.0<br />colour: height","year: 1994<br />height:   23.8<br />colour: height","year: 1995<br />height:   26.6<br />colour: height","year: 1996<br />height:  -81.4<br />colour: height","year: 1997<br />height:  -27.8<br />colour: height","year: 1998<br />height:   51.0<br />colour: height","year: 1999<br />height:   45.8<br />colour: height","year: 2000<br />height:   41.4<br />colour: height","year: 2001<br />height:   48.0<br />colour: height","year: 2002<br />height:   34.2<br />colour: height","year: 2003<br />height:   21.2<br />colour: height","year: 2004<br />height:   52.2<br />colour: height","year: 2005<br />height:   37.8<br />colour: height","year: 2006<br />height:   33.6<br />colour: height","year: 2007<br />height:   82.8<br />colour: height","year: 2008<br />height:   56.2<br />colour: height","year: 2009<br />height:   24.2<br />colour: height","year: 2010<br />height:   17.0<br />colour: height","year: 2011<br />height:   47.8<br />colour: height","year: 2012<br />height:   39.6<br />colour: height","year: 2013<br />height:   14.4<br />colour: height","year: 2014<br />height:   54.8<br />colour: height","year: 2015<br />height:   75.2<br />colour: height","year: 2016<br />height:   63.6<br />colour: height","year: 2017<br />height:   94.2<br />colour: height","year: 2018<br />height:   26.2<br />colour: height","year: 2019<br />height:   92.0<br />colour: height","year: 2020<br />height:   96.6<br />colour: height","year: 2021<br />height:   75.8<br />colour: height","year: 2022<br />height:   94.6<br />colour: height","year: 2023<br />height:  152.8<br />colour: height","year: 2024<br />height:  161.4<br />colour: height"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"height","legendgroup":"height","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[-203.40000000000001,-189.40000000000001,-174.40000000000001,-154,-147,-182,-197.59999999999999,-190.80000000000001,-153,-151,-189.40000000000001,-210.19999999999999,-238,-129.40000000000001,-155.40000000000001,-172.80000000000001,-144.80000000000001,-169.19999999999999,-179.19999999999999,-128.19999999999999,-119.2,-148,-87,-109.59999999999999,-95.799999999999997,-133.19999999999999,-86.200000000000003,-138.80000000000001,-140.40000000000001,-162,-162,-139.80000000000001,-136.40000000000001,-114.40000000000001,-138.59999999999999,-89.200000000000003,-108.2,-102,-89.200000000000003,-144.40000000000001,-90.599999999999994,-86.400000000000006,-101.2,-147,-133.80000000000001,-85.599999999999994,-68.200000000000003,-79.400000000000006,-72.400000000000006,-102.59999999999999,-98,-130.19999999999999,-115.8,-69.200000000000003,-63.399999999999999,-73.599999999999994,-81.200000000000003,-133.80000000000001,-65.200000000000003,-53.200000000000003,-64.973239122657517,-62.545172021752109,-50.338040818600433,-50.247919694630667,-71.57256646520905,-48.231145223964752,-87.40366908882919,-65.519621958516993,-57.314496566122813,-55.154175703706976,-57.021795607107492,-21.420628172934656,-54.581203926063154,-80.154907212797184,-54.283425475032949,-51.007803420293577,-13.838215394419933,-20.637873427947213,-32.294846239077017,-30.801113527849715,-37.361652827355798,-52.964111372909919,-48.833181649415593,-44.019479262971473,-61.723167441218635,-42.067466489398896,-50.58995100718262,-48.836601666220496,-39.286646004000517,-22.038263471882154,8.4660495077205464,4.4958295996795279,7.6430612235151258,8.0042220580357188,-3.0764853691537759,-6.2923766489780135,-15.751110387553354,3.4020776555699292,20.038891626146818,3.6515603144740272,-7.6294426747862829,-34.323861401438307,-13.707814946978933,-3.9531102499424158,2.6649344465288038,25.07943418220005,-7.6335161109297758,8.4551594715699245,16.477781476391925,17.735940745268444,21.994843113403583,41.326199665739786,54.438130471449369,56.027303682888288,41.819610755971894,44.520263856906901,35.486384459679527,59.776847012884986,39.673114705944585,33.798475218090466,42.46215111524117,41.429420268364446,42.542378510124905,42.859403797871288,59.048271826676768,59.585426812665808,86.591115086900857,77.822211992939771,68.464558169268543,86.346024625652106,90.638817091786123,92.897765299131578,106.47460060859608,129.16532474414353,144.55122653725772],"text":["year: 1890<br />height - surge_anomaly: -203.400000<br />colour: height-surge anomaly","year: 1891<br />height - surge_anomaly: -189.400000<br />colour: height-surge anomaly","year: 1892<br />height - surge_anomaly: -174.400000<br />colour: height-surge anomaly","year: 1893<br />height - surge_anomaly: -154.000000<br />colour: height-surge anomaly","year: 1894<br />height - surge_anomaly: -147.000000<br />colour: height-surge anomaly","year: 1895<br />height - surge_anomaly: -182.000000<br />colour: height-surge anomaly","year: 1896<br />height - surge_anomaly: -197.600000<br />colour: height-surge anomaly","year: 1897<br />height - surge_anomaly: -190.800000<br />colour: height-surge anomaly","year: 1898<br />height - surge_anomaly: -153.000000<br />colour: height-surge anomaly","year: 1899<br />height - surge_anomaly: -151.000000<br />colour: height-surge anomaly","year: 1900<br />height - surge_anomaly: -189.400000<br />colour: height-surge anomaly","year: 1901<br />height - surge_anomaly: -210.200000<br />colour: height-surge anomaly","year: 1902<br />height - surge_anomaly: -238.000000<br />colour: height-surge anomaly","year: 1903<br />height - surge_anomaly: -129.400000<br />colour: height-surge anomaly","year: 1904<br />height - surge_anomaly: -155.400000<br />colour: height-surge anomaly","year: 1905<br />height - surge_anomaly: -172.800000<br />colour: height-surge anomaly","year: 1906<br />height - surge_anomaly: -144.800000<br />colour: height-surge anomaly","year: 1907<br />height - surge_anomaly: -169.200000<br />colour: height-surge anomaly","year: 1908<br />height - surge_anomaly: -179.200000<br />colour: height-surge anomaly","year: 1909<br />height - surge_anomaly: -128.200000<br />colour: height-surge anomaly","year: 1910<br />height - surge_anomaly: -119.200000<br />colour: height-surge anomaly","year: 1911<br />height - surge_anomaly: -148.000000<br />colour: height-surge anomaly","year: 1912<br />height - surge_anomaly:  -87.000000<br />colour: height-surge anomaly","year: 1913<br />height - surge_anomaly: -109.600000<br />colour: height-surge anomaly","year: 1914<br />height - surge_anomaly:  -95.800000<br />colour: height-surge anomaly","year: 1915<br />height - surge_anomaly: -133.200000<br />colour: height-surge anomaly","year: 1916<br />height - surge_anomaly:  -86.200000<br />colour: height-surge anomaly","year: 1917<br />height - surge_anomaly: -138.800000<br />colour: height-surge anomaly","year: 1918<br />height - surge_anomaly: -140.400000<br />colour: height-surge anomaly","year: 1919<br />height - surge_anomaly: -162.000000<br />colour: height-surge anomaly","year: 1920<br />height - surge_anomaly: -162.000000<br />colour: height-surge anomaly","year: 1921<br />height - surge_anomaly: -139.800000<br />colour: height-surge anomaly","year: 1922<br />height - surge_anomaly: -136.400000<br />colour: height-surge anomaly","year: 1923<br />height - surge_anomaly: -114.400000<br />colour: height-surge anomaly","year: 1924<br />height - surge_anomaly: -138.600000<br />colour: height-surge anomaly","year: 1925<br />height - surge_anomaly:  -89.200000<br />colour: height-surge anomaly","year: 1926<br />height - surge_anomaly: -108.200000<br />colour: height-surge anomaly","year: 1927<br />height - surge_anomaly: -102.000000<br />colour: height-surge anomaly","year: 1928<br />height - surge_anomaly:  -89.200000<br />colour: height-surge anomaly","year: 1929<br />height - surge_anomaly: -144.400000<br />colour: height-surge anomaly","year: 1930<br />height - surge_anomaly:  -90.600000<br />colour: height-surge anomaly","year: 1931<br />height - surge_anomaly:  -86.400000<br />colour: height-surge anomaly","year: 1932<br />height - surge_anomaly: -101.200000<br />colour: height-surge anomaly","year: 1933<br />height - surge_anomaly: -147.000000<br />colour: height-surge anomaly","year: 1934<br />height - surge_anomaly: -133.800000<br />colour: height-surge anomaly","year: 1935<br />height - surge_anomaly:  -85.600000<br />colour: height-surge anomaly","year: 1936<br />height - surge_anomaly:  -68.200000<br />colour: height-surge anomaly","year: 1937<br />height - surge_anomaly:  -79.400000<br />colour: height-surge anomaly","year: 1938<br />height - surge_anomaly:  -72.400000<br />colour: height-surge anomaly","year: 1939<br />height - surge_anomaly: -102.600000<br />colour: height-surge anomaly","year: 1940<br />height - surge_anomaly:  -98.000000<br />colour: height-surge anomaly","year: 1941<br />height - surge_anomaly: -130.200000<br />colour: height-surge anomaly","year: 1942<br />height - surge_anomaly: -115.800000<br />colour: height-surge anomaly","year: 1943<br />height - surge_anomaly:  -69.200000<br />colour: height-surge anomaly","year: 1944<br />height - surge_anomaly:  -63.400000<br />colour: height-surge anomaly","year: 1945<br />height - surge_anomaly:  -73.600000<br />colour: height-surge anomaly","year: 1946<br />height - surge_anomaly:  -81.200000<br />colour: height-surge anomaly","year: 1947<br />height - surge_anomaly: -133.800000<br />colour: height-surge anomaly","year: 1948<br />height - surge_anomaly:  -65.200000<br />colour: height-surge anomaly","year: 1949<br />height - surge_anomaly:  -53.200000<br />colour: height-surge anomaly","year: 1950<br />height - surge_anomaly:  -64.973239<br />colour: height-surge anomaly","year: 1951<br />height - surge_anomaly:  -62.545172<br />colour: height-surge anomaly","year: 1952<br />height - surge_anomaly:  -50.338041<br />colour: height-surge anomaly","year: 1953<br />height - surge_anomaly:  -50.247920<br />colour: height-surge anomaly","year: 1954<br />height - surge_anomaly:  -71.572566<br />colour: height-surge anomaly","year: 1955<br />height - surge_anomaly:  -48.231145<br />colour: height-surge anomaly","year: 1956<br />height - surge_anomaly:  -87.403669<br />colour: height-surge anomaly","year: 1957<br />height - surge_anomaly:  -65.519622<br />colour: height-surge anomaly","year: 1958<br />height - surge_anomaly:  -57.314497<br />colour: height-surge anomaly","year: 1959<br />height - surge_anomaly:  -55.154176<br />colour: height-surge anomaly","year: 1960<br />height - surge_anomaly:  -57.021796<br />colour: height-surge anomaly","year: 1961<br />height - surge_anomaly:  -21.420628<br />colour: height-surge anomaly","year: 1962<br />height - surge_anomaly:  -54.581204<br />colour: height-surge anomaly","year: 1963<br />height - surge_anomaly:  -80.154907<br />colour: height-surge anomaly","year: 1964<br />height - surge_anomaly:  -54.283425<br />colour: height-surge anomaly","year: 1965<br />height - surge_anomaly:  -51.007803<br />colour: height-surge anomaly","year: 1966<br />height - surge_anomaly:  -13.838215<br />colour: height-surge anomaly","year: 1967<br />height - surge_anomaly:  -20.637873<br />colour: height-surge anomaly","year: 1968<br />height - surge_anomaly:  -32.294846<br />colour: height-surge anomaly","year: 1969<br />height - surge_anomaly:  -30.801114<br />colour: height-surge anomaly","year: 1970<br />height - surge_anomaly:  -37.361653<br />colour: height-surge anomaly","year: 1971<br />height - surge_anomaly:  -52.964111<br />colour: height-surge anomaly","year: 1972<br />height - surge_anomaly:  -48.833182<br />colour: height-surge anomaly","year: 1973<br />height - surge_anomaly:  -44.019479<br />colour: height-surge anomaly","year: 1974<br />height - surge_anomaly:  -61.723167<br />colour: height-surge anomaly","year: 1975<br />height - surge_anomaly:  -42.067466<br />colour: height-surge anomaly","year: 1976<br />height - surge_anomaly:  -50.589951<br />colour: height-surge anomaly","year: 1977<br />height - surge_anomaly:  -48.836602<br />colour: height-surge anomaly","year: 1978<br />height - surge_anomaly:  -39.286646<br />colour: height-surge anomaly","year: 1979<br />height - surge_anomaly:  -22.038263<br />colour: height-surge anomaly","year: 1980<br />height - surge_anomaly:    8.466050<br />colour: height-surge anomaly","year: 1981<br />height - surge_anomaly:    4.495830<br />colour: height-surge anomaly","year: 1982<br />height - surge_anomaly:    7.643061<br />colour: height-surge anomaly","year: 1983<br />height - surge_anomaly:    8.004222<br />colour: height-surge anomaly","year: 1984<br />height - surge_anomaly:   -3.076485<br />colour: height-surge anomaly","year: 1985<br />height - surge_anomaly:   -6.292377<br />colour: height-surge anomaly","year: 1986<br />height - surge_anomaly:  -15.751110<br />colour: height-surge anomaly","year: 1987<br />height - surge_anomaly:    3.402078<br />colour: height-surge anomaly","year: 1988<br />height - surge_anomaly:   20.038892<br />colour: height-surge anomaly","year: 1989<br />height - surge_anomaly:    3.651560<br />colour: height-surge anomaly","year: 1990<br />height - surge_anomaly:   -7.629443<br />colour: height-surge anomaly","year: 1991<br />height - surge_anomaly:  -34.323861<br />colour: height-surge anomaly","year: 1992<br />height - surge_anomaly:  -13.707815<br />colour: height-surge anomaly","year: 1993<br />height - surge_anomaly:   -3.953110<br />colour: height-surge anomaly","year: 1994<br />height - surge_anomaly:    2.664934<br />colour: height-surge anomaly","year: 1995<br />height - surge_anomaly:   25.079434<br />colour: height-surge anomaly","year: 1996<br />height - surge_anomaly:   -7.633516<br />colour: height-surge anomaly","year: 1997<br />height - surge_anomaly:    8.455159<br />colour: height-surge anomaly","year: 1998<br />height - surge_anomaly:   16.477781<br />colour: height-surge anomaly","year: 1999<br />height - surge_anomaly:   17.735941<br />colour: height-surge anomaly","year: 2000<br />height - surge_anomaly:   21.994843<br />colour: height-surge anomaly","year: 2001<br />height - surge_anomaly:   41.326200<br />colour: height-surge anomaly","year: 2002<br />height - surge_anomaly:   54.438130<br />colour: height-surge anomaly","year: 2003<br />height - surge_anomaly:   56.027304<br />colour: height-surge anomaly","year: 2004<br />height - surge_anomaly:   41.819611<br />colour: height-surge anomaly","year: 2005<br />height - surge_anomaly:   44.520264<br />colour: height-surge anomaly","year: 2006<br />height - surge_anomaly:   35.486384<br />colour: height-surge anomaly","year: 2007<br />height - surge_anomaly:   59.776847<br />colour: height-surge anomaly","year: 2008<br />height - surge_anomaly:   39.673115<br />colour: height-surge anomaly","year: 2009<br />height - surge_anomaly:   33.798475<br />colour: height-surge anomaly","year: 2010<br />height - surge_anomaly:   42.462151<br />colour: height-surge anomaly","year: 2011<br />height - surge_anomaly:   41.429420<br />colour: height-surge anomaly","year: 2012<br />height - surge_anomaly:   42.542379<br />colour: height-surge anomaly","year: 2013<br />height - surge_anomaly:   42.859404<br />colour: height-surge anomaly","year: 2014<br />height - surge_anomaly:   59.048272<br />colour: height-surge anomaly","year: 2015<br />height - surge_anomaly:   59.585427<br />colour: height-surge anomaly","year: 2016<br />height - surge_anomaly:   86.591115<br />colour: height-surge anomaly","year: 2017<br />height - surge_anomaly:   77.822212<br />colour: height-surge anomaly","year: 2018<br />height - surge_anomaly:   68.464558<br />colour: height-surge anomaly","year: 2019<br />height - surge_anomaly:   86.346025<br />colour: height-surge anomaly","year: 2020<br />height - surge_anomaly:   90.638817<br />colour: height-surge anomaly","year: 2021<br />height - surge_anomaly:   92.897765<br />colour: height-surge anomaly","year: 2022<br />height - surge_anomaly:  106.474601<br />colour: height-surge anomaly","year: 2023<br />height - surge_anomaly:  129.165325<br />colour: height-surge anomaly","year: 2024<br />height - surge_anomaly:  144.551227<br />colour: height-surge anomaly"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(0,186,56,1)","dash":"solid"},"hoveron":"points","name":"height-surge anomaly","legendgroup":"height-surge anomaly","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":23.305936073059364,"r":7.3059360730593621,"b":37.260273972602747,"l":48.949771689497737},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1883.25,2031.75],"tickmode":"array","ticktext":["1900","1920","1940","1960","1980","2000","2020"],"tickvals":[1900,1920,1940,1960,1980,2000,2020],"categoryorder":"array","categoryarray":["1900","1920","1940","1960","1980","2000","2020"],"nticks":null,"ticks":"outside","tickcolor":"rgba(179,179,179,1)","ticklen":3.6529680365296811,"tickwidth":0.33208800332088001,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(222,222,222,1)","gridwidth":0.33208800332088001,"zeroline":false,"anchor":"y","title":{"text":"jaar","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-295.52480808352374,183.15832419445351],"tickmode":"array","ticktext":["-200","-100","0","100"],"tickvals":[-200,-100,0,100],"categoryorder":"array","categoryarray":["-200","-100","0","100"],"nticks":null,"ticks":"outside","tickcolor":"rgba(179,179,179,1)","ticklen":3.6529680365296811,"tickwidth":0.33208800332088001,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(222,222,222,1)","gridwidth":0.33208800332088001,"zeroline":false,"anchor":"x","title":{"text":"mm","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(179,179,179,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.8897637795275593,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"orientation":"h","x":0.5,"y":-0.14999999999999999,"xanchor":"center","yanchor":"top","title":{"text":"colour","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"a1430c1383d":{"x":{},"y":{},"colour":{},"type":"scatter"},"a1428cd6443":{"x":{},"y":{},"colour":{}},"a1463db2d9":{"x":{},"y":{},"colour":{}}},"cur_data":"a1430c1383d","visdat":{"a1430c1383d":["function (y) ","x"],"a1428cd6443":["function (y) ","x"],"a1463db2d9":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

<p class="caption">

Yearly average sea level height, surge anomaly (GTSM), and height -
surge anomaly for the averaged station Netherlands (without Delfzijl).
Values of surge anomaly are presented 200 mm lower for visualization
purpose The yearly averages surge is calculated for 1950 - now. For
earlier years, an average surge is assumed.
</p>

</div>

``` r
# q
```

## Trend analysis

The sea level trend is calculated using generalized linear regression.
the following models are tested [Deltares
2022](https://publications.deltares.nl/11209266_000.pdf)):

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
  addBreakPoints() %>%
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
| linear | $`height = \alpha + \beta_{1}(year\ -\ epoch) + \beta_{2}(cos(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \beta_{3}(sin(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \epsilon`$ |
| broken_linear | $`height = \alpha + \beta_{1}(year\ -\ epoch) + \beta_{2}(from1993) + \beta_{3}(cos(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \beta_{4}(sin(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \epsilon`$ |
| broken_squared | $`height = \alpha + \beta_{1}(year\ -\ epoch) + \beta_{2}(from1960\_square) + \beta_{3}(cos(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \beta_{4}(sin(2\ *\ pi\ *\ (year\ -\ epoch)/(18.613))) + \epsilon`$ |

## Autocorrelation

Autocorrelation with previous year(s) can sometimes explain part of the
otherwise unexplained variance. Especially when a trend is detected in
the data, autocorrelation with relatively short lags (1 or few years)
often occurs. In case of autocorrelation, we recalculate standard errors
of the estimated parameters accordingly. Note that the nodal cycle
(period of 18.6 years) is already included in the regression, and is
therefore not expected to show up in this analysis.

``` r
library(ggfortify)

  models %>%
    mutate(
      ACF = map(augment, function(x) ggplot2::fortify(acf(x$.resid, plot = F)))
    ) %>%
    unnest(ACF) %>%
    mutate(ACF_pass = (ACF >= lower & ACF <= upper)) %>%
    filter(Lag >= 1) %>%
    ggplot(aes(Lag, ACF)) +
    geom_col(width = 0.4, aes(fill = ACF_pass)) +
    geom_vline(xintercept = 8.9, linetype = 3) +
    geom_vline(xintercept = 18.6, linetype = 3) +
    geom_line(aes(y = lower), linetype = "dotdash", linewidth = 0.5) +
    geom_line(aes(y = upper), linetype = "dotdash", linewidth = 0.5) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(10)
    )+
    facet_grid(station ~ modeltype) +
    theme_minimal() +
    theme(
      strip.text.y = element_text(angle = 0),
      legend.position = "bottom"
    )
```

<figure>
<img
src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/acf-plot-1.png"
alt="Autocorrelation plot for selected stations and models." />
<figcaption aria-hidden="true">Autocorrelation plot for selected
stations and models.</figcaption>
</figure>

There appears to be a consistent autocorrelation for all stations and
models with a lag of one year. Altough the autocorrelation does not
influence the value of the calculated trend parameters, it needs to be
taken into account when calculating standard errors. The [Newey West
autocorrelatie
term](https://search.r-project.org/CRAN/refmans/sandwich/html/NeweyWest.html)
is used to correctly calculate the standard errors.

At station ‘Vlissingen’ and ‘Harlingen’ there is autocorrelation with a
‘lag’ of 9 years. This could indicate an effect of the 8.8 year ‘lunar
perigee cycle’. Because this only occurs at two stations, this tidal
component is not further accounted for in the analysis.

There is no apparent autocorrelation with a ‘lag’ of 18.6 years, the
‘nodal tide’ cycle. This is because the nodal tide is already
incorporated in all three models.

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

<img src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/unnamed-chunk-21-1.png" alt="" width="100%" />

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

![](C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

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
<img
src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/prediction-plot-1.png"
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

![](C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

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

The broken linear model has one more degree of freedom than the linear
model. The broken linear model is significantly better than the linear
model (p \< 0.001).

## Conclusions

Based on the above analysis, the following conclusions are drawn:

Based on variance analysis the broken linear model is significantly
better than the linear model. the broken linear model is accepted as the
preferred model.

</div>
