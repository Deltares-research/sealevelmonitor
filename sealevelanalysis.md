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

<div class="figure">

<div class="leaflet html-widget html-fill-item" id="htmlwidget-60ecd45b4d730a5cc257" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-60ecd45b4d730a5cc257">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addCircleMarkers","args":[[51.442,51.978,52.964,53.326,53.176,52.462],[3.596,4.12,4.745,6.933,5.409,4.555],10,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":0.5,"fill":true,"fillColor":"#03F","fillOpacity":0.2},null,null,null,null,["Vlissingen","Hoek van Holland","Den Helder","Delfzijl","Harlingen","IJmuiden"],{"interactive":false,"permanent":true,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[51.442,53.326],"lng":[3.596,6.933]},"setView":[[52.558,4.893],6,[]]},"evals":[],"jsHooks":[]}</script>

<p class="caption">

Hoofdgetijstations in Nederland. Er is aangegeven welke stations zijn
meegenomen in dit rekendocument.
</p>

</div>

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

<div class="figure">

<div class="plotly html-widget html-fill-item" id="htmlwidget-1bad2fadc2f4be276a81" style="width:768px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-1bad2fadc2f4be276a81">{"x":{"data":[{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,27.713035494313395,7.4605935196759958,20.420001401951389,-13.233236336269409,34.15625005880829,21.534779630953288,0.17063638649599255,11.66402674417359,5.8834435632908963,-34.318319319854808,-15.096566283902405,32.53519537306989,26.018170877903096,-22.470204618165205,-17.273601995548308,32.202022635532096,15.63605001996919,65.585686194277798,-5.4678310294283108,-24.710310133689703,21.561329932469796,-7.0491375751489045,-41.718684086440064,41.221570776081201,39.55002131575359,-5.047391642630302,-21.365247872776504,24.292009704753688,6.1683848737238947,-6.0287025774877065,-0.48590949810300543,39.952639603334198,11.836220464704098,57.712138545799789,-3.9662433450976025,6.8643658022382965,13.441716973197195,-22.934923262419204,40.318191712279493,26.954826838950588,79.251761833470795,0.97244890470409484,17.309590824847795,3.7959645436081928,36.49411083347119,20.514533820594501,-69.919852011764206,-15.856840259679505,47.630141238950593,42.733829369087594,31.736447187689301,14.483148471361597,-12.356194003789106,-14.736027029542504,27.289641622115504,9.8447224170327985,14.440937181416396,47.841784063608195,35.042373933591001,-0.79500116104940588,-22.625290443241106,28.799493855388995,13.303858529765796,-16.922994026802805,3.7932824748409928,47.809423904704097,-12.631897035808009,41.136694880046498,-31.049220539131607,20.837498945799982,23.159536302443392,-3.4653549473507042,11.248780662698891,35.814373412789301,24.510861811644094],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1891<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1892<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1893<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1894<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1895<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1896<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1897<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1898<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1899<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1900<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1901<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1902<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1903<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1904<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1905<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1906<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1907<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1908<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1909<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1910<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1911<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1912<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1913<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1914<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1915<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1916<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1917<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1918<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1919<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1920<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1921<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1922<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1923<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1924<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1925<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1926<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1927<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1928<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1929<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1930<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1931<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1932<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1933<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1934<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1935<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1936<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1937<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1938<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1939<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1940<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1941<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1942<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1943<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1944<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1945<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1946<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1947<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1948<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1949<br />surge_anomaly:   0.0000000<br />station: Delfzijl","year: 1950<br />surge_anomaly:  27.7130355<br />station: Delfzijl","year: 1951<br />surge_anomaly:   7.4605935<br />station: Delfzijl","year: 1952<br />surge_anomaly:  20.4200014<br />station: Delfzijl","year: 1953<br />surge_anomaly: -13.2332363<br />station: Delfzijl","year: 1954<br />surge_anomaly:  34.1562501<br />station: Delfzijl","year: 1955<br />surge_anomaly:  21.5347796<br />station: Delfzijl","year: 1956<br />surge_anomaly:   0.1706364<br />station: Delfzijl","year: 1957<br />surge_anomaly:  11.6640267<br />station: Delfzijl","year: 1958<br />surge_anomaly:   5.8834436<br />station: Delfzijl","year: 1959<br />surge_anomaly: -34.3183193<br />station: Delfzijl","year: 1960<br />surge_anomaly: -15.0965663<br />station: Delfzijl","year: 1961<br />surge_anomaly:  32.5351954<br />station: Delfzijl","year: 1962<br />surge_anomaly:  26.0181709<br />station: Delfzijl","year: 1963<br />surge_anomaly: -22.4702046<br />station: Delfzijl","year: 1964<br />surge_anomaly: -17.2736020<br />station: Delfzijl","year: 1965<br />surge_anomaly:  32.2020226<br />station: Delfzijl","year: 1966<br />surge_anomaly:  15.6360500<br />station: Delfzijl","year: 1967<br />surge_anomaly:  65.5856862<br />station: Delfzijl","year: 1968<br />surge_anomaly:  -5.4678310<br />station: Delfzijl","year: 1969<br />surge_anomaly: -24.7103101<br />station: Delfzijl","year: 1970<br />surge_anomaly:  21.5613299<br />station: Delfzijl","year: 1971<br />surge_anomaly:  -7.0491376<br />station: Delfzijl","year: 1972<br />surge_anomaly: -41.7186841<br />station: Delfzijl","year: 1973<br />surge_anomaly:  41.2215708<br />station: Delfzijl","year: 1974<br />surge_anomaly:  39.5500213<br />station: Delfzijl","year: 1975<br />surge_anomaly:  -5.0473916<br />station: Delfzijl","year: 1976<br />surge_anomaly: -21.3652479<br />station: Delfzijl","year: 1977<br />surge_anomaly:  24.2920097<br />station: Delfzijl","year: 1978<br />surge_anomaly:   6.1683849<br />station: Delfzijl","year: 1979<br />surge_anomaly:  -6.0287026<br />station: Delfzijl","year: 1980<br />surge_anomaly:  -0.4859095<br />station: Delfzijl","year: 1981<br />surge_anomaly:  39.9526396<br />station: Delfzijl","year: 1982<br />surge_anomaly:  11.8362205<br />station: Delfzijl","year: 1983<br />surge_anomaly:  57.7121385<br />station: Delfzijl","year: 1984<br />surge_anomaly:  -3.9662433<br />station: Delfzijl","year: 1985<br />surge_anomaly:   6.8643658<br />station: Delfzijl","year: 1986<br />surge_anomaly:  13.4417170<br />station: Delfzijl","year: 1987<br />surge_anomaly: -22.9349233<br />station: Delfzijl","year: 1988<br />surge_anomaly:  40.3181917<br />station: Delfzijl","year: 1989<br />surge_anomaly:  26.9548268<br />station: Delfzijl","year: 1990<br />surge_anomaly:  79.2517618<br />station: Delfzijl","year: 1991<br />surge_anomaly:   0.9724489<br />station: Delfzijl","year: 1992<br />surge_anomaly:  17.3095908<br />station: Delfzijl","year: 1993<br />surge_anomaly:   3.7959645<br />station: Delfzijl","year: 1994<br />surge_anomaly:  36.4941108<br />station: Delfzijl","year: 1995<br />surge_anomaly:  20.5145338<br />station: Delfzijl","year: 1996<br />surge_anomaly: -69.9198520<br />station: Delfzijl","year: 1997<br />surge_anomaly: -15.8568403<br />station: Delfzijl","year: 1998<br />surge_anomaly:  47.6301412<br />station: Delfzijl","year: 1999<br />surge_anomaly:  42.7338294<br />station: Delfzijl","year: 2000<br />surge_anomaly:  31.7364472<br />station: Delfzijl","year: 2001<br />surge_anomaly:  14.4831485<br />station: Delfzijl","year: 2002<br />surge_anomaly: -12.3561940<br />station: Delfzijl","year: 2003<br />surge_anomaly: -14.7360270<br />station: Delfzijl","year: 2004<br />surge_anomaly:  27.2896416<br />station: Delfzijl","year: 2005<br />surge_anomaly:   9.8447224<br />station: Delfzijl","year: 2006<br />surge_anomaly:  14.4409372<br />station: Delfzijl","year: 2007<br />surge_anomaly:  47.8417841<br />station: Delfzijl","year: 2008<br />surge_anomaly:  35.0423739<br />station: Delfzijl","year: 2009<br />surge_anomaly:  -0.7950012<br />station: Delfzijl","year: 2010<br />surge_anomaly: -22.6252904<br />station: Delfzijl","year: 2011<br />surge_anomaly:  28.7994939<br />station: Delfzijl","year: 2012<br />surge_anomaly:  13.3038585<br />station: Delfzijl","year: 2013<br />surge_anomaly: -16.9229940<br />station: Delfzijl","year: 2014<br />surge_anomaly:   3.7932825<br />station: Delfzijl","year: 2015<br />surge_anomaly:  47.8094239<br />station: Delfzijl","year: 2016<br />surge_anomaly: -12.6318970<br />station: Delfzijl","year: 2017<br />surge_anomaly:  41.1366949<br />station: Delfzijl","year: 2018<br />surge_anomaly: -31.0492205<br />station: Delfzijl","year: 2019<br />surge_anomaly:  20.8374989<br />station: Delfzijl","year: 2020<br />surge_anomaly:  23.1595363<br />station: Delfzijl","year: 2021<br />surge_anomaly:  -3.4653549<br />station: Delfzijl","year: 2022<br />surge_anomaly:  11.2487807<br />station: Delfzijl","year: 2023<br />surge_anomaly:  35.8143734<br />station: Delfzijl","year: 2024<br />surge_anomaly:  24.5108618<br />station: Delfzijl"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"Delfzijl","legendgroup":"Delfzijl","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(248,118,109,0.5)","dash":"solid"},"frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22.617998554098101,6.2938222885843942,10.413894036198194,-28.753985971919409,25.567344834563393,4.3054345670829974,-16.675464402269807,-0.56850694952280634,2.9138652615144949,-43.724834105566586,-6.0880685877374034,23.858346522895197,7.3142506980525965,-29.672824663941508,-33.584121047713907,27.11237305466009,18.445172692324796,50.525636594099289,-10.610599188303404,-25.815868912780303,13.943469002046591,-23.453067354633607,-46.286356284701789,15.516283912425791,37.416263036398995,-19.697916981578107,-36.055503006410106,19.410209121177196,-0.97267489602740642,-2.2324310076247045,-11.352403886081206,23.252746277306791,0.77689203347119218,35.864734836210907,-13.757451898649506,-7.8893948268028069,-0.17161928351510625,-31.89584580488501,29.135974062006209,8.0387537184026954,57.131456507443794,-22.170244346419206,3.2344898680171923,-12.119263188446602,25.558815854567097,3.1277998827862916,-78.387017411491001,-33.645702215843905,40.045526184156095,33.479516934841094,25.901076980038987,7.9874995425122961,-18.845755828939804,-33.04147320378911,14.995220488235788,-4.6144157158439043,2.6908987019642936,25.933386091005396,22.880749894383293,-7.1830492514603108,-28.285030082693208,12.515380647580791,0.93462032293519559,-27.669804694200007,0.2085170353889936,23.891395384156091,-20.552127890999209,21.935061836758884,-41.553986119953485,10.459732412649297,12.310212011623696,-14.402800928172709,-7.0186278864744054,27.620422547422599,20.706844541079981],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1891<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1892<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1893<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1894<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1895<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1896<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1897<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1898<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1899<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1900<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1901<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1902<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1903<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1904<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1905<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1906<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1907<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1908<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1909<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1910<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1911<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1912<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1913<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1914<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1915<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1916<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1917<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1918<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1919<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1920<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1921<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1922<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1923<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1924<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1925<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1926<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1927<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1928<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1929<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1930<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1931<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1932<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1933<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1934<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1935<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1936<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1937<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1938<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1939<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1940<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1941<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1942<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1943<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1944<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1945<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1946<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1947<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1948<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1949<br />surge_anomaly:   0.0000000<br />station: Den Helder","year: 1950<br />surge_anomaly:  22.6179986<br />station: Den Helder","year: 1951<br />surge_anomaly:   6.2938223<br />station: Den Helder","year: 1952<br />surge_anomaly:  10.4138940<br />station: Den Helder","year: 1953<br />surge_anomaly: -28.7539860<br />station: Den Helder","year: 1954<br />surge_anomaly:  25.5673448<br />station: Den Helder","year: 1955<br />surge_anomaly:   4.3054346<br />station: Den Helder","year: 1956<br />surge_anomaly: -16.6754644<br />station: Den Helder","year: 1957<br />surge_anomaly:  -0.5685069<br />station: Den Helder","year: 1958<br />surge_anomaly:   2.9138653<br />station: Den Helder","year: 1959<br />surge_anomaly: -43.7248341<br />station: Den Helder","year: 1960<br />surge_anomaly:  -6.0880686<br />station: Den Helder","year: 1961<br />surge_anomaly:  23.8583465<br />station: Den Helder","year: 1962<br />surge_anomaly:   7.3142507<br />station: Den Helder","year: 1963<br />surge_anomaly: -29.6728247<br />station: Den Helder","year: 1964<br />surge_anomaly: -33.5841210<br />station: Den Helder","year: 1965<br />surge_anomaly:  27.1123731<br />station: Den Helder","year: 1966<br />surge_anomaly:  18.4451727<br />station: Den Helder","year: 1967<br />surge_anomaly:  50.5256366<br />station: Den Helder","year: 1968<br />surge_anomaly: -10.6105992<br />station: Den Helder","year: 1969<br />surge_anomaly: -25.8158689<br />station: Den Helder","year: 1970<br />surge_anomaly:  13.9434690<br />station: Den Helder","year: 1971<br />surge_anomaly: -23.4530674<br />station: Den Helder","year: 1972<br />surge_anomaly: -46.2863563<br />station: Den Helder","year: 1973<br />surge_anomaly:  15.5162839<br />station: Den Helder","year: 1974<br />surge_anomaly:  37.4162630<br />station: Den Helder","year: 1975<br />surge_anomaly: -19.6979170<br />station: Den Helder","year: 1976<br />surge_anomaly: -36.0555030<br />station: Den Helder","year: 1977<br />surge_anomaly:  19.4102091<br />station: Den Helder","year: 1978<br />surge_anomaly:  -0.9726749<br />station: Den Helder","year: 1979<br />surge_anomaly:  -2.2324310<br />station: Den Helder","year: 1980<br />surge_anomaly: -11.3524039<br />station: Den Helder","year: 1981<br />surge_anomaly:  23.2527463<br />station: Den Helder","year: 1982<br />surge_anomaly:   0.7768920<br />station: Den Helder","year: 1983<br />surge_anomaly:  35.8647348<br />station: Den Helder","year: 1984<br />surge_anomaly: -13.7574519<br />station: Den Helder","year: 1985<br />surge_anomaly:  -7.8893948<br />station: Den Helder","year: 1986<br />surge_anomaly:  -0.1716193<br />station: Den Helder","year: 1987<br />surge_anomaly: -31.8958458<br />station: Den Helder","year: 1988<br />surge_anomaly:  29.1359741<br />station: Den Helder","year: 1989<br />surge_anomaly:   8.0387537<br />station: Den Helder","year: 1990<br />surge_anomaly:  57.1314565<br />station: Den Helder","year: 1991<br />surge_anomaly: -22.1702443<br />station: Den Helder","year: 1992<br />surge_anomaly:   3.2344899<br />station: Den Helder","year: 1993<br />surge_anomaly: -12.1192632<br />station: Den Helder","year: 1994<br />surge_anomaly:  25.5588159<br />station: Den Helder","year: 1995<br />surge_anomaly:   3.1277999<br />station: Den Helder","year: 1996<br />surge_anomaly: -78.3870174<br />station: Den Helder","year: 1997<br />surge_anomaly: -33.6457022<br />station: Den Helder","year: 1998<br />surge_anomaly:  40.0455262<br />station: Den Helder","year: 1999<br />surge_anomaly:  33.4795169<br />station: Den Helder","year: 2000<br />surge_anomaly:  25.9010770<br />station: Den Helder","year: 2001<br />surge_anomaly:   7.9874995<br />station: Den Helder","year: 2002<br />surge_anomaly: -18.8457558<br />station: Den Helder","year: 2003<br />surge_anomaly: -33.0414732<br />station: Den Helder","year: 2004<br />surge_anomaly:  14.9952205<br />station: Den Helder","year: 2005<br />surge_anomaly:  -4.6144157<br />station: Den Helder","year: 2006<br />surge_anomaly:   2.6908987<br />station: Den Helder","year: 2007<br />surge_anomaly:  25.9333861<br />station: Den Helder","year: 2008<br />surge_anomaly:  22.8807499<br />station: Den Helder","year: 2009<br />surge_anomaly:  -7.1830493<br />station: Den Helder","year: 2010<br />surge_anomaly: -28.2850301<br />station: Den Helder","year: 2011<br />surge_anomaly:  12.5153806<br />station: Den Helder","year: 2012<br />surge_anomaly:   0.9346203<br />station: Den Helder","year: 2013<br />surge_anomaly: -27.6698047<br />station: Den Helder","year: 2014<br />surge_anomaly:   0.2085170<br />station: Den Helder","year: 2015<br />surge_anomaly:  23.8913954<br />station: Den Helder","year: 2016<br />surge_anomaly: -20.5521279<br />station: Den Helder","year: 2017<br />surge_anomaly:  21.9350618<br />station: Den Helder","year: 2018<br />surge_anomaly: -41.5539861<br />station: Den Helder","year: 2019<br />surge_anomaly:  10.4597324<br />station: Den Helder","year: 2020<br />surge_anomaly:  12.3102120<br />station: Den Helder","year: 2021<br />surge_anomaly: -14.4028009<br />station: Den Helder","year: 2022<br />surge_anomaly:  -7.0186279<br />station: Den Helder","year: 2023<br />surge_anomaly:  27.6204225<br />station: Den Helder","year: 2024<br />surge_anomaly:  20.7068445<br />station: Den Helder"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(205,150,0,1)"}},"hoveron":"points","name":"Den Helder","legendgroup":"Den Helder","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(205,150,0,0.5)","dash":"solid"},"frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,44.005481024331182,23.034969170687297,25.506469748877301,-11.076622087194806,49.760412204188789,16.71335311322769,-1.2465238657239013,16.59851055155449,14.3450106080466,-33.929856314724702,0.74542246590348782,42.986944239453685,28.651570970901695,-18.314350884285503,-19.316788951413507,44.186663176731493,30.920602193147893,79.923558502541795,-0.38519059990470339,-20.899058831758907,30.1840086513184,-10.736138145789106,-37.526881768033704,37.189323223637182,56.448322800658794,-5.8956166989900041,-26.950792445568503,36.612377951834894,11.190162428866294,9.3822204836081937,3.4731918516236959,42.292628682786194,23.173340466347888,63.271777704703801,2.6131967614597968,10.304191880046496,20.978355321142395,-21.037728394474005,52.842762458180793,28.885691332101295,90.733536608813793,-1.8008495665288038,24.550246299711201,5.0402993184026954,48.374566647991692,20.543897704704086,-77.104586661217809,-18.070030095295905,64.678124592374786,56.026121431827796,48.494100990968001,22.0004215896356,-3.8694455078987104,-19.054373196665807,36.842284190967995,11.401065604704094,22.433775948539697,48.417277277306802,45.316902444519904,7.1103300101834961,-25.125340944611004,37.27612604716979,20.148853949984392,-16.927313941871308,16.176398183690388,55.1126567266218,-5.6114376950976066,44.842010641690401,-32.664994651460304,29.009007077306784,36.350404138508999,0.31260589100539704,11.805407967744801,47.505232358243994,36.746277921251597],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1891<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1892<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1893<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1894<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1895<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1896<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1897<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1898<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1899<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1900<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1901<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1902<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1903<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1904<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1905<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1906<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1907<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1908<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1909<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1910<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1911<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1912<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1913<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1914<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1915<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1916<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1917<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1918<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1919<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1920<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1921<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1922<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1923<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1924<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1925<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1926<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1927<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1928<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1929<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1930<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1931<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1932<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1933<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1934<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1935<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1936<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1937<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1938<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1939<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1940<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1941<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1942<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1943<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1944<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1945<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1946<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1947<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1948<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1949<br />surge_anomaly:   0.0000000<br />station: Harlingen","year: 1950<br />surge_anomaly:  44.0054810<br />station: Harlingen","year: 1951<br />surge_anomaly:  23.0349692<br />station: Harlingen","year: 1952<br />surge_anomaly:  25.5064697<br />station: Harlingen","year: 1953<br />surge_anomaly: -11.0766221<br />station: Harlingen","year: 1954<br />surge_anomaly:  49.7604122<br />station: Harlingen","year: 1955<br />surge_anomaly:  16.7133531<br />station: Harlingen","year: 1956<br />surge_anomaly:  -1.2465239<br />station: Harlingen","year: 1957<br />surge_anomaly:  16.5985106<br />station: Harlingen","year: 1958<br />surge_anomaly:  14.3450106<br />station: Harlingen","year: 1959<br />surge_anomaly: -33.9298563<br />station: Harlingen","year: 1960<br />surge_anomaly:   0.7454225<br />station: Harlingen","year: 1961<br />surge_anomaly:  42.9869442<br />station: Harlingen","year: 1962<br />surge_anomaly:  28.6515710<br />station: Harlingen","year: 1963<br />surge_anomaly: -18.3143509<br />station: Harlingen","year: 1964<br />surge_anomaly: -19.3167890<br />station: Harlingen","year: 1965<br />surge_anomaly:  44.1866632<br />station: Harlingen","year: 1966<br />surge_anomaly:  30.9206022<br />station: Harlingen","year: 1967<br />surge_anomaly:  79.9235585<br />station: Harlingen","year: 1968<br />surge_anomaly:  -0.3851906<br />station: Harlingen","year: 1969<br />surge_anomaly: -20.8990588<br />station: Harlingen","year: 1970<br />surge_anomaly:  30.1840087<br />station: Harlingen","year: 1971<br />surge_anomaly: -10.7361381<br />station: Harlingen","year: 1972<br />surge_anomaly: -37.5268818<br />station: Harlingen","year: 1973<br />surge_anomaly:  37.1893232<br />station: Harlingen","year: 1974<br />surge_anomaly:  56.4483228<br />station: Harlingen","year: 1975<br />surge_anomaly:  -5.8956167<br />station: Harlingen","year: 1976<br />surge_anomaly: -26.9507924<br />station: Harlingen","year: 1977<br />surge_anomaly:  36.6123780<br />station: Harlingen","year: 1978<br />surge_anomaly:  11.1901624<br />station: Harlingen","year: 1979<br />surge_anomaly:   9.3822205<br />station: Harlingen","year: 1980<br />surge_anomaly:   3.4731919<br />station: Harlingen","year: 1981<br />surge_anomaly:  42.2926287<br />station: Harlingen","year: 1982<br />surge_anomaly:  23.1733405<br />station: Harlingen","year: 1983<br />surge_anomaly:  63.2717777<br />station: Harlingen","year: 1984<br />surge_anomaly:   2.6131968<br />station: Harlingen","year: 1985<br />surge_anomaly:  10.3041919<br />station: Harlingen","year: 1986<br />surge_anomaly:  20.9783553<br />station: Harlingen","year: 1987<br />surge_anomaly: -21.0377284<br />station: Harlingen","year: 1988<br />surge_anomaly:  52.8427625<br />station: Harlingen","year: 1989<br />surge_anomaly:  28.8856913<br />station: Harlingen","year: 1990<br />surge_anomaly:  90.7335366<br />station: Harlingen","year: 1991<br />surge_anomaly:  -1.8008496<br />station: Harlingen","year: 1992<br />surge_anomaly:  24.5502463<br />station: Harlingen","year: 1993<br />surge_anomaly:   5.0402993<br />station: Harlingen","year: 1994<br />surge_anomaly:  48.3745666<br />station: Harlingen","year: 1995<br />surge_anomaly:  20.5438977<br />station: Harlingen","year: 1996<br />surge_anomaly: -77.1045867<br />station: Harlingen","year: 1997<br />surge_anomaly: -18.0700301<br />station: Harlingen","year: 1998<br />surge_anomaly:  64.6781246<br />station: Harlingen","year: 1999<br />surge_anomaly:  56.0261214<br />station: Harlingen","year: 2000<br />surge_anomaly:  48.4941010<br />station: Harlingen","year: 2001<br />surge_anomaly:  22.0004216<br />station: Harlingen","year: 2002<br />surge_anomaly:  -3.8694455<br />station: Harlingen","year: 2003<br />surge_anomaly: -19.0543732<br />station: Harlingen","year: 2004<br />surge_anomaly:  36.8422842<br />station: Harlingen","year: 2005<br />surge_anomaly:  11.4010656<br />station: Harlingen","year: 2006<br />surge_anomaly:  22.4337759<br />station: Harlingen","year: 2007<br />surge_anomaly:  48.4172773<br />station: Harlingen","year: 2008<br />surge_anomaly:  45.3169024<br />station: Harlingen","year: 2009<br />surge_anomaly:   7.1103300<br />station: Harlingen","year: 2010<br />surge_anomaly: -25.1253409<br />station: Harlingen","year: 2011<br />surge_anomaly:  37.2761260<br />station: Harlingen","year: 2012<br />surge_anomaly:  20.1488539<br />station: Harlingen","year: 2013<br />surge_anomaly: -16.9273139<br />station: Harlingen","year: 2014<br />surge_anomaly:  16.1763982<br />station: Harlingen","year: 2015<br />surge_anomaly:  55.1126567<br />station: Harlingen","year: 2016<br />surge_anomaly:  -5.6114377<br />station: Harlingen","year: 2017<br />surge_anomaly:  44.8420106<br />station: Harlingen","year: 2018<br />surge_anomaly: -32.6649947<br />station: Harlingen","year: 2019<br />surge_anomaly:  29.0090071<br />station: Harlingen","year: 2020<br />surge_anomaly:  36.3504041<br />station: Harlingen","year: 2021<br />surge_anomaly:   0.3126059<br />station: Harlingen","year: 2022<br />surge_anomaly:  11.8054080<br />station: Harlingen","year: 2023<br />surge_anomaly:  47.5052324<br />station: Harlingen","year: 2024<br />surge_anomaly:  36.7462779<br />station: Harlingen"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"Harlingen","legendgroup":"Harlingen","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(124,174,0,0.5)","dash":"solid"},"frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.9166319592712924,-7.774747021986407,2.3806546950047931,-40.747482228085424,8.8262927700290916,-0.46705518367480892,-23.060467058386905,-11.412602741404209,-1.9287880732387064,-48.020016757215359,-8.0871224233721009,9.9423635069431953,-5.9515943094038093,-35.177731162401706,-40.431986988319913,17.112430125852292,11.591346371275691,26.425742657237382,-13.785740890358303,-23.218550228685007,6.8936326803509971,-29.482577212236706,-48.937938207095129,3.2995018936792917,25.822315536765792,-27.771455643295702,-38.462429183180397,8.9876598698632932,-6.8572828258148064,-11.263174771186309,-16.827339106299807,12.691569347169789,-17.081103700775405,16.044569636210895,-21.134738728704104,-19.795209546254902,-13.796593330912408,-37.135546944063108,13.1440674810773,-8.3297053884466123,29.591637697854701,-34.678539528172706,-11.236385022693206,-20.070298602145307,9.7971842005944936,-5.7155858468028029,-71.537746331163106,-43.758585848720607,22.008819881416393,16.435405939498587,6.7110847860499945,0.97953295812869356,-27.066780394748005,-41.046343984337042,-0.51383598280250453,-13.683658307624704,-12.110215119953509,13.0294115485397,4.063344627579994,-16.491374767624709,-24.642982265159009,-6.6999788295425091,-12.608545323239705,-33.048413192008304,-13.319480999405506,-1.4076465144740029,-30.243782047283403,4.5734272148409971,-46.109641117213755,-4.4216103958439064,-7.3653467112178035,-24.188381640501404,-22.019426238766705,13.593750084170892,8.4589661878107947],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1891<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1892<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1893<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1894<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1895<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1896<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1897<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1898<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1899<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1900<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1901<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1902<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1903<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1904<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1905<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1906<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1907<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1908<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1909<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1910<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1911<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1912<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1913<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1914<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1915<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1916<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1917<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1918<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1919<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1920<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1921<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1922<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1923<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1924<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1925<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1926<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1927<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1928<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1929<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1930<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1931<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1932<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1933<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1934<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1935<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1936<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1937<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1938<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1939<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1940<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1941<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1942<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1943<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1944<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1945<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1946<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1947<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1948<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1949<br />surge_anomaly:   0.0000000<br />station: Hoek van Holland","year: 1950<br />surge_anomaly:   5.9166320<br />station: Hoek van Holland","year: 1951<br />surge_anomaly:  -7.7747470<br />station: Hoek van Holland","year: 1952<br />surge_anomaly:   2.3806547<br />station: Hoek van Holland","year: 1953<br />surge_anomaly: -40.7474822<br />station: Hoek van Holland","year: 1954<br />surge_anomaly:   8.8262928<br />station: Hoek van Holland","year: 1955<br />surge_anomaly:  -0.4670552<br />station: Hoek van Holland","year: 1956<br />surge_anomaly: -23.0604671<br />station: Hoek van Holland","year: 1957<br />surge_anomaly: -11.4126027<br />station: Hoek van Holland","year: 1958<br />surge_anomaly:  -1.9287881<br />station: Hoek van Holland","year: 1959<br />surge_anomaly: -48.0200168<br />station: Hoek van Holland","year: 1960<br />surge_anomaly:  -8.0871224<br />station: Hoek van Holland","year: 1961<br />surge_anomaly:   9.9423635<br />station: Hoek van Holland","year: 1962<br />surge_anomaly:  -5.9515943<br />station: Hoek van Holland","year: 1963<br />surge_anomaly: -35.1777312<br />station: Hoek van Holland","year: 1964<br />surge_anomaly: -40.4319870<br />station: Hoek van Holland","year: 1965<br />surge_anomaly:  17.1124301<br />station: Hoek van Holland","year: 1966<br />surge_anomaly:  11.5913464<br />station: Hoek van Holland","year: 1967<br />surge_anomaly:  26.4257427<br />station: Hoek van Holland","year: 1968<br />surge_anomaly: -13.7857409<br />station: Hoek van Holland","year: 1969<br />surge_anomaly: -23.2185502<br />station: Hoek van Holland","year: 1970<br />surge_anomaly:   6.8936327<br />station: Hoek van Holland","year: 1971<br />surge_anomaly: -29.4825772<br />station: Hoek van Holland","year: 1972<br />surge_anomaly: -48.9379382<br />station: Hoek van Holland","year: 1973<br />surge_anomaly:   3.2995019<br />station: Hoek van Holland","year: 1974<br />surge_anomaly:  25.8223155<br />station: Hoek van Holland","year: 1975<br />surge_anomaly: -27.7714556<br />station: Hoek van Holland","year: 1976<br />surge_anomaly: -38.4624292<br />station: Hoek van Holland","year: 1977<br />surge_anomaly:   8.9876599<br />station: Hoek van Holland","year: 1978<br />surge_anomaly:  -6.8572828<br />station: Hoek van Holland","year: 1979<br />surge_anomaly: -11.2631748<br />station: Hoek van Holland","year: 1980<br />surge_anomaly: -16.8273391<br />station: Hoek van Holland","year: 1981<br />surge_anomaly:  12.6915693<br />station: Hoek van Holland","year: 1982<br />surge_anomaly: -17.0811037<br />station: Hoek van Holland","year: 1983<br />surge_anomaly:  16.0445696<br />station: Hoek van Holland","year: 1984<br />surge_anomaly: -21.1347387<br />station: Hoek van Holland","year: 1985<br />surge_anomaly: -19.7952095<br />station: Hoek van Holland","year: 1986<br />surge_anomaly: -13.7965933<br />station: Hoek van Holland","year: 1987<br />surge_anomaly: -37.1355469<br />station: Hoek van Holland","year: 1988<br />surge_anomaly:  13.1440675<br />station: Hoek van Holland","year: 1989<br />surge_anomaly:  -8.3297054<br />station: Hoek van Holland","year: 1990<br />surge_anomaly:  29.5916377<br />station: Hoek van Holland","year: 1991<br />surge_anomaly: -34.6785395<br />station: Hoek van Holland","year: 1992<br />surge_anomaly: -11.2363850<br />station: Hoek van Holland","year: 1993<br />surge_anomaly: -20.0702986<br />station: Hoek van Holland","year: 1994<br />surge_anomaly:   9.7971842<br />station: Hoek van Holland","year: 1995<br />surge_anomaly:  -5.7155858<br />station: Hoek van Holland","year: 1996<br />surge_anomaly: -71.5377463<br />station: Hoek van Holland","year: 1997<br />surge_anomaly: -43.7585858<br />station: Hoek van Holland","year: 1998<br />surge_anomaly:  22.0088199<br />station: Hoek van Holland","year: 1999<br />surge_anomaly:  16.4354059<br />station: Hoek van Holland","year: 2000<br />surge_anomaly:   6.7110848<br />station: Hoek van Holland","year: 2001<br />surge_anomaly:   0.9795330<br />station: Hoek van Holland","year: 2002<br />surge_anomaly: -27.0667804<br />station: Hoek van Holland","year: 2003<br />surge_anomaly: -41.0463440<br />station: Hoek van Holland","year: 2004<br />surge_anomaly:  -0.5138360<br />station: Hoek van Holland","year: 2005<br />surge_anomaly: -13.6836583<br />station: Hoek van Holland","year: 2006<br />surge_anomaly: -12.1102151<br />station: Hoek van Holland","year: 2007<br />surge_anomaly:  13.0294115<br />station: Hoek van Holland","year: 2008<br />surge_anomaly:   4.0633446<br />station: Hoek van Holland","year: 2009<br />surge_anomaly: -16.4913748<br />station: Hoek van Holland","year: 2010<br />surge_anomaly: -24.6429823<br />station: Hoek van Holland","year: 2011<br />surge_anomaly:  -6.6999788<br />station: Hoek van Holland","year: 2012<br />surge_anomaly: -12.6085453<br />station: Hoek van Holland","year: 2013<br />surge_anomaly: -33.0484132<br />station: Hoek van Holland","year: 2014<br />surge_anomaly: -13.3194810<br />station: Hoek van Holland","year: 2015<br />surge_anomaly:  -1.4076465<br />station: Hoek van Holland","year: 2016<br />surge_anomaly: -30.2437820<br />station: Hoek van Holland","year: 2017<br />surge_anomaly:   4.5734272<br />station: Hoek van Holland","year: 2018<br />surge_anomaly: -46.1096411<br />station: Hoek van Holland","year: 2019<br />surge_anomaly:  -4.4216104<br />station: Hoek van Holland","year: 2020<br />surge_anomaly:  -7.3653467<br />station: Hoek van Holland","year: 2021<br />surge_anomaly: -24.1883816<br />station: Hoek van Holland","year: 2022<br />surge_anomaly: -22.0194262<br />station: Hoek van Holland","year: 2023<br />surge_anomaly:  13.5937501<br />station: Hoek van Holland","year: 2024<br />surge_anomaly:   8.4589662<br />station: Hoek van Holland"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(0,190,103,1)"}},"hoveron":"points","name":"Hoek van Holland","legendgroup":"Hoek van Holland","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(0,190,103,0.5)","dash":"solid"},"frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,18.403674224615891,2.4177891273405905,9.1974773138055923,-32.699549312236606,21.585329974881397,3.6501964740146988,-17.503799396507308,-3.5092494915204071,3.4910595881099979,-44.261801840436618,-3.5977794620390071,20.392552419496504,3.878418089000093,-30.266060611480903,-35.342317772221406,26.181250627644793,19.274827042589187,43.618721946889998,-9.6562680070317057,-22.099194440205004,14.330029830254697,-24.802469109305207,-45.577283100501219,12.462462386059997,36.505995979884496,-22.077866191304903,-35.705403104644702,17.96137276242829,-0.63579383245500765,-3.5884807939261023,-10.833940659305206,21.585675247169796,-4.711430602145299,29.893114433471204,-14.392579912037505,-11.029487262419202,-3.5133427900905048,-32.282938537761709,25.518266072935202,2.3683011841560955,48.360056682786301,-25.868038925432906,-0.30789460466041163,-12.857725095843904,22.291683641690405,1.4017562189505952,-74.362848229824309,-36.870358517213802,36.326301989635603,29.205742225251988,20.786690160366895,8.015833140977989,-19.804877026254808,-35.784976779559003,11.180293893426992,-6.5210365035151057,-1.6874239281727057,23.062954945799991,17.292526195885991,-9.0398039966658033,-25.064969428720605,6.206840299224595,-2.6140669024746046,-28.676131931734307,-3.4147639199535078,15.312976076758892,-22.794034879523807,16.807979659224593,-42.480148169268553,6.5482555430601934,6.8083555947930918,-16.590634815843906,-11.925902463756202,25.5944241585591,18.508782408809505],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1891<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1892<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1893<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1894<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1895<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1896<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1897<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1898<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1899<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1900<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1901<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1902<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1903<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1904<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1905<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1906<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1907<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1908<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1909<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1910<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1911<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1912<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1913<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1914<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1915<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1916<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1917<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1918<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1919<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1920<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1921<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1922<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1923<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1924<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1925<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1926<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1927<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1928<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1929<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1930<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1931<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1932<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1933<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1934<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1935<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1936<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1937<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1938<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1939<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1940<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1941<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1942<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1943<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1944<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1945<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1946<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1947<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1948<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1949<br />surge_anomaly:   0.0000000<br />station: IJmuiden","year: 1950<br />surge_anomaly:  18.4036742<br />station: IJmuiden","year: 1951<br />surge_anomaly:   2.4177891<br />station: IJmuiden","year: 1952<br />surge_anomaly:   9.1974773<br />station: IJmuiden","year: 1953<br />surge_anomaly: -32.6995493<br />station: IJmuiden","year: 1954<br />surge_anomaly:  21.5853300<br />station: IJmuiden","year: 1955<br />surge_anomaly:   3.6501965<br />station: IJmuiden","year: 1956<br />surge_anomaly: -17.5037994<br />station: IJmuiden","year: 1957<br />surge_anomaly:  -3.5092495<br />station: IJmuiden","year: 1958<br />surge_anomaly:   3.4910596<br />station: IJmuiden","year: 1959<br />surge_anomaly: -44.2618018<br />station: IJmuiden","year: 1960<br />surge_anomaly:  -3.5977795<br />station: IJmuiden","year: 1961<br />surge_anomaly:  20.3925524<br />station: IJmuiden","year: 1962<br />surge_anomaly:   3.8784181<br />station: IJmuiden","year: 1963<br />surge_anomaly: -30.2660606<br />station: IJmuiden","year: 1964<br />surge_anomaly: -35.3423178<br />station: IJmuiden","year: 1965<br />surge_anomaly:  26.1812506<br />station: IJmuiden","year: 1966<br />surge_anomaly:  19.2748270<br />station: IJmuiden","year: 1967<br />surge_anomaly:  43.6187219<br />station: IJmuiden","year: 1968<br />surge_anomaly:  -9.6562680<br />station: IJmuiden","year: 1969<br />surge_anomaly: -22.0991944<br />station: IJmuiden","year: 1970<br />surge_anomaly:  14.3300298<br />station: IJmuiden","year: 1971<br />surge_anomaly: -24.8024691<br />station: IJmuiden","year: 1972<br />surge_anomaly: -45.5772831<br />station: IJmuiden","year: 1973<br />surge_anomaly:  12.4624624<br />station: IJmuiden","year: 1974<br />surge_anomaly:  36.5059960<br />station: IJmuiden","year: 1975<br />surge_anomaly: -22.0778662<br />station: IJmuiden","year: 1976<br />surge_anomaly: -35.7054031<br />station: IJmuiden","year: 1977<br />surge_anomaly:  17.9613728<br />station: IJmuiden","year: 1978<br />surge_anomaly:  -0.6357938<br />station: IJmuiden","year: 1979<br />surge_anomaly:  -3.5884808<br />station: IJmuiden","year: 1980<br />surge_anomaly: -10.8339407<br />station: IJmuiden","year: 1981<br />surge_anomaly:  21.5856752<br />station: IJmuiden","year: 1982<br />surge_anomaly:  -4.7114306<br />station: IJmuiden","year: 1983<br />surge_anomaly:  29.8931144<br />station: IJmuiden","year: 1984<br />surge_anomaly: -14.3925799<br />station: IJmuiden","year: 1985<br />surge_anomaly: -11.0294873<br />station: IJmuiden","year: 1986<br />surge_anomaly:  -3.5133428<br />station: IJmuiden","year: 1987<br />surge_anomaly: -32.2829385<br />station: IJmuiden","year: 1988<br />surge_anomaly:  25.5182661<br />station: IJmuiden","year: 1989<br />surge_anomaly:   2.3683012<br />station: IJmuiden","year: 1990<br />surge_anomaly:  48.3600567<br />station: IJmuiden","year: 1991<br />surge_anomaly: -25.8680389<br />station: IJmuiden","year: 1992<br />surge_anomaly:  -0.3078946<br />station: IJmuiden","year: 1993<br />surge_anomaly: -12.8577251<br />station: IJmuiden","year: 1994<br />surge_anomaly:  22.2916836<br />station: IJmuiden","year: 1995<br />surge_anomaly:   1.4017562<br />station: IJmuiden","year: 1996<br />surge_anomaly: -74.3628482<br />station: IJmuiden","year: 1997<br />surge_anomaly: -36.8703585<br />station: IJmuiden","year: 1998<br />surge_anomaly:  36.3263020<br />station: IJmuiden","year: 1999<br />surge_anomaly:  29.2057422<br />station: IJmuiden","year: 2000<br />surge_anomaly:  20.7866902<br />station: IJmuiden","year: 2001<br />surge_anomaly:   8.0158331<br />station: IJmuiden","year: 2002<br />surge_anomaly: -19.8048770<br />station: IJmuiden","year: 2003<br />surge_anomaly: -35.7849768<br />station: IJmuiden","year: 2004<br />surge_anomaly:  11.1802939<br />station: IJmuiden","year: 2005<br />surge_anomaly:  -6.5210365<br />station: IJmuiden","year: 2006<br />surge_anomaly:  -1.6874239<br />station: IJmuiden","year: 2007<br />surge_anomaly:  23.0629549<br />station: IJmuiden","year: 2008<br />surge_anomaly:  17.2925262<br />station: IJmuiden","year: 2009<br />surge_anomaly:  -9.0398040<br />station: IJmuiden","year: 2010<br />surge_anomaly: -25.0649694<br />station: IJmuiden","year: 2011<br />surge_anomaly:   6.2068403<br />station: IJmuiden","year: 2012<br />surge_anomaly:  -2.6140669<br />station: IJmuiden","year: 2013<br />surge_anomaly: -28.6761319<br />station: IJmuiden","year: 2014<br />surge_anomaly:  -3.4147639<br />station: IJmuiden","year: 2015<br />surge_anomaly:  15.3129761<br />station: IJmuiden","year: 2016<br />surge_anomaly: -22.7940349<br />station: IJmuiden","year: 2017<br />surge_anomaly:  16.8079797<br />station: IJmuiden","year: 2018<br />surge_anomaly: -42.4801482<br />station: IJmuiden","year: 2019<br />surge_anomaly:   6.5482555<br />station: IJmuiden","year: 2020<br />surge_anomaly:   6.8083556<br />station: IJmuiden","year: 2021<br />surge_anomaly: -16.5906348<br />station: IJmuiden","year: 2022<br />surge_anomaly: -11.9259025<br />station: IJmuiden","year: 2023<br />surge_anomaly:  25.5944242<br />station: IJmuiden","year: 2024<br />surge_anomaly:  18.5087824<br />station: IJmuiden"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"IJmuiden","legendgroup":"IJmuiden","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(0,191,196,0.5)","dash":"solid"},"frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19.096538517933492,2.6977422714060935,10.851700915825594,-28.998939643852683,23.003180397475592,6.7817509584628439,-14.301836361559673,-0.9563105772069076,3.0759877323174956,-42.424573466886656,-7.3312647080608224,21.77305603962386,7.4873650847031437,-29.11594475902988,-31.809412436730597,26.040173289499993,16.804521165344809,46.129175555668979,-9.4989333056738712,-23.45079041574019,14.561599011541462,-21.538096785099889,-45.092129306560345,17.219827848489761,35.527643086974457,-18.951676532605973,-33.235915472810568,18.079169672642692,-0.5663975177122561,-5.1395642030128217,-9.6360261727842893,24.078915267489428,-2.2298476088119217,34.448504709270196,-12.763969416554788,-7.9456251588119384,-0.80045518150600559,-30.990885256711476,27.35395559692423,6.7828375444300759,53.233162534567029,-20.734707348017398,2.1414442599570775,-10.239747367779955,23.694906433471196,4.6862271515990441,-73.125378576185895,-32.855439602921521,36.706872309498493,30.50902094045756,21.460371936778561,7.9753583571104443,-18.924474393505989,-31.478757573997324,13.198597973709346,-3.9594328112502879,0.83483581383646133,27.159591499863879,19.612800067311348,-8.1312295419169569,-24.989341003241151,10.108732085594459,-0.23467233680978811,-26.536668836026539,-2.9080127764238077,20.980381640229179,-21.264578745052056,20.504272485891281,-40.395335230912387,8.1845626362565742,8.827575140585461,-14.825696907168105,-8.0207037300469217,25.664624948678615,18.125788187559262],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1891<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1892<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1893<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1894<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1895<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1896<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1897<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1898<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1899<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1900<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1901<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1902<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1903<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1904<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1905<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1906<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1907<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1908<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1909<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1910<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1911<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1912<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1913<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1914<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1915<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1916<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1917<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1918<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1919<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1920<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1921<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1922<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1923<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1924<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1925<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1926<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1927<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1928<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1929<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1930<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1931<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1932<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1933<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1934<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1935<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1936<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1937<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1938<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1939<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1940<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1941<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1942<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1943<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1944<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1945<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1946<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1947<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1948<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1949<br />surge_anomaly:   0.0000000<br />station: Netherlands","year: 1950<br />surge_anomaly:  19.0965385<br />station: Netherlands","year: 1951<br />surge_anomaly:   2.6977423<br />station: Netherlands","year: 1952<br />surge_anomaly:  10.8517009<br />station: Netherlands","year: 1953<br />surge_anomaly: -28.9989396<br />station: Netherlands","year: 1954<br />surge_anomaly:  23.0031804<br />station: Netherlands","year: 1955<br />surge_anomaly:   6.7817510<br />station: Netherlands","year: 1956<br />surge_anomaly: -14.3018364<br />station: Netherlands","year: 1957<br />surge_anomaly:  -0.9563106<br />station: Netherlands","year: 1958<br />surge_anomaly:   3.0759877<br />station: Netherlands","year: 1959<br />surge_anomaly: -42.4245735<br />station: Netherlands","year: 1960<br />surge_anomaly:  -7.3312647<br />station: Netherlands","year: 1961<br />surge_anomaly:  21.7730560<br />station: Netherlands","year: 1962<br />surge_anomaly:   7.4873651<br />station: Netherlands","year: 1963<br />surge_anomaly: -29.1159448<br />station: Netherlands","year: 1964<br />surge_anomaly: -31.8094124<br />station: Netherlands","year: 1965<br />surge_anomaly:  26.0401733<br />station: Netherlands","year: 1966<br />surge_anomaly:  16.8045212<br />station: Netherlands","year: 1967<br />surge_anomaly:  46.1291756<br />station: Netherlands","year: 1968<br />surge_anomaly:  -9.4989333<br />station: Netherlands","year: 1969<br />surge_anomaly: -23.4507904<br />station: Netherlands","year: 1970<br />surge_anomaly:  14.5615990<br />station: Netherlands","year: 1971<br />surge_anomaly: -21.5380968<br />station: Netherlands","year: 1972<br />surge_anomaly: -45.0921293<br />station: Netherlands","year: 1973<br />surge_anomaly:  17.2198278<br />station: Netherlands","year: 1974<br />surge_anomaly:  35.5276431<br />station: Netherlands","year: 1975<br />surge_anomaly: -18.9516765<br />station: Netherlands","year: 1976<br />surge_anomaly: -33.2359155<br />station: Netherlands","year: 1977<br />surge_anomaly:  18.0791697<br />station: Netherlands","year: 1978<br />surge_anomaly:  -0.5663975<br />station: Netherlands","year: 1979<br />surge_anomaly:  -5.1395642<br />station: Netherlands","year: 1980<br />surge_anomaly:  -9.6360262<br />station: Netherlands","year: 1981<br />surge_anomaly:  24.0789153<br />station: Netherlands","year: 1982<br />surge_anomaly:  -2.2298476<br />station: Netherlands","year: 1983<br />surge_anomaly:  34.4485047<br />station: Netherlands","year: 1984<br />surge_anomaly: -12.7639694<br />station: Netherlands","year: 1985<br />surge_anomaly:  -7.9456252<br />station: Netherlands","year: 1986<br />surge_anomaly:  -0.8004552<br />station: Netherlands","year: 1987<br />surge_anomaly: -30.9908853<br />station: Netherlands","year: 1988<br />surge_anomaly:  27.3539556<br />station: Netherlands","year: 1989<br />surge_anomaly:   6.7828375<br />station: Netherlands","year: 1990<br />surge_anomaly:  53.2331625<br />station: Netherlands","year: 1991<br />surge_anomaly: -20.7347073<br />station: Netherlands","year: 1992<br />surge_anomaly:   2.1414443<br />station: Netherlands","year: 1993<br />surge_anomaly: -10.2397474<br />station: Netherlands","year: 1994<br />surge_anomaly:  23.6949064<br />station: Netherlands","year: 1995<br />surge_anomaly:   4.6862272<br />station: Netherlands","year: 1996<br />surge_anomaly: -73.1253786<br />station: Netherlands","year: 1997<br />surge_anomaly: -32.8554396<br />station: Netherlands","year: 1998<br />surge_anomaly:  36.7068723<br />station: Netherlands","year: 1999<br />surge_anomaly:  30.5090209<br />station: Netherlands","year: 2000<br />surge_anomaly:  21.4603719<br />station: Netherlands","year: 2001<br />surge_anomaly:   7.9753584<br />station: Netherlands","year: 2002<br />surge_anomaly: -18.9244744<br />station: Netherlands","year: 2003<br />surge_anomaly: -31.4787576<br />station: Netherlands","year: 2004<br />surge_anomaly:  13.1985980<br />station: Netherlands","year: 2005<br />surge_anomaly:  -3.9594328<br />station: Netherlands","year: 2006<br />surge_anomaly:   0.8348358<br />station: Netherlands","year: 2007<br />surge_anomaly:  27.1595915<br />station: Netherlands","year: 2008<br />surge_anomaly:  19.6128001<br />station: Netherlands","year: 2009<br />surge_anomaly:  -8.1312295<br />station: Netherlands","year: 2010<br />surge_anomaly: -24.9893410<br />station: Netherlands","year: 2011<br />surge_anomaly:  10.1087321<br />station: Netherlands","year: 2012<br />surge_anomaly:  -0.2346723<br />station: Netherlands","year: 2013<br />surge_anomaly: -26.5366688<br />station: Netherlands","year: 2014<br />surge_anomaly:  -2.9080128<br />station: Netherlands","year: 2015<br />surge_anomaly:  20.9803816<br />station: Netherlands","year: 2016<br />surge_anomaly: -21.2645787<br />station: Netherlands","year: 2017<br />surge_anomaly:  20.5042725<br />station: Netherlands","year: 2018<br />surge_anomaly: -40.3953352<br />station: Netherlands","year: 2019<br />surge_anomaly:   8.1845626<br />station: Netherlands","year: 2020<br />surge_anomaly:   8.8275751<br />station: Netherlands","year: 2021<br />surge_anomaly: -14.8256969<br />station: Netherlands","year: 2022<br />surge_anomaly:  -8.0207037<br />station: Netherlands","year: 2023<br />surge_anomaly:  25.6646249<br />station: Netherlands","year: 2024<br />surge_anomaly:  18.1257882<br />station: Netherlands"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(0,169,255,1)"}},"hoveron":"points","name":"Netherlands","legendgroup":"Netherlands","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(0,169,255,0.5)","dash":"solid"},"frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17.373239122657512,1.7451720217521127,8.9380408186004345,-32.152080305369338,20.772566465209053,3.831145223964755,-17.196330911170804,-3.4803780414830072,2.5144965661228156,-44.045824296293027,-5.7782043928925066,19.620628172934655,3.7812039260631538,-30.445092787202817,-34.716574524967051,24.807803420293578,17.038215394419932,42.237873427947214,-10.305153760922984,-23.198886472150285,13.161652827355796,-24.435888627090087,-45.766818350584401,12.419479262971471,34.723167441218635,-21.732533510601105,-35.610048992817383,16.836601666220496,-1.9133539959994863,-4.9617365281178447,-11.466049507720546,20.904170400320471,-5.0430612235151262,29.795777941964278,-14.523514630846226,-10.907623351021986,-3.6488896124466459,-32.602077655569929,24.76110837385318,2.7484396855259732,48.029442674786281,-25.076138598561695,-0.89218505302106621,-13.046889750057584,21.135065553471197,1.5205658177999524,-73.76648388907023,-36.255159471569925,34.522218523608075,28.064059254731553,19.405156886596416,6.6738003342602141,-20.238130471449367,-34.827303682888285,10.380389244028112,-6.7202638569069055,-1.8863844596795256,23.023152987115015,16.526885294055415,-9.5984752180904689,-25.462151115241166,6.3705797316355524,-2.9423785101249051,-28.459403797871285,-4.2482718266767678,15.614573187334196,-22.991115086900862,16.377788007060232,-42.264558169268547,5.6539753743478922,5.9611829082138756,-17.097765299131584,-11.874600608596085,23.634675255856475,16.848773462742294],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1891<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1892<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1893<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1894<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1895<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1896<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1897<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1898<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1899<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1900<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1901<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1902<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1903<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1904<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1905<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1906<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1907<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1908<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1909<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1910<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1911<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1912<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1913<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1914<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1915<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1916<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1917<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1918<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1919<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1920<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1921<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1922<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1923<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1924<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1925<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1926<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1927<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1928<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1929<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1930<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1931<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1932<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1933<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1934<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1935<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1936<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1937<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1938<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1939<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1940<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1941<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1942<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1943<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1944<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1945<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1946<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1947<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1948<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1949<br />surge_anomaly:   0.0000000<br />station: Netherlands (without Delfzijl)","year: 1950<br />surge_anomaly:  17.3732391<br />station: Netherlands (without Delfzijl)","year: 1951<br />surge_anomaly:   1.7451720<br />station: Netherlands (without Delfzijl)","year: 1952<br />surge_anomaly:   8.9380408<br />station: Netherlands (without Delfzijl)","year: 1953<br />surge_anomaly: -32.1520803<br />station: Netherlands (without Delfzijl)","year: 1954<br />surge_anomaly:  20.7725665<br />station: Netherlands (without Delfzijl)","year: 1955<br />surge_anomaly:   3.8311452<br />station: Netherlands (without Delfzijl)","year: 1956<br />surge_anomaly: -17.1963309<br />station: Netherlands (without Delfzijl)","year: 1957<br />surge_anomaly:  -3.4803780<br />station: Netherlands (without Delfzijl)","year: 1958<br />surge_anomaly:   2.5144966<br />station: Netherlands (without Delfzijl)","year: 1959<br />surge_anomaly: -44.0458243<br />station: Netherlands (without Delfzijl)","year: 1960<br />surge_anomaly:  -5.7782044<br />station: Netherlands (without Delfzijl)","year: 1961<br />surge_anomaly:  19.6206282<br />station: Netherlands (without Delfzijl)","year: 1962<br />surge_anomaly:   3.7812039<br />station: Netherlands (without Delfzijl)","year: 1963<br />surge_anomaly: -30.4450928<br />station: Netherlands (without Delfzijl)","year: 1964<br />surge_anomaly: -34.7165745<br />station: Netherlands (without Delfzijl)","year: 1965<br />surge_anomaly:  24.8078034<br />station: Netherlands (without Delfzijl)","year: 1966<br />surge_anomaly:  17.0382154<br />station: Netherlands (without Delfzijl)","year: 1967<br />surge_anomaly:  42.2378734<br />station: Netherlands (without Delfzijl)","year: 1968<br />surge_anomaly: -10.3051538<br />station: Netherlands (without Delfzijl)","year: 1969<br />surge_anomaly: -23.1988865<br />station: Netherlands (without Delfzijl)","year: 1970<br />surge_anomaly:  13.1616528<br />station: Netherlands (without Delfzijl)","year: 1971<br />surge_anomaly: -24.4358886<br />station: Netherlands (without Delfzijl)","year: 1972<br />surge_anomaly: -45.7668184<br />station: Netherlands (without Delfzijl)","year: 1973<br />surge_anomaly:  12.4194793<br />station: Netherlands (without Delfzijl)","year: 1974<br />surge_anomaly:  34.7231674<br />station: Netherlands (without Delfzijl)","year: 1975<br />surge_anomaly: -21.7325335<br />station: Netherlands (without Delfzijl)","year: 1976<br />surge_anomaly: -35.6100490<br />station: Netherlands (without Delfzijl)","year: 1977<br />surge_anomaly:  16.8366017<br />station: Netherlands (without Delfzijl)","year: 1978<br />surge_anomaly:  -1.9133540<br />station: Netherlands (without Delfzijl)","year: 1979<br />surge_anomaly:  -4.9617365<br />station: Netherlands (without Delfzijl)","year: 1980<br />surge_anomaly: -11.4660495<br />station: Netherlands (without Delfzijl)","year: 1981<br />surge_anomaly:  20.9041704<br />station: Netherlands (without Delfzijl)","year: 1982<br />surge_anomaly:  -5.0430612<br />station: Netherlands (without Delfzijl)","year: 1983<br />surge_anomaly:  29.7957779<br />station: Netherlands (without Delfzijl)","year: 1984<br />surge_anomaly: -14.5235146<br />station: Netherlands (without Delfzijl)","year: 1985<br />surge_anomaly: -10.9076234<br />station: Netherlands (without Delfzijl)","year: 1986<br />surge_anomaly:  -3.6488896<br />station: Netherlands (without Delfzijl)","year: 1987<br />surge_anomaly: -32.6020777<br />station: Netherlands (without Delfzijl)","year: 1988<br />surge_anomaly:  24.7611084<br />station: Netherlands (without Delfzijl)","year: 1989<br />surge_anomaly:   2.7484397<br />station: Netherlands (without Delfzijl)","year: 1990<br />surge_anomaly:  48.0294427<br />station: Netherlands (without Delfzijl)","year: 1991<br />surge_anomaly: -25.0761386<br />station: Netherlands (without Delfzijl)","year: 1992<br />surge_anomaly:  -0.8921851<br />station: Netherlands (without Delfzijl)","year: 1993<br />surge_anomaly: -13.0468898<br />station: Netherlands (without Delfzijl)","year: 1994<br />surge_anomaly:  21.1350656<br />station: Netherlands (without Delfzijl)","year: 1995<br />surge_anomaly:   1.5205658<br />station: Netherlands (without Delfzijl)","year: 1996<br />surge_anomaly: -73.7664839<br />station: Netherlands (without Delfzijl)","year: 1997<br />surge_anomaly: -36.2551595<br />station: Netherlands (without Delfzijl)","year: 1998<br />surge_anomaly:  34.5222185<br />station: Netherlands (without Delfzijl)","year: 1999<br />surge_anomaly:  28.0640593<br />station: Netherlands (without Delfzijl)","year: 2000<br />surge_anomaly:  19.4051569<br />station: Netherlands (without Delfzijl)","year: 2001<br />surge_anomaly:   6.6738003<br />station: Netherlands (without Delfzijl)","year: 2002<br />surge_anomaly: -20.2381305<br />station: Netherlands (without Delfzijl)","year: 2003<br />surge_anomaly: -34.8273037<br />station: Netherlands (without Delfzijl)","year: 2004<br />surge_anomaly:  10.3803892<br />station: Netherlands (without Delfzijl)","year: 2005<br />surge_anomaly:  -6.7202639<br />station: Netherlands (without Delfzijl)","year: 2006<br />surge_anomaly:  -1.8863845<br />station: Netherlands (without Delfzijl)","year: 2007<br />surge_anomaly:  23.0231530<br />station: Netherlands (without Delfzijl)","year: 2008<br />surge_anomaly:  16.5268853<br />station: Netherlands (without Delfzijl)","year: 2009<br />surge_anomaly:  -9.5984752<br />station: Netherlands (without Delfzijl)","year: 2010<br />surge_anomaly: -25.4621511<br />station: Netherlands (without Delfzijl)","year: 2011<br />surge_anomaly:   6.3705797<br />station: Netherlands (without Delfzijl)","year: 2012<br />surge_anomaly:  -2.9423785<br />station: Netherlands (without Delfzijl)","year: 2013<br />surge_anomaly: -28.4594038<br />station: Netherlands (without Delfzijl)","year: 2014<br />surge_anomaly:  -4.2482718<br />station: Netherlands (without Delfzijl)","year: 2015<br />surge_anomaly:  15.6145732<br />station: Netherlands (without Delfzijl)","year: 2016<br />surge_anomaly: -22.9911151<br />station: Netherlands (without Delfzijl)","year: 2017<br />surge_anomaly:  16.3777880<br />station: Netherlands (without Delfzijl)","year: 2018<br />surge_anomaly: -42.2645582<br />station: Netherlands (without Delfzijl)","year: 2019<br />surge_anomaly:   5.6539754<br />station: Netherlands (without Delfzijl)","year: 2020<br />surge_anomaly:   5.9611829<br />station: Netherlands (without Delfzijl)","year: 2021<br />surge_anomaly: -17.0977653<br />station: Netherlands (without Delfzijl)","year: 2022<br />surge_anomaly: -11.8746006<br />station: Netherlands (without Delfzijl)","year: 2023<br />surge_anomaly:  23.6346753<br />station: Netherlands (without Delfzijl)","year: 2024<br />surge_anomaly:  16.8487735<br />station: Netherlands (without Delfzijl)"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"Netherlands (without Delfzijl)","legendgroup":"Netherlands (without Delfzijl)","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(199,124,255,0.5)","dash":"solid"},"frame":null},{"x":[1890,1891,1892,1893,1894,1895,1896,1897,1898,1899,1900,1901,1902,1903,1904,1905,1906,1907,1908,1909,1910,1911,1912,1913,1914,1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024],"y":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-4.0775901490289073,-15.245973455865313,-2.8082917008837072,-47.482761927410458,-1.8765474576174055,-5.046202850826802,-27.495399832966108,-18.510041576522102,-6.2486645538183083,-50.292612463521856,-11.863473957217508,0.92293417588469839,-14.986625818234806,-38.794496613904457,-44.907657865166527,9.4463001165791969,4.9591286727620982,10.695707438967595,-17.087970119016806,-23.961759947322207,0.4571239728082972,-33.705191313485805,-50.505632392590172,-6.3701751009449055,17.422939852385092,-33.219812037836803,-40.876117224283199,1.2113886257987971,-12.291180854566504,-17.106816551460305,-21.789755738540208,4.6982324471697936,-27.373004314474009,3.9046930992245943,-25.94599937629981,-26.128216999679506,-21.741247978857604,-40.658328596665818,3.1644717950663974,-17.220842418583608,14.330525877032798,-40.863020626254858,-20.701381805480104,-25.227461182254807,-0.34692257748770317,-11.755038870638408,-67.44022081165491,-48.931120680775393,9.5523199704574964,5.173509742238295,-4.8671684844418053,-5.6142855599535082,-31.603793599405503,-45.209351250090478,-10.602016369687703,-20.183274362254902,-20.758957900775403,4.6727350729231887,-6.9190966920921042,-22.388478084885005,-24.192432855022005,-17.445469506254906,-20.572754597829807,-35.975355229542508,-20.892029433104209,-14.836515736391805,-35.754192921600307,-6.2695393172137059,-48.514020788446629,-13.325507765432905,-18.297710492638608,-30.619615002145306,-30.214454421727904,3.8595471308857938,-0.17700374524040541],"text":["year: 1890<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1891<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1892<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1893<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1894<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1895<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1896<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1897<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1898<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1899<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1900<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1901<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1902<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1903<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1904<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1905<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1906<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1907<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1908<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1909<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1910<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1911<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1912<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1913<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1914<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1915<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1916<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1917<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1918<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1919<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1920<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1921<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1922<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1923<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1924<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1925<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1926<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1927<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1928<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1929<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1930<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1931<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1932<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1933<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1934<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1935<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1936<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1937<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1938<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1939<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1940<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1941<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1942<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1943<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1944<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1945<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1946<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1947<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1948<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1949<br />surge_anomaly:   0.0000000<br />station: Vlissingen","year: 1950<br />surge_anomaly:  -4.0775901<br />station: Vlissingen","year: 1951<br />surge_anomaly: -15.2459735<br />station: Vlissingen","year: 1952<br />surge_anomaly:  -2.8082917<br />station: Vlissingen","year: 1953<br />surge_anomaly: -47.4827619<br />station: Vlissingen","year: 1954<br />surge_anomaly:  -1.8765475<br />station: Vlissingen","year: 1955<br />surge_anomaly:  -5.0462029<br />station: Vlissingen","year: 1956<br />surge_anomaly: -27.4953998<br />station: Vlissingen","year: 1957<br />surge_anomaly: -18.5100416<br />station: Vlissingen","year: 1958<br />surge_anomaly:  -6.2486646<br />station: Vlissingen","year: 1959<br />surge_anomaly: -50.2926125<br />station: Vlissingen","year: 1960<br />surge_anomaly: -11.8634740<br />station: Vlissingen","year: 1961<br />surge_anomaly:   0.9229342<br />station: Vlissingen","year: 1962<br />surge_anomaly: -14.9866258<br />station: Vlissingen","year: 1963<br />surge_anomaly: -38.7944966<br />station: Vlissingen","year: 1964<br />surge_anomaly: -44.9076579<br />station: Vlissingen","year: 1965<br />surge_anomaly:   9.4463001<br />station: Vlissingen","year: 1966<br />surge_anomaly:   4.9591287<br />station: Vlissingen","year: 1967<br />surge_anomaly:  10.6957074<br />station: Vlissingen","year: 1968<br />surge_anomaly: -17.0879701<br />station: Vlissingen","year: 1969<br />surge_anomaly: -23.9617599<br />station: Vlissingen","year: 1970<br />surge_anomaly:   0.4571240<br />station: Vlissingen","year: 1971<br />surge_anomaly: -33.7051913<br />station: Vlissingen","year: 1972<br />surge_anomaly: -50.5056324<br />station: Vlissingen","year: 1973<br />surge_anomaly:  -6.3701751<br />station: Vlissingen","year: 1974<br />surge_anomaly:  17.4229399<br />station: Vlissingen","year: 1975<br />surge_anomaly: -33.2198120<br />station: Vlissingen","year: 1976<br />surge_anomaly: -40.8761172<br />station: Vlissingen","year: 1977<br />surge_anomaly:   1.2113886<br />station: Vlissingen","year: 1978<br />surge_anomaly: -12.2911809<br />station: Vlissingen","year: 1979<br />surge_anomaly: -17.1068166<br />station: Vlissingen","year: 1980<br />surge_anomaly: -21.7897557<br />station: Vlissingen","year: 1981<br />surge_anomaly:   4.6982324<br />station: Vlissingen","year: 1982<br />surge_anomaly: -27.3730043<br />station: Vlissingen","year: 1983<br />surge_anomaly:   3.9046931<br />station: Vlissingen","year: 1984<br />surge_anomaly: -25.9459994<br />station: Vlissingen","year: 1985<br />surge_anomaly: -26.1282170<br />station: Vlissingen","year: 1986<br />surge_anomaly: -21.7412480<br />station: Vlissingen","year: 1987<br />surge_anomaly: -40.6583286<br />station: Vlissingen","year: 1988<br />surge_anomaly:   3.1644718<br />station: Vlissingen","year: 1989<br />surge_anomaly: -17.2208424<br />station: Vlissingen","year: 1990<br />surge_anomaly:  14.3305259<br />station: Vlissingen","year: 1991<br />surge_anomaly: -40.8630206<br />station: Vlissingen","year: 1992<br />surge_anomaly: -20.7013818<br />station: Vlissingen","year: 1993<br />surge_anomaly: -25.2274612<br />station: Vlissingen","year: 1994<br />surge_anomaly:  -0.3469226<br />station: Vlissingen","year: 1995<br />surge_anomaly: -11.7550389<br />station: Vlissingen","year: 1996<br />surge_anomaly: -67.4402208<br />station: Vlissingen","year: 1997<br />surge_anomaly: -48.9311207<br />station: Vlissingen","year: 1998<br />surge_anomaly:   9.5523200<br />station: Vlissingen","year: 1999<br />surge_anomaly:   5.1735097<br />station: Vlissingen","year: 2000<br />surge_anomaly:  -4.8671685<br />station: Vlissingen","year: 2001<br />surge_anomaly:  -5.6142856<br />station: Vlissingen","year: 2002<br />surge_anomaly: -31.6037936<br />station: Vlissingen","year: 2003<br />surge_anomaly: -45.2093513<br />station: Vlissingen","year: 2004<br />surge_anomaly: -10.6020164<br />station: Vlissingen","year: 2005<br />surge_anomaly: -20.1832744<br />station: Vlissingen","year: 2006<br />surge_anomaly: -20.7589579<br />station: Vlissingen","year: 2007<br />surge_anomaly:   4.6727351<br />station: Vlissingen","year: 2008<br />surge_anomaly:  -6.9190967<br />station: Vlissingen","year: 2009<br />surge_anomaly: -22.3884781<br />station: Vlissingen","year: 2010<br />surge_anomaly: -24.1924329<br />station: Vlissingen","year: 2011<br />surge_anomaly: -17.4454695<br />station: Vlissingen","year: 2012<br />surge_anomaly: -20.5727546<br />station: Vlissingen","year: 2013<br />surge_anomaly: -35.9753552<br />station: Vlissingen","year: 2014<br />surge_anomaly: -20.8920294<br />station: Vlissingen","year: 2015<br />surge_anomaly: -14.8365157<br />station: Vlissingen","year: 2016<br />surge_anomaly: -35.7541929<br />station: Vlissingen","year: 2017<br />surge_anomaly:  -6.2695393<br />station: Vlissingen","year: 2018<br />surge_anomaly: -48.5140208<br />station: Vlissingen","year: 2019<br />surge_anomaly: -13.3255078<br />station: Vlissingen","year: 2020<br />surge_anomaly: -18.2977105<br />station: Vlissingen","year: 2021<br />surge_anomaly: -30.6196150<br />station: Vlissingen","year: 2022<br />surge_anomaly: -30.2144544<br />station: Vlissingen","year: 2023<br />surge_anomaly:   3.8595471<br />station: Vlissingen","year: 2024<br />surge_anomaly:  -0.1770037<br />station: Vlissingen"],"type":"scatter","mode":"markers+lines","marker":{"autocolorscale":false,"color":"rgba(255,255,255,1)","opacity":1,"size":3.7795275590551185,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,97,204,1)"}},"hoveron":"points","name":"Vlissingen","legendgroup":"Vlissingen","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","line":{"width":2.8346456692913389,"color":"rgba(255,97,204,0.5)","dash":"solid"},"frame":null}],"layout":{"margin":{"t":23.305936073059364,"r":7.3059360730593621,"b":37.260273972602747,"l":43.105022831050242},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[1941,2029],"tickmode":"array","ticktext":["1960","1980","2000","2020"],"tickvals":[1960,1980,2000,2020],"categoryorder":"array","categoryarray":["1960","1980","2000","2020"],"nticks":null,"ticks":"outside","tickcolor":"rgba(179,179,179,1)","ticklen":3.6529680365296811,"tickwidth":0.33208800332088001,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(222,222,222,1)","gridwidth":0.33208800332088001,"zeroline":false,"anchor":"y","title":{"text":"jaar","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-86.843045112506246,99.189564309829038],"tickmode":"array","ticktext":["-50","0","50"],"tickvals":[-50,0,50],"categoryorder":"array","categoryarray":["-50","0","50"],"nticks":null,"ticks":"outside","tickcolor":"rgba(179,179,179,1)","ticklen":3.6529680365296811,"tickwidth":0.33208800332088001,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(222,222,222,1)","gridwidth":0.33208800332088001,"zeroline":false,"anchor":"x","title":{"text":"windopzet in mm","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(179,179,179,1)","width":0.66417600664176002,"linetype":"solid"},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.8897637795275593,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"orientation":"h","x":0.5,"y":-0.14999999999999999,"xanchor":"center","yanchor":"top","title":{"text":"station","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"5f8c7e4b2bb2":{"x":{},"y":{},"colour":{},"type":"scatter"},"5f8c26c75b97":{"x":{},"y":{},"colour":{}}},"cur_data":"5f8c7e4b2bb2","visdat":{"5f8c7e4b2bb2":["function (y) ","x"],"5f8c26c75b97":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

<p class="caption">

Modelled surge anomaly (deviation of storm surge from long year
avearge). The yearly averages surge is calculated for 1950 - now. For
earlier years, an average surge is assumed.
</p>

</div>

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
of the estimated parameters accordingly.

``` r
plot_ACF(models)
```

<figure>
<img
src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/acf-plot-1.png"
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

<img src="C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/unnamed-chunk-6-1.png" alt="" width="100%" />

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

![](C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

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

![](C:/git_checkouts/sealevelmonitor/sealevelanalysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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
