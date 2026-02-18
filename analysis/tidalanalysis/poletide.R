
# calculate pole tide
# 
df <- read_delim(
  file.path(
     "data/deltares/input/psmsl_gtsm_yr-latest.csv"),
  delim = ";") %>%
  filter(station == "Den Helder") %>%
  filter(year >= 1973) %>%
  mutate(date = ymd(paste(year, 6, 1)))

# documentation: https://gravitools.readthedocs.io/eop/

df_polar <- read_delim("https://datacenter.iers.org/data/csv/finals2000A.all.csv", delim = ";") %>%
  mutate(date = lubridate::ymd(paste(Year, Month, Day))) %>%
  select(
    date,
    xp_arcsec = x_pole,
    yp_arcsec = y_pole
  )

df_comb <- df %>% left_join(df_polar)
  
  
ggplot(df_polar, aes(date, sqrt(xp_arcsec^2 + yp_arcsec^2))) + geom_line()


# Set to your station location
lat <- 52.0 * pi/180      # latitude (radians)
lon <- 5.0  * pi/180      # longitude (radians)

# Constants
Omega <- 7.292115e-5        # Earth rotation rate (rad/s)
R     <- 6378136.6          # mean Earth radius (m)
g     <- 9.780327           # gravity (m/s^2)

# Convert arcsec → radians
asec2rad <- pi / (180 * 3600)

df_comb2 <- df_comb %>%
  mutate(
    xp = xp_arcsec * asec2rad,
    yp = yp_arcsec * asec2rad,
    pole_tide = -(Omega^2 * R / g) * sin(2 * lat) *
      (xp * cos(lon) + yp * sin(lon)) * 1000   # this is exagerrated, but check units!!!
  )

df_comb2 <- df_comb2 %>%
  mutate(
    sea_level_detr = resid(lm(height ~ date)),
    sea_level_ds = sea_level_detr - 
      predict(lm(sea_level_detr ~ sin(2*pi*decimal_date(date)) +
                   cos(2*pi*decimal_date(date))))
  )

df_comb2 <- df_comb2 %>%
  mutate(
    sea_level_detr = resid(lm(height ~ date)),
    sea_level_ds = sea_level_detr - 
      predict(lm(sea_level_detr ~ sin(2*pi*decimal_date(date)) +
                   cos(2*pi*decimal_date(date))))
  )

cor_test <- cor.test(df_comb2$sea_level_ds, df_comb2$pole_tide)
cor_test

spec <- spectrum(cbind(df_comb2$sea_level_ds, df_comb2$pole_tide),
                 spans=c(5,5), detrend=TRUE, plot=TRUE)

ggplot(df_comb2, aes(x=date)) +
  geom_line(aes(y = sea_level_ds), color="blue") +
  geom_line(aes(y = pole_tide), color="red") +
  labs(title="Detrended Tide Gauge vs. Theoretical Pole Tide",
       y="Sea level (m)") +
  theme_minimal()

# conclusion. equilibrium pole tide is very very small. 
# meteorological forcing with period of 14 months is not explained with pole tide
# 
# 
