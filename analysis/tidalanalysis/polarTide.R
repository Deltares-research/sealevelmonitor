

# generate pattern

epoch = 1970

df = data.frame(
  t = seq(1900, 2025, 1/12)
) %>%
  mutate(
    x = 2*cos(2*pi*(t-epoch)/1.4)
  )


# mean
df %>%
  group_by(year = trunc(t)) %>%
  summarize(x_avg = mean(x)) %>%
  ggplot(aes(year,x_avg)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm")

df %>%
  ggplot(aes(t,x)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_boxplot(aes(group = trunc(t)))

df %>%
  mutate(dec = round(t %% 1, 3)) %>%
  filter(dec == 0.083) %>%
ggplot(aes(t,x)) +
  geom_line() +
  geom_point() +
geom_smooth(method = "lm")

require("astsa")

df.t <- zoo::zoo(df$x, order.by = df$t)
spec <- spec.ar(df.t)


# next step, download read data from psmsl and try again

HvHData <- read_monthly_psmsl_csv(22) %>%
  filter(decimal_year >= 1890)

HvH.t <- zoo::zoo(HvHData$rlr_height_mm, order.by = HvHData$decimal_year)

HvHData %>%
  group_by(year = trunc(decimal_year)) %>%
  summarize(x_avg = mean(rlr_height_mm)) %>%
  ggplot(aes(year,x_avg)) +
  geom_line() +
  geom_smooth(method = "lm") +
  geom_point()

HvHData %>%
  filter(decimal_year > 1940) %>%
  ggplot(aes(decimal_year,rlr_height_mm)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  geom_boxplot(aes(group = trunc(decimal_year)), alpha = 0.6)


HvHData %>%
  mutate(dec = round(decimal_year %% 1, 3)) %>%
  filter(dec == 0.875) %>%
  ggplot(aes(decimal_year,rlr_height_mm)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) +
  scale_x_continuous(breaks = pretty_breaks(20))

spechvh <- spec.ar(HvH.t)

