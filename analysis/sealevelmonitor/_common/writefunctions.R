
write_aug_long <- function(df, filename){
  
metadata <- c(
"# year = year",
"# psmsl_id = ID of PSMSL station",
"# name = station name",
"# height = measured water level relative to NAP-2005",
"# surge_anomaly = yearly average surge normalized by the average surge",
"# surge = yearly average surge",
"# from1993 = term to be used in broken linear regression",
"# from1960_square = term to be used in broken quadratic regression",
"# offset(surge_anomaly) = surge anomaly used in regression (equals surge_anomaly)",
"# I(year - epoch) = year - 1970",
"# I(cos(2 * pi * (year - epoch)/(18.613))) = cosine term to estimate nodal tide",
"# I(sin(2 * pi * (year - epoch)/(18.613))) = sine term to estimate nodal tide",
"# trend_fit = fitted trend (broken linear)",
"# trend_se = standard error of trend_fit",
"# trend_lwr = lower value of 95% CI for trend_fit",
"# trend_upr = upper value of 95% CI for trend_fit",
"# nodal_fit = fitted nodal tide",
"# nodal_se = standard error of nodal_fit",
"# nodal_lwr = lower value of 95% CI for nodal_fit",
"# nodal_upr = upper value of 95% CI for nodal_fit",
"# offset = ",
"# fitted_total = fitted trend + fitted nodal + surge_anomaly + offset"
)

write_lines(metadata, filename) 
write_delim(df, filename, append = TRUE)
}

