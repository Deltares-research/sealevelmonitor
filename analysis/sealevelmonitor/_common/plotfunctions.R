require(tidyverse)
require(scales)


fitstyle =   theme_light() %+replace%
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom"
  ) 


plot_station <- function(stationi = "Netherlands (without Delfzijl)", predictions_all = predictions_all, correctionVariant, modelVariant, printNumbers = FALSE) {
  
  startyear = 1900
  zoomyear = 2010
  
  plotShapes = c(
    "height" = 1,
    "height - anomaly" = 24#,
    # "heigt - effect" = 22
  )
  
  pal <- hue_pal()(4)
  
  plotColors = c(
    "zeespiegel" = "darkgrey",
    "zeespiegel (- nodaal getij)" = pal[1],
    # "zeespiegel (-opzet)" = pal[2],
    "zeespiegel (-opzetanomalie -nodaal getij)" = pal[2],
    "predictie (-opzetanomalie +getij)" = pal[3],
    "predictie (-opzetanomalie -nodaal getij)" = pal[4]
  )
  plotFills = c(
    "predictie-interval" = pal[1],
    "betrouwbaarheids-interval" = pal[2]
  )
  
  predictions_all2 <- predictions_all %>%
    filter(station == stationi) %>%
    # filter(correction_variant == correctionVariant) %>%
    filter(modeltype == modelVariant)
  
  symboolgrootte = 1.5
  
  q <- ggplot() +
    geom_point(data = predictions_all2 %>% filter(data_year >= startyear), 
               aes(x = data_year, y = data_height/10, color = "zeespiegel"), 
               size = symboolgrootte, 
               alpha = 0.8) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= startyear),
              aes(x = data_year, y = (data_height - nodal_tide)/10, color = "zeespiegel (- nodaal getij)"),
              size = symboolgrootte, 
              alpha = 0.6) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= startyear), 
              aes(x = preds_year, y = (`preds_height-surge_anomaly` - nodal_tide)/10, color = "zeespiegel (-opzetanomalie -nodaal getij)"), 
              size = symboolgrootte, 
              alpha = 0.5) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= startyear),
              aes(x = preds_year, y = `preds_height-surge_anomaly`/10, color = "predictie (-opzetanomalie +getij)"),
              size = symboolgrootte, alpha = 0.7) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= startyear),
              aes(x = preds_year, y = prediction_recalc/10, color = "predictie (-opzetanomalie -nodaal getij)"),
              size = symboolgrootte, alpha = 0.7) +
    geom_vline(xintercept = 1993, linetype = 5) +
    annotate("text", label = "1993", x = 1993, y = 13) +
    coord_cartesian(ylim = c(NA, NA)) +
    xlab("jaar") +
    ylab("zeespiegel in cm tov NAP") +
    labs(subtitle = "Gecorrigeerde zeespiegel (GTSM) en trends") +
    fitstyle +
    theme(
      strip.text.y = element_text(angle = 0)) +
    theme(legend.title = element_blank()) +
    theme(legend.direction = "vertical",
          legend.box = "horizontal",
          legend.position = c(0.025,0.975),
          legend.justification = c(0, 1),
          legend.title = element_blank()) +
    scale_color_manual(values = plotColors) +
    scale_fill_manual(values = plotFills) +
    scale_x_continuous(breaks = scales::pretty_breaks())
  
  if(printNumbers) {
    nudge_xx = 1
    textsize = 3.5
    q = q +
      geom_text(data = predictions_all2 %>% filter(data_year ==2021), 
                aes(x = data_year, y = data_height/10, label = signif(data_height/10, 2), color = "zeespiegel"), 
                size = textsize, alpha = 0.5, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year ==2021), 
                aes(x = data_year, y = (data_height - nodal_tide)/10, label = signif((data_height - nodal_tide)/10, 2), color = "zeespiegel (- nodaal getij)"), 
                size = textsize, alpha = 0.5, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= 2021),
                aes(x = preds_year, y = (`preds_height-surge_anomaly` - nodal_tide)/10, label = signif((`preds_height-surge_anomaly` - nodal_tide)/10, 2), color = "zeespiegel (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 0.5, nudge_x = nudge_xx+1, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= 2021),
                aes(x = preds_year, y = prediction/10, label = signif(prediction/10, 2), color = "predictie (-opzetanomalie +getij)"),
                size = textsize, alpha = 0.7, nudge_x = nudge_xx, nudge_y = 0.1, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= 2021),
                aes(x = preds_year, y = prediction_recalc/10, label = signif(prediction_recalc/10, 2), color = "predictie (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 0.7, nudge_x = nudge_xx, fontface = "bold")
  }
  
  return(q)
  
}



map_selected_stations <- function(df, mainstations_df, mainstations_locs) {
  
  factpal <- colorFactor(c("red", "darkgreen"), c("selected", "not selected"))
  
  df %>%
    dplyr::distinct(station) %>%
    dplyr::left_join(mainstations_df, by = c(station = "name")) %>%
    dplyr::right_join(mainstations_locs %>% mutate(ID = as.character(ID)), by = c(id = "ID")) %>%
    mutate(`selected stations` = ifelse(!is.na(location), "selected", "not selected")) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lat = ~Lat, 
      lng = ~Lon, 
      color = ~ factpal(`selected stations`),
      label = ~ location,
      labelOptions = labelOptions(noHide = T)) %>%
    leaflet::setView(lng = mean(mainstations_locs$Lon), lat = mean(mainstations_locs$Lat), zoom = 6) %>%
    leaflet::addLegend("topleft", pal = factpal, values = ~`selected stations`)
}