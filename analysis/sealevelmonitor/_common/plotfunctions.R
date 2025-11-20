require(tidyverse)
require(scales)
require(RColorBrewer)

fitstyle =   theme_light() %+replace%
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "bottom"
  ) 

plotResidualDistribution <- function(models) {
  
  models %>%
    unnest(c(data, augment), names_sep = "_") %>% 
    mutate(
      surge_correction = case_when(
        params$wind_or_surge_type == "GTSM" ~ ifelse(data_year >= 1950, params$wind_or_surge_type, "none")
      )
    ) %>%
    ggplot(aes(x = augment_.resid)) +
    geom_density(
      aes(
        fill = surge_correction,
        color = surge_correction),
      # position = position_identity(), 
      alpha = 0.5, size = 1
    ) +
    facet_grid(station ~ modeltype) +
    geom_vline(xintercept = 0, alpha = 0.5) +
    theme(
      strip.text.y = element_text(angle = 0), 
      legend.position = "bottom"
    )
}

plot_ACF <- function(models) {
  models %>%
    mutate(
      ACF = map(augment, function(x) fortify(acf(x$.resid, plot = F)))
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
}

plot_station <- function(
    stationi = "Netherlands (without Delfzijl)", 
    predictions_all = predictions_all, 
    correctionVariant, 
    modelVariant, 
    printNumbers = FALSE, 
    plotVline = TRUE,
    datayear = 2023, 
    startyear = 1900
    ) {
  
  zoomyear = 2010
  
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
    filter(station %in% stationi) %>%
    # filter(correction_variant == correctionVariant) %>%
    filter(modeltype %in% modelVariant)
  
  symbolsize = 1
  
  q <- ggplot() +
    geom_point(
      data = predictions_all2 %>% filter(data_year >= startyear), 
      aes(
        x = data_year, 
        y = data_height/10, 
        color = "zeespiegel"
      ), 
      size = symbolsize, 
      alpha = 0.8) +
    geom_point(
      data = predictions_all2 %>% filter(preds_year >= startyear),
      aes(
        x = data_year, 
        y = (data_height - nodal_tide)/10, 
        color = "zeespiegel (- nodaal getij)"
      ),
      size = symbolsize, 
      alpha = 0.5) +
    # geom_line(data = predictions_all2 %>% filter(preds_year >= startyear),
    #           aes(x = data_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, color = "zeespiegel (-opzetanomalie -nodaal getij)"),
    #           size = symbolsize, 
    #           alpha = 0.6) +
    geom_point(
      data = predictions_all2 %>% filter(preds_year >= startyear), 
      aes(
        x = preds_year, 
        y = (`data_height-surge_anomaly` - nodal_tide)/10, 
        color = "zeespiegel (-opzetanomalie -nodaal getij)"), 
      size = symbolsize, 
      alpha = 0.8
    ) +
    # geom_line(                                                            # seems the same as below
    #   data = predictions_all2 %>% filter(preds_year >= startyear),
    #           aes(
    #             x = preds_year, 
    #             y = (`preds_height-surge_anomaly` - nodal_tide)/10, 
    #             color = "predictie (-opzetanomalie -nodaal getij)"),
    #           size = symbolsize,
    #           alpha = 0.5
    #   ) +
    # geom_line(
    #   data = predictions_all2 %>% filter(preds_year >= startyear),
    #   aes(
    #     x = preds_year, 
    #     y = `preds_height-surge_anomaly`/10, 
    #     color = "predictie (-opzetanomalie +getij)"),
    #   size = symbolsize, 
    #   alpha = 0.5
    # ) +
    geom_line(
      data = predictions_all2 %>% filter(preds_year >= startyear),
      aes(
        x = preds_year, 
        y = prediction_recalc/10, 
        color = "predictie (-opzetanomalie -nodaal getij)"),
      size = symbolsize, 
      alpha = 0.7
    ) +
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
  
# if(plotVline) {
#   if(modelVariant == "broken_linear"){
#     q = q + 
#       geom_vline(xintercept = 1993, linetype = 5) +
#       annotate("text", label = "1993", x = 1993, y = 13)
#   }
#   if(modelVariant == "broken_squared"){
#     q = q + 
#       geom_vline(xintercept = 1960, linetype = 5) +
#       annotate("text", label = "1960", x = 1960, y = -20)
#   }
# }
  
    if(printNumbers) {
    nudge_xx = 1
    textsize = 3.5
    q = q +
      geom_text(data = predictions_all2 %>% filter(data_year ==datayear), 
                aes(x = data_year, y = data_height/10, label = signif(data_height/10, 2), color = "zeespiegel"), 
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year ==datayear), 
                aes(x = data_year, y = (data_height - nodal_tide)/10, label = signif((data_height - nodal_tide)/10, 2), color = "zeespiegel (- nodaal getij)"), 
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
                aes(x = preds_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, label = signif((`data_height-surge_anomaly` - nodal_tide)/10, 2), color = "zeespiegel (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      # geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
      #           aes(x = preds_year, y = `preds_height-surge_anomaly`/10, label = signif(`preds_height-surge_anomaly`/10, 2), color = "predictie (-opzetanomalie +getij)"),
      #           size = textsize, alpha = 0.7, nudge_x = nudge_xx, nudge_y = 0.1, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
                aes(x = preds_year, y = prediction_recalc/10, label = signif(prediction_recalc/10, 2), color = "predictie (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 1, nudge_x = nudge_xx, fontface = "bold")
  }
  
  return(q)
  
}


plot_station_website_broken_linear <- function(
    stationi = "Netherlands (without Delfzijl)", 
    predictions_all = predictions_all, 
    correctionVariant, 
    modelVariant, 
    printNumbers = FALSE, 
    plotVline = TRUE,
    datayear = 2023, 
    startyear = 1900,
    accellerationyear = 1993,
    base_logo_path = "../../"
) {
  
  zoomyear = 2010
  
  pal <- hue_pal()(4)
  path = file.path(base_logo_path, "rapportage\\2026\\navbar\\images\\Deltares_logo_D-blauw_RGB\\Deltares_logo_D-blauw_RGB.svg")
  rlogo     <- svgparser::read_svg(path)
  
  
  plotColors = c(
    "gemeten zeespiegel" = "darkgrey",
    # "zeespiegel (- nodaal getij)" = pal[1],
    # "zeespiegel (-opzet)" = pal[2],
    # "gecorrigeerde zeespiegel" = "darkorange", #pal[1],
    "meting gecorrigeerd voor getij en wind in het specifieke jaar" = "darkorange",
    # "gecorrigeerde zeespiegel (gemiddelde windopzet)" = pal[3],
    "meting gecorrigeerd voor vaste variatie in getij (18,6 jaar)" = pal[3],
    "eerste 100 jaar: 1,8 mm/jr" = "darkblue",
    "laatste 30 jaar: 2,9 mm/jr" = "darkgreen"
  )

  plotFills = c(
    "predictie-interval" = pal[1],
    "betrouwbaarheids-interval" = pal[2]
  )
  
  predictions_all2 <- predictions_all %>%
    filter(station %in% stationi) %>%
    # filter(correction_variant == correctionVariant) %>%
    filter(modeltype %in% modelVariant)
  
  symbolsize = 2
  
  # add pre_accelleration and post_accelleration lines under other layers
  
  q <- ggplot() +
    geom_point(data = predictions_all2 %>% filter(data_year >= startyear), 
               aes(x = data_year, y = data_height/10, color = "gemeten zeespiegel"), 
               size = symbolsize, 
               alpha = 0.7) +
    geom_point(data = predictions_all2 %>% filter(preds_year >= 1950),
              aes(x = data_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, color = "meting gecorrigeerd voor getij en wind in het specifieke jaar"),
              size = symbolsize, 
              alpha = 1) +
    geom_point(data = predictions_all2 %>% filter(preds_year >= startyear & preds_year < 1950), 
              aes(x = preds_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, color = "meting gecorrigeerd voor vaste variatie in getij (18,6 jaar)"), 
              size = symbolsize, 
              alpha = 1) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= accellerationyear),
              aes(x = preds_year, y = pre_accelleration/10, color = ifelse(preds_year>=1993, "eerste 100 jaar: 1,8 mm/jr", "Laatste 30 jaar: 2,9 mm/jr")),
              linetype = 2,
              size = symbolsize/2, 
              alpha = 1
              ) +
    geom_line(data = predictions_all2 %>% filter(preds_year < accellerationyear),
              aes(x = preds_year, y = post_accelleration/10, color = ifelse(preds_year>=1993, "eerste 100 jaar: 1,8 mm/jr", "laatste 30 jaar: 2,9 mm/jr")),
              linetype = 2,
              size = symbolsize/2, 
              alpha = 1) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= startyear),
              aes(x = preds_year, y = prediction_recalc/10, color = ifelse(preds_year<1993, "eerste 100 jaar: 1,8 mm/jr", "laatste 30 jaar: 2,9 mm/jr")),
              linetype = 1,
              size = symbolsize, 
              alpha = 1) +
    annotation_custom(rlogo, xmin = 1993, xmax = 2015, ymin = -30, ymax = -25) +
    coord_cartesian(ylim = c(NA, NA)) +
    xlab("jaar") +
    ylab("zeespiegel in cm tov NAP") +
    labs(subtitle = "Gecorrigeerde zeespiegel (GTSM) en trends") +
    theme_light() +
    theme(
      strip.text.y = element_text(angle = 0)) +
    theme(legend.title = element_blank()) +
    theme(legend.direction = "vertical",
          legend.box = "horizontal",
          legend.position = c(0.025,0.975),
          legend.justification = c(0, 1),
          legend.title = element_blank()
    ) +
    guides(linetype = F) +
    scale_color_manual(values = plotColors, breaks = names(plotColors)) +
    scale_fill_manual(values = plotFills) +
    scale_x_continuous(breaks = scales::pretty_breaks(7))
  
  if(plotVline) {
    q = q + 
      geom_vline(xintercept = 1993, linetype = 5) +
      annotate("text", label = "1993", x = 1993, y = 13)
  }
  
  if(printNumbers) {
    nudge_xx = 1
    textsize = 3.5
    q = q +
      geom_text(data = predictions_all2 %>% filter(data_year == datayear), 
                aes(x = data_year, y = data_height/10, label = signif(data_height/10, 2), color = "zeespiegel"), 
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year == datayear), 
                aes(x = data_year, y = (data_height - nodal_tide)/10, label = signif((data_height - nodal_tide)/10, 2), color = "zeespiegel (- nodaal getij)"), 
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
                aes(x = preds_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, label = signif((`data_height-surge_anomaly` - nodal_tide)/10, 2), color = "zeespiegel (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      # geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
      #           aes(x = preds_year, y = `preds_height-surge_anomaly`/10, label = signif(`preds_height-surge_anomaly`/10, 2), color = "predictie (-opzetanomalie +getij)"),
      #           size = textsize, alpha = 0.7, nudge_x = nudge_xx, nudge_y = 0.1, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
                aes(x = preds_year, y = prediction_recalc/10, label = signif(prediction_recalc/10, 2), color = "predictie (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 1, nudge_x = nudge_xx, fontface = "bold")
  }
  
  return(q)
  
}



plot_station_website_broken_squared <- function(
    stationi = "Netherlands (without Delfzijl)", 
    predictions_all = predictions_all, 
    correctionVariant, 
    modelVariant, 
    printNumbers = FALSE, 
    plotVline = TRUE,
    datayear = 2023, 
    startyear = 1900,
    accellerationyear = 1960,
    base_logo_path = "../../"
) {
  
  zoomyear = 2010
  
  pal <- hue_pal()(4)
  path = file.path(base_logo_path, "rapportage\\2026\\navbar\\images\\Deltares_logo_D-blauw_RGB\\Deltares_logo_D-blauw_RGB.svg")
  rlogo     <- svgparser::read_svg(path)
  
  
  plotColors = c(
    "gemeten zeespiegel" = "darkgrey",
    # "zeespiegel (- nodaal getij)" = pal[1],
    # "zeespiegel (-opzet)" = pal[2],
    # "gecorrigeerde zeespiegel" = "darkorange", #pal[1],
    "meting gecorrigeerd voor getij en wind in het specifieke jaar" = "darkorange",
    # "gecorrigeerde zeespiegel (gemiddelde windopzet)" = pal[3],
    "meting gecorrigeerd voor vaste variatie in getij (18,6 jaar)" = pal[3],
    "eerste 100 jaar: 1,8 mm/jr" = "darkblue",
    "laatste 30 jaar: 2,9 mm/jr" = "darkgreen"
  )
  
  plotFills = c(
    "predictie-interval" = pal[1],
    "betrouwbaarheids-interval" = pal[2]
  )
  
  predictions_all2 <- predictions_all %>%
    filter(station %in% stationi) %>%
    # filter(correction_variant == correctionVariant) %>%
    filter(modeltype %in% modelVariant)
  
  symbolsize = 2
  
  # add pre_accelleration and post_accelleration lines under other layers
  
  q <- ggplot() +
    geom_point(data = predictions_all2 %>% filter(data_year >= startyear), 
               aes(x = data_year, y = data_height/10, color = "gemeten zeespiegel"), 
               size = symbolsize, 
               alpha = 0.7) +
    geom_point(data = predictions_all2 %>% filter(preds_year >= 1950),
               aes(x = data_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, color = "meting gecorrigeerd voor getij en wind in het specifieke jaar"),
               size = symbolsize, 
               alpha = 1) +
    geom_point(data = predictions_all2 %>% filter(preds_year >= startyear & preds_year < 1950), 
               aes(x = preds_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, color = "meting gecorrigeerd voor vaste variatie in getij (18,6 jaar)"), 
               size = symbolsize, 
               alpha = 1) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= accellerationyear),
              aes(x = preds_year, y = pre_accelleration/10, color = ifelse(preds_year>=accellerationyear, "eerste 100 jaar: 1,8 mm/jr", "Laatste 30 jaar: 2,9 mm/jr")),
              linetype = 2,
              size = symbolsize/2, 
              alpha = 1
    ) +
    geom_line(data = predictions_all2 %>% filter(preds_year < accellerationyear),
              aes(x = preds_year, y = post_accelleration/10, color = ifelse(preds_year>=accellerationyear, "eerste 100 jaar: 1,8 mm/jr", "laatste 30 jaar: 2,9 mm/jr")),
              linetype = 2,
              size = symbolsize/2, 
              alpha = 1) +
    geom_line(data = predictions_all2 %>% filter(preds_year >= startyear),
              aes(x = preds_year, y = prediction_recalc/10, color = ifelse(preds_year<accellerationyear, "eerste 100 jaar: 1,8 mm/jr", "laatste 30 jaar: 2,9 mm/jr")),
              linetype = 1,
              size = symbolsize, 
              alpha = 1) +
    annotation_custom(rlogo, xmin = accellerationyear, xmax = 2015, ymin = -30, ymax = -25) +
    coord_cartesian(ylim = c(NA, NA)) +
    xlab("jaar") +
    ylab("zeespiegel in cm tov NAP") +
    labs(subtitle = "Gecorrigeerde zeespiegel (GTSM) en trends") +
    theme_light() +
    theme(
      strip.text.y = element_text(angle = 0)) +
    theme(legend.title = element_blank()) +
    theme(legend.direction = "vertical",
          legend.box = "horizontal",
          legend.position = c(0.025,0.975),
          legend.justification = c(0, 1),
          legend.title = element_blank()
    ) +
    guides(linetype = F) +
    scale_color_manual(values = plotColors, breaks = names(plotColors)) +
    scale_fill_manual(values = plotFills) +
    scale_x_continuous(breaks = scales::pretty_breaks(7))
  
  if(plotVline) {
    q = q + 
      geom_vline(xintercept = accellerationyear, linetype = 5) +
      annotate("text", label = accellerationyear, x = accellerationyear, y = 13)
  }
  
  if(printNumbers) {
    nudge_xx = 1
    textsize = 3.5
    q = q +
      geom_text(data = predictions_all2 %>% filter(data_year == datayear), 
                aes(x = data_year, y = data_height/10, label = signif(data_height/10, 2), color = "zeespiegel"), 
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year == datayear), 
                aes(x = data_year, y = (data_height - nodal_tide)/10, label = signif((data_height - nodal_tide)/10, 2), color = "zeespiegel (- nodaal getij)"), 
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
                aes(x = preds_year, y = (`data_height-surge_anomaly` - nodal_tide)/10, label = signif((`data_height-surge_anomaly` - nodal_tide)/10, 2), color = "zeespiegel (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 0.8, nudge_x = nudge_xx, fontface = "bold") +
      # geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
      #           aes(x = preds_year, y = `preds_height-surge_anomaly`/10, label = signif(`preds_height-surge_anomaly`/10, 2), color = "predictie (-opzetanomalie +getij)"),
      #           size = textsize, alpha = 0.7, nudge_x = nudge_xx, nudge_y = 0.1, fontface = "bold") +
      geom_text(data = predictions_all2 %>% filter(preds_year >= datayear),
                aes(x = preds_year, y = prediction_recalc/10, label = signif(prediction_recalc/10, 2), color = "predictie (-opzetanomalie -nodaal getij)"),
                size = textsize, alpha = 1, nudge_x = nudge_xx, fontface = "bold")
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

map_stations <- function(df, mainstations_df, mainstations_locs) {
  
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
      label = ~ location,
      labelOptions = labelOptions(noHide = T)) %>%
    leaflet::setView(lng = mean(mainstations_locs$Lon), lat = mean(mainstations_locs$Lat), zoom = 6)
}


plot.windrose <- function(data = NULL,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0,
                          name_fill="Wind m/s",
                          plot.title = NA){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    # cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = name_fill, 
                      values = spd.colors,
                      drop = FALSE) +
    #theme_bw() +
    theme(axis.title.x = element_blank(),
          #panel.border = element_rect(colour = "blank"),
          panel.grid.major = element_line(colour="grey65"))
  
  if(!is.na(plot.title)){
    p.windrose <- p.windrose +
      ggtitle(plot.title)
  }
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose + theme_minimal())  
  
  # return the handle to the wind rose
  # return(p.windrose)
}
