#' Generates road length plots for the traffic volume tab.
#'
#' @param traffic_filtered dataframe
#' @param road_type string
#'
#' @returns Returns ggplot object.
#'
output_roadlength_plot <- function (traffic_filtered, road_type) {

  rl_p <- gbtraffic_line_plot(
    df = traffic_filtered,
    x = "factor(year)",
    y = paste0(road_type, "_link_length"),
    group = "factor(traffic_area_id)",
    title = toupper("Road Length"),
    subtitle = paste(stringr::str_to_title(road_type), "(miles)"),
    title_size = 9,
    subtitle_size = 9,
    x_tick_label_size = 8,
    y_tick_label_size = 8,
    legend = FALSE
  )

  rl_p <- rl_p + scale_x_discrete(breaks = seq(start_year, end_year, by = 4)) + theme(plot.margin = unit(c(8, 8, 8, 8), "pt"))

  return(rl_p)

}
