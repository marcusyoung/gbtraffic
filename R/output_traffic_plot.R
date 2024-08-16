#' Generates plots for the traffic volume tab.
#'
#' Plot type depends on the number of areas selected by the user (tv_num_areas).
#'
#' @param traffic_filtered dataframe
#' @param tv_num_areas integer
#' @param tv_type character. Traffic road type and mode
#' @param tv_type_cn character. Common name of traffic road type and mode
#' @param tv_area character
#'
#' @returns Returns ggplot object.

output_traffic_plot <-
  function (traffic_filtered,
            tv_num_areas,
            tv_type,
            tv_type_cn,
            tv_area) {
    if (tv_num_areas > 1) {
      tv_p <- gbtraffic_line_plot(
        df = traffic_filtered,
        x = "factor(year)",
        y = paste(tv_type),
        group = "traffic_area_id",
        title = toupper("Percent change in vehicle miles travelled"),
        subtitle = tv_type_cn,
        title_face = "bold",
        caption = "Data: DfT and derived from DfT",
        legend_nrow = 2,
        legend_title = toupper("Area"),
        legend_title_size = 9,
        legend_title_face = "plain",
        title_size = 11,
        subtitle_size = 11
      )

      tv_p <- tv_p + geom_hline(aes(yintercept = 0))

      # If one plot selected plot vehicle miles traveled by road type and mode

    } else {
      # set factor labels for the key column
      traffic_filtered$key <-
        factor(
          traffic_filtered$key,
          levels = c(
            "all_mv_calc_major_traffic",
            "cars_taxis_calc_major_traffic",
            "all_mv_calc_minor_traffic",
            "cars_taxis_calc_minor_traffic"
          ),
          labels = c(
            "All MV - major roads",
            "Cars/taxis - major roads",
            "All MV - minor roads",
            "Cars/taxis - minor roads"
          )
        )

      # exclude any rows where traffic is NA
      tv_p <- gbtraffic_line_plot(
        df = dplyr::filter(traffic_filtered,!is.na(traffic)),
        x = "factor(year)",
        y = "traffic / 1000000",
        group = "key",
        title = toupper("Vehicle miles travelled (millions)"),
        subtitle = names(c(regions, las)[c(regions, las) == tv_area]),
        caption = "Data: DfT and derived from DfT",
        legend_nrow = 2,
        title_size = 11,
        subtitle_size = 11
      )

      tv_p <- tv_p

    }

    return(tv_p)

  }
