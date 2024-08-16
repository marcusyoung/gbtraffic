#' Generates tables for the traffic volume tab from traffic_filtered
#'
#' Table generated depends on the number of areas selected by user.
#'
#' @param traffic_filtered dataframe
#' @param tv_num_areas integer
#'
#' @returns Returns a reactable object


output_traffic_table <- function (traffic_filtered, tv_num_areas) {


percent_style <- function(value) {
  # ignore if NA (base year will be NA so not numeric value)
  if (!is.na(value)) {
    if (value > 0) {
      color <- "#008000"
    } else if (value < 0) {
      color <- "#e00000"
    } else {
      color <- "#777"
    }
    list(color = color)
  }
}

if (tv_num_areas > 1) {
  traffic_table <- gbtraffic_rtable(
    dplyr::select(
      traffic_filtered,
      c(
        traffic_area_id,
        year,
        all_countpoints_sum,
        major_countpoints_sum,
        pc_diff_all_mv_allroads_traffic,
        pc_diff_cars_taxis_allroads_traffic,
        pc_diff_all_mv_calc_major_traffic,
        pc_diff_cars_taxis_calc_major_traffic
      )
    ),
    groupBy = "traffic_area_id",
    columns = list(
      traffic_area_id = colDef(name = "Area"),
      year = colDef(name = "Year", align = "left"),
      all_countpoints_sum = colDef(name = "All roads"),
      major_countpoints_sum = colDef(name = "Major roads"),
      pc_diff_all_mv_allroads_traffic = colDef(name = "All motor vehicles", style = percent_style),
      pc_diff_cars_taxis_allroads_traffic = colDef(name = "Cars and taxis", style = percent_style),
      pc_diff_all_mv_calc_major_traffic = colDef(name = "All motor vehicles", style = percent_style),
      pc_diff_cars_taxis_calc_major_traffic = colDef(name = "Cars and taxis", style = percent_style)
    ),
    columnGroups = list(
      colGroup(
        name = "Number of count points with data",
        columns = c(
          "all_countpoints_sum",
          "major_countpoints_sum"
        )
      ),
      colGroup(
        name = "Annual % change in miles travelled (all roads)",
        columns = c(
          "pc_diff_all_mv_allroads_traffic",
          "pc_diff_cars_taxis_allroads_traffic"
        )
      ),
      colGroup(
        name = "Annual % change in miles travelled (major roads)",
        columns = c(
          "pc_diff_all_mv_calc_major_traffic",
          "pc_diff_cars_taxis_calc_major_traffic"
        )
      ),
      # need this so heading "Grouped" doesn't appear above column
      colGroup(name = "", columns = c("traffic_area_id"))
    )
  )

} else {
  # traffic_filtered will be in long format so need to convert back to wide for the table
  # use transmute to select and convert traffic volumes to millions of miles
  traffic_table <- gbtraffic_rtable(
    dplyr::transmute(tidyr::pivot_wider(traffic_filtered, names_from = "key", values_from = "traffic"),
        year,
        major_link_length,
        minor_link_length,
        all_mv_calc_major_traffic = all_mv_calc_major_traffic / 1000000,
        cars_taxis_calc_major_traffic = cars_taxis_calc_major_traffic / 1000000,
        all_mv_calc_minor_traffic = all_mv_calc_minor_traffic / 1000000,
        cars_taxis_calc_minor_traffic = cars_taxis_calc_minor_traffic / 1000000
    ),
    columns = list(
      year = colDef(name = "Year", align = "left"),
      major_link_length = colDef(name = "Major"),
      minor_link_length = colDef(name = "Minor"),
      all_mv_calc_major_traffic = colDef(name = "All motor vehicles", format = colFormat(digits = 0)),
      cars_taxis_calc_major_traffic = colDef(name = "Cars and taxis", format = colFormat(digits = 0)),
      all_mv_calc_minor_traffic = colDef(name = "All motor vehicles", format = colFormat(digits = 0)),
      cars_taxis_calc_minor_traffic = colDef(name = "Cars and taxis", format = colFormat(digits = 0))

    ),
    columnGroups = list(
      colGroup(
        name = "Road length (miles)",
        columns = c("major_link_length", "minor_link_length")
      ),
      colGroup(
        name = "Miles travelled on major roads (millions)",
        columns = c(
          "all_mv_calc_major_traffic",
          "cars_taxis_calc_major_traffic"
        )
      ),
      colGroup(
        name = "Miles travelled on minor roads (millions)",
        columns = c(
          "all_mv_calc_minor_traffic",
          "cars_taxis_calc_minor_traffic"
        )
      )
    )
  )

}

return(traffic_table)

}
