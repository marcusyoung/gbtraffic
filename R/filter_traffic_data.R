#' Generates traffic_filtered from la_traffic
#'
#' Depends on the area(s) selected by the user in tv_area and the
#' the number of areas selected. If more than 1 group by area and
#' create the % diff columns for each mode; if one area then la_traffic
#' is pivoted long to enable the display of multiple lines for each vehicle
#' type and mode.
#'
#' @param la_traffic dataframe
#' @param tv_area character
#' @param tv_num_areas integer
#'
#' @returns Returns dataframe traffic_filtered. This will be in wide format if it contains more
#' than one area (single % change line to be plotted for each area). For a single area it will be
#' in long format (multiple lines to be plotted for the same area).

filter_traffic_data <- function (la_traffic, tv_area, tv_num_areas) {

traffic_filtered <-
  la_traffic %>% dplyr::filter(traffic_area_id %in% tv_area)

# set factor labels for traffic_area_id
# so these are used as legend labels in plots

traffic_filtered$traffic_area_id <-
  factor(
    traffic_filtered$traffic_area_id,
    levels = unlist(c(regions,las), use.names = FALSE),
    labels = names(c(regions,las))
  )

# if more than one area selected we calculate % year on year difference

if (tv_num_areas > 1) {
  traffic_filtered <-
    # group by local authority and then calculate annual percentage change
    # making use of lag() function.
    traffic_filtered %>% dplyr::group_by(traffic_area_id) %>% dplyr::filter(n() > 1) %>%
    # calculate for all roads - all motor vehicles
    dplyr::mutate(pc_diff_all_mv_allroads_traffic = round(((all_mv_traffic_all_roads - dplyr::lag(all_mv_traffic_all_roads)) / dplyr::lag(all_mv_traffic_all_roads)
    ) * 100, 2)) %>%
    # calculate for all roads - cars and taxis
    dplyr::mutate(pc_diff_cars_taxis_allroads_traffic = round(((
      cars_taxis_traffic_all_roads - dplyr::lag(cars_taxis_traffic_all_roads)
    ) / dplyr::lag(cars_taxis_traffic_all_roads)
    ) * 100, 2)) %>%
    # calculate for major Roads - all motor vehicles
    dplyr::mutate(pc_diff_all_mv_calc_major_traffic = round(((all_mv_calc_major_traffic - dplyr::lag(all_mv_calc_major_traffic)) / dplyr::lag(all_mv_calc_major_traffic)
    ) * 100, 2)) %>%
    # calculate for major Roads - cars and taxis
    dplyr::mutate(pc_diff_cars_taxis_calc_major_traffic = round(((
      cars_taxis_calc_major_traffic - dplyr::lag(cars_taxis_calc_major_traffic)
    ) / dplyr::lag(cars_taxis_calc_major_traffic)
    ) * 100, 2)) %>%
    # calculate for minor Roads - all motor vehicles
    dplyr::mutate(pc_diff_all_mv_calc_minor_traffic = round(((all_mv_calc_minor_traffic - dplyr::lag(all_mv_calc_minor_traffic)) / dplyr::lag(all_mv_calc_minor_traffic)
    ) * 100, 2)) %>%
    # calculate for minor Roads - cars and taxis
    dplyr::mutate(pc_diff_cars_taxis_calc_minor_traffic = round(((
      cars_taxis_calc_minor_traffic - dplyr::lag(cars_taxis_calc_minor_traffic)
    ) / dplyr::lag(cars_taxis_calc_minor_traffic)
    ) * 100, 2))

  # otherwise comparing by road type (minor / major) and mode (all mv or cars/taxis)
} else {

  # convert table from wide format into long format
  traffic_filtered <- traffic_filtered %>%
    tidyr::pivot_longer(
      c(
        all_mv_calc_major_traffic,
        cars_taxis_calc_major_traffic,
        all_mv_calc_minor_traffic,
        cars_taxis_calc_minor_traffic
      ),
      names_to = "key",
      values_to = "traffic"
    )

}

return(traffic_filtered)

}
