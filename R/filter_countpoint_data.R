#' Filters countpoint_aadf, countpoint_raw, countpoint_aadf_bydirection or countpoint_raw_bydirection
#'
#' Depends on countpoint_plot_type and filters on traffic_class, countpoint_year_from
#' and countpoint_estimation_method
#'
#' @param df1 dataframe any direction
#' @param df2 dataframe by direction
#' @param df3 dataframe by day of week
#' @param plot_type character
#' @param traffic_class character
#' @param year_from integer
#' @param year_to integer
#' @param estimation_method character
#' @return Returns dataframe countpoint_filtered


filter_countpoint_data <-
  function (df1,
            df2,
            df3,
            plot_type,
            traffic_class,
            year_from,
            year_to,
            estimation_method) {
    if (plot_type == "bydirection") {
      countpoint_filtered <- df2
    } else if (plot_type == "byday") {
      countpoint_filtered <- df3
    # if plot type is "byclass" we only pivot_longer if traffic class is set to "all_mv"
    # byclass does nothing if a single class/mode is selected
    } else if (plot_type == "byclass" &
               traffic_class == "all_mv") {
      countpoint_filtered <-
        tidyr::pivot_longer(
          df1,
          c(
            two_wheeled_mv,
            cars_and_taxis,
            buses_and_coaches,
            lgvs,
            hgvs_all
          ),
          names_to = "mode"
        )
      # set mode factor labels for plots
      countpoint_filtered$mode <-
        factor(countpoint_filtered$mode,
               levels = rev(
                 c(
                   "cars_and_taxis",
                   "buses_and_coaches",
                   "lgvs",
                   "hgvs_all",
                   "pedal_cycles",
                   "two_wheeled_mv"
                 )
               ),
               labels = rev(
                 c(
                   "Cars/Taxis",
                   "Buses/Coaches",
                   "LGVs",
                   "HGVs",
                   "Bicycles",
                   "Motorcycles"
                 )
               ))

    } else {
      # case when plot type is standard year
      countpoint_filtered <- df1
    }

    # filter by year
    countpoint_filtered <-
      countpoint_filtered %>% dplyr::filter(year >= year_from, year <= year_to) %>%
      # rename the selected traffic class column to selected_class
      # this is then used to display mode-specific counts
      dplyr::rename(selected_class = traffic_class)

    # filter by estimation method if it is counted or estimated
    if (estimation_method %in% c("Counted", "Estimated")) {
      countpoint_filtered <-
        countpoint_filtered %>% dplyr::filter(estimation_method %in% (!!estimation_method))
    }


    return(countpoint_filtered)

  }
