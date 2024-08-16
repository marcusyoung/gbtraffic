#' Generates the bar chart for output$countpoint_plot using countpoint_aadf_filtered
#'
#' Validates that there are rows to display; validates that there are rows for
#' the currently selected class and the type of bar chart generated depends on
#' aadf_plot_type.
#'
#' @param df dataframe of filtered countpoint data
#' @param plot_type character
#' @param estimation_method character
#' @param traffic_class character
#' @param title character
#' @param subtitle character
#' @param caption character
#'
#' @returns Returns ggplot object

output_countpoint_plot <-
  function (df,
            plot_type,
            estimation_method,
            traffic_class,
            title = NULL,
            subtitle = NULL,
            caption = NULL) {

    # define factor labels
    factor_labels <- sort(unique(df$year))

    # For estimation_method raw_year we need to group by year as there are values for each hour
    # Additional grouping is required dependent on the graph type.
    # For 'single' and 'byclass' the grouping prevents white horizontal lines appearing in the bars
    # as otherwise the bars would be built up from multiple rows (each hour)

    if (estimation_method == "raw_year") {
      if (plot_type == "bydirection") {
        df <-
          df %>% dplyr::group_by(year, direction_of_travel) %>% dplyr::summarize(selected_class = sum(selected_class, na.rm = TRUE))
      } else if (plot_type == "single") {
        df <-
          df %>% dplyr::group_by(year) %>% dplyr::summarize(selected_class = sum(selected_class, na.rm = TRUE))
      } else if (plot_type == "byclass") {
        df <-
          df %>% dplyr::group_by(year, mode) %>% dplyr::summarize(value = sum(value, na.rm = TRUE))
      }
    }

    cp_p <- ggplot(df)
    cp_p_labs <-
      labs(title = title,
           subtitle = subtitle,
           caption = caption)

    cp_p_theme <-
      ggthemes::theme_fivethirtyeight() + theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1,
          margin = margin(-2, 0, 0, 0)
        ),
        legend.title = element_text(size = 9, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "plain")
      )
    if (estimation_method %in% c("any", "aadf", "Estimated", "Counted", "raw_year")) {
      # by year plots
      # for byclass - only use if traffic class is all_mv
      if (plot_type == "byclass" &
          traffic_class == "all_mv") {
        cp_p <-  cp_p + aes(
          x = factor(year, labels = factor_labels),
          y = value,
          fill = mode
        ) +
          geom_bar(width = 0.7, stat = "identity") + cp_p_labs + cp_p_theme +
          scale_fill_brewer(
            palette = "Set2",
            name = toupper("Vehicle\nclass"),
            direction = -1
          ) + guides(fill = guide_legend(nrow = 2))

      } else if (plot_type == "bydirection") {
        cp_p <-
          cp_p + aes(x = factor(year, labels = factor_labels), y = selected_class) +
          geom_bar(
            width = 0.65,
            aes(fill = factor(
              direction_of_travel, levels = c("N", "S", "E", "W", "C")
            )),
            position = position_dodge(0.65),
            stat = "identity"
          ) + cp_p_labs + cp_p_theme +
          scale_fill_brewer(palette = "Set2",
                            name = toupper("Direction\nof travel"))

      } else if (plot_type == "byday") {
        cp_p <-
          cp_p + aes(x = factor(year, labels = factor_labels), y = selected_class) +
          geom_bar(
            width = 0.65,
            aes(fill = factor(
              day_of_week,
              levels = c("1", "2", "3", "4", "5"),
              labels = c("Mon", "Tue", "Wed", "Thu", "Fri")
            )),
            position = position_dodge(0.65),
            stat = "identity"
          ) + cp_p_labs + cp_p_theme +
          scale_fill_brewer(palette = "Set2",
                            name = toupper("Day\nof week"))

      } else {
        cp_p <-
          cp_p + aes(x = factor(year, labels = factor_labels), y = selected_class) +
          geom_bar(
            width = 0.7,
            stat = "identity",
            fill = RColorBrewer::brewer.pal(4, "Set2")[1]
          ) + cp_p_labs + cp_p_theme
      }
    } else if (estimation_method %in% c("raw_hour")) {
      # Hourly plots - raw count (dft)

      raw_theme <- theme(legend.key.size = unit(10, "pt"))

      facet_theme <- theme (
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0.5, "lines"),
        panel.background = element_rect(fill = "grey88"),
        legend.margin = margin(0, 0, 0, 0)
      )

      raw_guides <- guides(fill = guide_legend(nrow = 2))

      if (plot_type == "byclass" &
          traffic_class == "all_mv") {
        cp_p <- cp_p + aes(x = factor(year), y = value) +
          geom_col(width = 0.7,
                   aes(fill = factor(count_hour, levels = c(
                     min(count_hour):max(count_hour)
                   ))),
                   position = position_dodge(0.7)) + cp_p_labs + cp_p_theme + raw_theme + facet_theme + raw_guides +
          scale_fill_viridis_d(
            option = "turbo",
            direction = -1,
            name = toupper("Hour")
          ) + facet_wrap(. ~ factor(mode), scales = "free_y")

      } else if (plot_type == "bydirection") {
        cp_p <- cp_p + aes(x = factor(year), y = selected_class) +
          geom_col(width = 0.7,
                   aes(fill = factor(count_hour, levels = c(
                     min(count_hour):max(count_hour)
                   ))),
                   position = position_dodge(0.7)) + cp_p_labs + cp_p_theme + raw_theme + facet_theme + raw_guides +
          scale_fill_viridis_d(
            option = "turbo",
            direction = -1,
            name = toupper("Hour")
          ) + facet_wrap(. ~ factor(
            direction_of_travel,
            levels = c('E', 'W', 'N', 'S'),
            labels = c("East", "West", "North", "South")
          ))

      } else if (plot_type == "byday") {
        cp_p <- cp_p + aes(x = factor(year), y = selected_class) +
          geom_col(width = 0.7,
                   aes(fill = factor(count_hour, levels = c(
                     min(count_hour):max(count_hour)
                   ))),
                   position = position_dodge(0.7)) + cp_p_labs + cp_p_theme + raw_theme + facet_theme + raw_guides +
          scale_fill_viridis_d(
            option = "turbo",
            direction = -1,
            name = toupper("Hour")
          ) + facet_wrap(. ~ factor(
            day_of_week,
            levels = c("1", "2", "3", "4", "5"),
            labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
          ))
      } else {
        cp_p <-
          cp_p + aes(x = factor(year), y = selected_class) +
          geom_col(width = 0.65,
                   aes(fill = factor(count_hour, levels = c(
                     min(count_hour):max(count_hour)
                   ))),
                   position = position_dodge(0.65)) + cp_p_labs + cp_p_theme + raw_theme + raw_guides +
          scale_fill_viridis_d(
            option = "turbo",
            direction = -1,
            name = toupper("Hour")
          )
      }

    }

    return(cp_p)

  }
