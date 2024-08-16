#' Generate line plot
#'
#' @param df data frame
#' @param x character, name of column for x axis. Can also be a function to be applied to that column, e.g "factor(year)".
#' @param y character, name of column for x axis. Can also be a function to be applied to that column.
#' @param group character, name of column to partition by. Can also be a function to be applied to that column, e.g. "factor(traffic_area_id)".
#' @param title character.
#' @param title_face character, font face.
#' @param title_size integer, font size to use for plot title.
#' @param subtitle_size integer, font size to use for plot subtitle.
#' @param legend_title character.
#' @param legend_title_size integer font size to use for legend title.
#' @param legend_title_face character, font size to use for legend title.
#' @param legend Boolean, whether a legend should be shown. Default is TRUE.
#' @param legend_nrow integer, number of rows to use for the legend. Default is 1.
#' @param highlight character, single value from the group column where specific line thickness is to be set.
#' @param highlight_size integer, weight of a line to be highlighted.
#' @param x_tick_label_size integer, font size of the x tick labels.
#' @param y_tick_label_size integer, font size of the y tick labels.
#' @param x_label character, label for the x axis.
#' @param y_label character, label for the x axis.
#' @param colour_palette character, name of a Color Brewer palette from brewer.pal.info.
#' @keywords internal
gbtraffic_line_plot <-
  function(df,
           x,
           y,
           group,
           title = NULL,
           subtitle = NULL,
           title_face = NULL,
           caption = NULL,
           title_size = NULL,
           subtitle_size = NULL,
           legend_title = NULL,
           legend_title_size = NULL,
           legend_title_face = NULL,
           legend = TRUE,
           legend_nrow = 1,
           highlight = NULL,
           highlight_size = 1,
           x_tick_label_size = NULL,
           y_tick_label_size = NULL,
           x_label = NULL,
           y_label = NULL,
           colour_palette = "Set1") {
    # the highlight and highlight_size parameters are used to show a thicker line for the specified traffic_area_id
    if (!is.null(highlight)) {
      # create a new column highlight_line in df and set to TRUE if the value in the group column == highlight, and otherwise FALSE.
      # this column is later used in an aesthetic mapping for geom_line() and the TRUE and FALSE values are converted to
      # numeric values using scale_size_manual(). Also used to adjust the highlighted lines weight in the legend.
      df <-
        df %>% mutate(highlight_line = !!sym(group) == highlight)
    } else {
      highlight_line <- FALSE
    }

    # set col guides - either to "none" or to specified rows
    if (!legend) {
      col_def <- "none"
    } else if (!is.null(highlight)) {
      # If using a highlight adjust width of highlighted line in the legend to the highlight_size.
      # This might be a bit of a hack - works I think because of the order of factor level names; it may not work in all scenarios.
      # Gets the highlight_line Boolean column from df after a distinct on group column and uses an ifelse to set the FALSEs to 0.5
      # and the TRUEs (where the group value matches the highlight variable) to highlight_size.
      # This create a vector of line weights (which is reversed) to apply to each value in the group column.
      col_def <-
        guide_legend(nrow = legend_nrow, override.aes = list(size = rev(c(
          ifelse(
            dplyr::distinct(df, !!sym(group), .keep_all = TRUE)$highlight_line,
            highlight_size,
            0.5
          )
        ))))
    } else {
      col_def <- guide_legend(nrow = legend_nrow)
    }

    p_theme <-
      ggthemes::theme_fivethirtyeight() + theme(
        text = element_text(family = "sans"),
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1,
          size = x_tick_label_size
        ),
        axis.text.y = element_text(size = y_tick_label_size),
        axis.title.y = element_blank(),
        plot.title = element_text(size = title_size, face = title_face),
        plot.subtitle = element_text(size = subtitle_size),
        legend.title = element_text(size = legend_title_size, face = legend_title_face),
      )
    p_labs <-
      labs(
        title = title,
        subtitle = subtitle,
        caption = caption,
        color = legend_title
      )
    p <- ggplot(df) + aes_string(
      x = x,
      y = y,
      colour = group,
      group = group
    ) + geom_line(aes(size = highlight_line), na.rm = TRUE) + geom_point(na.rm = TRUE) + p_theme + p_labs + guides(col = col_def) + scale_size_manual(values = c("TRUE" = highlight_size, "FALSE" = 0.5)) + guides(size = "none") + labs(x_label, y_label) + scale_color_brewer(palette = colour_palette)
  }

#' Generate a reactable table
#'
#' @param df dataframe
#' @param columns list of columns
#' @param columnGroups list of column groups
#' @param groupBy character column to group by
#' @param bordered boolean
#' @param striped boolean
#' @param compact boolean
#' @param showSortable boolean
#' @param defaultPageSize integer
#' @param showPageSizeOptions boolean
#' @param pageSizeOptions vector of integers
#' @param defaultColDef colDef object
#' @keywords internal
gbtraffic_rtable <-
  function(df,
           columns,
           columnGroups = NULL,
           groupBy = NULL,
           bordered = TRUE,
           striped = TRUE,
           compact = TRUE,
           showSortable = FALSE,
           defaultPageSize = 5,
           showPageSizeOptions = TRUE,
           pageSizeOptions = c(5, 10, 20, 40),
           defaultColDef = colDef(minWidth = 50)) {
    table_theme <- reactableTheme(style = list(fontSize = "small"))

    reactable(
      data = df,
      columns = columns,
      columnGroups = columnGroups,
      groupBy = groupBy,
      bordered = bordered,
      striped = striped,
      compact = compact,
      showSortable = showSortable,
      defaultPageSize = defaultPageSize,
      showPageSizeOptions = showPageSizeOptions,
      pageSizeOptions = pageSizeOptions,
      defaultColDef = defaultColDef,
      theme = table_theme
    )

  }

#' Generate information icon
#'
#' @param text string, text to be used for the label
#' @param id string, id to identify the actionLink when clicked
#' @param size string, size of icon to be used for the actionLink
#' @returns Returns a list of two items to be used as an input label: the passed text and an icon-based actionLink
#' @keywords internal
gbtraffic_info_link <- function(text, id, size = "standard") {
  # Currently fonts-font-awesome package installed by Ubuntu is  5.0.10
  # therefore need to use info-circle as icon name rather than circle-info.
  if (size == "small") {
    list(text, actionLink(
      id,
      label = "",
      icon = icon("info-circle", class = "sftz_info_link_small", lib = "font-awesome")
    ))
  } else {
    list(text, actionLink(
      id,
      label = "",
      icon = icon("info-circle", class = "sftz_info_link", lib = "font-awesome")
    ))
  }
}

#' Show a modal
#'
#' @param title string, text to be used for the model title
#' @param file string, file name of the html file with modal content
#' @keywords internal
gbtraffic_info_modal <- function (title = NULL, file) {
  showModal(
    modalDialog(
      renderUI(includeHTML(file)),
      footer = modalButton("Close"),
      fade = FALSE,
      title = title,
      size = "l",
      easyClose = TRUE
    )
  )
}
