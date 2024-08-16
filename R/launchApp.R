#' Launches the GB Traffic Data Explorer app
#'
#' @export launchApp
#' @return shiny application object
#' @import shiny
#'


# wrapper for shiny::shinyApp()
launchApp <- function() {

  shinyApp(
    ui = ui, server = server
  )
}
