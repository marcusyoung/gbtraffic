#' UI function
#' @param req req
#' @import shiny
#' @import reactable

ui <- function(req) {
  fluidPage(
    tags$head(
      HTML(
        "<script async data-id='101430202' src='//static.getclicky.com/js'></script>"
      ),
      tags$meta(name = "description", content = "Visually explore DfT traffic statistics for the whole of Great Britain from 2000-2023, including count points, area summaries, and major road links."),
      tags$link(rel = "stylesheet", type = "text/css", href = "trafficr.css"),
      tags$link(
        rel = "apple-touch-icon",
        sizes = "180x180",
        href = "apple-touch-icon.png"
      ),
      tags$link(
        rel = "icon",
        type = "image/png",
        sizes = "32x32",
        href = "favicon-32x32.png"
      ),
      tags$link(
        rel = "icon",
        type = "image/png",
        sizes = "16x16",
        href = "favicon-16x16.png"
      ),
      tags$link(
        rel = "manifest",
        href = "site.webmanifest",
        crossorigin = "use-credentials"
      ),
      tags$title("GB Traffic Data Explorer")
    ),
    theme = shinythemes::shinytheme("flatly"),
    tags$div(
      class = "page-header",
      tags$h3(
        id = "title",
        "GB Traffic Data Explorer",
        tags$span(id = "version", paste0(
          "v", utils::packageVersion(utils::packageName())
        ))
      ),
      tags$span(
        id = "subheading",
        "Developed by Marcus Young at the",
        tags$a(
          href = "https://www.southampton.ac.uk/research/groups/transportation-group",
          target = "_blank",
          "Transportation Research Group"
        )
      )
    ),
    fluidRow(
      # map
      column(
        id = "map_column",
        5,
        leafletOutput("map",  height = "85vh"),
        absolutePanel(
          top = 10,
          right = 15,
          width = 185,
          draggable = FALSE,
          wellPanel(
            id = "layercontrol",
            checkboxInput(
              'showMenu',
              tags$span(id = "gbtraffic_menu_toggle", 'Menu show/hide'),
              TRUE
            ),
            conditionalPanel(
              # prevent showing briefly on load
              style = "display: none;",
              condition = 'input.showMenu',
              h6(strong("Background map")),
              tags$div(
                class = "choose_option",
                selectInput(
                  inputId = "choose_basemap",
                  label = NULL,
                  choices = c(
                    "Streetmap (grey)" = "Stadia.AlidadeSmooth",
                    "Streetmap (colour) " = "OpenStreetMap",
                    "Satellite" = "Esri.WorldImagery"
                  ),
                  selected = "Stadia.AlidadeSmooth"
                )
              ),
              h6(strong("DfT Count Points")),
              hr(class = "layer_divider"),
              checkboxInput(
                inputId = "countpoints",
                label = "on/off",
                value = TRUE
              ),
              conditionalPanel(
                # prevent showing briefly on load
                style = "display: none;",
                condition = 'input.countpoints',
                radioButtons(
                  inputId = "road_type",
                  label = NULL,
                  choices = c(
                    "All" = "all",
                    "Major roads" = "major",
                    "Minor roads" = "minor"
                  ),
                  selected = "major"
                )
              ),
              h6(strong("DfT Major Road Network")),
              hr(class = "layer_divider"),
              checkboxInput(
                inputId = "aadf_edges",
                label = gbtraffic_info_link("on/off", "aadf_links_info", size = "small"),
                value = FALSE
              ),
              conditionalPanel(
                # prevent showing briefly on load
                style = "display: none;",
                condition = 'input.aadf_edges',
                tags$div(
                  class = "choose_aadf",
                  selectizeInput(
                    inputId =  "aadf_region",
                    label = NULL,
                    choices = c("Select region" = "", regions[order(names(regions))]),
                    multiple = FALSE,
                    options = list(maxItems = 1)
                  ),
                  selectizeInput(
                    inputId =  "aadf_year",
                    label = NULL,
                    choices = c("Select year" = "", end_year:start_year),
                    multiple = FALSE,
                    options = list(maxItems = 1)
                  ),
                  selectizeInput(
                    inputId =  "aadf_class",
                    label = NULL,
                    choices = c(modes),
                    multiple = FALSE,
                    selected = "all_mv",
                    options = list(maxItems = 1)
                  )
                )
              )
            )
          )
        ),
        tags$span(
          id = "footer",
          "Contains public sector information obtained from the ",
          tags$a(href = "https://roadtraffic.dft.gov.uk/about", target = "_blank", "DfT"),
          " and licensed under the ",
          tags$a(
            href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
            target = "_blank",
            " Open Government Licence v3.0"
          )
        )
      ),
      # outputs and filtering
      column(7,
             tabsetPanel(
               id = "mainTabset",
               #### Countpoints Panel ----
               tabPanel(
                 "Count Points",
                 value = 'panel_countpoints',
                 fluidRow(class = "panel_intro", column(
                   12,
                   p(
                     "Click on a count point to display its data. Filter the data and change the chart type using the drop-down lists below. On the map, view the Annual Average Daily Flow (AADF) of major road count points as lines of proportional width on the DfTs generalised road network (for clarity the scale factor varies across vehicle class and region but is constant across years). Set the region, year and vehicle class using the map-menu selectors. Click on a road link to explore the data on its associated count point."
                   ),
                   tags$hr()
                 )),
                 fluidRow(
                   class = "data_filters",
                   column(
                     3,
                     selectInput(
                       inputId = "traffic_class",
                       label = gbtraffic_info_link("Vehicle class", "traffic_class_info"),
                       choices = modes
                     )
                   ),
                   column(
                     2,
                     selectInput(
                       inputId =  "year_from",
                       label = "Year from",
                       choices = start_year:end_year,
                       selected = start_year
                     )
                   ),
                   column(
                     2,
                     selectInput(
                       inputId =  "year_to",
                       label = "Year to",
                       choices = start_year:end_year,
                       selected = end_year
                     )
                   ),
                   column(
                     2,
                     selectInput(
                       inputId =  "estimation_method",
                       label = gbtraffic_info_link("Method", "estimation_method_info"),
                       choices = list(
                         "AADF based on:" = list(
                           "Any" = "any",
                           "Count" = "Counted",
                           "Estimate" = "Estimated"
                         ),
                         "Raw count" = list("Day" = "raw_year", "Hour" = "raw_hour")
                       )
                     )
                   ),
                   column(
                     3,
                     selectInput(
                       inputId =  "plot_type",
                       label = "Chart type",
                       choices = list(
                         "Single" = "single",
                         "By direction" = "bydirection",
                         "By class" = "byclass"
                       )
                     )
                   )
                 ),
                 fluidRow(class = "data_filters", column(3,
                                                         uiOutput("multi_points")),
                          column(9)),

                 fluidRow(column(
                   12,
                   plotOutput("countpoint_plot", height = "500px", width = "100%")
                 )),
                 fluidRow(column(
                   12,
                   conditionalPanel(
                     # prevent showing briefly on load
                     style = "display: none;",
                     condition = "output.info_flag==true",
                     div(
                       style = "font-size: 85%",
                       id = "infoPanel",
                       h5("About this count point"),
                       tableOutput("countpoint_info"),
                       downloadButton("downloadCpData", "Download CSV file", class = "btn-xs btn-info")
                     )
                   )
                 ))
                 # close tabpanel 1
               ),
               # open tabpanel 2
               #### Traffic Panel ----
               tabPanel(
                 "Traffic Volume",
                 value = "panel_traffic",
                 fluidRow(class = "panel_intro", column(
                   12,
                   p(
                     "Select up to six regions and/or local authorities to display data. You can pick from the dropdown list or start typing a name to restrict options. A single area will show a plot and table of annual vehicle miles travelled by road type and vehicle class. Multiple areas will show a plot and a table of annual percentage change in vehicle miles travelled; select road type and vehicle class using the dropdowns. To remove an area from the selection, click on it and press delete or backspace."
                   ),
                   tags$hr()
                 )),
                 fluidRow(
                   class = "data_filters",
                   column(
                     4,
                     selectizeInput(
                       inputId =  "tv_area",
                       label = gbtraffic_info_link("Select Area(s)", "tv_area_info"),
                       choices = list("Regions" = regions[order(names(regions))], "LAs" = las[order(names(las))]),
                       multiple = TRUE,
                       options = list(maxItems = 6, selectOnTab = TRUE)
                     )
                   ),
                   column(
                     2,

                     conditionalPanel(
                       # prevent showing briefly on load
                       style = "display: none;",
                       condition = "input.tv_area.length > 1",
                       selectInput(
                         inputId =  "tv_road",
                         label = "Road type",
                         choices = list(
                           "All" = "all_roads",
                           "Major" = "major_roads",
                           "Minor" = "minor_roads"
                         )
                       )
                     )

                   ),
                   column(
                     3,
                     conditionalPanel(
                       # prevent showing briefly on load
                       style = "display: none;",
                       condition = "input.tv_area.length > 1",
                       selectInput(
                         inputId =  "tv_class",
                         label = "Vehicle class",
                         choices = list("All motor vehicles" = "all_mv",
                                        "Cars and taxis" = "cars_taxis")
                       )
                     )
                   ),
                   column(3),
                 ),
                 fluidRow(
                   column(9,
                          plotOutput(
                            "traffic_plot", width = "100%", height = "400px"
                          )),
                   column(3,
                          class = "traffic_column",
                          fluidRow(column(
                            12,
                            plotOutput("mjrl_plot", width = "100%", height = "195px"),
                            plotOutput("mnrl_plot", width = "100%", height = "195px")
                          )))
                 ),
                 fluidRow(
                   class = "margin-15",
                   column(9,
                          reactableOutput("traffic_table")),
                   column(
                     class = "traffic_column",
                     3,
                     conditionalPanel(
                       # prevent showing briefly on load
                       style = "display: none;",
                       condition = "input.tv_area.length > 0",
                       downloadButton("downloadData", "Download CSV file", class = "btn-xs btn-info")
                     )
                   )
                 )
               )
               # close tab panel 2
             ))
    ),
    fluidRow(# map
      column(6,
             p(
               uiOutput("debug")
             )))
  )
}
