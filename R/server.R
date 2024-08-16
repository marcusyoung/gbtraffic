#' Server function
#'
#' @param input input object
#' @param output output object
#' @param session session object
#'
#' @import shiny
#' @import leaflet
#' @import dplyr
#' @import ggplot2
#' @import reactable
# We must specifically import one (any one) sf function to enable use of tidyverse function like
# # filter, rename on an sf object. Otherwise get errors like:
# Error: Input must be a vector, not a `sfc_POLYGON/sfc` object. See:
# https://community.rstudio.com/t/convert-a-geojson-column-to-sf-use-tibble-printing-in-package/54955/6
#' @importFrom sf st_read
#' @importFrom stats sd
#'
server <- function(input, output, session) {
  # control debug status
  debug_status <- "off"
  debug <- reactiveVal(c())


  ## Setup database pool ----
  pool <- pool::dbPool (
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv('GBTRAFFIC_PG_DB'),
    host = Sys.getenv('GBTRAFFIC_PG_HOST'),
    port = Sys.getenv('GBTRAFFIC_PG_PORT'),
    user = Sys.getenv('GBTRAFFIC_PG_USER'),
    password = Sys.getenv('GBTRAFFIC_PG_PASSWORD'),
    # ensure don't get integer64 returned as R doesn't support (generally)
    bigint = "integer"
  )

  # after a user session ends close the pool
  onStop(function() {
    # print("closing pool")
    pool::poolClose(pool)
  })

  ## Define reactive Values ----
  count <- reactiveValues()
  count$id <- NULL
  count$type <- NULL

  layer <- reactiveValues()
  layer$id <- NULL
  layer_type <- NULL

  # map_basemap_init
  map_basemap_init <-
    reactiveVal("Stadia.AlidadeSmooth", label = "map_basemap_init")

  ## Define some static variables ----
  # Map groups related to counts
  count_map_groups <-
    c("mg_countpoints",
      "mg_aadf_edges")

  # stadia variables

  map_stadia_url <-
    'https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png'
  map_stadia_att <-
    '&copy; <a href="https://stadiamaps.com/">Stadia Maps</a>, &copy; <a href="https://openmaptiles.org/">OpenMapTiles</a> &copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors'

  ## Create initial map view ----
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6)) %>%
      # set up map panes to control layer order
      # Recommended: a zIndex value between 400 (default overlay pane) and
      # 500 (default shadow pane)
      addMapPane("mp_aadf_edges", zIndex = 401) %>%
      addMapPane("mp_countpoints", zIndex = 402) %>%
      setView(-4, 55, zoom = 6) %>%
      addTiles(
        urlTemplate = map_stadia_url,
        attribution = map_stadia_att,
        group = "mg_basemap",
        options = tileOptions()
      )
  })

  ## Map layer controls ----

  ### Basemaps ----

  observeEvent(input$choose_basemap, {
    req(input$choose_basemap != map_basemap_init())

    if (input$choose_basemap == "OpenStreetMap") {
      leafletProxy("map") %>%
        clearGroup("mg_basemap") %>%
        addProviderTiles("OpenStreetMap",
                         group = "mg_basemap",
                         options = providerTileOptions())
    } else if (input$choose_basemap == "Stadia.AlidadeSmooth") {
      leafletProxy("map") %>%
        clearGroup("mg_basemap") %>%
        addTiles(
          urlTemplate = map_stadia_url,
          attribution = map_stadia_att,
          group = "mg_basemap",
          options = tileOptions()
        )
    } else if (input$choose_basemap == "Esri.WorldImagery") {
      leafletProxy("map") %>%
        clearGroup("mg_basemap") %>%
        addProviderTiles("Esri.WorldImagery",
                         group = "mg_basemap",
                         options = providerTileOptions())
    }
    map_basemap_init(input$choose_basemap)
  })


  ### AADF Edges ----

  # switch to bounds of aadf_region
  observeEvent(input$aadf_region, {
    req(isTRUE(input$aadf_edges))

    leafletProxy("map") %>%
      fitBounds(
        lng1 = regions_bbox$lng1[regions_bbox$region == input$aadf_region],
        lat1 = regions_bbox$lat1[regions_bbox$region == input$aadf_region],
        lng2 = regions_bbox$lng2[regions_bbox$region == input$aadf_region],
        lat2 = regions_bbox$lat2[regions_bbox$region == input$aadf_region]
      )

  })

  # filter aadf_edges by traffic_class and map_year
  aadf_edges_filtered <- reactive ({
    aadf_edges %>%
      filter(region_id %in% input$aadf_region) %>%
      rename(selected_class = input$aadf_class) %>%
      filter(year == input$aadf_year &
               selected_class != 0)
  }, label = "aadf_edges_filtered")

  # set up aadf edges legend
  aadf_edges_legend_title <- reactive ({
    tags$div(paste0("AADF (", input$aadf_year, ")"),
             tags$br(),
             tags$small(names(modes)[modes == input$aadf_class]))
  }, label = "aadf_edges_legend_title")

  # define the edge scaling factor
  map_edges_scaling_factor <- reactive({
    case_when (
      input$aadf_class %in% c("pedal_cycles") ~ max(aadf_edges[[input$aadf_class]][aadf_edges$region == input$aadf_region]) /
        30,
      TRUE ~ quantile(aadf_edges[[input$aadf_class]][aadf_edges$region == input$aadf_region], c(0.95)) /
        8
    )
  }, label = "map_edges_scaling_factor")

  # Hide or show AADF edges group dependent on check box
  observeEvent(input$aadf_edges, {
    if (input$aadf_edges == TRUE) {
      leafletProxy("map") %>%
        showGroup("mg_aadf_edges")
    } else {
      leafletProxy("map") %>%
        hideGroup("mg_aadf_edges") %>%
        removeControl(1)
    }

    # leaflegend doesn't play nicely with showGroup and hideGroup
    # so if it should be there, we need to re-create the legend
    # Also do fitBounds again if layer has been hidden
    if (input$aadf_edges == TRUE &&
        input$aadf_region != "" && input$aadf_year != "") {
      leafletProxy("map") %>%
        leaflegend::addLegendSize(
          group = "mg_aadf_edges",
          color = "firebrick",
          shape = "rect",
          baseSize = mean((
            aadf_edges_filtered()$selected_class / map_edges_scaling_factor()
          )
          ),
          values = aadf_edges_filtered()$selected_class,
          layerId = 1,
          fillOpacity = 0.5,
          strokeWidth = 0,
          title = aadf_edges_legend_title(),
          position = "topleft",
          breaks = 4
        ) %>%
        fitBounds(
          lng1 = regions_bbox$lng1[regions_bbox$region == input$aadf_region],
          lat1 = regions_bbox$lat1[regions_bbox$region == input$aadf_region],
          lng2 = regions_bbox$lng2[regions_bbox$region == input$aadf_region],
          lat2 = regions_bbox$lat2[regions_bbox$region == input$aadf_region]
        )
    }

  })

  # control the aadf edges layer
  observeEvent({
    aadf_edges_filtered()
  }, {
    if (input$aadf_edges == TRUE &&
        input$aadf_region != "" && input$aadf_year != "") {
      leafletProxy("map", data = aadf_edges_filtered()) %>%
        clearGroup("mg_aadf_edges") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 12)) %>%
        addPolylines(
          options = pathOptions(pane = "mp_aadf_edges"),
          group = "mg_aadf_edges",
          layerId = ~ link_id,
          weight = (
            aadf_edges_filtered()$selected_class / map_edges_scaling_factor()
          ),
          color = "firebrick",
          opacity = 0.5,
          highlightOptions = highlightOptions(
            color = "firebrick",
            bringToFront = F,
            opacity = 0.9
          )
        ) %>%
        leaflegend::addLegendSize(
          group = "mg_aadf_edges",
          color = "firebrick",
          shape = "rect",
          baseSize = mean((
            aadf_edges_filtered()$selected_class / map_edges_scaling_factor()
          )
          ),
          values = aadf_edges_filtered()$selected_class,
          layerId = 1,
          fillOpacity = 0.5,
          strokeWidth = 0,
          title = aadf_edges_legend_title(),
          position = "topleft",
          breaks = 4
        ) %>%
        leaflet.extras2::stopSpinner()
    }
  })



  ### Filter map DfT countpoints ----

  map_countpoints_filtered <- reactive({
    if (input$road_type == "all") {
      countpoints
    } else {
      filter(countpoints, road_type == input$road_type)
    }
  }, label = "map_countpoints_filtered")


  # Hide and show countpoints layer depending on menu checkbox for the layer
  observeEvent(input$countpoints, {
    if (input$countpoints == TRUE) {
      leafletProxy("map") %>%
        showGroup("mg_countpoints")
    } else {
      leafletProxy("map") %>%
        hideGroup("mg_countpoints")
    }

  })

  observeEvent({
    map_countpoints_filtered()
  }, {
    if (input$countpoints == TRUE) {
      leafletProxy("map",
                   data = map_countpoints_filtered()) %>%
        clearGroup("mg_countpoints") %>%
        leaflet.extras2::addSpinner() %>%
        leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 12)) %>%
        addCircleMarkers(
          options = pathOptions(pane = "mp_countpoints"),
          group = "mg_countpoints",
          label = ~ paste0(count_point_id, ": ", road_name),
          layerId = ~ count_point_id,
          radius = 10,
          color = "purple",
          fillOpacity = 0.3,
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = TRUE,
            spiderfyOnMaxZoom = FALSE,
            removeOutsideVisibleBounds = TRUE,
            disableClusteringAtZoom = 13,
          ),
          clusterId = "cluster_countpoints"

        ) %>%
        leaflet.extras2::stopSpinner()
    }
  })

  ## Observe map clicks ----

  ### Observe circle marker (countpoint click) ----
  observeEvent(input$map_marker_click, {
    req(input$map_marker_click$clusterId)
    if (input$map_marker_click$clusterId == "cluster_countpoints") {
      count$id <- input$map_marker_click$id
      # count$source is used to determine whether count_id change is the result
      # of a map click on a feature or some other method - such as the duplicate
      # countpoint at the same location check.
      count$source <- "map_click"
      count$type <- "dft_countpoint"
      updateTabsetPanel(inputId = "mainTabset", selected = "panel_countpoints")

    } else {
      # if any other active layer clicked set everything to NULL
      count$id <- NULL
      count_type <- NULL
    }

    # clear any existing marker
    # may revisit this and the way markers are added for countpoints
    leafletProxy("map") %>%
      clearGroup("mg_map_marker")

  })

  ### Observe click on aadf_edge ----

  observeEvent(input$map_shape_click, {
    if (input$map_shape_click$group == "mg_aadf_edges") {
      count$id <- input$map_shape_click$id
      count$source <- "map_click"
      count$type <- "aadf_edge"

      updateTabsetPanel(inputId = "mainTabset", selected = "panel_countpoints")

    } else {
      # if any other active layer clicked set everything to NULL
      count$id <- NULL
      count_type <- NULL
    }

    # clear any existing marker
    # may revisit this and the way markers are added for countpoints
    leafletProxy("map") %>%
      clearGroup("mg_map_marker")

  })


  ## Observe count$id ----

  # If count$id changes and count$type is dft_countpoint or aadf_edge then we
  # need to check for possible duplicate points at same location.

  observeEvent(count$id, {
    # note req that count$source is a map_click and not from the duplicate count
    # point selector.
    req(count$id,
        count$source == "map_click")

    if (count$type %in% c("dft_countpoint", "aadf_edge")) {
      multi_check <-
        paste0(
          "select count_point_id from traffic_gb.countpoints where st_intersects(geom,
        (select geom from traffic_gb.countpoints where count_point_id  = ",
          count$id,
          "))"
        )
      multi_points <- DBI::dbGetQuery(pool, multi_check)

      if (nrow(multi_points) > 1) {
        showNotification(
          "There are multiple count points at this location. Select the required
        count point from the drop down list.",
          action = NULL,
          type = c("warning"),
          duration = 3
        )
        output$multi_points <- renderUI({
          tagList(
            selectInput(
              inputId =  "countpoint_select",
              label = "Multiple count points available",
              choices = multi_points$count_point_id,
              selected = count$id
            )
          )
        })
      } else {
        # render empty if not any duplicates
        output$multi_points <- renderUI({

        })
      }
    } else {
      # need to clear the dropdown irrespective of count$type
      output$multi_points <- renderUI({

      })
    }
  })

  ## Observe count point selector ----
  observeEvent(input$countpoint_select, {
    # don't update count_id unless there is a change to the selection
    req(
      count$type %in% c("dft_countpoint", "aadf_edge"),
      input$countpoint_select != count$id
    )
    count$id <- input$countpoint_select
    # set the source of the count id change to "duplicate selector". This prevents
    # the duplicate checker running again when count points are selected via the
    # drop down.
    count$source <- "duplicate_selector"
  })

  ## Get count point info and data ----

  # countpoint_info
  countpoint_info <- reactive({
    req(count$id,
        count$type %in% c("dft_countpoint",
                          "aadf_edge"))

    get_countpoint_info(countpoints,
                        count_id = count$id,
                        count_type = count$type)
  }, label = "countpoint_info")

  # countpoint_data
  countpoint_data <- reactive({
    req(count$id,
        count$type %in% c("dft_countpoint",
                          "aadf_edge"))
    get_countpoint_data(pool,
                        count_id = count$id,
                        count_type = count$type)
  }, label = "countpoint_data")

  # if get new countpoint_info zoom map to the countpoint and mark it
  observeEvent(countpoint_info(), {
    # only set new zoom if we need to
    if (input$map_zoom < 14) {
      selection_zoom <- 14
    } else {
      selection_zoom <- input$map_zoom
    }

    leafletProxy("map") %>%
      clearGroup("mg_map_marker") %>%
      setView(
        lng = countpoint_info()$coords[2],
        lat = countpoint_info()$coords[1],
        zoom = selection_zoom
      ) %>%
      addAwesomeMarkers(
        lng = countpoint_info()$coords[2],
        lat = countpoint_info()$coords[1],
        group = "mg_map_marker",
        label = paste0("Currently selected count point (", countpoint_info()$table$Id, ")"),
        icon = icon_road
      )
  })

  ## Filter countpoint data  ----

  # count point data can be filtered by:
  #           1) traffic class/mode
  #           2) year from and/or to
  #           3) estimation method
  #           4) type of plot required

  countpoint_filtered <- reactive ({
    req(countpoint_data())
    if (input$estimation_method %in% c("any", "aadf", "Estimated", "Counted")) {
      filter_countpoint_data(
        countpoint_data()$countpoint_aadf,
        countpoint_data()$countpoint_aadf_bydirection,
        countpoint_data()$countpoint_aadf_byday,
        year_from = input$year_from,
        year_to = input$year_to,
        traffic_class = input$traffic_class,
        estimation_method = input$estimation_method,
        plot_type = input$plot_type
      )
    } else if (input$estimation_method %in% c("raw_year", "raw_hour")) {
      filter_countpoint_data(
        countpoint_data()$countpoint_raw,
        countpoint_data()$countpoint_raw_bydirection,
        year_from = input$year_from,
        year_to = input$year_to,
        traffic_class = input$traffic_class,
        estimation_method = input$estimation_method,
        plot_type = input$plot_type
      )
    } else if (input$estimation_method %in% c("aahf")) {
      filter_countpoint_data(
        countpoint_data()$countpoint_aahf,
        countpoint_data()$countpoint_aahf_bydirection,
        countpoint_data()$countpoint_aahf_byday,
        year_from = input$year_from,
        year_to = input$year_to,
        traffic_class = input$traffic_class,
        estimation_method = input$estimation_method,
        plot_type = input$plot_type
      )
    }
  }, label = "countpoint_filtered")

  ## Filter Traffic Volume data ----

  # reactives for traffic filters
  tv_num_areas <-
    reactive(length(input$tv_area), label = "tv_num_areas")

  tv_type <- reactive({
    case_when (
      input$tv_road == "all_roads" & input$tv_class == "all_mv" ~ "pc_diff_all_mv_allroads_traffic",
      input$tv_road == "all_roads" & input$tv_class == "cars_taxis"  ~ "pc_diff_cars_taxis_allroads_traffic",
      input$tv_road == "major_roads" & input$tv_class == "all_mv"  ~ "pc_diff_all_mv_calc_major_traffic",
      input$tv_road == "major_roads" & input$tv_class == "cars_taxis"  ~ "pc_diff_cars_taxis_calc_major_traffic",
      input$tv_road == "minor_roads" & input$tv_class == "all_mv"  ~ "pc_diff_all_mv_calc_minor_traffic",
      input$tv_road == "minor_roads" & input$tv_class == "cars_taxis"  ~ "pc_diff_cars_taxis_calc_minor_traffic"
    )
  }, label = "tv_type")

  tv_type_cn <- reactive({
    case_when (
      input$tv_road == "all_roads" & input$tv_class == "all_mv" ~ "All roads: All motor vehicles",
      input$tv_road == "all_roads" & input$tv_class == "cars_taxis" ~ "All roads: Cars and taxis",
      input$tv_road == "major_roads" & input$tv_class == "all_mv" ~ "Major roads: All motor vehicles",
      input$tv_road == "major_roads" & input$tv_class == "cars_taxis" ~ "Major roads: Cars and taxis",
      input$tv_road == "minor_roads" & input$tv_class == "all_mv" ~ "Minor roads: All motor vehicles",
      input$tv_road == "minor_roads" & input$tv_class == "cars_taxis" ~ "Minor roads: Cars and taxis"
    )
  }, label = "tv_type_cn")

  # Traffic volume data depends upon:
  #                 1) the areas(s) selected by the user
  #                 2) the number of areas selected by the user
  #                 3) the mode

  traffic_filtered <- reactive({
    filter_traffic_data(la_traffic,
                        tv_area = input$tv_area,
                        tv_num_areas = tv_num_areas())
  }, label = "traffic_filtered")

  ## Outputs ----

  ### Count point Tab ----

  countpoint_plot_title <- reactive({
    req(input$estimation_method)
    case_when (
      input$estimation_method == "raw_year" ~ toupper("Raw counts by day"),
      input$estimation_method == "raw_hour" ~ toupper("Raw counts by hour of day"),
      input$estimation_method == "aahf" ~ toupper("Annual Average Hourly Flow"),
      TRUE ~ toupper("Annual Average Daily Flow")
    )
  })

  countpoint_plot_prefix <- reactive({
    req(count$type)
    case_when (count$type %in% c("dft_countpoint", "aadf_edge") ~ "DfT")
  })

  countpoint_plot_subtitle <- reactive({
    req(countpoint_info(), input$traffic_class)
    paste0(
      countpoint_plot_prefix(),
      " count point: ",
      countpoint_info()$table$Id ,
      " - ",
      names(modes)[modes == input$traffic_class]
    )
  })

  #### Bar chart ----

  output$countpoint_plot <- renderPlot({
    req(countpoint_filtered())


    # validation before calling the plot function
    # logical year range
    validate(
      need(
        input$year_to >= input$year_from,
        "'Year to' must be greater than or equal to 'Year from'."
      )
    )

    # if byclass then need to have all motor vehicles selected
    if (input$plot_type == "byclass" &&
        input$traffic_class != "all_mv") {
      validate("If chosen chart type is 'By class' then traffic class must be 'All motorised'.")
    }

    # validate there is data to show
    # that there are some rows and the selected_class is not all NAs
    # example of all NAs is the dummy pedal_cycle column in the drakewell data
    validate(
      need(
        nrow(countpoint_filtered()) > 0 &
          !all(is.na(
            countpoint_filtered()$selected_class
          )),
        "There are no matches in the dataset. Try changing one or more filters."
      )
    )

    # selected_class has a sum > 0 for at least one year - otherwise nothing to display.
    validate(
      need(
        sum(countpoint_filtered()$selected_class, na.rm = TRUE) > 0,
        "The requested value is zero across all years for this combination of filters. This may indicate that the selected class was not recorded at this count point."
      )
    )

    # call countpoint plot function
    output_countpoint_plot(
      countpoint_filtered(),
      estimation_method = input$estimation_method,
      plot_type = input$plot_type,
      traffic_class = input$traffic_class,
      title = countpoint_plot_title(),
      subtitle = countpoint_plot_subtitle(),
      caption = paste0("Data: ", countpoint_plot_prefix())
    )
  }, res = 96)

  #### Count point information output ----

  output$countpoint_info <-
    renderTable({
      req(countpoint_info())
      countpoint_info()$table
    }, width = "100%", align = "l",  spacing = 'xs')

  #### File download ----

  output$downloadCpData <-
    downloadHandler(
      filename = function() {
        tolower(
          paste0(
            countpoint_info()$table$Id,
            "_",
            input$year_from,
            "_",
            input$year_to,
            "_",
            input$estimation_method,
            "_",
            input$plot_type,
            ".csv"
          )
        )
      },
      content = function(file) {
        df_csv <- countpoint_filtered()
        # set factors for the local authority and region ids if present
        # so that file has the names rather than the DfT numbers
        if (all(c("local_authority_id", "region_id") %in% colnames(df_csv))) {
          df_csv$local_authority_id <-
            factor(
              df_csv$local_authority_id,
              levels = unlist(c(las), use.names = FALSE),
              labels = names(c(las))
            )
          df_csv$region_id <-
            factor(
              df_csv$region_id,
              levels = unlist(c(regions_dft), use.names = FALSE),
              labels = names(c(regions_dft))
            )
          # and rename columns
          df_csv <- df_csv %>%
            rename(local_authority = local_authority_id) %>%
            rename(region = region_id)
        }
        df_csv %>%
          rename_with(~ input$traffic_class, selected_class) %>%
          utils::write.csv(file, row.names = FALSE)
      },
      contentType = "text/csv"
    )

  ### Traffic Tab ----

  #### Line charts  ----

  output$traffic_plot <- renderPlot({
    req(tv_num_areas() > 0)
    output_traffic_plot(
      traffic_filtered(),
      tv_num_areas = tv_num_areas(),
      tv_type = tv_type(),
      tv_type_cn = tv_type_cn(),
      tv_area = input$tv_area
    )
  } , res = 96)

  #### Data tables ----

  output$traffic_table <- renderReactable ({
    req(tv_num_areas() > 0)
    output_traffic_table(traffic_filtered(), tv_num_areas = tv_num_areas())
  })

  #### File download ----

  output$downloadData <-
    downloadHandler(
      filename = function() {
        if (tv_num_areas() > 1) {
          "area_comparison_traffic.csv"
        } else {
          tolower(paste0(gsub(" | - ", "_", names(c(
            regions, las
          ))[c(regions, las) == input$tv_area]), "_traffic", ".csv"))
        }
      },
      content = function(file) {
        # need to convert to wide format if single area
        df_csv <- traffic_filtered()
        df_csv <- df_csv %>%
          rename(region_or_la = traffic_area_id)
        df_csv <- df_csv %>% mutate(across(c("all_mv_traffic_all_roads", "cars_taxis_traffic_all_roads"), ~ format( ., scientific = FALSE)))


        if (tv_num_areas() > 1) {
          utils::write.csv(df_csv, file, row.names = FALSE)
        } else {
          utils::write.csv(
            tidyr::pivot_wider(df_csv,
                               names_from = "key",
                               values_from = "traffic"),
            file,
            row.names = FALSE
          )
        }
      },

      contentType = "text/csv"
    )

  #### Road length plots ----

  output$mjrl_plot <- renderPlot({
    if (tv_num_areas() == 1) {
      output_roadlength_plot(traffic_filtered(), "major")
    }
  }, res = 96)

  output$mnrl_plot <- renderPlot({
    if (tv_num_areas() == 1) {
      output_roadlength_plot(traffic_filtered(), "minor")
    }
  }, res = 96)



  ## Conditional panels ----

  # control conditional panel for countpoint info
  # don't want to show it unless we have info to show
  output$info_flag <-
    reactive(!is.null(countpoint_info()), label = "output$info_flag")
  outputOptions(output, "info_flag", suspendWhenHidden = FALSE)


  # information modals

  observeEvent(input$aadf_links_info, {
    gbtraffic_info_modal(title = "DfT Major Road Network", file = "info_aadf_links.html")
  })

  observeEvent(input$estimation_method_info, {
    gbtraffic_info_modal(title = "Count points - Method", file = "info_method.html")
  })

  observeEvent(input$tv_area_info, {
    gbtraffic_info_modal(title = "Traffic Volume - Areas", file = "info_tv.html")
  })

  observeEvent(input$traffic_class_info, {
    gbtraffic_info_modal(title = "Count points - Vehicle Class", file = "info_traffic_class.html")
  })

  ## Debug output ----
  output$debug <- renderText ({
    req(debug_status == "on")
    paste(debug())
  })


}
