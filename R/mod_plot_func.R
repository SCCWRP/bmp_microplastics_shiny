#' plot_func UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_func_ui <- function(id, pool){
  ns <- NS(id)

  pool <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = Sys.getenv("dbname"),
    host = Sys.getenv("host"),
    user = Sys.getenv("user"),
    password = Sys.getenv("password")
  )

  # bmps <- pool::dbGetQuery(
  #   pool,
  #   'SELECT DISTINCT bmp from tbl_bmp_particle_raw_all ORDER BY bmp'
  # )$bmp
  #
  # years <- pool::dbGetQuery(
  #   pool,
  #   'SELECT DISTINCT year from tbl_bmp_particle_raw_all ORDER BY year'
  # )$year
  #
  # size_fraction <- pool::dbGetQuery(
  #   pool,
  #   'SELECT DISTINCT size_fraction from tbl_bmp_particle_raw_all ORDER BY size_fraction'
  # )$size_fraction

  card_select <- bslib::card(
    full_screen = TRUE,
    card_header("Control Panel"),
    card_body(
      class = "fs-6",
      selectInput(ns("bmp_select"), "Select BMP:", choices = NULL),
      selectInput(ns("year_select"), "Select Year:", choices = NULL),
      selectInput(ns("sizefraction_select"), "Select Size Fraction:", choices = NULL),

      downloadButton(ns("download_constants"), "Download Constants"),
      downloadButton(ns("download_rawall"), "Download Microscopy Raw Data"),
      downloadButton(ns("download_rawftir"), "Download Spectroscopy Raw Data")

    )
  )

  card_plot <- bslib::card(
    full_screen = TRUE,
    card_header("Plots"),
    card_body(
      class = "fs-6",
      shinycssloaders::withSpinner( plotOutput(ns('concentration_plot')), type = 7),
      downloadButton(ns("download_summary"), "Download Summary Data for the Selected BMP and Year")
    )
  )


  tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::layout_column_wrap(
        card_select,
        width = 1,
        heights_equal = "row",
      ),
      card_plot

    )
  )

}


#' plot_func Server Functions
#'
#' @noRd
#' @import ggplot2 glue dplyr

mod_plot_func_server <- function(id, pool, raw_data_list){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    # Reactive expressions to get distinct values from the database
    bmps <- reactive({
      pool::dbGetQuery(pool, 'SELECT DISTINCT bmp FROM tbl_bmp_particle_raw_all ORDER BY bmp')$bmp
    })

    years <- reactive({
      req(input$bmp_select)
      pool::dbGetQuery(pool, glue::glue("SELECT DISTINCT year FROM tbl_bmp_particle_raw_all WHERE bmp = '{input$bmp_select}' ORDER BY year"))$year
    })

    size_fraction <- reactive({
      req(input$bmp_select, input$year_select)
      pool::dbGetQuery(pool, glue::glue("SELECT DISTINCT size_fraction FROM tbl_bmp_particle_raw_all WHERE bmp = '{input$bmp_select}' AND year = '{input$year_select}' ORDER BY size_fraction"))$size_fraction
    })

    # Initialize BMP choices when app starts
    observe({
      updateSelectInput(session, "bmp_select", choices = bmps())
    })

    observeEvent(input$bmp_select, {
      updateSelectInput(session, "year_select", choices = years())
    })

    observeEvent(input$year_select, {
      updateSelectInput(session, "sizefraction_select", choices = size_fraction())
    })



    # Reactive expression to load and process data
    processed_data <- reactive({

      selected_bmp <- input$bmp_select
      selected_year <- input$year_select
      selected_sizefraction <- input$sizefraction_select

      print(selected_bmp)
      print(selected_year)
      print(selected_sizefraction)


      dat <- raw_data_list$dat_rawall
      constants <- raw_data_list$constants

      # FILTERING
      dat <- dat %>% filter(bmp == selected_bmp & year == selected_year & size_fraction == selected_sizefraction)

      grouped_dat <- dat %>%
        group_by(bmp, year, event, location, matrix, size_fraction, replicate) %>%
        summarize(count = n()) %>%
        ungroup()

      # Left join with constants
      result <- grouped_dat %>%
        left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
        arrange(bmp, year, event, location, matrix, size_fraction, replicate) %>%
        mutate(concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing))

      # Group by location and event, and sum the concentration
      final_result <- result %>%
        filter(!is.na(concentration) & is.finite(concentration)) %>%
        group_by(location, event) %>%
        summarize(total_concentration = sum(concentration)) %>%
        ungroup()

      # Ensure the locations are in the right order
      location_levels <- c(
        sort(unique(final_result$location[grepl("^influent", final_result$location)])),
        sort(unique(final_result$location[grepl("^effluent", final_result$location)]))
      )

      # Convert location to a factor with the custom levels
      final_result$location <- factor(final_result$location, levels = location_levels)

      list(final_result = final_result, constants = constants, result = result)


    })
    output$concentration_plot <- renderPlot({
      req(input$bmp_select, input$year_select)

      data <- processed_data()
      COLOR_PALETTE <- c(`1`="#0000FF0A", `2`="#00008B", `3` = "#FFC0CB", `4` = "#8B0000")

      final_result <- data$final_result
      dodge <- position_dodge(width = 0.7)
      y_lim <- max(final_result$total_concentration) + (max(final_result$total_concentration) * 0.1)

      ggplot(final_result, aes(x = location, y = total_concentration, group = event, fill = as.factor(event))) +
        geom_bar(stat = "identity", position = dodge, width = 0.5, color = "black") +
        geom_text(aes(label = round(total_concentration, 1)),
                  position = dodge,
                  vjust = -0.5,
                  size = 6) +
        ylim(0, y_lim) +
        scale_fill_manual(values = COLOR_PALETTE) +
        labs(title = glue("{input$bmp_select} - Year {input$year_select} - SF {input$sizefraction_select}"),
             x = "Location",
             y = "Concentration (P/L)",
             fill = "Event") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right",
          legend.text = element_text(size = 20),
          text = element_text(size = 30)
        )

    })

    # Download handler for constants dataframe
    output$download_constants <- downloadHandler(
      filename = function() {
        paste("constants-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- processed_data()
        write.csv(data$constants, file, row.names = FALSE)
      }
    )

    output$download_rawall <- downloadHandler(
      filename = function() {
        paste("microscopy-raw-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(raw_data_list$dat_rawall, file, row.names = FALSE)
      }
    )

    output$download_rawftir <- downloadHandler(
      filename = function() {
        paste("spectroscopy-raw-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(raw_data_list$dat_rawftir, file, row.names = FALSE)
      }
    )

    output$download_summary <- downloadHandler(
      filename = function() {
        paste("summary-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- processed_data()
        write.csv(data$result, file, row.names = FALSE)
      }
    )


  })
}
