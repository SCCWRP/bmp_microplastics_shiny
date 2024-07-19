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

  bmps <- pool::dbGetQuery(
    pool,
    'SELECT DISTINCT bmp from tbl_bmp_particle_raw_all ORDER BY bmp'
  )$bmp

  years <- pool::dbGetQuery(
    pool,
    'SELECT DISTINCT year from tbl_bmp_particle_raw_all ORDER BY year'
  )$year

  card_select <- bslib::card(
    full_screen = TRUE,
    card_header("Menu"),
    card_body(
      class = "fs-6",
      selectInput(ns('bmp_select'), 'Select a BMP:', choices = bmps),
      selectInput(ns('year_select'), 'Select a Year:', choices = years),
      downloadButton(ns("download_constants"), "Download Constants")

    )
  )

  card_plot <- bslib::card(
    full_screen = TRUE,
    card_header("Plot"),
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

mod_plot_func_server <- function(id, pool){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Reactive expression to load and process data
    processed_data <- reactive({

      selected_bmp <- input$bmp_select
      selected_year <- input$year_select

      data_qry <- glue(
        "SELECT * FROM tbl_bmp_particle_raw_all
        WHERE location != 'not applicable'
        AND bmp = '{selected_bmp}'
        AND year = '{selected_year}'"
      )
      constants_qry <- "SELECT * FROM bmp_constants"

      dat <- pool::dbGetQuery(pool, data_qry)
      constants <- pool::dbGetQuery(pool, constants_qry) %>% select(-objectid)

      # FILTERING
      dat <- dat %>% filter(bmp == selected_bmp & year == selected_year)

      grouped_dat <- dat %>%
        group_by(bmp, year, event, location, matrix, size_fraction, replicate) %>%
        summarize(count = n()) %>%
        ungroup()

      # Left join with constants
      result <- grouped_dat %>%
        left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
        arrange(bmp, year, event, location, matrix, size_fraction, replicate)

      filtered_result <- result %>%
        filter(!is.na(unit_passing))

      # Calculate concentration
      filtered_result <- filtered_result %>%
        mutate(concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing))

      # Group by location and event, and sum the concentration
      final_result <- filtered_result %>%
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
      data <- processed_data()
      COLOR_PALETTE <- c(`1`="#0000FF0A", `2`="#00008B", `3` = "#FFC0CB", `4` = "#8B0000")

      final_result <- data$final_result
      dodge <- position_dodge(width = 0.5)
      y_lim <- max(final_result$total_concentration) + (max(final_result$total_concentration) * 0.1)

      ggplot(final_result, aes(x = location, y = total_concentration, group = event, fill = as.factor(event))) +
        geom_bar(stat = "identity", position = dodge, width = 0.5, color = "black") +
        geom_text(aes(label = round(total_concentration, 2)),
                  position = position_dodge(width = 0.5),
                  vjust = -0.5,
                  size = 6) +
        ylim(0, y_lim) +
        scale_fill_manual(values = COLOR_PALETTE) +
        labs(title = "All particle's concentration",
             x = "Location",
             y = "Concentration (P/L)",
             fill = "Event") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right",
          legend.text = element_text(size = 20),
          text = element_text(size = 24)
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

    # Download handler for result dataframe
    output$download_summary <- downloadHandler(
      filename = function() {
        paste("result-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- processed_data()
        write.csv(data$result, file, row.names = FALSE)
      }
    )


  })
}
