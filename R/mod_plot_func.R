#' plot_func UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_func_ui <- function(id){
  ns <- NS(id)

  filter_side_bar <- sidebar(
    open = "open",
    width = "15%",
    title = h4('Control Panel'),
    shinyWidgets::pickerInput(ns("bmp_select"), "Select BMP:", choices = NULL),
    shinyWidgets::pickerInput(ns("year_select"), "Select Year:", choices = NULL),
    shinyWidgets::pickerInput(ns("sizefraction_select"), "Select Size Fraction:", choices = NULL),
    shinyWidgets::pickerInput(ns("replicate_select"), "Select Replicate:", choices = NULL)
  )

  layout_sidebar(
    sidebar = filter_side_bar,
    layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        card_header("Pie Plots"),
        card_body(
          class = "fs-6",
          shinyWidgets::pickerInput(
            ns('pie_type'),
            choices = c('morphology','color')
          ),
          shinycssloaders::withSpinner( plotOutput(ns('pie_plot')), type = 5),
          downloadButton(ns("download_summary_pie"), "Download Data for this plot")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        card_header("Concentration Plots"),
        card_body(
          class = "fs-6",
          shinycssloaders::withSpinner( plotOutput(ns('concentration_plot')), type = 5),
          downloadButton(ns("download_summary"), "Download Data for this plot")
        )
      )
    )
  )




  # tagList(
  #   bslib::layout_columns(
  #     col_widths = c(4, 8),
  #     bslib::layout_column_wrap(
  #       card_select,
  #       width = 1,
  #       heights_equal = "row",
  #     ),
  #     card_plot
  #   )
  # )

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

    replicate <- reactive({
      req(input$bmp_select, input$year_select, input$sizefraction_select)
      pool::dbGetQuery(pool, glue::glue(
        "SELECT DISTINCT replicate FROM tbl_bmp_particle_raw_all
        WHERE bmp::VARCHAR = '{input$bmp_select}' AND year::VARCHAR = '{input$year_select}' AND size_fraction::VARCHAR = '{input$sizefraction_select}'
        ORDER BY replicate"))$replicate
    })

    # Initialize BMP choices when app starts
    observe({
      shinyWidgets::updatePickerInput(session, "bmp_select", choices = bmps())
    })

    observeEvent(input$bmp_select, {
      shinyWidgets::updatePickerInput(session, "year_select", choices = years())
    })

    observeEvent(input$year_select, {
      shinyWidgets::updatePickerInput(session, "sizefraction_select", choices = size_fraction())
    })

    observeEvent(input$sizefraction_select, {
      shinyWidgets::updatePickerInput(session, "replicate_select", choices = replicate())
    })



    # Reactive expression to load and process data
    processed_data <- reactive({
      req(input$bmp_select, input$year_select, input$sizefraction_select, input$replicate_select)

      selected_bmp <- input$bmp_select
      selected_year <- input$year_select
      selected_sizefraction <- input$sizefraction_select
      selected_replicate <- input$replicate_select

      dat <- raw_data_list$dat_rawall
      constants <- raw_data_list$constants

      # FILTERING
      dat <- dat %>% filter(bmp == selected_bmp & year == selected_year & size_fraction == selected_sizefraction & replicate == selected_replicate)

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
      list(
        final_result = final_result,
        constants = constants,
        result = result,
        original_dat = dat
      )

    })

    pieplot_dat <- reactive({
      req(input$pie_type)
      dat <- processed_data()$original_dat

      # Group by the required columns and get the count for each group
      plot_dat <- dat %>%
        group_by(bmp, year, location, matrix, size_fraction, replicate, !!sym(input$pie_type)) %>%
        summarise(count = n()) %>%
        mutate(percentage = (count / sum(count)) * 100) %>%
        ungroup()
      list(
        plot_dat = plot_dat
      )
    })


    output$pie_plot <- renderPlot({
      req(input$pie_type)
      plot_dat <- pieplot_dat()$plot_dat

      # Ensure `plot_dat` has the required columns
      breakdowntype <- input$pie_type

      location_levels <- c(
        sort(unique(plot_dat$location[grepl("^influent", plot_dat$location)])),
        sort(unique(plot_dat$location[grepl("^effluent", plot_dat$location)]))
      )

      # Convert location to a factor with the custom levels
      plot_dat$location <- factor(plot_dat$location, levels = location_levels)

      ggplot(plot_dat, aes(x = factor(1), y = percentage, fill = as.factor(!!sym(breakdowntype)))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        facet_wrap(~location) +
        geom_text(aes(label = paste0(round(percentage, 1), "%")),
                  position = position_stack(vjust = 0.5)) +
        theme_void() +
        theme(
          legend.position = "right",
          legend.text = element_text(size = 20),
          text = element_text(size = 24)
        ) +
        labs(
          title = glue("{input$bmp_select}-Y{input$year_select}-SF{input$sizefraction_select}-Rep{input$replicate_select}"),
          fill = breakdowntype
        )

    })


    output$concentration_plot <- renderPlot({
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
        labs(title = glue("{input$bmp_select}-Y{input$year_select}-SF{input$sizefraction_select}-Rep{input$replicate_select}"),
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
