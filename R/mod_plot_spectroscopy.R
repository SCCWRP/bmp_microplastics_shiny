#' pie_plot_func UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pie_plot_func_ui <- function(id){
  ns <- NS(id)

  filter_side_bar <- sidebar(
    open = "open",
    width = "15%",
    title = h4('Control Panel'),
    shinyWidgets::pickerInput(ns("matrix_select"), "Select Matrix:", choices = c('media','stormwater'), selected = 'stormwater'),
    shinyWidgets::pickerInput(ns("bmp_select"), "Select BMP:", choices = NULL),
    shinyWidgets::pickerInput(ns("year_select"), "Select Sampling Year:", choices = NULL),
    shinyWidgets::pickerInput(ns("replicate_select"), "Select Lab Replicate:", choices = NULL),
    shinyWidgets::pickerInput(ns("sizefraction_select"), "Select Size Fraction (can be multiple):", choices = NULL, multiple = TRUE)
  )

  layout_sidebar(
    sidebar = filter_side_bar,
    bslib::navset_card_underline(
      id = ns("main_infiltration"),
      bslib::nav_panel(
        title = "Instruction",
        layout_columns(
          fillable = FALSE,
          fill = TRUE,
          padding = 0,
          gap = 0,
          col_widths = 12,
          row_heights = c(3, 1),
          bslib::card(
            height = '1000px',
            full_screen = FALSE,
            card_header(
              "Sample Composition Plots",
              layout_column_wrap(
                width = 1/2,
                fillable = TRUE,
                full_screen = FALSE,
                shinyWidgets::pickerInput(
                  ns('pie_type'),
                  choices = c('size_fraction','morphology','color', 'chemicaltype')
                ),
                shinyWidgets::awesomeCheckbox(
                  inputId = ns("is_mp_pie"),
                  label = "Only Microplastics",
                  value = FALSE,
                  status = "danger"
                )
              )
            ),
            card_body(
              fill = TRUE,
              fillable = TRUE,
              padding = 0,
              gap = 0,
              shinycssloaders::withSpinner(plotOutput(ns('pie_plot'), height="800px"), type = 5)
            ),
            card_footer(
              downloadButton(ns("download_pie_plot_dat"), "Download Data"),
              downloadButton(ns("download_pie_plot"), "Download Plot"),
            )
          ),
          bslib::card(
            fillable = FALSE,
            full_screen = TRUE,
            card_header("Microplastics Concentration Plots"),
            card_body(
              fillable = FALSE,
              class = "fs-6",
              # shinyWidgets::awesomeCheckbox(
              #   inputId = ns("is_mp_concentration"),
              #   label = "Only Microplastics",
              #   value = FALSE,
              #   status = "danger"
              # ),
              shinycssloaders::withSpinner( plotOutput(ns('concentration_plot')), type = 5),
              DT::DTOutput(ns("dynamicTable"))
            ),
            card_footer(
              downloadButton(ns("download_concentration_plot_dat"), "Download Data"),
              downloadButton(ns("download_concentration_plot"), "Download Plot"),  # <-- Add this
              fill = FALSE
            )

          ),
        )
      ),
      bslib::nav_panel(
        title = "Method",
        h3("Microplastics Concentration Calculation"),
        tags$ul(
          tags$li("Extracts spectroscopy and microscopy data along with constants."),
          tags$li("Groups and summarizes the microscopy and spectroscopy data to count particles."),
          tags$li("Calculates the percentage of microplastic particles using:"),
          tags$li(HTML("<strong>percentage_is_mp = (# of MP particles) / (total # of spectroscopy particles)</strong>")),
          tags$li("Joins summaries and constants to compute an adjusted particle count and concentration.")
        ),
        p("Key formulas used include:"),
        p(HTML("<strong>Subsample adjustment:</strong> count = {count_micro * percentage_is_mp (if subsample) or count_spectro (if not subsample)")),
        p(HTML("<strong>Back-calculated particle count:</strong> count / (pct_filter_counted * pct_sample_processed)")),
        p(HTML("<strong>Concentration:</strong> (count / pct_filter_counted) / (pct_sample_processed * unit_passing)"))
      )
    )

  )
}

#' pie_plot_func Server Functions
#'
#' @noRd
mod_pie_plot_func_server <- function(id, pool, raw_data_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Reactive expressions to get distinct values from the database
    bmps <- reactive({
      req(input$matrix_select)

      get_bmp_options(
        dat = raw_data_list$dat_rawftir,
        matrixselect = input$matrix_select
      )
    })

    years <- reactive({
      req(input$bmp_select)
      get_year_options(
        dat = raw_data_list$dat_rawftir,
        bmpselect = input$bmp_select
      )
    })

    replicate <- reactive({
      req(input$bmp_select, input$year_select)
      get_replicate_options(
        dat = raw_data_list$dat_rawftir,
        bmpselect = input$bmp_select,
        yearselect = input$year_select
      )
    })

    size_fraction <- reactive({
      req(input$bmp_select, input$year_select, input$replicate_select)
      get_sizefraction_options(
        dat = raw_data_list$dat_rawftir,
        bmpselect = input$bmp_select,
        yearselect = input$year_select,
        replicateselect = input$replicate_select
      )
    })

    event <- reactive({
      req(input$bmp_select, input$year_select, input$sizefraction_select, input$replicate_select)
      get_event_options(
        dat = raw_data_list$dat_rawftir,
        bmpselect = input$bmp_select,
        yearselect = input$year_select,
        sizefractionselect = input$sizefraction_select,
        replicateselect = input$replicate_select
      )
    })

    # Reactive expression to gather unit information
    unit_info <- reactive({
      req(input$bmp_select, input$year_select, input$sizefraction_select,
          input$replicate_select)

      constants <- raw_data_list$constants %>% filter(
        bmp == input$bmp_select &
          year == input$year_select &
          replicate == input$replicate_select &
          size_fraction %in% input$sizefraction_select
      )

      location_levels <- c(
        sort(unique(constants$location[grepl("^influent", constants$location)])),
        sort(unique(constants$location[grepl("^effluent", constants$location)]))
      )

      # Set location as a factor with the custom levels
      constants$location <- factor(constants$location, levels = location_levels)

      # Arrange and select only the required columns
      constants <- constants %>%
        arrange(location) %>%
        distinct(event, location, sample_volume, sub_sample, unit)
      constants
    })

    output$dynamicTable <- DT::renderDT({
      DT::datatable(
        unit_info(),
        colnames = c("Event", "Location", "Sample Volume", 'Subsample Volume', 'Unit'),  # Replace with your desired names
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE
        )
      )
    })

    observeEvent(input$matrix_select, {
      shinyWidgets::updatePickerInput(session, "bmp_select", choices = bmps())
      shinyWidgets::updatePickerInput(session, "year_select", choices = NULL, selected = NULL)
      shinyWidgets::updatePickerInput(session, "replicate_select", choices = NULL, selected = NULL)
      shinyWidgets::updatePickerInput(session, "sizefraction_select", choices = NULL, selected = NULL)
      shinyWidgets::updatePickerInput(session, "event_select", choices = NULL, selected = NULL)
    })

    observeEvent(input$bmp_select, {
      shinyWidgets::updatePickerInput(session, "year_select", choices = years())
    })

    observeEvent(list(input$bmp_select, input$year_select), {
      shinyWidgets::updatePickerInput(session, "replicate_select", choices = replicate())
    })

    observeEvent(list(input$bmp_select, input$year_select, input$replicate_select), {
      shinyWidgets::updatePickerInput(session, "sizefraction_select", choices = size_fraction(), selected = size_fraction())
    })

    observeEvent(list(input$bmp_select, input$sizefraction_select, input$replicate_select), {
      shinyWidgets::updatePickerInput(session, "event_select", choices = event())
    })

    # Reactive expression to load and process data
    processed_data <- reactive({
      req(input$matrix_select, input$bmp_select, input$year_select,
          input$sizefraction_select, input$replicate_select, input$pie_type)

      pie_plot_dat <- get_pieplot_data(
        dat = raw_data_list$dat_rawftir,
        constants = raw_data_list$constants,
        matrixselect = input$matrix_select,  # New parameter
        bmpselect = input$bmp_select,
        yearselect = input$year_select,
        sizefractionselect = input$sizefraction_select,
        replicateselect = input$replicate_select,
        pie_type = input$pie_type,
        is_mp = input$is_mp_pie
      )

      concentration_plot_dat <- get_concentrationplot_data(
        raw_data_list = raw_data_list,
        matrixselect = input$matrix_select,  # New parameter
        bmpselect = input$bmp_select,
        yearselect = input$year_select,
        sizefractionselect = input$sizefraction_select,
        replicateselect = input$replicate_select
      )

      list(
        pie_plot_dat = pie_plot_dat,
        concentration_plot_dat = concentration_plot_dat
      )
    })



    output$pie_plot <- renderPlot({
      req(input$pie_type)
      p <- get_stacked_bar_plot (
        plot_dat = processed_data()$pie_plot_dat,
        breakdowntype = input$pie_type
      )
      p
    })


    output$concentration_plot <- renderPlot({
      p <- get_concentration_plot(
        plot_dat = processed_data()$concentration_plot_dat$plot_dat,
        bmpselect = input$bmp_select,
        yearselect=  input$year_select,
        sizefractionselect = input$sizefraction_select,
        replicateselect = input$replicate_select
      )
      p
    })


    output$download_pie_plot_dat <- downloadHandler(
      filename = function() {
        paste("pieplot-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        dat <- processed_data()$pie_plot_dat
        write.csv(dat, file, row.names = FALSE)
      }
    )

    output$download_concentration_plot_dat <- downloadHandler(
      filename = function() {
        paste("concentration-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        dat <- processed_data()$concentration_plot_dat$concentration_dat
        write.csv(dat, file, row.names = FALSE)
      }
    )

    output$download_pie_plot <- downloadHandler(
      filename = function() {
        paste("pie-plot-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        plot_obj <- get_stacked_bar_plot(
          plot_dat = processed_data()$pie_plot_dat,
          breakdowntype = input$pie_type
        )
        ggsave(file, plot = plot_obj + theme_light() + theme(
          strip.text = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 18)
        ), width = 16, height = 8, dpi = 300, units = "in")
      }
    )

    output$download_concentration_plot <- downloadHandler(
      filename = function() {
        paste("concentration-plot-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        plot_obj <- get_concentration_plot(
          plot_dat = processed_data()$concentration_plot_dat$plot_dat,
          bmpselect = input$bmp_select,
          yearselect = input$year_select,
          sizefractionselect = input$sizefraction_select,
          replicateselect = input$replicate_select
        )
        ggsave(file, plot = plot_obj  + theme_light() + theme(
          strip.text = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size = 14),
          plot.title = element_text(size = 18)
        ), width = 16, height = 8, dpi = 300, units = "in")
      }
    )

  })
}


