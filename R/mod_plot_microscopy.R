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
          layout_column_wrap(
            width = 1/2,
            shinyWidgets::pickerInput(
              ns('event_select'),
              label = "Event",
              choices = NULL
            ),
            shinyWidgets::pickerInput(
              ns('pie_type'),
              label = 'Broken down by',
              choices = c('morphology','color')
            )
          )
        ),
        card_body(

          shinycssloaders::withSpinner( plotOutput(ns('pie_plot')), type = 5)
        ),
        card_body(
          downloadButton(ns("download_pie_plot_dat"), "Download plot's data"),
          fill = FALSE
        )
      ),
      bslib::card(
        full_screen = TRUE,
        card_header("Concentration Plots"),
        card_body(
          class = "fs-6",
          shinycssloaders::withSpinner( plotOutput(ns('concentration_plot')), type = 5),
        ),
        card_body(
          downloadButton(ns("download_concentration_plot_dat"), "Download plot's data"),
          fill = FALSE
        )
      ),
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
      get_bmp_options(
        dat = raw_data_list$dat_rawall
      )
    })

    years <- reactive({
      req(input$bmp_select)
      get_year_options(
        dat = raw_data_list$dat_rawall,
        bmpselect = input$bmp_select
      )
    })

    size_fraction <- reactive({
      req(input$bmp_select, input$year_select)
      get_sizefraction_options(
        dat = raw_data_list$dat_rawall,
        bmpselect = input$bmp_select,
        yearselect = input$year_select
      )
    })

    replicate <- reactive({
      req(input$bmp_select, input$year_select, input$sizefraction_select)
      get_replicate_options(
        dat = raw_data_list$dat_rawall,
        bmpselect = input$bmp_select,
        yearselect = input$year_select,
        sizefractionselect = input$sizefraction_select
      )
    })

    event <- reactive({
      req(input$bmp_select, input$year_select, input$sizefraction_select, input$replicate_select)
      get_event_options(
        dat = raw_data_list$dat_rawall,
        bmpselect = input$bmp_select,
        yearselect = input$year_select,
        sizefractionselect = input$sizefraction_select,
        replicateselect = input$replicate_select
      )
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

    observeEvent(input$replicate_select, {
      shinyWidgets::updatePickerInput(session, "event_select", choices = event())
    })


    # Reactive expression to load and process data
    processed_data <- reactive({
      req(input$bmp_select, input$year_select, input$sizefraction_select, input$replicate_select, input$event_select, input$pie_type)

      pie_plot_dat <- get_pieplot_data(
        dat = raw_data_list$dat_rawall,
        constants = raw_data_list$constants,
        bmpselect = input$bmp_select,
        yearselect = input$year_select,
        sizefractionselect = input$sizefraction_select,
        replicateselect = input$replicate_select,
        eventselect = input$event_select,
        pie_type = input$pie_type
      )

      concentration_plot_dat <- get_concentrationplot_data(
        raw_data_list = raw_data_list,
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
      p <- get_pie_plot(
        plot_dat = processed_data()$pie_plot_dat,
        breakdowntype = input$pie_type
      )
      p
    })


    output$concentration_plot <- renderPlot({
      p <- get_concentration_plot(
        plot_dat = processed_data()$concentration_plot_dat,
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
        paste("concentrationplot-data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        dat <- processed_data()$concentration_plot_dat
        write.csv(dat, file, row.names = FALSE)
      }
    )





  })
}

