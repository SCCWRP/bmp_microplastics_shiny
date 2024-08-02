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
  tagList(

  )
}

#' pie_plot_func Server Functions
#'
#' @noRd
mod_pie_plot_func_server <- function(id, pool, raw_data_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_pie_plot_func_ui("pie_plot_func_1")

## To be copied in the server
# mod_pie_plot_func_server("pie_plot_func_1")
