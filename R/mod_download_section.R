#' download_section UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_section_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Download all data"),
    downloadButton(ns("download_constants"), "Download Constants"),
    downloadButton(ns("download_rawall"), "Download Microscopy Raw Data"),
    downloadButton(ns("download_rawftir"), "Download Spectroscopy Raw Data"),
    downloadButton(ns("download_summaryall"), "Download Spectroscopy Summary Data")
  )
}

#' download_section Server Functions
#'
#' @noRd
mod_download_section_server <- function(id, pool, raw_data_list){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Download handler for constants dataframe
    output$download_constants <- downloadHandler(
      filename = function() {
        paste("constants-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(raw_data_list$constants, file, row.names = FALSE)
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

    output$download_summaryall <- downloadHandler(
      filename = function() {
        paste("summary-all-microscopy-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(raw_data_list$dat_summaryall, file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_download_section_ui("download_section_1")

## To be copied in the server
# mod_download_section_server("download_section_1")
