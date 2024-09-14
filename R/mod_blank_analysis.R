#' blank_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
mod_blank_analysis_ui <- function(id){
  ns <- NS(id)
    bslib::card(
      bslib::card_header(
        class = "fs-6",
        "Blank Analysis Table"
      ),
      bslib::card_body(
        class = 'p-0',
        shinycssloaders::withSpinner( DT::DTOutput(ns("blank_table")),type = 5 ),
        height = "100%"
      ),
      bslib::card_body(
        class = 'p-0',
        fill=FALSE,
        downloadButton(ns("download_blank_analysis"), "Download Blank Analysis", class = "btn-primary")
      )
    )
}

#' blank_analysis Server Functions
#'
#' @noRd
mod_blank_analysis_server <- function(id, pool, raw_data_list){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    blank_analysis <- raw_data_list$blank_analysis

    # Render the DataTable with conditional formatting for the "result" column
    output$blank_table <- DT::renderDT({
      DT::datatable(blank_analysis, options = list(
        paging = FALSE,  # Disable pagination
        scrollY = '100%',   # Optional: Add a vertical scroll bar if there are too many rows
        dom = 't'        # Optional: Simplify the DataTable controls (only the table is shown)
      ),
      rownames = FALSE) %>%
        # Apply different color styles based on the values in the result column
        DT::formatStyle(
          'result',
          target = 'cell',
          backgroundColor = DT::styleEqual(
            c('ND', 'D', NA),     # Conditions: 'ND', 'D', and NA/null
            c('red', 'green', 'yellow')  # Colors for each condition
          )
        )
    })

    output$download_blank_analysis <- shiny::downloadHandler(
      filename = function() {
        paste("blank_analysis", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        readr::write_csv(blank_analysis, file)
      }
    )




  })
}

