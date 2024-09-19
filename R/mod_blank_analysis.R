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
        shinycssloaders::withSpinner( DT::DTOutput(ns("mda_table")),type = 5 ),
        height = "100%"
      ),
      bslib::card_body(
        class = 'p-0',
        fill=FALSE,
        downloadButton(ns("download_mda_analysis"), "Download MDA Analysis", class = "btn-primary")
      )
    )
}

#' blank_analysis Server Functions
#'
#' @noRd
mod_blank_analysis_server <- function(id, pool, raw_data_list){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mda_analysis <- raw_data_list$mda_analysis

    # Render the DataTable with conditional formatting for the "result" column
    output$mda_table <- DT::renderDT({
      DT::datatable(mda_analysis, options = list(
        paging = FALSE,  # Disable pagination
        scrollY = '100%',   # Optional: Add a vertical scroll bar if there are too many rows
             # Optional: Simplify the DataTable controls (only the table is shown),
        filter = 'top'    # Enable filtering at the top of each column
      ),
      rownames = FALSE) %>%
        # Apply color styling to the 'result_from_sample_count' column
        DT::formatStyle(
          'result_from_sample_count',
          target = 'cell',
          backgroundColor = DT::styleEqual(
            c('ND', 'D', NA),     # Conditions: 'ND', 'D', and NA/null
            c('red', 'green', 'yellow')  # Colors for each condition
          )
        ) %>%
        # Apply color styling to the 'result_from_blank' column
        DT::formatStyle(
          'result_from_blank',
          target = 'cell',
          backgroundColor = DT::styleEqual(
            c('ND', 'D', NA),     # Conditions: 'ND', 'D', and NA/null
            c('red', 'green', 'yellow')  # Colors for each condition
          )
        )
    })


    output$download_mda_analysis <- shiny::downloadHandler(
      filename = function() {
        paste("mda_analysis", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        readr::write_csv(mda_analysis, file)
      }
    )




  })
}

