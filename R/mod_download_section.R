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
    downloadButton(ns("download_analysis_ftir"), "Download Spectroscopy Analysis Data"),
    downloadButton(ns("download_analysis_blank"), "Download Blank Analysis Data")
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
        paste0("constants-", as.integer(Sys.time()), ".csv")
      },
      content = function(file) {
        write.csv(raw_data_list$constants, file, row.names = FALSE)
      }
    )

    output$download_rawall <- downloadHandler(
      filename = function() {
        paste0("microscopy-raw-data-", as.integer(Sys.time()), ".csv")
      },
      content = function(file) {
        write.csv(raw_data_list$dat_rawall, file, row.names = FALSE)
      }
    )

    output$download_rawftir <- downloadHandler(
      filename = function() {
        paste0("spectroscopy-raw-data-", as.integer(Sys.time()), ".csv")
      },
      content = function(file) {
        write.csv(raw_data_list$dat_rawftir, file, row.names = FALSE)
      }
    )

    output$download_analysis_ftir <- downloadHandler(
      filename = function() {
        paste0("spectroscopy-analysis-data-", as.integer(Sys.time()), ".xlsx")
      },
      content = function(file) {

        all_spectroscopy_summary <- get_concentration(raw_data_list) %>% select(-note)

        wb <- openxlsx::createWorkbook()

        # Add "All Particles Summary" sheet
        openxlsx::addWorksheet(wb, "Analysis")
        openxlsx::writeData(wb, "Analysis", all_spectroscopy_summary)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    output$download_analysis_blank <- downloadHandler(
      filename = function() {
        paste0("blank-analysis-data-", as.integer(Sys.time()), ".xlsx")
      },
      content = function(file) {
        raw_all_blanks  <- raw_data_list$dat_rawall_blanks
        raw_ftir_blanks <- raw_data_list$dat_rawftir_blanks

        # Define the “base” grouping columns
        base_cols <- c(
          "bmp", "year", "event", "location",
          "matrix", "replicate", "size_fraction"
        )

        # Summarize raw_all_blanks → count_micro
        all_blanks_summary <- raw_all_blanks %>%
          group_by(across(all_of(base_cols))) %>%
          summarise(count_micro = n(), .groups = "drop")

        # Summarize raw_ftir_blanks → count_specstro & percentage_is_mp
        ftir_summary <- raw_ftir_blanks %>%
          group_by(across(all_of(base_cols))) %>%
          summarise(
            count_specstro   = n(),
            percentage_is_mp = round(sum(is_mp == "y") / n() * 100, 2),
            .groups          = "drop"
          )

        # Extract one type_blank per base group
        type_blank_lookup <- raw_ftir_blanks %>%
          select(all_of(c(base_cols, "typeblank"))) %>%
          distinct()

        # Join them: FTIR  ←  All Blanks, then add type_blank
        final_summary <- ftir_summary %>%
          left_join(all_blanks_summary, by = base_cols) %>%
          left_join(type_blank_lookup, by = base_cols) %>%
          replace_na(list(count_micro = 0)) %>%
          arrange(across(all_of(c(base_cols, "typeblank"))))

        # write to Excel
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Blank Analysis")
        openxlsx::writeData(wb, "Blank Analysis", final_summary)
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}


