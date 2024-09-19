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
    downloadButton(ns("download_analysis_ftir"), "Download Spectroscopy Analysis Data")
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

    output$download_analysis_ftir <- downloadHandler(
      filename = function() {
        paste("spectroscopy-analysis-data-", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {

        # Load data from raw_data_list
        rawftir <- raw_data_list$dat_rawftir
        rawall <- raw_data_list$dat_rawall
        constants <- raw_data_list$constants

        # Filtered for 'is_mp == y' (MP particles)
        filtered_mp <- rawftir %>% filter(is_mp == 'y')

        # Unfiltered for 'is_mp == n' or other (all particles)
        unfiltered <- rawftir

        # Calculate microscopy summary
        microscopy_summary <- rawall %>%
          group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
          summarize(count_micro = n()) %>%
          ungroup()

        # Case 1: Spectroscopy summary for MP particles
        mp_spectroscopy_summary <- filtered_mp %>%
          group_by(bmp, year, event, location, matrix, replicate, size_fraction, is_subsample) %>%
          summarize(count_spectro = n()) %>%
          ungroup()

        # Case 2: Spectroscopy summary for all particles
        all_spectroscopy_summary <- unfiltered %>%
          group_by(bmp, year, event, location, matrix, replicate, size_fraction, is_subsample) %>%
          summarize(count_spectro = n()) %>%
          ungroup()

        # Calculate percentage of MP in each case
        pct_mp_dat <- unfiltered %>%
          group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
          summarise(
            percentage_is_mp = sum(is_mp == "y") / n()
          ) %>%
          ungroup()

        # Join with MP particles summary
        mp_spectroscopy_summary <- mp_spectroscopy_summary %>%
          left_join(pct_mp_dat, by = c("bmp", "year", "event", "location", "matrix", "replicate", "size_fraction")) %>%
          left_join(microscopy_summary, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
          mutate(
            count = case_when(
              is_subsample == 'y' ~ count_micro * percentage_is_mp,
              is_subsample == 'n' ~ count_spectro
            )
          ) %>%
          left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
          mutate(
            back_calculated_particle_count = count / (pct_filter_counted * pct_sample_processed),
            concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing)
          ) %>%
          select(bmp, year, event, location, matrix, replicate, size_fraction,
                 count_spectro, count_micro, is_subsample, sample_volume, sub_sample,
                 pct_sample_processed, pct_filter_counted, unit_passing, unit,
                 percentage_is_mp, back_calculated_particle_count, concentration)

        # Join with all particles summary
        all_spectroscopy_summary <- all_spectroscopy_summary %>%
          left_join(pct_mp_dat, by = c("bmp", "year", "event", "location", "matrix", "replicate", "size_fraction")) %>%
          left_join(microscopy_summary, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
          mutate(
            count = case_when(
              is_subsample == 'y' ~ count_micro,
              is_subsample == 'n' ~ count_spectro
            )
          ) %>%
          left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
          mutate(
            back_calculated_particle_count = count / (pct_filter_counted * pct_sample_processed),
            concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing)
          ) %>%
          select(bmp, year, event, location, matrix, replicate, size_fraction,
                 count_spectro, count_micro, is_subsample, sample_volume, sub_sample,
                 pct_sample_processed, pct_filter_counted, unit_passing, unit,
                 percentage_is_mp, back_calculated_particle_count, concentration)

        # Create a new Excel workbook
        wb <- openxlsx::createWorkbook()

        # Add "All Particles Summary" sheet
        openxlsx::addWorksheet(wb, "All Particles Summary")
        openxlsx::writeData(wb, "All Particles Summary", all_spectroscopy_summary)

        # Add "MP Particles Summary" sheet
        openxlsx::addWorksheet(wb, "MP Particles Summary")
        openxlsx::writeData(wb, "MP Particles Summary", mp_spectroscopy_summary)

        # Save the workbook to the file path
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )



  })
}


