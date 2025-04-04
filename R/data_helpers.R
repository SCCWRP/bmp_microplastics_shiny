#' Get Pie Plot Data
#'
#' Processes data to generate pie plot data based on selected BMP, year, size fraction, replicate, and pie chart type. Filters the data and calculates the percentage for each category.
#'
#' @param dat A data frame containing the raw data.
#' @param constants A data frame containing constants used for processing.
#' @param bmpselect A string representing the selected BMP.
#' @param yearselect A string representing the selected year.
#' @param sizefractionselect A string representing the selected size fraction.
#' @param replicateselect A string representing the selected replicate.
#' @param pie_type A string representing the type of pie chart to generate.
#' @param is_mp A boolean indicating if the data pertains to microplastics (default is FALSE).
#'
#' @return A data frame containing the filtered and processed data with count and percentage for each category.
#' @noRd
get_pieplot_data <- function(
    dat,
    constants,
    matrixselect,         # New parameter
    bmpselect,
    yearselect,
    sizefractionselect,
    replicateselect,
    pie_type,
    is_mp = FALSE
){
  if ("typeblank" %in% colnames(dat)) {
    dat <- dat %>% filter(typeblank == 'non-blank')
  }

  # Add matrix filtering along with the other filters
  filtered_dat <- dat %>% filter(
    matrix == matrixselect &      # New filtering condition
      bmp == bmpselect &
      year == yearselect &
      replicate == replicateselect &
      size_fraction %in% sizefractionselect
  )

  if (is_mp){
    filtered_dat <- filtered_dat %>% filter(is_mp == 'y')
  }

  plot_dat <- filtered_dat %>%
    group_by(bmp, year, matrix, replicate, event, location, !!sym(pie_type)) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(bmp, year, matrix, replicate, event, location) %>%
    mutate(percentage = (count / sum(count)) * 100) %>%
    ungroup()

  plot_dat
}


#' Get Concentration Plot Data
#'
#' Processes data to generate concentration plot data based on selected BMP, year, size fraction, and replicate. Filters the data, performs a left join with constants, calculates the concentration, and summarizes the total concentration.
#'
#' @param dat A data frame containing the raw data.
#' @param constants A data frame containing constants used for processing.
#' @param bmpselect A string representing the selected BMP.
#' @param yearselect A string representing the selected year.
#' @param sizefractionselect A string representing the selected size fraction.
#' @param replicateselect A string representing the selected replicate.
#' @param is_mp A boolean indicating if the data pertains to microplastics (default is FALSE).
#'
#' @return A data frame containing the summarized total concentration for each location and event.
#' @noRd
get_concentrationplot_data <- function(
    raw_data_list,
    matrixselect,         # New parameter
    bmpselect,
    yearselect,
    sizefractionselect,
    replicateselect
){
  concentration_dat <- get_concentration(raw_data_list) %>% filter(
    matrix == matrixselect &     # New filtering condition
      bmp == bmpselect &
      year == yearselect &
      replicate == replicateselect &
      size_fraction %in% sizefractionselect
  ) %>%
    select(bmp, year, event, location, matrix, replicate, size_fraction,
           count_spectro, count_micro, is_subsample, sample_volume, sub_sample,
           pct_sample_processed, pct_filter_counted, unit_passing, unit,
           percentage_is_mp, back_calculated_particle_count, concentration)

  plot_dat <- concentration_dat %>%
    filter(!is.na(concentration) & is.finite(concentration)) %>%
    group_by(location, event) %>%
    summarize(total_concentration = sum(concentration)) %>%
    ungroup()

  # if (spectroscopy){
  #   dat <- raw_data_list$dat_rawftir %>% filter(typeblank == 'non-blank')
  # } else {
  #   dat <- raw_data_list$dat_rawall %>% filter(typeblank == 'non-blank')
  # }


#
#   constants <- raw_data_list$constants
#
#   # Filter with the new matrix selection included
#   filtered_dat <- dat %>% filter(
#     matrix == matrixselect &     # New filtering condition
#       bmp == bmpselect &
#       year == yearselect &
#       replicate == replicateselect &
#       size_fraction %in% sizefractionselect
#   )
#
#   pct_mp_dat <- filtered_dat %>%
#     group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
#     summarise(
#       total = n(),
#       percentage_is_mp = sum(is_mp == "y") / n()
#     ) %>%
#     ungroup()
#
#   if (is_mp){
#     filtered_dat <- filtered_dat %>% filter(is_mp == 'y')
#   }
#
#   if (spectroscopy){
#
#     raw_all <- raw_data_list$dat_rawall %>% filter(typeblank == 'non-blank')
#
#     microscopy_summary <- raw_all %>%
#       group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
#       summarize(count_micro = n()) %>%
#       ungroup()
#
#     spectroscopy_summary <- filtered_dat %>%
#       group_by(bmp, year, event, location, matrix, replicate, size_fraction, is_subsample) %>%
#       summarize(count_spectro = n()) %>%
#       ungroup()
#
#     spectroscopy_summary <- spectroscopy_summary %>%
#       left_join(pct_mp_dat,
#                 by = c("bmp", "year", "event", "location", "matrix", "replicate", "size_fraction")
#       )
#     if (is_mp){
#       concentration_dat <- spectroscopy_summary %>%
#         left_join(
#           microscopy_summary,
#           by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")
#         ) %>%
#         mutate(
#           count = case_when(
#             is_subsample == 'y' ~ count_micro * percentage_is_mp,
#             is_subsample == 'n' ~ count_spectro
#           )
#         )
#     } else {
#       concentration_dat <- spectroscopy_summary %>%
#         left_join(
#           microscopy_summary,
#           by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")
#         ) %>%
#         mutate(
#           count = case_when(
#             is_subsample == 'y' ~ count_micro,
#             is_subsample == 'n' ~ count_spectro
#           )
#         )
#     }
#
#     concentration_dat <- concentration_dat %>%
#       left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
#       mutate(
#         back_calculated_particle_count = count / (pct_filter_counted * pct_sample_processed),
#         concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing)
#       ) %>%
#       select(bmp, year, event, location, matrix, replicate, size_fraction,
#              count_spectro, count_micro, is_subsample, sample_volume, sub_sample,
#              pct_sample_processed, pct_filter_counted, unit_passing, unit,
#              percentage_is_mp, back_calculated_particle_count, concentration)
#
#   } else {
#
#     concentration_dat <- filtered_dat %>%
#       group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
#       summarize(count = n()) %>%
#       ungroup() %>%
#       left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
#       arrange(bmp, year, event, location, matrix, replicate, size_fraction) %>%
#       mutate(concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing))
#   }
#
#   plot_dat <- concentration_dat %>%
#     filter(!is.na(concentration) & is.finite(concentration)) %>%
#     group_by(location, event) %>%
#     summarize(total_concentration = sum(concentration)) %>%
#     ungroup()

  list(
    plot_dat = plot_dat,
    concentration_dat = concentration_dat
  )
}



calculate_dat_summaryall <- function(dat_rawall, constants) {
  dat_rawall %>%
    left_join(constants, by = c("bmp" = "bmp",
                                "year" = "year",
                                "event" = "event",
                                "location" = "location",
                                "matrix" = "matrix",
                                "size_fraction" = "size_fraction",
                                "replicate" = "replicate")) %>%
    group_by(bmp, year, event, location, matrix, size_fraction, replicate) %>%
    summarize(count = n(),
              sample_volume = first(sample_volume),
              sub_sample = first(sub_sample),
              pct_filter_counted = first(pct_filter_counted),
              pct_sample_processed = first(pct_sample_processed),
              unit_passing = first(unit_passing),
              concentration = count / (pct_filter_counted / 100) /
                ((pct_sample_processed / 100) * unit_passing),
              .groups = "drop") %>%
    arrange(bmp, year, event, location, matrix, size_fraction, replicate)
}


calculate_mda_analysis <- function(dat_rawftir) {
  # Step 1: Create mpb_counts
  mpb_counts <- dat_rawftir %>%
    dplyr::filter(typeblank == "MPB", predetermined_mp_yesno == "y", hqi_exceed_sixty_yesno == "y") %>%
    dplyr::group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
    dplyr::summarize(
      samplename = paste(bmp, "Y", year, "E", event, location, matrix, "R", replicate, size_fraction, sep = "_"),
      particle_count_mpb = dplyr::n(),
      mda = (dplyr::n() + 3) + 4.65 * sqrt(dplyr::n()),
      .groups = "drop"
    )

  # Step 2: Create crosstab_data for counts of `typeblank` values
  crosstab_data <- dat_rawftir %>%
    dplyr::filter(typeblank != "MPB" & typeblank != "non-blank", predetermined_mp_yesno == "y", hqi_exceed_sixty_yesno == "y") %>%
    dplyr::group_by(bmp, year, event, location, matrix, replicate, size_fraction, typeblank) %>%
    dplyr::summarize(particle_count = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(samplename = paste(bmp, "Y", year, "E", event, location, matrix, "R", replicate, size_fraction, sep = "_")) %>%
    tidyr::pivot_wider(names_from = typeblank, values_from = particle_count, values_fill = 0) %>%
    dplyr::rename(count_eb = EB, count_fb = FB)

  # Step 3: Combine mpb_counts and crosstab_data with conditional results
  result <- dplyr::full_join(mpb_counts, crosstab_data, by = "samplename") %>%
    dplyr::mutate(
      bmp = stringr::str_extract(samplename, "^([^_]+)"),
      year = stringr::str_extract(samplename, "Y([0-9]+)") %>% as.integer(),
      event = stringr::str_extract(samplename, "E([0-9]+)") %>% as.integer(),
      location = stringr::str_extract(samplename, "E[0-9]+_([^_]+)") %>% as.character(),
      matrix = stringr::str_extract(samplename, "([^_]+)_R") %>% as.character(),
      replicate = stringr::str_extract(samplename, "_R([0-9]+)") %>% as.integer(),
      size_fraction = stringr::str_extract(samplename, "_([0-9]+)$") %>% as.integer(),
      result = dplyr::case_when(
        !is.na(mda) & !is.na(count_eb) & count_eb >= mda ~ "D",
        !is.na(mda) & !is.na(count_fb) & count_fb >= mda ~ "D",
        !is.na(mda) ~ "ND",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(samplename, bmp, year, event, location, matrix, replicate, size_fraction, particle_count_mpb, count_eb, count_fb, mda, result) %>%
    dplyr::arrange(samplename)

  return(result)
}

get_concentration <- function(raw_data_list){
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
    )

  all_spectroscopy_summary

}



