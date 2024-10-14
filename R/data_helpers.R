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
    bmpselect,
    yearselect,
    sizefractionselect,
    replicateselect,
    eventselect,
    pie_type,
    is_mp = FALSE
  ){


  if ("typeblank" %in% colnames(dat)) {
    dat <- dat %>% filter(typeblank == 'non-blank')
  }

  # FILTERING
  filtered_dat <- dat %>% filter(
    bmp == bmpselect &
    year == yearselect &
    replicate == replicateselect &
    size_fraction %in% sizefractionselect &
    event == eventselect
  )

  if (is_mp){
    filtered_dat <- filtered_dat %>% filter(is_mp == 'y')
  }

  plot_dat <- filtered_dat %>%
    group_by(bmp, year, event,  location, matrix, replicate, !!sym(pie_type)) %>%
    summarise(count = n()) %>%
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
    bmpselect,
    yearselect,
    sizefractionselect,
    replicateselect,
    is_mp = FALSE,
    spectroscopy = FALSE
  ){

  if (spectroscopy){
    dat <- raw_data_list$dat_rawftir %>% filter(typeblank == 'non-blank')
  } else {
    dat <- raw_data_list$dat_rawall
  }

  constants <- raw_data_list$constants

  filtered_dat <- dat %>% filter(
    bmp == bmpselect &
    year == yearselect &
    replicate == replicateselect &
    size_fraction %in% sizefractionselect
  )

  pct_mp_dat <- filtered_dat %>%
    group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
    summarise(
      percentage_is_mp = sum(is_mp == "y") / n()
    ) %>%
    ungroup()

  if (is_mp){
    filtered_dat <- filtered_dat %>% filter(is_mp == 'y')
  }

  if (spectroscopy){

    raw_all <- raw_data_list$dat_rawall

    microscopy_summary <- raw_all %>%
      group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
      summarize(count_micro = n()) %>%
      ungroup()

    spectroscopy_summary <- filtered_dat %>%
      group_by(bmp, year, event, location, matrix, replicate, size_fraction, is_subsample) %>%
      summarize(count_spectro = n()) %>%
      ungroup()

    spectroscopy_summary <- spectroscopy_summary %>%
      left_join(pct_mp_dat
        ,
        by = c("bmp", "year", "event", "location", "matrix", "replicate", "size_fraction")
      )
    if (is_mp){
      concentration_dat <- spectroscopy_summary %>%
        left_join(
          microscopy_summary,
          by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")
        ) %>%
        mutate(
          count = case_when(
            is_subsample == 'y' ~ count_micro * percentage_is_mp,
            is_subsample == 'n' ~ count_spectro,
          )
        )
    } else {
      concentration_dat <- spectroscopy_summary %>%
        left_join(
          microscopy_summary,
          by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")
        ) %>%
        mutate(
          count = case_when(
            is_subsample == 'y' ~ count_micro,
            is_subsample == 'n' ~ count_spectro,
          )
        )

    }

    concentration_dat <- concentration_dat %>%
      left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
      mutate(
        back_calculated_particle_count = count / (pct_filter_counted * pct_sample_processed),
        concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing)
      ) %>%
      select(bmp, year, event, location, matrix, replicate, size_fraction,
             count_spectro, count_micro, is_subsample, sample_volume, sub_sample,
             pct_sample_processed, pct_filter_counted, unit_passing, unit,
             percentage_is_mp, back_calculated_particle_count, concentration)

  } else {

    concentration_dat <- filtered_dat %>%
      group_by(bmp, year, event, location, matrix, replicate, size_fraction) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
      arrange(bmp, year, event, location, matrix, replicate, size_fraction) %>%
      mutate(concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing))

  }

  plot_dat <- concentration_dat %>%
    filter(!is.na(concentration) & is.finite(concentration)) %>%
    group_by(location, event) %>%
    summarize(total_concentration = sum(concentration)) %>%
    ungroup()

  list(
    plot_dat = plot_dat,
    concentration_dat = concentration_dat
  )

}
