get_pieplot_data <- function(
    dat,
    constants,
    bmpselect,
    yearselect,
    sizefractionselect,
    replicateselect,
    pie_type,
    is_mp = FALSE
  ){

  # FILTERING
  filtered_dat <- dat %>% filter(
    bmp == bmpselect &
    year == yearselect &
    size_fraction == sizefractionselect &
    replicate == replicateselect
  )

  if (is_mp){
    filtered_dat <- filtered_dat %>% filter(is_mp == 'y')
  }

  plot_dat <- filtered_dat %>%
    group_by(bmp, year, event,  location, matrix, size_fraction, replicate, !!sym(pie_type)) %>%
    summarise(count = n()) %>%
    mutate(percentage = (count / sum(count)) * 100) %>%
    ungroup()

  plot_dat

}



get_concentrationplot_data <- function(
    dat,
    constants,
    bmpselect,
    yearselect,
    sizefractionselect,
    replicateselect,
    is_mp = FALSE
  ){

  # FILTERING
  filtered_dat <- dat %>% filter(
    bmp == bmpselect &
    year == yearselect &
    size_fraction == sizefractionselect &
    replicate == replicateselect
  )

  if (is_mp){
    filtered_dat <- filtered_dat %>% filter(is_mp == 'y')
  }

  # Left join with constants
  concentration_dat <- filtered_dat %>%
    group_by(bmp, year, event, location, matrix, size_fraction, replicate) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    left_join(constants, by = c("bmp", "year", "event", "location", "matrix", "size_fraction", "replicate")) %>%
    arrange(bmp, year, event, location, matrix, size_fraction, replicate) %>%
    mutate(concentration = (count / pct_filter_counted) / (pct_sample_processed * unit_passing))

  plot_dat <- concentration_dat %>%
    filter(!is.na(concentration) & is.finite(concentration)) %>%
    group_by(location, event) %>%
    summarize(total_concentration = sum(concentration)) %>%
    ungroup()

  plot_dat

}
