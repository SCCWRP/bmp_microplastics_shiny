get_bmp_options <- function(dat){
  # Return unique values of 'bmp' from 'dat' ordered by 'bmp'
  return(sort(unique(dat$bmp)))
}



get_year_options <- function(dat, bmpselect){
  # Filter the data based on the selected 'bmp'
  filtered_data <- dat[dat$bmp == bmpselect, ]

  # Return the unique years from the filtered data
  return(sort(unique(filtered_data$year)))
}


get_replicate_options <- function(dat, bmpselect, yearselect) {
  # Filter the data based on the selected 'bmp', 'year', and 'sizefraction'
  filtered_data <- dat[dat$bmp == bmpselect & dat$year == yearselect, ]

  # Return the unique replicates from the filtered data
  return(sort(unique(filtered_data$replicate)))
}

get_sizefraction_options <- function(dat, bmpselect, yearselect, replicateselect) {
  # Filter the data based on the selected 'bmp' and 'year'
  filtered_data <- dat[dat$bmp == bmpselect & dat$year == yearselect & dat$replicate == replicateselect, ]

  # Return the unique size fractions from the filtered data

  return(sort(unique(filtered_data$size_fraction)))
}



get_event_options <- function(dat, bmpselect, yearselect, sizefractionselect, replicateselect) {
  # Filter the data based on the selected 'bmp', 'year', 'sizefraction', and 'replicate'
  filtered_data <- dat[dat$bmp == bmpselect &
                       dat$year == yearselect &
                       dat$replicate == replicateselect &
                       dat$size_fraction %in% sizefractionselect, ]

  # Return the unique events from the filtered data
  return(sort(unique(filtered_data$event)))
}



