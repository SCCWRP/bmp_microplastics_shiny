#' Get BMP Options
#'
#' Retrieves distinct BMP options from the specified table.
#'
#' @param pool A database connection pool object.
#' @param tablename A string representing the name of the table to query.
#'
#' @return A vector of distinct BMP options.
#' @noRd
get_bmp_options <- function(pool, tablename){
  qry <- glue::glue("SELECT DISTINCT bmp from {tablename} ORDER BY bmp")
  pool::dbGetQuery(pool, qry)$bmp
}


#' Get Year Options
#'
#' Retrieves distinct year options based on the selected BMP from the specified table.
#'
#' @param pool A database connection pool object.
#' @param tablename A string representing the name of the table to query.
#' @param bmpselect A string representing the selected BMP.
#'
#' @return A vector of distinct year options.
#' @noRd
get_year_options <- function(pool, tablename, bmpselect){
  qry <- glue::glue("SELECT DISTINCT year FROM {tablename} WHERE bmp = '{bmpselect}' ORDER BY year")
  pool::dbGetQuery(pool, qry)$year
}


#' Get Size Fraction Options
#'
#' Retrieves distinct size fraction options based on the selected BMP and year from the specified table.
#'
#' @param pool A database connection pool object.
#' @param tablename A string representing the name of the table to query.
#' @param bmpselect A string representing the selected BMP.
#' @param yearselect A string representing the selected year.
#'
#' @return A vector of distinct size fraction options.
#' @noRd
get_sizefraction_options <- function(pool, tablename, bmpselect, yearselect){
  qry <- glue::glue("SELECT DISTINCT size_fraction FROM {tablename} WHERE bmp = '{bmpselect}' AND year = '{yearselect}' ORDER BY size_fraction")
  pool::dbGetQuery(pool, qry)$size_fraction
}


#' Get Replicate Options
#'
#' Retrieves distinct replicate options based on the selected BMP, year, and size fraction from the specified table.
#'
#' @param pool A database connection pool object.
#' @param tablename A string representing the name of the table to query.
#' @param bmpselect A string representing the selected BMP.
#' @param yearselect A string representing the selected year.
#' @param sizefractionselect A string representing the selected size fraction.
#'
#' @return A vector of distinct replicate options.
#' @noRd
get_replicate_options <- function(pool, tablename, bmpselect, yearselect, sizefractionselect){
  qry <- glue::glue("SELECT DISTINCT replicate FROM {tablename}
        WHERE bmp = '{bmpselect}'
        AND year = '{yearselect}'
        AND size_fraction = '{sizefractionselect}'
        ORDER BY replicate")
  pool::dbGetQuery(pool, qry)$replicate
}

