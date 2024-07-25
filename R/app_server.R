#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {


  credentials <- data.frame(
    user = c("sccwrp"),
    password = c(Sys.getenv("apppw"))
  )


  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(credentials)
  )


  pool <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = Sys.getenv("dbname"),
    host = Sys.getenv("host"),
    user = Sys.getenv("user"),
    password = Sys.getenv("password")
  )

  excluded_cols <- c('login_email', 'created_date', 'submissionid', 'warnings', 'last_edited_date', 'globalid', 'created_user', 'last_edited_user')

  dat_rawall <- pool::dbGetQuery(pool, "SELECT * FROM tbl_bmp_particle_raw_all WHERE location != 'not applicable' ORDER BY bmp, year, event, location, matrix, size_fraction, replicate, sampleid, particleid" )
  dat_rawftir <- pool::dbGetQuery(pool, "SELECT * FROM tbl_bmp_particle_raw_ftir WHERE location != 'not applicable' ORDER BY bmp, year, event, location, matrix, size_fraction, replicate, sampleid, particleid")
  dat_summaryall <- pool::dbGetQuery(pool, "SELECT * FROM vw_bmp_summary_microscopy")
  constants <- pool::dbGetQuery(pool, "SELECT * FROM bmp_constants ORDER BY bmp, year, event, location, matrix, size_fraction, replicate")
  print(dat_summaryall)

  # Exclude columns that are actually present in the constants data frame
  dat_rawall <- dat_rawall %>% select(-intersect(names(dat_rawall), excluded_cols))
  dat_rawftir <- dat_rawftir %>% select(-intersect(names(dat_rawftir), excluded_cols))
  constants <- constants %>% select(-intersect(names(constants), excluded_cols))

  raw_data_list <- list(
    dat_rawall = dat_rawall,
    dat_rawftir = dat_rawftir,
    constants = constants,
    dat_summaryall = dat_summaryall
  )

  # Ensure the connection is closed when the app stops
  onStop(function() {
    pool::poolClose(pool)
  })

  mod_plot_func_server("plot_func_1", pool, raw_data_list)


}
