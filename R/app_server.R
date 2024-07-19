#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  pool <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = Sys.getenv("dbname"),
    host = Sys.getenv("host"),
    user = Sys.getenv("user"),
    password = Sys.getenv("password")
  )

  # Ensure the connection is closed when the app stops
  onStop(function() {
    pool::poolClose(pool)
  })

  mod_plot_func_server("plot_func_1", pool)


}
