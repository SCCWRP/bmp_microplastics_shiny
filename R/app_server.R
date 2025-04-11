#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  ggplot2::theme_set(ggplot2::theme_bw(base_size = 24))
  thematic::thematic_shiny(font = "auto")

  # Set up DB pool
  pool <- pool::dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = Sys.getenv("dbname"),
    host = Sys.getenv("host"),
    user = Sys.getenv("user"),
    password = Sys.getenv("password")
  )

  update <- FALSE

  # Date string for filename
  today <- format(Sys.Date(), "%Y-%m-%d")
  filename <- paste0("data-raw/bmp_data_raw_", today, ".Rdata")

  if (update) {
    excluded_cols <- c('login_email', 'created_date', 'submissionid', 'warnings', 'last_edited_date', 'globalid', 'created_user', 'last_edited_user')

    # Raw tables
    dat_rawall <- pool::dbGetQuery(pool, "SELECT * FROM tbl_bmp_particle_raw_all ORDER BY bmp, year, event, location, matrix, size_fraction, replicate, sampleid, particleid")
    dat_rawftir <- pool::dbGetQuery(pool, "SELECT * FROM vw_bmp_raw_ftir ORDER BY bmp, year, event, location, matrix, size_fraction, replicate, sampleid, particleid")
    constants <- pool::dbGetQuery(pool, "SELECT * FROM bmp_constants ORDER BY bmp, year, event, location, matrix, size_fraction, replicate")

    # Analysis tables
    dat_summaryall <- pool::dbGetQuery(pool, "SELECT * FROM vw_bmp_summary_microscopy")
    mda_analysis <- pool::dbGetQuery(
      pool,
      glue("
      SELECT
          samplename,
          microplastic_particle_count,
          particle_count_mpb,
          particle_count_eb,
          particle_count_fb,
          mda,
          CASE
              WHEN mda IS NULL THEN NULL
              WHEN microplastic_particle_count <= mda THEN 'ND'
              ELSE 'D'
          END AS result_from_sample_count,
          CASE
              WHEN mda IS NULL THEN NULL
              WHEN (particle_count_eb IS NULL AND particle_count_fb IS NULL) THEN NULL
              WHEN COALESCE(particle_count_eb, particle_count_fb) <= mda THEN 'ND'
              ELSE 'D'
          END AS result_from_blank
      FROM
          vw_bmp_mda_analysis;
    ")
    )

    # Remove excluded columns
    dat_rawall <- dat_rawall %>% select(-intersect(names(dat_rawall), excluded_cols))
    dat_rawftir <- dat_rawftir %>% select(-intersect(names(dat_rawftir), excluded_cols))
    constants <- constants %>% select(-intersect(names(constants), excluded_cols))

    # Save
    save(
      dat_rawall,
      dat_rawftir,
      dat_summaryall,
      constants,
      mda_analysis,
      file = filename
    )

    raw_data_list <- list(
      dat_rawall = dat_rawall,
      dat_rawftir = dat_rawftir,
      dat_summaryall = dat_summaryall,
      constants = constants,
      mda_analysis = mda_analysis
    )
  } else {
    # Load the most recent .Rdata file
    files <- list.files("data-raw", pattern = "^bmp_data_raw_\\d{4}-\\d{2}-\\d{2}\\.Rdata$", full.names = TRUE)
    if (length(files) == 0) stop("No saved .Rdata files found in data-raw/")

    latest_file <- files[which.max(file.mtime(files))]
    load(latest_file)

    raw_data_list <- list(
      dat_rawall = dat_rawall,
      dat_rawftir = dat_rawftir,
      dat_summaryall = dat_summaryall,
      constants = constants,
      mda_analysis = mda_analysis
    )
  }
  # Ensure the connection is closed when the app stops
  onStop(function() {
    pool::poolClose(pool)
  })

  mod_plot_func_server("plot_func_1", pool, raw_data_list)
  mod_pie_plot_func_server("pie_plot_func_1", pool, raw_data_list)
  mod_download_section_server("download_section_1", pool, raw_data_list)
  mod_blank_analysis_server("blank_analysis_1", pool, raw_data_list)

}
