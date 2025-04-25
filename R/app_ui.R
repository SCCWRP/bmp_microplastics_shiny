#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bslib
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinyjs::useShinyjs(),
    useBusyIndicators(),
    page_navbar(
      title = "BMP Microplastics (last updated 4/25/25) test",
      theme = bs_theme(preset = "cosmo"),
      navbar_options = bslib::navbar_options(
        bg = bslib::bs_get_variables(bslib::bs_theme(preset = "cosmo"), "primary")
      ),
      nav_panel(title = "Spectroscopy", mod_pie_plot_func_ui("pie_plot_func_1")),
      #nav_panel(title = "Microscopy", mod_plot_func_ui("plot_func_1")),
      shinyjs::disabled(nav_panel(title = "MDA Analysis", mod_blank_analysis_ui("blank_analysis_1"))),
      nav_panel(title = "Download Data", mod_download_section_ui("download_section_1"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$style(
    "[tooltip]:before {left: 110% !important; right: auto !important}",
    ".container-fab {right: auto !important; left: 0 !important;}"
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BMP Microplastics Shiny App"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
