#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @importFrom magrittr %>%
#' @import sass
#' @noRd

# sass::sass(sass::sass_file("inst/app/www/custom.sass"))

theme_test <- bs_theme(
  version = 5,
  bg = '#390CF3',
  fg = '#c6f30c',
  primary = "#9bf10e",
  secondary = '#d12ea5',
  base_font = font_google('Prompt'),
  heading_font = font_google('Proza Libre')
  )


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      "Gapminder Analysis",
      id = 'nvpage',
      selected = 'Country Profile',
      theme = theme_test,
      tabPanel(
        "About",
        mod_about_ui("about_1")
      ),
      tabPanel(
        "Country Profile",
        mod_country_ui("country_1")
      ),
      navbarMenu(
        "More",
        tabPanel("Summary"),
        "----",
        "Section header",
        tabPanel("Table")
      )
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

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gapminder"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}
