#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom gargoyle init
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  #### <<< GARGOYLE INIT >>> ####
  init('trgr_tb1')
  init('trgr_country_focus')


  #### <<< environments >>> ####
  pass_around <- environment()


  #### <<< CALL MODULES >>> ####
  mod_country_server("country_1", pass_around)
  mod_about_server("about_1")
  mod_dashboard_details_server("dashboard_details_1")

}
