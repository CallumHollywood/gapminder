#' dashboard_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_details_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    h5('This dashboard has been prepared as a portfolio example.'),
    br(),
    h5('The dashboard has been prepared with R’s Shiny Framework using Golem architecture. The page applies a vanilla ‘navbarPage’ layout.'),
    br(),
    h5('Designed by Callum Hollywood'),
    h5('nexusdatascience.com'),
    h5('contact:'),
    h5('callumhollywood@gmail.com')
  )
}

#' dashboard_details Server Functions
#'
#' @noRd
mod_dashboard_details_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

