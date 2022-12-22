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
    h5('The dashboard has been prepared with R’s Shiny Framework using Golem architecture.
       The page applies a vanilla ‘navbarPage’ layout uses reactable and higherchart visualisation technologies.'),
    br(),
    h5('Designed by Callum Hollywood'),
    h5('nexusdatascience.com'),
    br(),
    h5('Contact:'),
    h5('callumhollywood@gmail.com'),
    br(),
    h5('Check me out on Github'),
    h5('https://github.com/CallumHollywood'),
    br(),
    h5('The app code can be viewed at'),
    h5('https://github.com/CallumHollywood/gapminder/tree/master')
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

