#' country_review UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_country_review_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = 'bannerlike',
        div(class = 'bnr1',
            fluidRow(
              column(12,
                     align = 'center',
                     h2('Metric Analysis')
              )
            )
        ),
        br(),
        fluidRow(
          column(4,
                 mod_xy_and_size_ui(ns("xy_and_size_1"))
          ),
          column(4,
                 mod_xy_and_size_ui(ns("xy_and_size_2"))
          ),
          column(4,
                 mod_xy_and_size_ui(ns("xy_and_size_3"))
          )
        )
    )


    # fluidRow(
    #   column(4,
    #          mod_xy_and_size_ui(ns("xy_and_size_1"))
    #           ),
    #   column(4,
    #          mod_xy_and_size_ui(ns("xy_and_size_2"))
    #   ),
    #   column(4,
    #          mod_xy_and_size_ui(ns("xy_and_size_3"))
    #   )
    #
    # )
  )
}

#' country_review Server Functions
#'
#' @noRd
mod_country_review_server <- function(
    id,
    country_rctv
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<< CALL MODULES >>> ####
    mod_xy_and_size_server(
      "xy_and_size_1",
      title_in    = 'Population v Life Expectancy',
      subtitle_in = 'and GDP per Capita',
      x_title_in  = 'Population Count',
      y_title_in  = 'Life Expectancy (Years)',
      z_title_in  = 'GDP per Capita',
      country_rctv = country_rctv,
      x_in = 'pop',
      y_in = 'lifeExp',
      z_in = 'gdpPercap'
      )

    mod_xy_and_size_server(
      "xy_and_size_2",
      title_in    = 'Population v GDP per Capita',
      subtitle_in = 'and Life Expectancy',
      x_title_in  = 'Population Count',
      y_title_in  = 'GDP per Capita (USD)',
      z_title_in  = 'Life Exp',
      country_rctv = country_rctv,
      x_in = 'pop',
      y_in = 'gdpPercap',
      z_in = 'lifeExp'
    )

    mod_xy_and_size_server(
      "xy_and_size_3",
      title_in    = 'GDP per Capita v Life Expectancy',
      subtitle_in = 'and Population',
      x_title_in  = 'GDP per Capita',
      y_title_in  = 'Life Expectancy (Years)',
      z_title_in  = 'Population',
      country_rctv = country_rctv,
      x_in = 'gdpPercap',
      y_in = 'lifeExp',
      z_in = 'pop'
    )


  })
}
