#' interplay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom gargoyle trigger watch on

mod_interplay_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns('tb1'),
      selected = 'country_review',
      tabPanel('Country Review',
               value = 'country_review',
               br(),
               mod_country_review_ui(ns("country_review_1"))
               ),
      tabPanel('Compare',
               value = 'compare',
               br(),
               div(class = 'bannerlike',
                   div(class = 'bnr1',
                       fluidRow(
                         column(12,
                                align = 'center',
                                h2('Country Comparison')
                         )
                       )
                   ),
                   br(),
                   fluidRow(
                     column(4,
                            mod_compare_countries_ui(ns("compare_countries_1"))
                     ),
                     column(4,
                            mod_compare_countries_ui(ns("compare_countries_2"))
                     ),
                     column(4,
                            mod_compare_countries_ui(ns("compare_countries_3"))
                     )
                   )
               )
      )
    )
  )
}

#' interplay Server Functions
#'
#' @noRd
mod_interplay_server <- function(
    id,
    country_rctv,
    pass_around,
    input_tb1
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<< CALL MODULES >>> ####

    mod_country_review_server(
      "country_review_1",
      country_rctv
      )

    mod_compare_countries_server(
      "compare_countries_1",
      gapminder_compare_rctv,
      metric_in = 'lifeExp',
      title = 'Life Expectancy by Countries',
      y_title = "Life Expectancy (Years)"
    )

    mod_compare_countries_server(
      "compare_countries_2",
      gapminder_compare_rctv,
      metric_in = 'pop',
      title = 'Population by Countries',
      y_title = "Population Count"
    )

    mod_compare_countries_server(
      "compare_countries_3",
      gapminder_compare_rctv,
      metric_in = 'gdpPercap',
      title = 'GDP per Capita by Countries',
      y_title = "GDP (USD)"
    )


    #### <<< observeEvent >>> ####

    observeEvent(input_tb1(),{

      if(input_tb1() == 'interplay'){

        updateTabsetPanel(
          session
          , 'tb1'
          , selected = 'country_review'
        )
      }
    })


    observeEvent(input$tb1,{

      pass_around$tb1 <- input$tb1

      trigger('trgr_tb1')

    }, ignoreInit = TRUE)


    #### <<< reactives   >>> ####

    gapminder_compare_rctv <- reactive({

      watch('trgr_country_focus')
      gapminder %>%
        filter(name %in% pass_around$country_focus_rctv$country)
    })

  })
}

