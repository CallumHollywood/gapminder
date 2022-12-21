#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter


mod_about_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             div(class = 'colike',
                 align = 'center',
                 br(),
                 h3('GAPMINDER'),
                 br(),
                 h5('The gapminder dataset is drawn from the R gapminder package.'),
                 br(),
                 a("GAPMINDER", href="https://github.com/jennybc/gapminder", target = '_blank'),
                 br(),
                 h5('The data provides a time series review of ‘Life Expectancy’, ‘Population Size’ and ‘GDP per Capita’ every five years between 1952 and 2007, for 142 countries.'),
                 br(),
                 h5('This dashboard serves to provide an interactive review of the data.'),
                 br(),
                 h5('Visitors are encouraged to consider ...'),
                 tags$ol(
                   tags$li("‘Life Expectancy’, ‘Population Size’ and ‘GDP per Capita’ for individual countries across time"),
                   tags$li("‘Life Expectancy’, ‘Population Size’ and ‘GDP per Capita’ between continents and between countries"),
                   tags$li("The interplay between ‘Life Expectancy’, ‘Population Size’ and ‘GDP per Capita’")
                 )
             )

      ),
      column(6,
             # style = 'margin-top: 22px;',
                 # offset = 2,
             div(class = 'colike',
                 # column(10,
                 align = 'center',
                 br(),
                 h3('Life Expectancy by Continent'),
                 br(),
                 highchartOutput(ns('ot_intro_chart'), height = '450px')
                 # )
             )
      )
    )
  )
}




#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ot_intro_chart <- renderHighchart({

      gap_contiment <- gapminder %>%
        group_by(continent, year) %>%
        summarise(lifeExp = mean(lifeExp)) %>%
        ungroup()

      hchart(gap_contiment,
             "line",
             hcaes(x = year, y = round(lifeExp, 1), group = continent),
             # name = "Continent",
             dataLabels = list(enabled = TRUE, format = "{point.name}")
      ) %>%
        # hc_title(
        #   text = 'Life Expectancy by Continent',
        #   margin = 20,
        #   align = "center",
        #   style = list(color = "#000000", fontSize = '30px', useHTML = TRUE)
        # ) %>%
        hc_yAxis(title = list(text = "Life Expectancy (Years)")) %>%
        hc_xAxis(title = list(text = NULL))

    })


  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
