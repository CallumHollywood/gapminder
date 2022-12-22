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
             div(class = 'colike',
                 align = 'center',
                 style = 'padding: 10px;',
                 br(),
                 fluidRow(
                   column(6,
                          div(class = 'bannerlike',
                              div(class = 'bnr1',
                                  fluidRow(
                                    column(12,
                                           align = 'center',
                                           h6('Life Expectancy by Continent')
                                    )
                                  )
                              ),
                              br(),
                              div(style = 'height: 240px; fontsize; 100px;',
                                  fluidRow(
                                    # column(6,
                                    style = 'height: 100px;',
                                    div(class = 'dv_height',
                                        highchartOutput(ns('ot_intro_chart'), height = '250px')
                                    )
                                    # )
                                  )
                              )
                          )

                   ),
                   column(6,
                          div(class = 'bannerlike',
                              div(class = 'bnr1',
                                  fluidRow(
                                    column(12,
                                           align = 'center',
                                           h6('Population by Continent')
                                    )
                                  )
                              ),
                              br(),
                              div(style = 'height: 240px; fontsize; 100px;',
                                  fluidRow(
                                    # column(6,
                                    style = 'height: 100px;',
                                    div(class = 'dv_height',
                                        highchartOutput(ns('ot_world_pop'), height = '250px')
                                    )
                                    # )
                                  )
                              )
                          )

                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          div(class = 'bannerlike',
                              div(class = 'bnr1',
                                  fluidRow(
                                    column(12,
                                           align = 'center',
                                           h6('Life Expectancy, Top and Bottom 5 Countries')
                                    )
                                  )
                              ),
                              br(),
                              div(style = 'height: 240px; fontsize; 100px;',
                                  fluidRow(
                                    # column(6,
                                    style = 'height: 100px;',
                                    div(class = 'dv_height',
                                        highchartOutput(ns('ot_lifeexp_ranks'), height = '450px', width = '80%')
                                    )
                                    # )
                                  )
                              )
                          )

                   )
                 )

                 # fluidRow(
                 #   column(12,
                 #          highchartOutput(ns('ot_lifeexp_ranks'), height = '250px')
                 #          )
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
             dataLabels = list(enabled = TRUE, format = "{point.name}")
      ) %>%
        hc_yAxis(title = list(text = "Life Expectancy (Years)")) %>%
        hc_xAxis(title = list(text = NULL))

    })



    output$ot_world_pop <- renderHighchart({

      world_pop <- gapminder %>%
        group_by(continent) %>%
        summarise(pop = sum(pop))


      hc <- hchart(
        world_pop,
        "packedbubble",
        hcaes(name = continent, value = pop, group = continent),
        padding = 0
      )

      q95 <- as.numeric(quantile(world_pop$pop, .95))

      hc %>%
        hc_tooltip(
          useHTML = F,
          pointFormat = "<b>{point.name}:</b> {point.value}",
          borderColor = 'black'
        ) %>%
        hc_plotOptions(
          packedbubble = list(
            maxSize = "150%",
            zMin = 0,
            dataLabels = list(
              enabled = F,
              format = "{point.name}",
              filter = list(
                property = "y",
                operator = ">",
                value = q95
              ),
              style = list(
                color = "black",
                textOutline = "none",
                fontWeight = "normal",
                borderColor = 'black'
              )
            )
          )
        )

    })


    output$ot_lifeexp_ranks <- renderHighchart({


      lifeExp <- gapminder %>%
        select(name, lifeExp, year) %>%
        filter(year == 2007) %>%
        mutate(lifeExp = round(lifeExp, 1)) %>%
        arrange(desc(lifeExp))

      lifeExp <- lifeExp %>%
        head(5) %>%
        mutate(rank = 'Top 5 Country') %>%
        bind_rows(lifeExp %>% tail(5) %>% mutate(rank = 'Bottom 5 Country')) %>%
        select(-year)

      hchart(
        lifeExp,
        "bar",
        hcaes(x = name, y = lifeExp, group = rank),
        color = c("#7CB5EC", "#F7A35C"),
        # name = c("Year 1999", "Year 2008"),
        showInLegend = c(TRUE)
      ) %>%
        hc_yAxis(
          title = list(text = "Life Expectancy (Years)", style = list(color = "#ffffff", fontSize = 25)),
          labels = list(style = list(color = "#ffffff", fontSize = 25))
          ) %>%
        hc_xAxis(title = list(text = "Country", style = list(color = "#ffffff", fontSize = 40)),
                 labels = list(style = list(color = "#ffffff", fontSize = 25))
                 ) %>%
        # hc_legend(labels = list(style = list(color = "#ffffff", maxHeight = 100)))
        hc_legend(enabled = FALSE)


    })







  })
}
