#' compare_countries UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_compare_countries_ui <- function(id){
  ns <- NS(id)
  tagList(
    highchartOutput(ns('ot_compare_countries')
                    , height = '400px'
                       )
  )
}

#' compare_countries Server Functions
#'
#' @noRd
mod_compare_countries_server <- function(
    id,
    gapminder_compare_rctv,
    metric_in,
    title,
    y_title
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(gapminder_compare_rctv(),{

      req(gapminder_compare_rctv())

      output$ot_compare_countries <- renderHighchart({

        req(gapminder_compare_rctv())

        gapminder_compare_data <- gapminder_compare_rctv()

        gapminder_compare_data <- gapminder_compare_data %>%
          select('name','year', metric_in) %>%
          rename(metric = 3)

        hchart(gapminder_compare_data,
               "line",
               hcaes(x = year, y = round(metric, 1), group = name),
               dataLabels = list(enabled = TRUE, format = "{point.name}")
        ) %>%
          hc_title(
            text = title,
            margin = 20,
            align = "left",
            style = list(color = "#390CF3", useHTML = TRUE)
            ) %>%
          hc_yAxis(
            title = list(text = y_title, style = list(color = "#ffffff")),
            labels = list(style = list(color = "#ffffff"))
          ) %>%
          hc_xAxis(
            title = list(text = NULL), style = list(color = "#ffffff"),
            labels = list(style = list(color = "#ffffff"))
            ) %>%
          hc_legend(enabled = FALSE)

      })

    })

  })
}


