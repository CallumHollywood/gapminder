#' metrics_by_year UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metrics_by_year_ui <- function(id){
  ns <- NS(id)
  tagList(
    highchartOutput(ns('ot_country_metric'), height = '250px')
  )
}

#' metrics_by_year Server Functions
#'
#' @noRd
mod_metrics_by_year_server <- function(
    id,
    metric,
    country_rctv,
    plottitle,
    ylab
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    fn_hchart <- function(metric, country, plottitle, ylab){

      fn_data <- gapminder %>%
        filter(name == country) %>%
        select('year', metric) %>%
        rename(metric_col = 2)

      hchart(fn_data, "line",
             hcaes(x = year, y = metric_col),
             name = plottitle,
             color = '#390CF3'
      ) %>%
        hc_title(
          text = plottitle,
          margin = 20,
          align = "left",
          style = list(color = "#390CF3", useHTML = TRUE)
        ) %>%
        hc_yAxis(
          title = list(text = ylab, style = list(color = "#ffffff")),
          labels = list(style = list(color = "#ffffff"))
        ) %>%
        hc_xAxis(
          title = list(text = NULL),
          labels = list(style = list(color = "#ffffff"))
        ) %>%
        hc_plotOptions(
          series = list(
            showInLegend = FALSE,
            pointFormat = "{point.y}",
            colorByPoint = FALSE
          )
        )

    }


    output$ot_country_metric <- renderHighchart({

      req(country_rctv())

      fn_hchart(metric, isolate(country_rctv()), plottitle, ylab)

    })

  })
}
