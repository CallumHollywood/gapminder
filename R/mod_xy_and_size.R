#' xy_and_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_xy_and_size_ui <- function(id){
  ns <- NS(id)
  tagList(
      highchartOutput(ns('ot_xy_and_size'), height = '400px')
  )
}

#' xy_and_size Server Functions
#'
#' @noRd
mod_xy_and_size_server <- function(
    id,
    title_in,
    subtitle_in,
    x_title_in,
    y_title_in,
    z_title_in,
    country_rctv,
    x_in,
    y_in,
    z_in
    ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ot_xy_and_size <- renderHighchart({

      req(country_rctv())

      gapminder_review <- gapminder %>%
        filter(name == country_rctv()) %>%
        rename(
          x = x_in,
          y = y_in,
          z = z_in
        )

      hchart(gapminder_review,
             "line",
             hcaes(x = x, y = y)
      ) %>%
        hc_add_series(
          gapminder_review,
          type = "point",
          name = "Population",
          hcaes(x = x, y = y, size = round(z, 0)),
          dataLabels = list(enabled = TRUE, format = paste0(z_title_in, "<br>{point.z}"))
        ) %>%
        hc_tooltip(enabled = F) %>%
        hc_title(
          text = title_in,
          margin = 20,
          align = "left",
          style = list(color = "#390CF3", useHTML = TRUE)
        ) %>%
        hc_subtitle(
          text = subtitle_in,
          margin = 20,
          align = "left",
          style = list(color = "#390CF3", useHTML = TRUE)
          ) %>%
        hc_xAxis(
          title = list(text = x_title_in, style = list(color = "#ffffff")),
          labels = list(style = list(color = "#ffffff"))
          ) %>%
        hc_yAxis(title = list(text = y_title_in, style = list(color = "#ffffff")),
                 labels = list(style = list(color = "#ffffff"))
                 )

    })
  })
}



## To be copied in the UI
# mod_xy_and_size_ui("xy_and_size_1")

## To be copied in the server
# mod_xy_and_size_server("xy_and_size_1")
