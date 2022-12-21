#' rak_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rank_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactableOutput(ns('ot_ranks'))
  )
}

#' rak_table Server Functions
#'
#' @noRd
mod_rank_table_server <- function(
    id,
    country_rctv,
    metric
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ot_ranks <- renderReactable({

      req(country_rctv())

      rank_table <- gapminder %>%
        filter(year == 2007) %>%
        select(name, {{metric}}) %>%
        arrange(desc({{metric}})) %>%
        tibble::rownames_to_column('rank') %>%
        mutate(rank = as.numeric(rank)) %>%
        rename(
          country = name,
          metric = {{metric}}
        )

      reactable(rank_table,
                columns = list(
                  country = colDef(
                    style = function(country) {
                      if (country == country_rctv()) {
                        color <- "#ff0000"
                      } else {
                        color <- "#152d8c"
                      }
                      list(color = color, fontWeight = "bold")
                    }
                  )
                  ,
                  rank = colDef(
                    style = function(country) {
                      if (country == country_rctv()) {
                        color2 <- "#ff0000"
                      } else {
                        color2 <- "#152d8c"
                      }
                      list(color = color2, fontWeight = "normal")
                    }
                  ),
                  metric = colDef(
                    style = function(country) {
                      if (country == country_rctv()) {
                        color3 <- "#ff0000"
                      } else {
                        color3 <- "#152d8c"
                      }
                      list(color = color3, fontWeight = "normal")
                    }
                  )
                ))
    })
  })
}

## To be copied in the UI
# mod_rak_table_ui("rank_table_1")

## To be copied in the server
# mod_rak_table_server("rank_table_1")
