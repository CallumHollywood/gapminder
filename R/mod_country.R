#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @import reactable
#' @import shinyWidgets
#' @importFrom gargoyle watch on trigger


gapminder <- readr::read_csv('inst/app/data/gapminder.csv') %>%
  rename(name = country) %>%
  mutate(name = as.character(name)) %>%
  mutate(name = ifelse(name == 'United States', 'United States of America', name)) %>%
  mutate(name = ifelse(name == 'Congo, Rep.', 'Republic of Congo', name)) %>%
  mutate(name = ifelse(name == 'Congo, Dem. Rep.', 'Democratic Republic of the Congo', name)) %>%
  mutate(name = ifelse(name == 'Korea, Dem. Rep.', 'North Korea', name)) %>%
  mutate(name = ifelse(name == 'Korea, Rep.', 'South Korea', name)) %>%
  mutate(name = ifelse(name == 'Serbia', 'Republic of Serbia', name)) %>%
  mutate(name = ifelse(name == 'Tanzania', 'United Republic of Tanzania', name)) %>%
  mutate(name = ifelse(name == 'Yemen, Rep.', 'Yemen', name)) %>%
  mutate(name = ifelse(name == "Cote d'Ivoire", 'Ivory Coast', name)) %>%
  mutate(name = ifelse(name == "Guinea-Bissau", 'Guinea Bissau', name))


fn_hchart <- function(metric, country, plottitle, ylab){

  fn_data <- gapminder %>%
    filter(name == country) %>%
    select('year', metric) %>%
    rename(metric_col = 2)

  hchart(fn_data, "line",
         hcaes(x = year, y = metric_col),
         name = plottitle,
  ) %>%
    hc_title(
      text = plottitle,
      margin = 20,
      align = "left",
      style = list(color = "#7cb5ec", useHTML = TRUE)
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


fn_country_metric_rank <- function(country, metric){

  gapminder %>%
    filter(year == 2007) %>%
    select(name, {{metric}}) %>%
    arrange(desc({{metric}})) %>%
    tibble::rownames_to_column('index') %>%
    filter(name == country) %>%
    mutate(index = as.numeric(index)) %>%
    pull(index)

}



mod_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             # offset = 1,
             align = 'center',
             div(class = 'sidelike',
                 prettyRadioButtons(
                   inputId = ns("chckbx_picktype"),
                   label = h4("Select a country from a :"),
                   choices = c("Map", "Picker"),
                   status = "primary",
                   selected = 'Map',
                   inline = TRUE
                 ),
                 div(style = 'height: 140px; fontsize; 100px;',
                     fluidRow(
                       column(6,
                              uiOutput(ns('ot_slt_country')),
                              ),
                       column(6)
                     )
                 ),
                 div(id = ns('dv_id1'), style = 'height: 800px; overflow-y: auto;',
                     conditionalPanel(condition = paste0('input[\'', ns('chckbx_picktype'), "\'] == \'Map\'"),
                                      column(1),
                                      column(10,
                                             highchartOutput(ns('ot_world_pop'), height = '400px')
                                      ),
                                      column(1)
                     ),
                     conditionalPanel(condition = paste0('input[\'', ns('chckbx_picktype'), "\'] == \'Picker\'"),
                                      br(),
                                      br(),
                                      br(),
                                      fluidRow(
                                        column(5,
                                               h6('Country')
                                        ),
                                        column(7,
                                               pickerInput(
                                                 inputId = ns('slt_country'),
                                                 label =  NULL,
                                                 choices  = sort(unique(gapminder$name)),
                                                 selected = NULL,
                                                 choicesOpt = NULL,
                                                 width = '90%',
                                                 inline = FALSE,
                                                 multiple = TRUE,
                                                 options = pickerOptions(maxOptions = 1)
                                               )
                                        )
                                      ),
                                      # br(),
                                      shinyjs::hidden(
                                        div(id = ns('dv_country_compare'),
                                            fluidRow(
                                              column(5,
                                                     h6('Comparison Countries')
                                              ),
                                              column(7,
                                                     pickerInput(
                                                       inputId = ns('slt_country_compare'),
                                                       label =  NULL,
                                                       choices  = NULL,
                                                       selected = NULL,
                                                       choicesOpt = NULL,
                                                       width = '90%',
                                                       inline = FALSE,
                                                       multiple = TRUE
                                                     )
                                              )
                                            )
                                        )
                                      )
                     )
                 )
             )
      ),
      column(8,
             div(class = 'sidelike',
                 tabsetPanel(
                   id = ns('tb_1'),
                   selected = c('interplay', 'through_years')[2],
                   tabPanel(h4('Profile'),
                            value = 'through_years',
                            br(),
                            div(class = 'bannerlike',
                                div(class = 'bnr1',
                                    fluidRow(
                                      column(12,
                                             align = 'center',
                                             uiOutput(ns('ot_banner_title'))
                                      )
                                    )
                                ),
                                br(),
                                fluidRow(
                                  column(4,
                                         mod_metrics_by_year_ui(ns("metrics_by_year_1"))
                                  ),
                                  column(4,
                                         mod_metrics_by_year_ui(ns("metrics_by_year_2"))
                                  ),
                                  column(4,
                                         mod_metrics_by_year_ui(ns("metrics_by_year_3"))
                                  )
                                )
                            )
                            # fluidRow(
                            #   column(4,
                            #          align = 'center',
                            #          div(class = 'cardlike',
                            #              br(),
                            #              h4('Life Expectancy')
                            #              # ,
                            #              # uiOutput(ns('ot_rank_lifeExp'))
                            #
                            #          )
                            #   ),
                            #   column(4,
                            #          align = 'center',
                            #          div(class = 'cardlike',
                            #              br(),
                            #              h4('Population')
                            #              # ,
                            #              # uiOutput(ns('ot_rank_pop'))
                            #
                            #          )
                            #   ),
                            #   column(4,
                            #          align = 'center',
                            #          div(class = 'cardlike',
                            #              br(),
                            #              h4('GDP per Capita')
                            #              # ,
                            #              # uiOutput(ns('ot_rank_gdpPercap'))
                            #          )
                            #   )
                            # ),
                            # br(),
                            # fluidRow(
                            #   column(4,
                            #          highchartOutput(ns('ot_country_lifeExp'), height = '250px')
                            #   ),
                            #   column(4,
                            #          highchartOutput(ns('ot_country_pop'))
                            #   ),
                            #   column(4,
                            #          highchartOutput(ns('ot_country_gdpPercap'))
                            #   )
                            # )
                   ),
                   tabPanel(h4('Interplay'),
                            value = 'interplay',
                            div(style = 'margin-left: 10px; margin-right: 10px;',
                                br(),
                                mod_interplay_ui(ns("interplay_1"))
                            )
                   )
                 )
             )
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_country_server <- function(
    id,
    pass_around
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    observeEvent(country_rctv(),{

      choices_fks <- sort(unique(gapminder$name))
      choices_fks <- choices_fks[!choices_fks == country_rctv()]

      updatePickerInput(
        session = session,
        label =  NULL,
        inputId  = 'slt_country_compare',
        choices   = choices_fks
      )

    })




    country_compare_rctv <- reactiveVal('')
    country_focus_rctv   <- reactiveVal('')


    observeEvent(input$slt_country_compare,{

      country_compare_rctv(

        tibble(
          country = input$slt_country_compare,
          status = 'compare'
        )

      )

    })



    observeEvent(
      c(country_compare_rctv(),
        country_focus_rctv()
      ),{

        req(country_compare_rctv())

        country_focus_rctv(
          country_compare_rctv() %>%
            bind_rows(
              tibble(
                country = country_rctv(),
                status = 'focus'
              )
            )
        )

      })



    observeEvent(country_focus_rctv(),{


      pass_around$country_focus_rctv <- country_focus_rctv()

      trigger('trgr_country_focus')

      print(country_focus_rctv())
      message('country_focus_rctv()')

    }, ignoreInit = TRUE)









    #### <<< CALL MODULES >>> ####

    mod_metrics_by_year_server(
      "metrics_by_year_1",
      metric = 'lifeExp',
      country_rctv = country_rctv,
      plottitle = "Life Expectancy",
      ylab = "Life Expectancy (Years)"
    )

    mod_metrics_by_year_server(
      "metrics_by_year_2",
      metric = 'pop',
      country_rctv = country_rctv,
      plottitle = "Population",
      ylab = "Population Count"
    )

    mod_metrics_by_year_server(
      "metrics_by_year_3",
      metric = 'gdpPercap',
      country_rctv = country_rctv,
      plottitle = "GDP per Capita",
      ylab = "GDP (USD)"
    )

    mod_interplay_server(
      "interplay_1",
      country_rctv,
      pass_around,
      input_tb1 = reactive({input$tb_1})
    )

    mod_rank_table_server(
      "rak_table_1",
      country_rctv,
      metric = lifeExp
    )

    mod_rank_table_server(
      "rak_table_2",
      country_rctv,
      metric = pop
    )

    mod_rank_table_server(
      "rak_table_3",
      country_rctv,
      metric = gdpPercap
    )



    #### <<< STATIC VALUES >>> ####

    # population
    population <- gapminder %>%
      select(name, year, pop) %>%
      pivot_wider(1:1, names_from = year, values_from = pop) %>%
      select(name, "2007") %>%
      rename(population = 2) %>%
      arrange()


    #### <<< REACTVE VALUES >>> ####

    country_rctv <- reactiveVal('')


    observeEvent(input$slt_country,{

      country_rctv(input$slt_country)

      message('country_rctv wt input$slt_country')

    }, ignoreInit = TRUE)

    observeEvent(input$Clicked,{

      country_rctv(input$Clicked)

      message('country_rctv wt input$Clicked')

    })


    # observeEvent(country_rctv(),{
    #
    #   print(country_rctv())
    #   message('country_rctv()')
    #
    # })


    #### <<< GARGOYLE ON >>> ####

    gargoyle::on('trgr_tb1',{

      if(pass_around$tb1 != 'compare'){

        shinyjs::hide("dv_country_compare")

        updatePrettyRadioButtons(
          session,
          'chckbx_picktype',
          choices = c('Map', 'Picker'),
          selected = 'Map',
          inline = TRUE
        )

      } else {

        shinyjs::show("dv_country_compare")

        updatePrettyRadioButtons(
          session,
          'chckbx_picktype',
          choices = 'Picker',
          selected = 'Picker',
          inline = TRUE
        )
      }



    })

    #### <<< OBSERVE EVENTS >>> ####

    observeEvent(input$tb_1,{



    # })
    # gargoyle::on('trgr_tb1',{

      if(input$tb_1 != 'interplay'){

        shinyjs::hide("dv_country_compare")

        updatePrettyRadioButtons(
          session,
          'chckbx_picktype',
          choices = c('Map', 'Picker'),
          selected = 'Map',
          inline = TRUE
        )

      }
      # else {
      #
      #   shinyjs::show("dv_country_compare")
      #
      #   updatePrettyRadioButtons(
      #     session,
      #     'chckbx_picktype',
      #     choices = 'Picker',
      #     selected = 'Picker',
      #     inline = TRUE
      #   )
      # }



    }, ignoreInit = TRUE)


    observeEvent(country_rctv(),{

      # output$ot_country_lifeExp <- renderHighchart({
      #
      #   fn_hchart('lifeExp', isolate(country_rctv()), "Life Expectancy", "Life Expectancy (Years)")
      #
      # })

      # output$ot_country_pop <- renderHighchart({
      #
      #   fn_hchart('pop', isolate(country_rctv()), "Population", "Abundance")
      #
      # })

      # output$ot_country_gdpPercap <- renderHighchart({
      #
      #   fn_hchart('gdpPercap', isolate(country_rctv()), "GDP per Capita", "USD")
      #
      # })


      output$ot_slt_country <- renderUI({

        tagList(
          h4(country_rctv()),
          br(),
          uiOutput(ns('ot_rank_lifeExp')),
          uiOutput(ns('ot_rank_pop')),
          uiOutput(ns('ot_rank_gdpPercap'))

        )

      })

      output$ot_rank_lifeExp <- renderUI({

        actionButton(ns('btn_rank_country_lifeExp'),
                     label = paste0('Life Expectancy Rank: ', fn_country_metric_rank(country_rctv(), lifeExp)),
                     width = '90%',
                     class = 'rank_btns'
        )

      })

      output$ot_rank_pop <- renderUI({

        actionButton(ns('btn_rank_country_pop'),
                     label = paste0('Population Rank: ', fn_country_metric_rank(country_rctv(), pop)),
                     width = '90%',
                     class = 'rank_btns'
        )

      })

      output$ot_rank_gdpPercap <- renderUI({

        actionButton(ns('btn_rank_country_gdpPercap'),
                     label = paste0('GDP Per Capita Rank: ', fn_country_metric_rank(country_rctv(), gdpPercap)),
                     width = '90%',
                     class = 'rank_btns'
        )

      })

    }, ignoreInit = TRUE)



    # observeEvent(input$Clicked,{
    #
    #   output$ot_country_lifeExp <- renderHighchart({
    #
    #     fn_hchart('lifeExp', isolate(input$Clicked), "Life Expectancy", "Life Expectancy (Years)")
    #
    #   })
    #
    #   output$ot_country_pop <- renderHighchart({
    #
    #     fn_hchart('pop', isolate(input$Clicked), "Population", "Abundance")
    #
    #   })
    #
    #   output$ot_country_gdpPercap <- renderHighchart({
    #
    #     fn_hchart('gdpPercap', isolate(input$Clicked), "GDP per Capita", "USD")
    #
    #   })
    #
    #
    #   output$ot_slt_country <- renderUI({
    #
    #     h4(input$Clicked)
    #
    #   })
    #
    #   output$ot_rank_lifeExp <- renderUI({
    #
    #     actionButton(ns('btn_rank_country_lifeExp'),
    #                  label = paste0('Rank: ', fn_country_metric_rank(isolate(input$Clicked), lifeExp)),
    #                  width = '80%',
    #                  class = 'rank_btns'
    #     )
    #
    #   })
    #
    #   output$ot_rank_pop <- renderUI({
    #
    #     actionButton(ns('btn_rank_country_pop'),
    #                  label = paste0('Rank: ', fn_country_metric_rank(isolate(input$Clicked), pop)),
    #                  width = '80%',
    #                  class = 'rank_btns'
    #     )
    #
    #   })
    #
    #   output$ot_rank_gdpPercap <- renderUI({
    #
    #     actionButton(ns('btn_rank_country_gdpPercap'),
    #                  label = paste0('GDP Per Capita: ', fn_country_metric_rank(isolate(input$Clicked), gdpPercap)),
    #                  width = '80%',
    #                  class = 'rank_btns'
    #     )
    #
    #   })
    #
    # }, ignoreInit = TRUE)


    observeEvent(input$btn_rank_country_lifeExp,{

      showModal(modalDialog(
        title = "Life Expectancy Ranking Table",
        # reactableOutput(ns('ot_ranks_lifeExp'))
        mod_rank_table_ui(ns("rak_table_1"))
      )
      )

    })

    observeEvent(input$btn_rank_country_pop,{

      showModal(modalDialog(
        title = "Population Ranking Table",
        # reactableOutput(ns('ot_ranks_pop'))
        mod_rank_table_ui(ns("rak_table_2"))
      )
      )

    })

    observeEvent(input$btn_rank_country_gdpPercap,{

      showModal(modalDialog(
        title = "GDP Per Capita Ranking Table",
        # reactableOutput(ns('ot_ranks_gdpPercap'))
        mod_rank_table_ui(ns("rak_table_3"))
      )
      )

    })


    #### <<< OUTPUTS >>> ####

    output$ot_banner_title <- renderUI({

      tagList(
          paste0(country_rctv(), ' Through the Years')
      )

    })


    output$ot_ranks_gdpPercap <- renderReactable({

      req(input$Clicked)

      rank_pop <- gapminder %>%
        filter(year == 2007) %>%
        select(name, gdpPercap) %>%
        arrange(desc(gdpPercap)) %>%
        tibble::rownames_to_column('rank') %>%
        mutate(rank = as.numeric(rank)) %>%
        rename(
          country = name,
          `GDP Per Capita` = gdpPercap
        )

      reactable(rank_pop,
                columns = list(
                  country = colDef(
                    style = function(country) {
                      if (country == input$Clicked) {
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
                      if (country == input$Clicked) {
                        color2 <- "#ff0000"
                      } else {
                        color2 <- "#152d8c"
                      }
                      list(color = color2, fontWeight = "normal")
                    }
                  ),
                  `GDP Per Capita` = colDef(
                    style = function(country) {
                      if (country == input$Clicked) {
                        color3 <- "#ff0000"
                      } else {
                        color3 <- "#152d8c"
                      }
                      list(color = color3, fontWeight = "normal")
                    }
                  )
                ))

    })


    output$ot_ranks_pop <- renderReactable({

      req(input$Clicked)

      rank_pop <- gapminder %>%
        filter(year == 2007) %>%
        select(name, pop) %>%
        arrange(desc(pop)) %>%
        tibble::rownames_to_column('rank') %>%
        mutate(rank = as.numeric(rank)) %>%
        rename(
          country = name,
          population = pop
        )

      reactable(rank_pop,
                columns = list(
                  country = colDef(
                    style = function(country) {
                      if (country == input$Clicked) {
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
                      if (country == input$Clicked) {
                        color2 <- "#ff0000"
                      } else {
                        color2 <- "#152d8c"
                      }
                      list(color = color2, fontWeight = "normal")
                    }
                  ),
                  population = colDef(
                    style = function(country) {
                      if (country == input$Clicked) {
                        color3 <- "#ff0000"
                      } else {
                        color3 <- "#152d8c"
                      }
                      list(color = color3, fontWeight = "normal")
                    }
                  )
                ))

    })


    output$ot_ranks_pop <- renderReactable({

      req(input$Clicked)

      rank_pop <- gapminder %>%
        filter(year == 2007) %>%
        select(name, pop) %>%
        arrange(desc(pop)) %>%
        tibble::rownames_to_column('rank') %>%
        mutate(rank = as.numeric(rank)) %>%
        rename(
          country = name,
          population = pop
        )

      reactable(rank_pop,
                columns = list(
                  country = colDef(
                    style = function(country) {
                      if (country == input$Clicked) {
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
                      if (country == input$Clicked) {
                        color2 <- "#ff0000"
                      } else {
                        color2 <- "#152d8c"
                      }
                      list(color = color2, fontWeight = "normal")
                    }
                  ),
                  population = colDef(
                    style = function(country) {
                      if (country == input$Clicked) {
                        color3 <- "#ff0000"
                      } else {
                        color3 <- "#152d8c"
                      }
                      list(color = color3, fontWeight = "normal")
                    }
                  )
                ))

    })



    # output$ot_ranks_lifeExp <- renderReactable({
    #
    #   # req(input$Clicked)
    #   req(country_rctv())
    #
    #   rank_lifeExp <- gapminder %>%
    #     filter(year == 2007) %>%
    #     select(name, lifeExp) %>%
    #     arrange(desc(lifeExp)) %>%
    #     tibble::rownames_to_column('rank') %>%
    #     mutate(rank = as.numeric(rank)) %>%
    #     rename(country = name)
    #
    #   reactable(rank_lifeExp,
    #             columns = list(
    #               country = colDef(
    #                 style = function(country) {
    #                   if (country == country_rctv()) {
    #                     color <- "#ff0000"
    #                   } else {
    #                     color <- "#152d8c"
    #                   }
    #                   list(color = color, fontWeight = "bold")
    #                 }
    #               )
    #               ,
    #               rank = colDef(
    #                 style = function(country) {
    #                   if (country == country_rctv()) {
    #                     color2 <- "#ff0000"
    #                   } else {
    #                     color2 <- "#152d8c"
    #                   }
    #                   list(color = color2, fontWeight = "normal")
    #                 }
    #               ),
    #               lifeExp = colDef(
    #                 style = function(country) {
    #                   if (country == country_rctv()) {
    #                     color3 <- "#ff0000"
    #                   } else {
    #                     color3 <- "#152d8c"
    #                   }
    #                   list(color = color3, fontWeight = "normal")
    #                 }
    #               )
    #             ))
    #
    # })


    output$ot_world_pop <- renderHighchart({

      req(population)

      ClickFunction <- JS(paste0("function(event) {Shiny.onInputChange('", session$ns("Clicked"), "', event.point.name);}"))

      hcmap("custom/world",
            data = population,
            joinBy = c("name", "name"),
            name = "Country Profile",
            dataLabels = list(enabled = TRUE, format = "{point.name}"),
            borderColor = "#FAFAFA",
            borderWidth = 0.1,
            showInLegend = FALSE
      ) %>%
        hc_plotOptions(series = list(
          events = list(click = ClickFunction)),
          showInLegend = FALSE,
          pointFormat = "{point.y}"
        ) %>%
        hc_legend(enabled = F)
    })

    output$ot_slt_country <- renderUI({

      h4('')

    })

  })
}


