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

               # fluidRow(
               # #   column(3,
               # #          style = 'margin-left: 10px;',
               # #          # pickerInput(
               # #          #   inputId = ns('slt_country_compare'),
               # #          #   label = 'Comparison Countries',
               # #          #   choices  = '',
               # #          #   selected = NULL,
               # #          #   choicesOpt = NULL,
               # #          #   width = '100%',
               # #          #   inline = FALSE,
               # #          #   multiple = TRUE
               # #          # )
               # #   ),
               #   column(12,
               #          fluidRow(
               #            column(12,
               #                   mod_compare_countries_ui(ns("compare_countries_1"))
               #            )
               #          )
               #          ,
               #          fluidRow(
               #            column(12,
               #                   mod_compare_countries_ui(ns("compare_countries_2"))
               #            )
               #          ),
               #          fluidRow(
               #            column(12,
               #                   mod_compare_countries_ui(ns("compare_countries_3"))
               #            )
               #          )
               #   )
               # )
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

    observeEvent(input_tb1(),{

      message('input_tb1')

      if(input_tb1() == 'interplay'){

        updateTabsetPanel(
          session
          , 'tb1'
          , selected = 'country_review'
        )

      }

    })


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



    observeEvent(input$tb1,{

      pass_around$tb1 <- input$tb1

      trigger('trgr_tb1')

    }, ignoreInit = TRUE)

    # observeEvent(country_rctv(),{
    #
    #   choices_fks <- sort(unique(gapminder$name))
    #   choices_fks <- choices_fks[!choices_fks == country_rctv()]
    #
    #   updatePickerInput(
    #     session = session,
    #     inputId  = 'slt_country_compare',
    #     choices   = choices_fks
    #   )
    #
    # })


    # country_compare_rctv <- reactiveVal('')
    # country_focus_rctv   <- reactiveVal('')


    # observeEvent(input$slt_country_compare,{
    #
    #   country_compare_rctv(
    #
    #     tibble(
    #       country = input$slt_country_compare,
    #       status = 'compare'
    #     )
    #
    #   )
    #
    # })


    # observeEvent(
    #   c(country_compare_rctv(),
    #     country_focus_rctv()
    #   ),{
    #
    #     req(country_compare_rctv())
    #
    #     country_focus_rctv(
    #       country_compare_rctv() %>%
    #         bind_rows(
    #           tibble(
    #             country = country_rctv(),
    #             status = 'focus'
    #           )
    #         )
    #     )
    #
    #   })

    # observeEvent(country_focus_rctv(),{
    #
    #   print(country_focus_rctv())
    #   message('country_focus_rctv()')
    #
    # })



    #### <<< OUTPUT S>>> ####

    gapminder_compare_rctv <- reactive({

      watch('trgr_country_focus')
      print(pass_around$country_focus_rctv$country)
      message('!! trgr_country_focus')

      # req(country_focus_rctv())

      x <- gapminder %>%
        filter(name %in% pass_around$country_focus_rctv$country)

      print(gapminder)
      message('xxxxxxxxxx')

      x

    })

    # output$ot_compare_countries_lifeExp <- renderHighchart({
    #
    #   req(country_focus_rctv())
    #
    #   hchart(gapminder_compare_rctv(),
    #          "line",
    #          hcaes(x = year, y = round(lifeExp, 1), group = name),
    #          dataLabels = list(enabled = TRUE, format = "{point.name}")
    #   ) %>%
    #     hc_title(text = 'Life Expectancy by Countries') %>%
    #     hc_yAxis(title = list(text = "Life Expectancy (Years)")) %>%
    #     hc_xAxis(title = list(text = NULL))
    #
    # })


    # output$ot_compare_countries_pop <- renderHighchart({
    #
    #   req(country_focus_rctv())
    #
    #   hchart(gapminder_compare_rctv(),
    #          "line",
    #          hcaes(x = year, y = round(pop, 1), group = name),
    #          dataLabels = list(enabled = TRUE, format = "{point.name}")
    #   ) %>%
    #     hc_title(text = 'Population by Countries') %>%
    #     hc_yAxis(title = list(text = "Life Expectancy (Years)")) %>%
    #     hc_xAxis(title = list(text = NULL))
    #
    # })


  })
}

