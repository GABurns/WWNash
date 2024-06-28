#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bsicons bs_icon

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header(h2("Pairwise Simulation Comparision"),
                class = "bg-primary"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        helpText("Help"),
        layout_columns(
          col_widths = c(6, 6),
          card(card_title("Baseline")), 
          card(card_title("Comparator"))
        ),
        actionButton(ns("compare"), label = "Compare Simulations")
      ),
      card(
        layout_columns(
          row_heights = "200px",
          value_box(
            title = "Mean Number of Participants ",
            value = textOutput(ns("leader")),
            showcase = bs_icon("person-arms-up"),
            p(
              bs_icon("arrow-up-circle-fill", size = "2em"),
              textOutput(ns("leader_story"))
            )
          ),
          value_box(
            title = "Mean Number of Cohorts",
            value = textOutput(ns("Cohorts")),
            showcase = bs_icon("collection-fill"),
            p(
              bs_icon("arrow-up-circle-fill", size = "2em"),
              textOutput(ns("cohort"))
            )
          ),
          value_box(
            title = "False Discovery Rate",
            value = textOutput(ns("FDR")),
            showcase = bs_icon("slash-square-fill"),
            p(
              bs_icon("arrow-up-circle-fill", size = "2em"),
              textOutput(ns("role_story"))
            )
          )
        ),
        card(plotOutput(ns("plot")))
      )
    )
    
  )
}

#' dashboard Server Functions
#'
#' @noRd
mod_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data <- eventReactive(input$compare, {
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    output$plot <- renderPlot({
      validate(need())
    })
    
  })
}
