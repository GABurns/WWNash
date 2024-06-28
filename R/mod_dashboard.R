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
        actionButton(ns("create_plot"), label = "Compare Simulations")
      ),
      card(
        layout_columns(
          row_heights = "200px",
          value_box(
            title = "Mean Number of Patients",
            value = textOutput(ns("leader")),
            showcase = bs_icon("person-arms-up"),
            p(
              bs_icon("arrow-up-circle-fill", size = "2em"),
              textOutput(ns("leader_story"))
            )
          ),
          value_box(
            title = "Top Organisation",
            value = textOutput(ns("org")),
            showcase = bs_icon("dice-6-fill"),
            p(
              bs_icon("arrow-up-circle-fill", size = "2em"),
              textOutput(ns("org_story"))
            )
          ),
          value_box(
            title = "Top Role",
            value = textOutput(ns("role")),
            showcase = bs_icon("trophy-fill"),
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
    
  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard_1")

## To be copied in the server
# mod_dashboard_server("dashboard_1")
