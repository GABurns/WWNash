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
#' @importFrom shinyWidgets pickerInput

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header(h2("Pairwise Simulation Comparision"),
                class = "bg-primary"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        h3("Choose Comparision"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_title("Baseline"),
            card_body(
              pickerInput(
                inputId = ns("steqB"),
                label = "Shortterm Endpoint Quality",
                choices = unique(SimData$ShortTermEndpointQuality)
              ),
              pickerInput(
                inputId = ns("mncB"),
                label = "Maximumnumberofcohorts",
                choices = unique(SimData$Maximumnumberofcohorts)
              ),
              pickerInput(
                inputId = ns("tdsB"),
                label = "Type of Data Sharing",
                choices = unique(SimData$TypeofDataSharing)
              ),
              pickerInput(
                inputId = ns("cirB"),
                label = "CohortInclusionRate",
                choices = unique(SimData$CohortInclusionRate)
              ),
              pickerInput(
                inputId = ns("fcssB"),
                label = "FinalCohortSampleSize",
                choices = unique(SimData$FinalCohortSampleSize)
              ),
              pickerInput(
                inputId = ns("ifsB"),
                label = "InterimFutilityStopping",
                choices = unique(SimData$InterimFutilityStopping)
              ),
              pickerInput(
                inputId = ns("tesB"),
                label = "TreatmentEfficacySetting",
                choices = unique(SimData$TreatmentEfficacySetting)
              )
            )
          ),
          card(
            card_title("Comparator"),
            card_body(
              pickerInput(
                inputId = ns("steqC"),
                label = "Shortterm Endpoint Quality",
                choices = unique(SimData$ShortTermEndpointQuality)
              ),
              pickerInput(
                inputId = ns("mncC"),
                label = "Maximumnumberofcohorts",
                choices = unique(SimData$Maximumnumberofcohorts)
              ),
              pickerInput(
                inputId = ns("tdsC"),
                label = "Type of Data Sharing",
                choices = unique(SimData$TypeofDataSharing)
              ),
              pickerInput(
                inputId = ns("cirC"),
                label = "CohortInclusionRate",
                choices = unique(SimData$CohortInclusionRate)
              ),
              pickerInput(
                inputId = ns("fcssC"),
                label = "FinalCohortSampleSize",
                choices = unique(SimData$FinalCohortSampleSize)
              ),
              pickerInput(
                inputId = ns("ifsC"),
                label = "InterimFutilityStopping",
                choices = unique(SimData$InterimFutilityStopping)
              ),
              pickerInput(
                inputId = ns("tesC"),
                label = "TreatmentEfficacySetting",
                choices = unique(SimData$TreatmentEfficacySetting)
              )
            )
          )
        ),
        actionButton(inputId = ns("compare"), label = "Compare Simulations")
      ),
      card(
        layout_columns(
          row_heights = "200px",
          value_box(
            title = "Mean Number of Participants ",
            value = textOutput(ns("participants")),
            showcase = bs_icon("person-arms-up"),
            uiOutput(ns("icon")),
            textOutput(ns("participants_story"))
          ),
          value_box(
            title = "Mean Number of Cohorts",
            value = textOutput(ns("cohorts")),
            showcase = bs_icon("collection-fill"),
            uiOutput(ns("icon2")),
            textOutput(ns("cohorts_story"))
          ),
          value_box(
            title = "False Discovery Rate",
            value = textOutput(ns("fdr")),
            showcase = bs_icon("slash-square-fill"),
            uiOutput(ns("icon3")),
            textOutput(ns("fdr_story"))
          )
        ),
        layout_columns(colum_widths = c(4, 8),
                       wellPanel(
                         h3("Description"),
                         p(
                           "Radar charts are effective tools for visualizing multivariate data.
                                   They allow for the comparison of multiple variables on a single plot,
                                   making them particularly useful when the variables are on different scales or units."
                         )
                       ),
                       plotOutput(ns("plot")))
      )
    )
  )
}

#' dashboard Server Functions
#' @import fmsb
#'
#' @noRd
mod_dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data <- eventReactive(input$compare,
                          {
                            SimData$Type <- NA
                            
                            SimData[SimData$ShortTermEndpointQuality == input$steqB &
                                      SimData$Maximumnumberofcohorts ==  input$mncB &
                                      SimData$TypeofDataSharing == input$tdsB &
                                      SimData$CohortInclusionRate  == input$cirB &
                                      SimData$FinalCohortSampleSize == input$fcssB &
                                      SimData$InterimFutilityStopping == input$ifsB &
                                      SimData$TreatmentEfficacySetting == input$tesB,]$Type <-
                              "Baseline"
                            
                            SimData[SimData$ShortTermEndpointQuality == input$steqC &
                                      SimData$Maximumnumberofcohorts ==  input$mncC &
                                      SimData$TypeofDataSharing == input$tdsC &
                                      SimData$CohortInclusionRate  == input$cirC &
                                      SimData$FinalCohortSampleSize == input$fcssC &
                                      SimData$InterimFutilityStopping == input$ifsC &
                                      SimData$TreatmentEfficacySetting == input$tesC,]$Type <-
                              "Comparator"
                            
                            return(SimData[!is.na(SimData$Type),])
                          },
                          ignoreNULL = TRUE,
                          ignoreInit = FALSE)
    
    output$participants <- renderText({
      req(isTruthy(data()))
      as.character(round(data()$Avg_Pat[data()$Type == "Comparator"]), 2)
    })
    
    output$icon <- renderUI({
      req(isTruthy(data()))
      diff <-
        round(data()$Avg_Pat[data()$Type == "Comparator"] - data()$Avg_Pat[data()$Type == "Baseline"], 0)
      logic <- ifelse(diff >= 0, "more", "less")
      
      if (logic == "more") {
        bs_icon("arrow-up-circle-fill",  size = "2em")
      } else {
        bs_icon("arrow-down-circle-fill",  size = "2em")
      }
    })
    
    output$participants_story <- renderText({
      req(isTruthy(data()))
      diff <-
        round(data()$Avg_Pat[data()$Type == "Comparator"] - data()$Avg_Pat[data()$Type == "Baseline"], 0)
      logic <- ifelse(diff >= 0, "more", "less")
      
      sprintf("The comparator had %1$s average participants than baseline by %2$s",
              logic,
              diff)
      
    })
    
    output$cohorts <- renderText({
      req(isTruthy(data()))
      as.character(round(data()$Avg_Cohorts[data()$Type == "Comparator"]), 2)
    })
    
    output$icon2 <- renderUI({
      req(isTruthy(data()))
      diff <-
        round(data()$Avg_Cohorts[data()$Type == "Comparator"] - data()$Avg_Cohorts[data()$Type == "Baseline"], 0)
      logic <- ifelse(diff >= 0, "more", "less")
      
      if (logic == "more") {
        bs_icon("arrow-up-circle-fill",  size = "2em")
      } else {
        bs_icon("arrow-down-circle-fill",  size = "2em")
      }
    })
    
    output$cohorts_story <- renderText({
      req(isTruthy(data()))
      
      diff <-
        round(data()$Avg_Cohorts[data()$Type == "Comparator"] - data()$Avg_Cohorts[data()$Type == "Baseline"], 0)
      logic <- ifelse(diff >= 0, "greater", "lower")
      
      sprintf("The comparator had %1$s average cohort size than baseline by %2$s",
              logic,
              diff)
    })
    
    output$fdr <-  renderText({
      req(isTruthy(data()))
      as.character(round(data()$FDR[data()$Type == "Comparator"]), 2)
    })
    
    output$icon3 <- renderUI({
      req(isTruthy(data()))
      diff <-
        round(data()$FDR[data()$Type == "Comparator"] - data()$FDR[data()$Type == "Baseline"], 0)
      logic <- ifelse(diff >= 0, "more", "less")
      
      if (logic == "more") {
        bs_icon("arrow-up-circle-fill",  size = "2em")
      } else {
        bs_icon("arrow-down-circle-fill",  size = "2em")
      }
    })
    
    output$fdr_story <-  renderText({
      req(isTruthy(data()))
      diff <-
        round(data()$FDR[data()$Type == "Comparator"] - data()$FDR[data()$Type == "Baseline"], 2)
      logic <- ifelse(diff >= 0, "positive", "negative")
      
      sprintf("The comparator had %1$s false discovery rate than baseline by %2$s",
              logic,
              diff)
    })
    
    output$plot <- renderPlot({
      validate(need(
        nrow(data()) > 1,
        "Please select a baseline simulation and a different comparatator"
      ))
      
      maxmin <-
        as.data.frame(apply(SimData[8:ncol(SimData)], 2, function(x)
          c(
            max(x, na.rm = TRUE), min(x, na.rm = TRUE)
          )))
      
      maxmin$Type <- "Agg Stat"
      
      dataset <- rbind(maxmin,
                       data()[8:ncol(data())])
      
      # Need to remove NAs
      
      dataset <- dataset[complete.cases(dataset)]
      
      # Reduce plot margin using par()
      op <- par(mar = c(1, 2, 2, 2))
      # Create the radar charts
      create_beautiful_radarchart(data = dataset,
                                  color = c("#00AFBB", "#FC4E07"))
      # Add an horizontal legend
      legend(
        x = "bottom",
        legend = c("Baseline", "Comparison"),
        horiz = TRUE,
        bty = "n",
        pch = 20 ,
        col = c("#00AFBB", "#E7B800", "#FC4E07"),
        text.col = "black",
        cex = 1,
        pt.cex = 1.5
      )
      par(op)
    })
  })
}
