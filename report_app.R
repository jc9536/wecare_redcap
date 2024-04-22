library(shiny)
library(shinydashboard)
library(targets)
library(officer)
library(officedown)
library(here)

ui <- dashboardPage(
  dashboardHeader(title = "WeCare Report"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .center-div {
          display: flex;
          justify-content: center;  /* Center horizontally */
          align-items: center;  /* Center vertically */
          height: 100vh;  /* Take full height of the viewport */
        }
      "))
    ),
    uiOutput("loginPage"),
    uiOutput("mainUI")
  )
)

server <- function(input, output, session) {
  USER <- reactiveValues(loggedIn = FALSE)
  reportPath <- reactiveVal()  # Initialize reactive value to store report path
  
  observe({
    if (isTruthy(input$btnLogin)) {
      if (input$pwd == "wecare") {
        USER$loggedIn <- TRUE
      } else {
        showNotification("Wrong password!", type = "error")
      }
    }
  })
  
  output$loginPage <- renderUI({
    if (!USER$loggedIn) {
      div(
          tagList(
            passwordInput("pwd", "Enter password"),
            actionButton("btnLogin", "Login")
          )
      )
    }
  })
  
  output$mainUI <- renderUI({
    if (USER$loggedIn) {
      div(class = "center-div",
          tagList(
            actionButton("runWorkflow", "Run Data Workflow"),
            downloadButton("downloadReport", "Download Report")
          )
      )
    }
  })
  
  observeEvent(input$runWorkflow, {
    withProgress(message = "Running data workflow...", {
      setProgress(value = 0.1)
      # Run the targets pipeline
      targets::tar_make()
      # After completion, copy the report to a temporary location
      tempReportPath <- tempfile(fileext = ".docx")
      file.copy(from = here("output", "progress_report.docx"), to = tempReportPath, overwrite = TRUE)
      reportPath(tempReportPath)  # Update the reactive value to the new path
      setProgress(value = 1)
      showNotification("Workflow completed successfully. You can now download the report.", 
                       type = "message", duration = NULL)
    })
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      file.copy(reportPath(), file)  # Copy from the updated report path
    }
  )
}

shinyApp(ui = ui, server = server)
