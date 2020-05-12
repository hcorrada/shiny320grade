#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Your 320 Grade"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3,
            numericInput("project_1",
                      "Project 1 (Points from ELMS):",
                      min = 0,
                      max = 95,
                      value = 80
                    ),
            numericInput("project_2",
                      "Project 2 (Points from ELMS):",
                      min = 0,
                      max = 70,
                      value = 60
                    ),
            numericInput("project_3",
                      "Project 3 (Points from ELMS):",
                      min = 0,
                      max = 80,
                      value = 70
                    ),
            numericInput("project_4",
                      "Project 4 (Points you guess):",
                      min = 0,
                      max = 60,
                      value = 50
                    ),
            numericInput("project_5",
                      "Project 5 (Points you guess):",
                      min = 0,
                      max = 50,
                      value = 40
                    ),
            numericInput("final_project",
                      "Final project (Points you guess):",
                      min = 0,
                      max = 60,
                      value = 50
                    ),
            numericInput("midterms",
                        "Midterms (Pct from ELMS):",
                        min = 0,
                        max = 100,
                        value = 80
                    ),
            numericInput("homeworks",
                      "Homeworks (Pct from ELMS:",
                      min = 0,
                      max = 100,
                      value = 80
                    )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Projects (Pct)"),
            textOutput("project_pct"),
            h3("Final Grade (Pct)"),
            textOutput("final_pct"),
            h3("Final Grade (Letter)"),
            textOutput("final_grade"),
            h3("Pass/Fail"),
            textOutput("pass_fail")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    grade_wts <- c(.20, .25, .40, .15)
    project_totals <- c(95, 70, 80, 60, 50)
    grades <- c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+")
    cutoffs <- c(0, 50, 63, 67, 70, 73, 77, 80, 83, 87, 90, 93, 97)
    
        project_scores <- reactive(c(input$project_1,
                            input$project_2,
                            input$project_3,
                            input$project_4,
                            input$project_5))
        
        project_pct <- reactive({
            max(sapply(3:5, function(i) {
                numerator <- sum(project_scores())
                denominator <- sum(project_totals[-i])
                numerator / denominator
            }))
        })
        
        final_pcts <- reactive(c(input$midterms,
                        input$homeworks,
                        project_pct() * 100,
                        input$final_project / 60 * 100))
        
        final_pct <- reactive(sum(grade_wts * final_pcts()))

        output$project_pct <- renderText(format(project_pct() * 100, digits=4))
        output$final_pct <- renderText(format(final_pct(), digits=4))
        
        tmp <- reactive(rev(which(final_pct() > cutoffs)))
        output$final_grade  <- renderText(ifelse(length(tmp()) > 0, grades[tmp()[1]], "A+"))
        output$pass_fail = renderText(ifelse(final_pct() >= 63, "PASS", "FAIL"))
}

# Run the application 
shinyApp(ui = ui, server = server)
