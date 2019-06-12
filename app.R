#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# https://shiny.rstudio.com/tutorial/
# ended at about 22 minutes

library(shiny)

# Define UI for application 
ui <- fluidPage(
    # *Input() functions
    sliderInput(inputId = "num", label = "Choose a Number", 
                value = 25, min = 1, max = 100),
    # *Output() functions
    plotOutput(outputId = "hist")
)

# Define server logic required to assemble inputs into outputs
server <- function(input, output) {
    # Server rules: 
    # 1. save objects to display to output$
    # 2. build objects to display with render*()
    # 3. use input values with input$
    output$hist <- renderPlot({ # braces around code to pass many lines of code to render plot
        title <- paste0(input$num, " random standard normal values")
        hist(rnorm(input$num), main = title)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
