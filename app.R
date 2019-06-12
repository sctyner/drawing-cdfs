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
    textInput(inputId = "title", label = "Write a title", 
              value = "Histogram of Random Normal Values"),
    # *Output() functions
    plotOutput(outputId = "hist"), 
    verbatimTextOutput("stats")
    # action button 
)

# Define server logic required to assemble inputs into outputs
server <- function(input, output) {
    # Server rules: 
    # 1. save objects to display to output$
    # 2. build objects to display with render*()
    # 3. use input values with input$
    
    # Reactive Values: a value that changes with input 
    # reactive values work with reactive functions: functions that 
    # take ractive values and know what to do with them
    # "Operation not allowed without an active reactive context" means:
    # you tried to use a reactive value without also using a reactive function 
    # that goes with it. 
    # 1. reactive values notify. 2. reactive functions respond. 
    # reactive functions: 
    # 1. Use a code chunk {} to build & rebuild an object
    #  What code will the function use?
    # 2. The object will respond to changes in a set of reactive values. 
    #  Which reactive values will the object respond to? 
    # Display output with render*() functions: on re-run all code in {} will be run
    # Question: How to change one reactive value without changing rendered output? 
    ## Modularize code with reactive()
    # data <- reactive({rnorm(input$num)})
    # call like a function; it caches its value
    # prevent reactions with isolate() - circumvent updating. prevent title field from updating the plot
    # isolate outputs normal r values - breaks link from input to output
    # will be called when something else reactive is changed. 
    # observe event triggers code to run on server observeEvent(x, {}):
    # x is a reactive value to respond to. code in {} is treated as isolated
    # app should never depend on the value of the action button. 
    # observe({}): just give a signle block of code, then it will rerun anytime a
    # reactive value changes. 
    # stop at 1:18:36
    
    data <- reactive({
        rnorm(input$num)
    })
    
    output$hist <- renderPlot({ # braces around code to pass many lines of code to render plot
        hist(data(), main = isolate({input$title}))
    })
    output$stats <- renderPrint({
        summary(data())
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
