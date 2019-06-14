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
library(ggplot2)
library(plotly)
library(purrr)
library(dplyr)
library(DT)


# other functions needed 

## augment data for approximating the cdf. 
augment_cdf <- function(dat,xrange, yrange = c(0,1)){
    dat2 <-  dat %>% arrange(x) %>% mutate(monotone = y == cummax(y)) %>% filter(monotone) 
    
    hits_ymin <- min(dat2$y) == yrange[1]
    hits_ymax <- max(dat2$y) == yrange[2]
    hits_xmin <- min(dat2$x) == xrange[1]
    hits_xmax <- max(dat2$x) == xrange[2]
    
    
    if (hits_ymin & !hits_xmin) {
        dat2 <- dat2 %>% add_row(x = xrange[1], y = yrange[1])
    }
    if (hits_ymax & !hits_xmax) {
        dat2 <- dat2 %>% add_row(x = xrange[2], y = yrange[2])
    }
    if (hits_xmax & !hits_ymax) {
        # if the max x value is present, but it doesn't hit the ymax, replace it
        dat2[which(dat2$x == xrange[2] & dat2$y == max(dat2$y)),] <- c(xrange[2], yrange[2])
    }
    if (!hits_xmin & !hits_ymin){
        dat2 <- dat2 %>% add_row(x = xrange[1], y = yrange[1])
    }
    if (!hits_xmax & !hits_ymax){
        dat2 <- dat2 %>% add_row(x = xrange[2], y = yrange[2])
    }
    
    dat2 %>% arrange(x)
    
}


# Define UI for application 
ui <- fluidPage( 
    titlePanel("Draw your own CDFs"),
    # *Input() functions
    sidebarLayout(
        sidebarPanel(
            numericInput("xmin", label = "Minimum X Value", value = 0),
            numericInput("xmax", label = "Maximum X Value", value = 1),
            numericInput("seq", label = "Distance between interpolated points", value = .02),
            p("Optional: You can also upload your own data (in a single vector .csv file).
              Upon upload, the empirical CDF will be drawn on both plots."),
            fileInput("userdat", "Upload .csv",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values",
                                 ".csv")), 
            actionButton("draw1", label = "Draw curve 1 with approx", icon =icon("pencil-ruler")), 
            downloadButton('downloadData', 'Download', icon = icon("download")), 
            width = 3 # width of sidebar panel
        ),
        mainPanel( 
            fluidRow(
                
                column(8,  h3("Click to generate points"),
                       p("To reset the dragging plot, click this plot again."),
                       plotOutput("clickplot", click = "plot_clicks", 
                                      width = 600, height = 400)), 
                column(4, h3("Clicked points below"), dataTableOutput("datatab"))
            ),
            fluidRow(
                column(8, h3("Drag points to adjust"), 
                       p("To remove a point, drag it off the plot. Note: clicking above plot will cause the below plot to reset. All changes will be lost."),
                       plotlyOutput("dragplot", width = "600px", height = "400px")),
                column(4, h3("Dragged points below"), dataTableOutput("datatab2"))
            )
        )
    )
    # sliderInput(inputId = "num", label = "Choose a Number", 
    #             value = 25, min = 1, max = 100),
    # textInput(inputId = "title", label = "Write a title", 
    #           value = "Histogram of Random Normal Values"),
    # actionButton(inputId = "norm", label = "Normal"),
    # actionButton(inputId = "unif", label = "Uniform"),
    # # *Output() functions
    # plotOutput(outputId = "hist"), 
    # verbatimTextOutput("stats")
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
    # delay reactions with eventReactive()
    # eventReactive(input$go, {rnorm(input$num)})
    # first arg is value(s) to respond to.
    # second arg is the code used to build object. will be treated as isolated 
    # manage state with reactiveValues() 
    # inputs change based on user values, you can't overwrite input values in general
    # own list of reactive values: create a list of reactive values to manipulate programmatically 
    # 
    
    click_points_data <- data.frame(x = numeric(), y = numeric())
    
    # collect clicked points
    click_points <- reactive({
        newpoint <- input$plot_clicks
        if (!is.null(newpoint))
            click_points_data <<- data.frame(
                x = c(click_points_data$x, newpoint$x),
                y = c(click_points_data$y, newpoint$y), stringsAsFactors = FALSE)
        click_points_data
    })
    
    
    # rv is a "persistent state" 
    rv <- reactiveValues(
        #data = rnorm(input$num) # cannot pass inputs to reactiveValues? 
        points2 = data.frame(type = character(0), x = numeric(0), y = numeric(0), stringsAsFactors = FALSE)
        )
    
    # add to rv everytime plot is clicked
    observeEvent(input$plot_clicks, {
        rv$points2 <-  data.frame(type = "click", click_points(), stringsAsFactors = F) %>% 
            filter(x >= input$xmin , x <= input$xmax, y >= 0, y <= 1 ) # remove points outside the range
        
    })
    
    
    userData <- reactive({
        if(!is.null(input$userdat)){
            input$userdat
        df <- read.csv(input$userdat$datapath, stringsAsFactors = F)
        df2 <- tibble(x = df[,1], weight = (1/nrow(df)))
        df2 <- df2 %>% group_by(x) %>% 
            summarize(weight2 = sum(weight)) %>% 
            mutate(y = cumsum(weight2))
        df2
        }
        })
    
    approx_curve1 <- eventReactive(input$draw1, {
        curve1_dat <- augment_cdf(dat = click_points(), xrange = c(input$xmin, input$xmax))
        approx_curve1_dat <- approx(x = curve1_dat$x, y = curve1_dat$y, n = (((input$xmax - input$xmin) / input$seq) + 1))
         data.frame(x = approx_curve1_dat$x, y = approx_curve1_dat$y)
    })

    
    output$clickplot <- renderPlot({ # braces around code to pass many lines of code to render plot
        p <- ggplot() +
            #  Clicked points are red circles
            geom_point(data = click_points(), aes(x = x, y = y), color = "red", shape = 1) +
            labs(x = "", y = "") +
            xlim(input$xmin,input$xmax) +
            ylim(0,1) # restrict to CDFs
        if(!is.null(input$userdat)){
         p <- p + geom_line(data = userData(), aes(x = x, y = y))
        }
        if (input$draw1){
            p <- p + geom_line(data = approx_curve1(), aes(x = x, y = y), color = "red")
        }
        p
    }) 
    
    output$dragplot <- renderPlotly({
        circles <- map2(click_points()$x, click_points()$y,
                        ~list(
                            type = "circle",
                            # anchor circles at (mpg, wt)
                            xanchor = .x,
                            yanchor = .y,
                            # give each circle a 2 pixel diameter
                            x0 = -4, x1 = 4,
                            y0 = -4, y1 = 4,
                            xsizemode = "pixel",
                            ysizemode = "pixel",
                            # other visual properties
                            fillcolor = "red",
                            line = list(color = "transparent")
                        )
        )
        
       py <- plot_ly() %>%
            add_trace(x = userData()$x, y = userData()$y, 
                      type = 'scatter', mode = 'lines', name = 'Empirical CDF',
                      line = list(color = '#45171D')) %>% 
            layout(shapes = circles, 
                   xaxis = list(range = c(input$xmin,input$xmax)),
                   yaxis = list(range = c(0,1))) %>%
            config(edits = list(shapePosition = TRUE))
       if (input$draw1){
          py <-  py %>% 
               add_trace(x = approx_curve1()$x, y= approx_curve1()$y,
                        type='scatter', mode = 'lines', name = 'Interpolated 1',
                          line = list(color = 'red'))
       }
       py 
    })
    
    
    # observe and edit the dragged values 
    
    observe({
        ed <- event_data("plotly_relayout")
        ed
        shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed))]
        if (length(shape_anchors) != 2) return()
        row_index <- unique(readr::parse_number(names(shape_anchors)) + 1)
        pts <- as.numeric(shape_anchors)
        rv$points2$x[row_index] <- pts[1]
        rv$points2$y[row_index] <- pts[2]
        rv$points2$type[row_index] <- "dragged"
    })
    

    selectedPoints <- reactive({
        # A workaround to deal with case where click_points() has zero rows
        data.frame(type = rep("click", nrow(click_points())), click_points(), 
                   stringsAsFactors = FALSE) %>% 
            filter(x >= input$xmin , x <= input$xmax, y >= 0, y <= 1 ) # remove points outside the range
    })
    
    output$datatab <- renderDataTable({
        datatable(selectedPoints(),
                  options = list(pageLength = 5, lengthChange=FALSE)) %>%
            formatRound(c(2:3), 3)
    })
    
    
    output$datatab2 <- renderDataTable({
        datatable(filter(rv$points2, x>=input$xmin, x <= input$xmax, y >=0, y <=1), ## remove points dragged outside of plot region 
                  options = list(pageLength = 5, lengthChange=FALSE)) %>%
            formatRound(c(2:3), 3)
    })
    
    # Download the dragged points
    
    output$downloadData <- downloadHandler(
        
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste0(paste(Sys.Date(), "my-cdf", input$xmin, input$xmax, sep = "-"), ".csv")
        },
        
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            
            # Write to a file specified by the 'file' argument
            write.table(filter(rv$points2, x>=input$xmin, x <= input$xmax, y >=0, y <=1), file, sep = ",",
                        row.names = FALSE)
        }
    )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
