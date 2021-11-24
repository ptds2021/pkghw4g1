
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pkghw4g1)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    simulate <- reactive({
        # simulate the area of S and measure the time here
        options(digits.secs=3)
        start.time <- Sys.time()
        estimate_area(B = input$B, seed = input$seed)
        end.time <- Sys.time()
        execution_time <- end.time-start.time
    })

    output$plot <- renderPlot({
        # plot area
        plot(simulate())
    })

    output$time <- renderText({
        # extract the time of the execution
        paste("The necessary execution time was:", execution_time)
    })

    output$area <- renderText({
        # extract the estimated value
        area_s <- simulate()
        paste("The estimated shape of the area is:", area_s)
    })

})
