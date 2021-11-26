
library(shiny)
library(pkghw4g1)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    simulate <- reactive({
        # simulate the area of S and measure the time here
        estimate_area(B = input$B, seed = input$seed)
    })

    output$plot <- renderPlot({
        # plot area
        plot_area(simulate())
    })

    output$time <- renderText({
        # extract the time of the execution
        start_time <- Sys.time()
        simulate()
        end_time <- Sys.time()
        execution_time <- end_time - start_time
        paste("The necessary execution time was:", execution_time)
    })

    output$area <- renderText({
        # extract the estimated value
        area_s <- simulate()$estimated_area
        paste("The estimated shape of the area is:", area_s)
    })

})
