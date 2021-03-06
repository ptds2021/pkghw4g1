
library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Area Estimation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "seed",
                         label = "Set a Seed:",
                         value = 10),
            sliderInput(inputId = "B",
                        label = "Set the number of points to be simulated:",
                        min = 1,
                        max = 1000000,
                        value = 2000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot"),
            textOutput("time"),
            textOutput("area")
        )
    )
))



