
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Eartquakes off Fiji"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("magnitude",
                        "Magnitude (MB):",
                        min = min(quakes$mag),
                        max = max(quakes$mag),
                        value = c(min,max)),
            
            sliderInput("depth",
                        "Depth (km):",
                        min = min(quakes$depth),
                        max = max(quakes$depth),
                        value = c(min,max))
            
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", br(), 
                                 h3("Here is the plot with events choose"),
                                 plotOutput("distPlot1")),
                        tabPanel("Linear fit", br(), 
                                 h3("Here there are a linear regression between stations and magnitud"),
                                 plotOutput("distPlot2"),
                                 h3("Intercept"),textOutput("intercept"),
                                 h3("Slope"),textOutput("slope"),
                                 h3("Maybe the results are very obvious, but I just want practice")))
            
        )
    )
))
