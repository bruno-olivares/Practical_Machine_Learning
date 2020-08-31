
library(shiny)
library(ggplot2)
data(quakes)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    logi <- reactive({
        (quakes$mag>=input$magnitude[1] & quakes$mag<=input$magnitude[2]) &
            (quakes$depth>=input$depth[1] & quakes$depth<=input$depth[2])
    })

    output$distPlot1 <- renderPlot({
          qplot(lat,long, colour = depth, size = mag, data = quakes[logi(),])
    })
    
    output$distPlot2 <- renderPlot({
        qplot(stations,mag, data = quakes[logi(),],
              geom = c("point","smooth"), method = "lm", se = FALSE)
    })
    
    model <- reactive({
        lm(mag ~ stations, data = quakes[logi(),])
    })
    
    output$intercept <- renderText({
        model()['coefficients'][[1]][[1]] 
    })

    output$slope <- renderText({
        model()['coefficients'][[1]][[2]] 
    })
})
