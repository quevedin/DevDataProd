#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
abalone <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", 
                    header = FALSE)
names(abalone) <- c("sex", "length", "diameter", "height", "weight.whole", "weight.shucked", 
                    "weight.viscera", "weight.shell", "rings")


# Define server logic required to draw the plot
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    #input$checkGroup
    lev=input$checkGroup
    if (length(lev)==0){lev=levels(abalone$sex)}
    data=subset(abalone, sex %in% lev)
    fit <- lm( rings~length, data)
    
    ggplot(data) + aes(length, rings, color = sex) + geom_point() + 
      labs(x = "Shell Length", y = "Number of Rings", title = "Number of Rings vs Length", color = "Sex of Abalone") + 
      theme(legend.position = c(0, 1), legend.justification = c(0, 1), legend.background = element_rect(fill = "white", 
                                                                                                        color = "black")) + 
      scale_color_hue(labels = c("Female", "Infant", "Male"))+
      geom_smooth(method = "lm",inherit.aes = FALSE,aes(length, rings),color='black')
  })
})
