#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Abalone Dataset"),
  
  # Sidebar with CheckGroup for the dataset
  sidebarLayout(
    sidebarPanel(
        checkboxGroupInput("checkGroup",
                                 h3("Sample by type"),
                                 choices = list("Male" = 'M',
                                                "Female" = 'F',
                                                "Infant" = 'I'),
                                 selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
) )
