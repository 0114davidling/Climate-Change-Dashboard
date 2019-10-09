#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Climate Change Study and Data Analysis in Canada"),
  
  # Sidebar with a slider input
sidebarLayout(
    
sidebarPanel(
      
 conditionalPanel(condition="input.tabselected==1",
                  
      selectInput("p", "Select Column of CO2 Northern Hemisphere Dataset:",
                  choices = c("Latitude30value"='a', "Latitude33value"='b', "Latitude37value"='c', "Latitude41value"='d', "Latitude44value"='e', "Latitude49value"='f', "Latitude53value"='g', "Latitude58value"='h', "Latitude64value"='i', "Latitude72value"='j', "Latitude90value"='k')),
      radioButtons("colour", "Select the colour of plot:",
                  choices = c("Green", "Blue", "Yellow"), selected = "Green"),
      br(),

      checkboxInput("checkbox", "Add Moving Average", FALSE), uiOutput("conditionalInput"),
      
      #hidden moving average panel 
      conditionalPanel(
        condition = "input.checkbox == true",
        h4("Moving Average",style = "color:black"),
        sliderInput("ord","Order Size:",min = 1, max = 100, step= 1, value = 15)
      )
 ),
 conditionalPanel(condition="input.tabselected==2", selectInput("p2", "Select Cities in Canada",
                                                                choices = c("Charlottetown, Prince Edward Island"='a', "Edmonton, Alberta"='b', "Fredericton, New Brunswick"='c', "Halifax, Nova Scotia"='d', "Iqaluit, Nunavut"='e', "Ottawa, Canada"='f', "Québec, Quebec"='g', "Regina, Saskatchewan"='h', "St. John's, Newfoundland and Labrador"='i', "Toronto, Ontario"='j', "Victoria, British Columbia"='k', "Winnipeg, Manitoba"='l', "Whitehorse, Yukon"='m', "Yellowknife, Northwest Territories"='n')), 
  checkboxInput("checkbox2", "Add Total Average Across Canada", FALSE)),
 conditionalPanel(condition="input.tabselected==3", 
                  selectInput("p3", "Select Province of Cities in Canada",
                  choices = c("Alberta"='a', "British Columbia"='b', "Manitoba"='c', "New Brunswick"='d', "New Foundland and Labrador"='e', "Nova Scotia"='f', "Northwest Territories"='g', "Nunavut"='h', "Ontario"='i', "Prince Edward Island"='j', "Quebec"='j', "Saskatchewan"='l', "Yukon"='m')),
        
                  checkboxInput("checkbox3", "Add Total Average Across Canada", FALSE),
                  checkboxInput("checkbox4", "Add Linear regression", FALSE), 
                  conditionalPanel(condition = "input.checkbox4 == true", checkboxInput("checkbox5", "Zoom in?", FALSE)))
                  
), #\sidebarPane
    
    mainPanel(
    tabsetPanel(
      tabPanel("About", htmlOutput("about")),
      tabPanel("CO2 Level", value = 1, plotOutput("co2plot"), plotOutput("co2northplot")),
      tabPanel("Temperature", value = 2, plotOutput("temperature"), plotOutput("annualtemp")),
      tabPanel("Snowfall/Rain", value = 3, plotOutput("avgsnow"), plotOutput("avgrain")),
      tabPanel("Significance", htmlOutput("significance")),
      tabPanel("Reference", htmlOutput("reference")),
      id = "tabselected"
      )
    
    )
), #\sidebarlayout
tags$footer("April 2019, Data Science SFU", align = "left", style = "
            position:relative;
            bottom:0;
            width:100%;
            height:50px;   /* Height of the footer */
            color: black;
            padding: 10px;
            z-index: 1000;")
))
