#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required
shinyServer(function(input, output) {
  
  output$co2plot <- renderPlot({
    #read data
    C02Worldwide<-load("C02Worldwide.Rdata")

    input$ord
    input$checkbox
    
    x    <- Co2World$YearDecimal
    y    <- Co2World$Value
    
    
    plot(x, y, xlab ="Year", ylab = "CO2 ppm", col = input$colour, main = "CO2 Levels Worldwide Average")
    
      if(input$checkbox == TRUE){
      lines(ksmooth(x,y,bandwidth = input$ord), col="red", lwd=3)
       }
  
  })
######################################  
  output$co2northplot <- renderPlot({
    #read data
    C02NorthernHemisphere<-load("C02NorthernHemisphere.Rdata")
    
    if(input$p=='a'){
      i<-2
    }
    
    if(input$p=='b'){
      i<-4
    }
    
    if(input$p=='c'){
      i<-6
    }
    
    if(input$p=='d'){
      i<-8
    }
    
    if(input$p=='e'){
      i<-10
    }
    
    if(input$p=='f'){
      i<-12
    }
    
    if(input$p=='g'){
      i<-14
    }
    
    if(input$p=='h'){
      i<-16
    }
    
    if(input$p=='i'){
      i<-18
    }
    
    if(input$p=='j'){
      i<-20
    }
    
    if(input$p=='k'){
      i<-22
    }
    
    input$ord
    input$checkbox
    
    x    <- Co2North$YearDecimal
    y    <- Co2North[, i] 
    
    plot(x, y, xlab ="Year", ylab = "CO2 ppm", col = input$colour, main = "CO2 Levels Northern Hemisphere Average")
    
    if(input$checkbox == TRUE){
      lines(ksmooth(x,y,bandwidth = input$ord), col="red", lwd=3)
    }
    
  })
  
######################################    
  output$temperature <- renderPlot({
    CanadianMaxTemp<-load("CanadianMaxTemp.Rdata")
    CanadianMeanTemp<-load("CanadianMeanTemp.Rdata")
    CanadianMinTemp <- load("CanadianMinTemp.Rdata")
    
    if(input$p2=='a'){
        i = "CHARLOTTETOWN"
    }
    
    if(input$p2=='b'){
        i = "EDMONTON"
    }
    
    if(input$p2=='c'){
        i = "FREDERICTON"
    }
    
    if(input$p2=='d'){
        i = "HALIFAX"
    }
    
    if(input$p2=='e'){
        i = "IQALUIT"
    }
    
    if(input$p2=='f'){
        i = "OTTAWA"
    }
    
    if(input$p2=='g'){
        i = "QUEBEC"
    }
    
    if(input$p2=='h'){
        i = "REGINA"
    }
    
    if(input$p2=='i'){
        i = "SAINT JOHN"
    }
    
    if(input$p2=='j'){
        i = "TORONTO"
    }
    
    if(input$p2=='k'){
        i = "VICTORIA"
    }
    
    if(input$p2=='l'){
        i = "WINNIPEG"
    }
    
    if(input$p2=='m'){
        i = "WHITEHORSE"
    }
    
    if(input$p2=='n'){
        i = "YELLOWKNIFE"
    }
    

    Mean=subset(MeanTemp, MeanTemp$Winter!="-9999.9" & MeanTemp$Summer!="-9999.9" & MeanTemp$Year>"1980" & MeanTemp$`InfoTemp[2]`== i)
    Total=subset(MeanTemp, MeanTemp$Winter!="-9999.9" & MeanTemp$Summer!="-9999.9" & MeanTemp$Year>"1980")


    a    <- Mean$Year
    b    <- Mean$Winter
    c    <- Mean$Year
    d    <- Mean$Summer

    par(mar=c(4,4,4,4))
    plot(a, b, col = "blue", ylim = c(-40,40), xlab = "Year", ylab = "Degree Celsius", main = "Winter/Summer Temperature in Canada")
    lines(a,b, col = "blue")
    points(c, d, col = "red")
    lines(c, d, col = "red")
    legend(x="topleft", pch = c(1,1), lty = c(1,1), col = c("red","blue"), c("Average Summer","Average Winter"), cex = 0.75)
    
    if(input$checkbox2 == TRUE){
      abline(h=mean(Total$Winter), col="green", lwd=3)
      abline(h=mean(Total$Summer), col="pink", lwd=3)
      legend(x="topleft", pch = c(1,1,NA,NA), lty = c(1,1,1,1), lwd = c(1,1,3,3),col = c("red","blue","pink","green"), c("Average Summer","Average Winter","Total Summer Average","Total Winter Average"), cex = 0.75)
    }
    
  })
  
  output$avgsnow <- renderPlot({
    CanadianAvgSnow <- load("CanadianAvgSnow.Rdata")
    
    if(input$p3=='a'){
      i = "CHARLOTTETOWN"
    }
    
    if(input$p3=='b'){
      i = "EDMONTON"
    }
    
    if(input$p3=='c'){
      i = "FREDERICTON"
    }
    
    if(input$p3=='d'){
      i = "HALIFAX STANFIELD"
    }
    
    if(input$p3=='e'){
      i = "IQALUIT"
    }
    
    if(input$p3=='f'){
      i = "OTTAWA"
    }
    
    if(input$p3=='g'){
      i = "REGINA"
    }
    
    if(input$p3=='h'){
      i = "SAINT JOHN"
    }
    
    if(input$p3=='i'){
      i = "TORONTO"
    }
    
    if(input$p3=='j'){
      i = "VICTORIA"
    }
    
    if(input$p3=='k'){
      i = "WINNIPEG RICHARDSON"
    }
    
    if(input$p3=='l'){
      i = "WHITEHORSE"
    }
    
    if(input$p3=='m'){
      i = "YELLOWKNIFE"
    }
    
    Annual=subset(AllSnow, AllSnow$Annual!="-9999.9" & AllSnow$Year>"1980" & AllSnow$`InfoTemp[2]`== i)
    Total_snow=subset(AllSnow, AllSnow$Annual!="-9999.9" & AllSnow$Year>"1980")

    bins <- seq(min(Annual$Annual), max(Annual$Annual), length.out = input$bins1 + 1)
  
    
    hist(Annual$Annual, breaks = bins, xlim = c(0,800), col = "darkgray", border = 'white', xlab = "Inches" ,main = "Average Annual Snow Fall in Canada")
    box()
  
      if(input$checkbox3 == TRUE){
      abline(v=mean(Total_snow$Annual), col="blue", lwd = 3)   
      legend(x="topleft", lty = c(1,1,1), col = c("darkgray","blue"), c("Histogram", "Average Annual Snow Fall Across All Cities"), cex = 0.75)
        
    }
    if(input$checkbox4 == TRUE){
      hist(Annual$Annual, breaks = bins, probability = TRUE, xlim = c(0,800), col = 'darkgray', border = 'white', xlab = "Inches" ,main = "Average Annual Snow Fall in Canada")
      box()
      lines(density(Annual$Annual), col = "red")
      legend(x="topleft", lty = c(1,1), col = c("darkgray","red"), c("Histogram", "Density Bound"), cex = 0.75)
      
    }
    if(input$checkbox5 == TRUE){
      hist(Annual$Annual, breaks = bins, probability = TRUE, xlim = c(0,800), col = 'darkgray', border = 'white', xlab = "Inches" ,main = "Average Annual Snow Fall in Canada")
      box()
      reflected=c(Annual$Annual,-Annual$Annual)
      density2=density(reflected, from = 0)
      lines(density2$x, density2$y*2, col = "red")
      legend(x="topleft", lty = c(1,1), col = c("darkgray","red"), c("Histogram", "Density Bound"), cex = 0.75)
      
    }
    if(input$checkbox3 == TRUE & input$checkbox4 == TRUE){
      hist(Annual$Annual, breaks = bins, probability = TRUE, xlim = c(0,800), col = 'darkgray', border = 'white', xlab = "Inches" ,main = "Average Annual Snow Fall in Canada")
      box()
      abline(v=mean(Total_snow$Annual), col="blue", lwd = 3)  
      lines(density(Annual$Annual), col = "red")
      legend(x="topleft", lty = c(1,1,1), col = c("darkgray","blue","red"), c("Histogram", "Average Annual Snow Fall Across All Cities", "Density Bound"), cex = 0.75)
    }
    if(input$checkbox3 == TRUE & input$checkbox5 == TRUE){
      hist(Annual$Annual, breaks = bins, probability = TRUE, xlim = c(0,800), col = 'darkgray', border = 'white', xlab = "Inches" ,main = "Average Annual Snow Fall in Canada")
      box()
      abline(v=mean(Total_snow$Annual), col="blue", lwd = 3)   
      reflected=c(Annual$Annual,-Annual$Annual)
      density2=density(reflected, from = 0)
      lines(density2$x, density2$y*2, col = "red")
      legend(x="topleft", lty = c(1,1,1), col = c("darkgray","blue","red"), c("Histogram", "Average Annual Snow Fall Across All Cities", "Density Reflected Bound"), cex = 0.75)
      
    }

    })
  output$avgrain <- renderPlot({
    CanadianPrecip<-load("CanadianPrecip.Rdata")
    
    if(input$p3=='a'){
      i = "CHARLOTTETOWN"
    }
    
    if(input$p3=='b'){
      i = "EDMONTON"
    }
    
    if(input$p3=='c'){
      i = "FREDERICTON"
    }
    
    if(input$p3=='d'){
      i = "HALIFAX STANFIELD"
    }
    
    if(input$p3=='e'){
      i = "IQALUIT"
    }
    
    if(input$p3=='f'){
      i = "OTTAWA"
    }
    
    if(input$p3=='g'){
      i = "REGINA"
    }
    
    if(input$p3=='h'){
      i = "SAINT JOHN"
    }
    
    if(input$p3=='i'){
      i = "TORONTO"
    }
    
    if(input$p3=='j'){
      i = "VICTORIA"
    }
    
    if(input$p3=='k'){
      i = "WINNIPEG RICHARDSON"
    }
    
    if(input$p3=='l'){
      i = "WHITEHORSE"
    }
    
    if(input$p3=='m'){
      i = "YELLOWKNIFE"
    }
    
    Annual_precip=subset(AllPrecip, AllPrecip$Annual!="-9999.9" & AllPrecip$Year>"1980" & AllPrecip$`InfoTemp[2]`== i)
    Total_precip=subset(AllPrecip, AllPrecip$Annual!="-9999.9" & AllPrecip$Year>"1980")
    
    bins <- seq(min(Annual_precip$Annual*0.0393701), max(Annual_precip$Annual*0.0393701), length.out = input$bins2 + 1)
    
    hist(Annual_precip$Annual*0.0393701, breaks = bins, xlim = c(0,100), col = "darkgray", border = 'white', xlab = "Inches" ,main = "Average Annual Precipitation in Canada")
    
    box()
    if(input$checkbox3 == TRUE){
      abline(v=mean(Total_precip$Annual*0.0393701), col="blue", lwd = 3)   
      legend(x="topleft", lty = c(1,1,1), col = c("darkgray","blue"), c("Histogram", "Average Annual Snow Fall Across All Cities"), cex = 0.75)
      
    }
    if(input$checkbox4 == TRUE){
      hist(Annual_precip$Annual*0.0393701, breaks = bins, probability = TRUE, xlim = c(0,100), col = 'darkgray', border = 'white', xlab = "Inches" ,main = "Average Annual Precipitation in Canada")
      box()
      lines(density(Annual_precip$Annual*0.0393701), col = "red")
      legend(x="topleft", lty = c(1,1), col = c("darkgray","red"), c("Histogram", "Density Bound"), cex = 0.75)
      
    }
    if(input$checkbox5 == TRUE){
      hist(Annual_precip$Annual*0.0393701, breaks = bins, probability = TRUE, xlim = c(0,100), col = 'darkgray', border = 'white', xlab = "Inches" ,main = "Average Annual Precipitation in Canada")
      box()
      reflected=c(Annual_precip$Annual*0.0393701,-Annual_precip$Annual*0.0393701)
      density2=density(reflected, from = 0)
      lines(density2$x, density2$y*2, col = "red")
      legend(x="topleft", lty = c(1,1), col = c("darkgray","red"), c("Histogram", "Density Bound"), cex = 0.75)
      
    }
    if(input$checkbox3 == TRUE & input$checkbox4 == TRUE){
      hist(Annual_precip$Annual*0.0393701, breaks = bins, probability = TRUE, xlim = c(0,100), col = 'darkgray', border = 'white', xlab = "Inches" , main = "Average Annual Precipitation in Canada")
      box()
      abline(v=mean(Total_precip$Annual*0.0393701), col="blue", lwd = 3)  
      lines(density(Annual_precip$Annual*0.0393701), col = "red")
      legend(x="topleft", lty = c(1,1,1), col = c("darkgray","blue","red"), c("Histogram", "Average Annual Precipitation Across All Cities", "Density Bound"), cex = 0.75)
    }
    if(input$checkbox3 == TRUE & input$checkbox5 == TRUE){
      hist(Annual_precip$Annual*0.0393701, breaks = bins, probability = TRUE, xlim = c(0,100), col = 'darkgray', border = 'white', xlab = "Inches" , main = "Average Annual Precipitation in Canada")
      box()
      abline(v=mean(Total_precip$Annual*0.0393701), col="blue", lwd = 3)   
      reflected=c(Annual_precip$Annual*0.0393701,-Annual_precip$Annual*0.0393701)
      density2=density(reflected, from = 0)
      lines(density2$x, density2$y*2, col = "red")
      legend(x="topleft", lty = c(1,1,1), col = c("darkgray","blue","red"), c("Histogram", "Average Annual Precipitation Across All Cities", "Density Reflected Bound"), cex = 0.75)
      
    }
    
  })
  
  output$about    <- renderText({
    HTML("<br> <b>Welcome to my first shiny app!</b> <br>
         My name is David Ling. I am a Data Science student in Simon Fraser University. <br>
         This page is to show you how awesome data can show us the evidence which gives us insights of the future. <br>
         Climate change is one of the biggest issues around the world. This has riased a significant awareness and is crucial to everything of our future. <br>
         So I am presenting some data visualization and analysis in the shiny app and hopefully this will raise your awareness on this issue as well! <br>
         <img src='protrait.jpg', width='100', height='100', alt='This is alternate text'/>")
    
    #img(src='protrait.jpg', width='100px', height='100px', alt='This is alternate text')
    
  
    })
  output$significance    <- renderText({
    HTML("<br> This page is to showcase the trend of global warming based on variety of data sets from Environment and Climate Change Data provided by Statistics Canada. <br> 
         The Co2 plots show the average of Carbon Dioxide surface level worldwide and Northern Hemisphere. <br> The temperature plot shows the average temperture in summer and winter across Canada from 1980 and onwards. <br> 
         The snowfall plot shows the frequeny of inches of snow throughout each year from 1980 and onwards.")
  })
  output$reference    <- renderText({
  HTML(" <br> <b>CO2 Worldwide/ Northern Hemisphere surface average data:</b> <br>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Dlugokencky, E.J., K.W. Thoning,
P.M. Lang, and P.P. Tans (2017), NOAA Greenhouse Gas Reference from
Atmospheric Carbon Dioxide Dry Air Mole Fractions from the NOAA ESRL
Carbon Cycle Cooperative Global Air Sampling Network. <br>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Data Source: <a href='ftp://aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/'>ftp:
//aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/</a> <br> <br>
      <b>Canadian Weather Data:</b> <br>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Data Source: <a href='http://data.ec.gc.ca/data/
climate/scientificknowledge/adjusted-and-homogenized-canadian-climate-data-ahccd/'>http://data.ec.gc.ca/data/
climate/scientificknowledge/adjusted-and-homogenized-canadian-climate-data-ahccd/</a>")
  })
  
  
})
