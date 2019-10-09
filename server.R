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
      lines(ksmooth(x, y, bandwidth = input$ord), col="red", lwd=3)
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
      lines(ksmooth(x, y, bandwidth = input$ord), col="red", lwd=3)
    }
    
  })
  
######################################    
  
  output$temperature <- renderPlot({

    CanadianMeanTemp<-load("CanadianMeanTemp.Rdata")
  
    
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


    plot(a, b, col = "blue", ylim = c(-40,40), xlab = "Year", ylab = "Degree Celsius", main = "Winter/Summer Temperature of City in Canada")
    lines(a,b, col = "blue")
    points(c, d, col = "red")
    lines(c, d, col = "red")
    legend(x="topleft", pch = c(1,1), lty = c(1,1), col = c("red","blue"), c("Average Summer of City","Average Winter of City"), cex = 0.75)
    
    if(input$checkbox2 == TRUE){
      abline(h=mean(Total$Winter), col="green", lwd=3)
      abline(h=mean(Total$Summer), col="pink", lwd=3)
      legend(x="topleft", pch = c(1,1,NA,NA), lty = c(1,1,1,1), lwd = c(1,1,3,3),col = c("red","blue","pink","green"), c("Average Summer of City","Average Winter of City","Average Summer Across Canada","Average Winter Across Canada"), cex = 0.75)
    }
    
  })
  
 ####################################### 
  output$annualtemp <- renderPlot({
    CanadianMeanTemp<-load("CanadianMeanTemp.Rdata")
    
    Mean=subset(MeanTemp, MeanTemp$Annual!="-9999.9")
    plot(Mean$Year, Mean$Annual, col = "darkgray", xlab = "Year", ylab = "Degree Celsius", main = "Annual Average Temperature Across Canada")
    
  })
  
 ####################################### 
  output$avgsnow <- renderPlot({
    CanadianAvgSnow <- load("CanadianAvgSnow.Rdata")
    
  
    if(input$p3=='a'){
      i = "AB"
    }
    
    if(input$p3=='b'){
      i = "BC"
    }
    
    if(input$p3=='c'){
      i = "MB"
    }
    
    if(input$p3=='d'){
      i = "NB"
    }
    
    if(input$p3=='e'){
      i = "NL"
    }
    
    if(input$p3=='f'){
      i = "NS"
    }
    
    if(input$p3=='g'){
      i = "NT"
    }
    
    if(input$p3=='h'){
      i = "NU"
    }
    
    if(input$p3=='i'){
      i = "ON"
    }
    
    if(input$p3=='j'){
      i = "PE"
    }
    
    if(input$p3=='k'){
      i = "QC"
    }
    
    if(input$p3=='l'){
      i = "SK"
    }
    
    if(input$p3=='m'){
      i = "YK"
    }
    
    Annual=subset(AllSnow, AllSnow$Annual!="-9999.9" & AllSnow$Year>"1980" & AllSnow$`InfoTemp[3]`== i)

    p<-ggplot(Annual,aes(x=Year,y=Annual))+geom_point()+labs(title="Annual Snow Fall of Cities Since 1980")+ylab("Inches")
    print(p)

    if(input$checkbox3 == TRUE & input$checkbox4 == FALSE){
     p<- ggplot(Annual,aes(Year,Annual))+geom_point()+geom_line(aes(x=Year,y=mean(Annual), color = "Average Snow Across Canada"))+scale_colour_manual("",values = c("Average Snow Across Canada" = "red"))+labs(title="Annual Snow Fall of Cities Since 1980")+ylab("Inches")
     print(p)
    }
    if(input$checkbox4 == TRUE & input$checkbox3 == FALSE){
     p<-ggplot(Annual,aes(Year,Annual))+geom_point()+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Snow Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Snow Fall of Cities Since 1980")+ylab("Inches")
     print(p)
    }
    if(input$checkbox5 == TRUE & input$checkbox4 == TRUE){
     p<-ggplot(Annual,aes(Year,Annual))+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Snow Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Snow Fall of Cities Since 1980")+ylab("Inches")
     print(p)
    }
    if(input$checkbox3 == TRUE & input$checkbox4 == TRUE){
     p<-ggplot(Annual,aes(Year,Annual))+geom_point()+geom_line(aes(x=Year,y=mean(Annual), color = "Average Snow Across Canada"))+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Snow Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Snow Fall of Cities Since 1980")+ylab("Inches")
     print(p) 
    }
    if(input$checkbox3 == TRUE & input$checkbox5 == TRUE){
      p<- ggplot(Annual,aes(Year,Annual))+geom_point()+geom_line(aes(x=Year,y=mean(Annual), color = "Average Snow Across Canada"))+scale_colour_manual("",values = c("Average Snow Across Canada" = "red"))+labs(title="Annual Snow Fall of Cities Since 1980")+ylab("Inches")
      print(p)
    }
    if(input$checkbox3 == TRUE & input$checkbox4 == TRUE & input$checkbox5 == TRUE){
     p<-ggplot(Annual,aes(Year,Annual))+geom_line(aes(x=Year,y=mean(Annual), color = "Average Snow Across Canada"))+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Snow Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Snow Fall of Cities Since 1980")+ylab("Inches")
     print(p)
    }

    })
  
 ####################################### 
  output$avgrain <- renderPlot({
    
    CanadianPrecip<-load("CanadianPrecip.Rdata")
    
    if(input$p3=='a'){
      i = "AB"
    }
    
    if(input$p3=='b'){
      i = "BC"
    }
    
    if(input$p3=='c'){
      i = "MB"
    }
    
    if(input$p3=='d'){
      i = "NB"
    }
    
    if(input$p3=='e'){
      i = "NL"
    }
    
    if(input$p3=='f'){
      i = "NS"
    }
    
    if(input$p3=='g'){
      i = "NT"
    }
    
    if(input$p3=='h'){
      i = "NU"
    }
    
    if(input$p3=='i'){
      i = "ON"
    }
    
    if(input$p3=='j'){
      i = "PE"
    }
    
    if(input$p3=='k'){
      i = "QC"
    }
    
    if(input$p3=='l'){
      i = "SK"
    }
    
    if(input$p3=='m'){
      i = "YK"
    }
    
    Annual_precip=subset(AllPrecip, AllPrecip$Annual!="-9999.9" & AllPrecip$Year>"1980" & AllPrecip$`InfoTemp[3]`== i)
    
    p<-ggplot(Annual_precip,aes(x=Year,y=Annual*0.0393701))+geom_point()+labs(title="Annual Precipitation of Cities Since 1980")+ylab("Inches")
    print(p)
    
    if(input$checkbox3 == TRUE & input$checkbox4 == FALSE){
      p<- ggplot(Annual_precip,aes(Year,Annual*0.0393701))+geom_point()+geom_line(aes(x=Year,y=mean(Annual*0.0393701), color = "Average Rain Across Canada"))+scale_colour_manual("",values = c("Average Rain Across Canada" = "red"))+labs(title="Annual Precipitation of Cities Since 1980")+ylab("Inches")
      print(p)
    }
    if(input$checkbox4 == TRUE & input$checkbox3 == FALSE){
      p<-ggplot(Annual_precip,aes(Year,Annual*0.0393701))+geom_point()+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Rain Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Precipitation of Cities Since 1980")+ylab("Inches")
      print(p)
    }
    if(input$checkbox5 == TRUE & input$checkbox4 == TRUE){
      p<-ggplot(Annual_precip,aes(Year,Annual*0.0393701))+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Rain Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Precipitation of Cities Since 1980")+ylab("Inches")
      print(p)
    }
    if(input$checkbox3 == TRUE & input$checkbox4 == TRUE){
      p<-ggplot(Annual_precip,aes(Year,Annual*0.0393701))+geom_point()+geom_line(aes(x=Year,y=mean(Annual*0.0393701), color = "Average Rain Across Canada"))+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Rain Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Precipitation of Cities Since 1980")+ylab("Inches")
      print(p) 
    }
    if(input$checkbox3 == TRUE & input$checkbox5 == TRUE){
      p<- ggplot(Annual_precip,aes(Year,Annual*0.0393701))+geom_point()+geom_line(aes(x=Year,y=mean(Annual*0.0393701), color = "Average Rain Across Canada"))+scale_colour_manual("",values = c("Average Rain Across Canada" = "red"))+labs(title="Annual Precipitation of Cities Since 1980")+ylab("Inches")
      print(p)
    }
    if(input$checkbox3 == TRUE & input$checkbox4 ==TRUE & input$checkbox5 == TRUE){
      p<-ggplot(Annual_precip,aes(Year,Annual*0.0393701))+geom_line(aes(x=Year,y=mean(Annual*0.0393701), color = "Average Rain Across Canada"))+geom_smooth(method = "lm", se = FALSE, aes(color="Trend"), size=0.5)+scale_colour_manual("",values = c("Average Rain Across Canada" = "red", "Trend"="blue"))+labs(title="Annual Precipitation of Cities Since 1980")+ylab("Inches")
      print(p)
    }
  })
  
 #######################################  
   output$about    <- renderText({
    
    HTML("<div>
         <br> <b>Welcome to my first shiny app!</b> <br>
         My name is David Ling and I am a Data Science student from Simon Fraser University. <br>
         This website is to showcase how awesome data can reveal us the evidence and gives insights of the future.
         </div> <br>
         <div>
         Climate change is one of the biggest issues around the world. This has raised significant awareness around the world and is crucial to everything for our future and next generation. <br>
         So I am presenting some data visualization and analysis in the shiny app and hopefully this will raise your awareness on this issue as well! <br>
         </div> <br>
         <div>
         <img src='protrait.jpg', width='210', height='auto', alt='image'/> <br>
         David Ling <br>
         Email: dla150@sfu.ca <br>
         <a href='https://www.linkedin.com/in/david-ling-01171a132/'>LinkedIn</a>
         </div>")
    })
  
  output$significance    <- renderText({
    HTML("<div><br> 
         The increase of greenhouse gas emissions is one of the most important problems that are causing global climate change.
         Carbon Dioxide is the first factor in climate change. <br>
         In the CO2 Level plots, you can see CO2 emissions are in proportional increase over the year with periodically seasonal fratuation which can be observed more obviously in higer latitude area as lower temperature water stores CO2 more easily. <br>
         <img src='CO2level.png', width='500', height='350', alt='CO2 level'/>
         </div> <br>
         <div>
         The mean temperature of winter and summer season throughout major cities in Canada has shown the evidence of climate change.
         Average temperature in most major cities are seen fratuated renferencing from the mean temperature across Canada. In fact, the CO2 emissions are higher in the major cities as air pollution level.
         In city Toronto, we can see the fratuation of winter temperature in the recent past years.
         <img src='temperature.png', width='500', height='350', alt='temperature'/> <br>
         The annual temperature across Canada has also shown the increase of both extreme temperature over the year.
         <img src='Annual Avg.png', width='500', height='350', alt='Annual Avg'/>
         </div> <br>
         <div>
         The snow and ice play an important role in the climate system. They help moderate climate through the reflection of energy from sunlight back into the atmosphere. <br>
         In fact, the warmer climate reduces snowfall and increases rainfall. <br>
         You can observe the differences in all the city of provinces by comparing its linear regression trend to the mean value across Canadian cities.<br>
         More importantly, we find that British Columbia shows the trend of decreasing amount of snowfall over the past 3 decades and went below the average of snowfall aross Canada at 1996.
         And it also shows the trend of increasing amount of rainfall over the past 3 decades and went above the average of rainfall across Canada at 1996.<br>
         <img src='Snow fall.png', width='500', height='350', alt='Snow fall'/> <br>
         <img src='Rain.png', width='500', height='350', alt='Rain'/>
         </div> <br>
         <div>
         Three evidence based on statistical data have provided insights of climate change. Hopefully, we can make actions on how to deal with climate change and how to prevent disasters from the warnings of global warming. 
         </div>
         ")
    })
  
  output$reference       <- renderText({
  HTML("<div>
       <br> <b>CO2 Worldwide/ Northern Hemisphere surface average data:</b> <br>
       &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Dlugokencky, E.J., K.W. Thoning,
       P.M. Lang, and P.P. Tans (2017), NOAA Greenhouse Gas Reference from
       Atmospheric Carbon Dioxide Dry Air Mole Fractions from the NOAA ESRL
       Carbon Cycle Cooperative Global Air Sampling Network.
       Data Source: <br> <a href='ftp://aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/'>ftp:
       //aftp.cmdl.noaa.gov/data/trace_gases/co2/flask/surface/</a>
       </div> <br>
       <b>Canadian Weather Data:</b> <br>
       <div>
       &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Data Source: <a href='http://data.ec.gc.ca/data/
       climate/scientificknowledge/adjusted-and-homogenized-canadian-climate-data-ahccd/'>http://data.ec.gc.ca/data/
       climate/scientificknowledge/adjusted-and-homogenized-canadian-climate-data-ahccd/</a>
       </div>
       ")
  })
  
})
