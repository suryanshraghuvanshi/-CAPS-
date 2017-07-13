# -CAPS-
This project is based on machine learning using R and Shiny for predicting the crime trend from past dataset by training them and predict the future crime by testing the same dataset using linear regression from certain parameters such as location, time &amp; date, crime type etc. 
library(shiny)
library(rCharts)
library(ggmap)
library(ggplot2)
library(dplyr)
library(leaflet.extras)
library(rMaps)
library(plyr)
library(data.table)
library(RColorBrewer)

#-------------------------------------------------------------------------------------------------------------------------------------

d <- read.csv("del.csv")


options(stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
#------------------------------------------------------------------Linear Regression--------------------------------------------------  

  output$mod1 <- renderPlot({
  
  options = reactiveValues(choose="")
  
  choice = d[,grep(options$choose, colnames(d))]
  
  input$city
  input$year
  input$variable
  
  pid <- input$city
  pid = as.numeric(as.character(pid))
  
  y <- input$year
  y = as.numeric(as.character(y))
  
  c <- input$variable
  c = as.numeric(as.character(c))
  
  #cr= as.numeric(as.character(cr))
  
  
  #cr= as.numeric(as.character(cr))
  
  d$cit <- d$Count[pid]
  

 mod1 <- lm( cit ~ viol2017, data = d)
         cit ~ viol2017

         class( cit ~ viol2017 )

        

         qplot( viol2017, predict(mod1), data = d, geom = "line")

         qplot( viol2017, cit, data = d) +
         geom_smooth(se = FALSE, method = lm)

      
         lm(cit ~ 1 + viol2017, data = d)
         lm(cit ~ viol2017, data = d)  
         lm(cit ~ viol2017 - 1, data = d)
         lm(cit ~ 0 + viol2017, data = d)

         resid(mod1)
    summary(mod1)
   plot(mod1)
   })
  
#----------------------------------------------------------------------------------------------------------------------------------------
  output$mod2 <- renderPlot({
    
    options = reactiveValues(choose="")
    
    choice = d[,grep(options$choose, colnames(d))]
    
    input$city
    input$year
    input$variable
    
    pid <- input$city
    pid = as.numeric(as.character(pid))
    
    y <- input$year
    y = as.numeric(as.character(y))
    
    c <- input$variable
    c = as.numeric(as.character(c))
    
    #cr= as.numeric(as.character(cr))
    
    
    #cr= as.numeric(as.character(cr))
    
    d$cit <- d$Count[pid]
    
    mod2 <- lm( cit ~ prop2017, data = d)
    cit ~ prop2017
    
    class( cit ~ prop2017 )
    
    
    
    qplot( prop2017, predict(mod2), data = d, geom = "line")
    
    qplot( prop2017, cit, data = d) +
      geom_smooth(se = FALSE, method = lm)
    
    
    lm(cit ~ 1 + prop2017, data = d)
    lm(cit ~ prop2017, data = d)  
    lm(cit ~ prop2017 - 1, data = d)
    lm(cit ~ 0 + prop2017, data = d)
    
    resid(mod2)
    summary(mod2)
    plot(mod2)
  })
#----------------------------------------------------------------------------------------------------------------------------------------
  output$mod3 <- renderPlot({
    
    options = reactiveValues(choose="")
    
    choice = d[,grep(options$choose, colnames(d))]
    
    input$city
    input$year
    input$variable
    
    pid <- input$city
    pid = as.numeric(as.character(pid))
    
    y <- input$year
    y = as.numeric(as.character(y))
    
    c <- input$variable
    c = as.numeric(as.character(c))
    
    #cr= as.numeric(as.character(cr))
    
    
    #cr= as.numeric(as.character(cr))
    
    d$cit <- d$Count[pid]
    mod3 <- lm( cit ~ violChan, data = d)
    cit ~ violChan
    
    class( cit ~ violChan )
    
    
    
    qplot( violChan, predict(mod3), data = d, geom = "line")
    
    qplot( violChan, cit, data = d) +
      geom_smooth(se = FALSE, method = lm)
    
    
    lm(cit ~ 1 + violChan, data = d)
    lm(cit ~ violChan, data = d)  
    lm(cit ~ violChan - 1, data = d)
    lm(cit ~ 0 + violChan, data = d)
    
    resid(mod3)
    summary(mod3)
    plot(mod3)
  })
#----------------------------------------------------------------------------------------------------------------------------------------
  output$mod4 <- renderPlot({
    
    options = reactiveValues(choose="")
    
    choice = d[,grep(options$choose, colnames(d))]
    
    input$city
    input$year
    input$variable
    
    pid <- input$city
    pid = as.numeric(as.character(pid))
    
    y <- input$year
    y = as.numeric(as.character(y))
    
    c <- input$variable
    c = as.numeric(as.character(c))
    
    #cr= as.numeric(as.character(cr))
    
    
    #cr= as.numeric(as.character(cr))
    
    d$cit <- d$Count[pid]
    mod4 <- lm( cit ~ propChan, data = d)
    cit ~ propChan
    
    class( cit ~ propChan )
    
    
    
    qplot( propChan, predict(mod4), data = d, geom = "line")
    
    qplot( propChan, cit, data = d) +
      geom_smooth(se = FALSE, method = lm)
    
    
    lm(cit ~ 1 + propChan, data = d)
    lm(cit ~ propChan, data = d)  
    lm(cit ~ propChan - 1, data = d)
    lm(cit ~ 0 + propChan, data = d)
    
    resid(mod4)
    summary(mod4)
    plot(mod4)
  })
#---------------------------------------------------CALCULATING SUM OF SQUARED ERROR------------------------------------------------------------------------
    output$SSET <- renderPrint({
      
      input$city
      pid <- input$city
      pid = as.numeric(as.character(pid))
      d$cit <- d$Count[pid]
      
      options = reactiveValues(choose="")
      mod1 <- lm(Count ~ viol2017 + violChan + prop2017 + propChan, data = d)
         
        
         SSE <- sum(mod1$residuals^2)
         SSE
  
   })
#------------------------------------------------------PREDICTIVE VALUES FOR ALL CITIES--------------------------------------------------------------------------------------------------
    output$predict <- renderPrint({
      input$city
      pid <- input$city
      pid = as.numeric(as.character(pid))
      d$cit <- d$Count[pid]
      
      options = reactiveValues(choose="")
        mod1 <- lm(Count ~ viol2017 + violChan + prop2017 + propChan, data = d)
  
        predictTest <- predict(mod1, newdata=d)
        predictTest
  
   })
#---------------------------------------------------- HEAT MAP FOR CRIMES OF ALL CITIES---------------------------------------------------------------------------------------------------
    nwl <- read.csv("del.csv", stringsAsFactors = FALSE)
    nwl$long1 = as.numeric(as.character(nwl$longg))
    nwl$lat1 = as.numeric(as.character(nwl$latt))
    
    crimes <- read.csv("del.csv", stringsAsFactors = FALSE)
    crimecounts <- as.data.frame(table(round(crimes$long,3),round(crimes$lat,3)))    
    crimecounts$long = as.numeric(as.character(crimecounts$Var1))
    crimecounts$lat = as.numeric(as.character(crimecounts$Var2))
    
    
    options = reactiveValues(choose="")
    
    
    display <- eventReactive(input$go, {
      input$city
      pid <- input$city
      pid= as.numeric(as.character(pid))
      
      plon <- nwl$long1[pid]
      plat <- nwl$lat1[pid]
      
      
      mp1 <- get_map(location = c(plon, plat), zoom = 12)
      #ggmap(mp1) + geom_point( data = crimecounts , aes(x = long, y=lat)) + scale_color_gradient (low="yellow", high="red")
      
      ggmap(mp1) + geom_point(data=crimecounts, aes( x= long, y = lat, color=Freq, size=Freq)) +
        scale_color_gradient(low='green', high='red')
      
      
      
  
      
      
    #  ggmap(mp1, extent = "device") + geom_density2d(data = crimecounts, 
    #  aes(x = long, y = lat), size = 0.3) + stat_density2d(data = crimecounts, 
    #  aes(x = long, y = lat, alpha = .5), size = 0.01, 
    #  bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
    #    scale_alpha(range = c(0, 0.3), guide = FALSE)
      
     
      
#--------this part of code is to mount the data for heat map using lealet where we use rjson for connection------------------------------
       # data(crimecounts, package="ggmap")
        #crimecounts <- as.data.table(crimecounts)
        
        
         # baseMap <- Leaflet$new()
          #baseMap$setView(c(plon,plan), 10)
          #baseMap$tileLayer(provider = "Esri.WorldStreetMap")
          #baseMap
  
        
        
          
          ## changed to use data.table for speed
          #crime_dat <- crimecounts[(lat != ""), .(count = .N), by=.(lat, long)]
          ## there's a blank in there somewhere
          
          ## I was having issues with toJSON, so I'm creating my own JSON
          #j <- paste0("[",crime_dat[,lat], ",", crime_dat[,long], ",", crime_dat[,count], "]", collapse=",")
         # j <- paste0("[",j,"]")
          
        #  tags$body(tags$script(HTML(sprintf("
             #                                var addressPoints = %s
            #                                 var heat = L.heatLayer(addressPoints).addTo(map)"
           #                                  , j
          #))))
      
#----------------------------------------------------------------------------------------------------------------------------------------       
        
  })
  
#----------------------------------------------------------------------------------------------------------------------------------------
      output$del.csv <- renderPlot({
    
        display()
  })
  
#-------------------------------------------------------------------------------------------------------------------------------------
      # output$my.map <- renderMap({
    
       #my.map <- Leaflet$new()
       #my.map$setView(c(28.4236173,77.0764361), 10)
       #my.map$tileLayer(provider = "Esri.WorldStreetMap")
       #my.map
    
     #})
  
#-------------------------------------------------------------------------------------------------------------------------------------
      observeEvent(input$go,{
    
       name_link = switch(input$variable, "Violent Crime"="viol", "Property Crime"="prop")
      
       options$choose = paste(name_link, substr(input$year,1,4), sep="")
      
       choice = d[,grep(options$choose, colnames(d))]
    
       output$hist <- renderPlot({
    
       hist(choice, xlab=NULL, breaks=15, col="dodgerblue", 
      
       ylab="# of Cities", border="white", main=paste(input$year, input$variable, sep=" "))
      
       abline(v=d[,grep(options$choose, colnames(d))][d$CITY==input$city], lwd=2) 
      
       abline(v=mean(choice, na.rm=T), lty=2)
      
       box()
      
       if(input$year=="2017"){position <- "topright"} else{position <- "topleft"}
      
       legend(position, inset=0.05, c(input$city, "Avg of All Cities"), lwd=c(2,1), lty=c(1,2), bty="n")
    
    })
    
       output$data1 <- renderPrint({
       
         paste(input$city, " ", input$variable, " ", input$year, ": ", d[,grep(options$choose, colnames(d))][d$CITY==input$city],
            " (", round(ecdf(choice[!is.na(choice)])(d[,grep(options$choose, colnames(d))][d$CITY==input$city]),4), " percentile)", sep="")
    })
    
       output$data2 <- renderPrint({
       
         summary(choice)
    })
  })
  
       output$var_desc <- renderText({
       
         data_link = switch(input$variable,
                       "Property Crime" = "is the number of nonviolent crimes including burglary, motor vehicle theft and larceny per 100,000 population.",
                       "Violent Crime"= "is the number of violent crimes including aggravated assault, robbery, and homicide per 100,000 population. ")
         paste(input$variable, data_link)
  })
  
       output$yr_desc <- renderText({
       
         yr_link = switch(input$year, 
                       "2017" = "figures show a predicted crime rate for 2017.  These values are generated by ILSSC based on a model using the trend in crime over the last 15 years.
                        The forecasted value has a degree of uncertainty, but is the model's 'best guess' for the level of crime in 2017.",
                        "Change, 2015-2017" = "presents the difference in the predicted crime rate in 2017 compared to the actual crime rate in 2015.")
         paste(input$year, yr_link)
  })

  
})
