# server.R
#Author Akshay Sapra
# Student ID: 29858186


#Librarues used 

library(shiny)
library(leaflet)
library(datasets)
library(ggplot2) 
library(threejs)
library (dplyr)
library(png)
library(ggpubr)
library(gganimate)
library(shinyanimate)
library(maps)
library(ggthemes)
library(jpeg)


# Reading the data
clean = read.csv("cleaned.csv", header=TRUE, sep=',')
# To use small data set you can uncomment below line
#clean = clean[1:20000,]



shinyServer(function(input, output) {
  #Pre loading the gif of spread of terrorism
  worldFile <- tempfile(fileext='.gif')
  world = clean %>% group_by(Country,Year,Latitude,Longitude,Nkill,Region.Txt) %>%
    summarise(Terror_attack = length(Eventid)) %>% ggplot()+ borders("world", colour = "gray85", fill = "gray80") +
    theme_map()+aes(x=Longitude,y= Latitude,frame=Year,color= Region.Txt)  +
    geom_point()+
    scale_color_brewer(type = 'div', palette = 'Spectral') +
    ggtitle( "Number of Attacks and Deaths in the year {frame_time}") + labs(color="Regions")+
    transition_time(Year) +
    ease_aes("linear") +
    enter_fade() +
    exit_fade()
  anim_save("WorldFile.gif",animate(world))

  #Pre loading the gif of relationship of prosperity and Terrorism in years
  Test_if <- tempfile(fileext='.gif')
    e = clean %>% group_by(Country,Prosperity_Index,Year,Region.Txt) %>%
      summarise(Terror_attack = length(Eventid)) %>% ggplot()+ aes(Prosperity_Index,Terror_attack,frame=Year,color=Region.Txt)  +
      geom_point()+
      scale_color_brewer(type = 'div', palette = 'Spectral') +
      ggtitle( "Prosperity Index vs Terror attacks in {frame_time}") + ylab("No. of Terror Attacks")+xlab("Prosperity Index")+ labs(color="Regions")+
      transition_time(Year) +
      ease_aes("linear") +
      enter_fade() +
      exit_fade()
    anim_save("Test_if.gif",animate(e))
  

 
  # Return the formula text for printing as a caption
  
  
  
  #plot for the Scatter plot of Prosperity Indicator with respect to year (in 2nd tab)
  output$IndicatorlPlot <- renderPlot({
    # check for the input variable
    if (input$country2!="All"){
      # ggplot to plot the graph for specific country provided in the input
      ggplot(clean[clean$Country==input$country2,])+ aes(as.factor(Year),get(input$Indicator),group=1)  +
        geom_point(col="blue") +
         geom_smooth(method="loess") + 
        ggtitle( label=paste0(" Trend of ",input$Indicator, " of ",input$country2)) + xlab("Year")+ylab(input$Indicator)
    }
    else {
      # ggplot to plot the graph for all the countries
      ggplot(clean)+ aes(as.factor(Year),get(input$Indicator),group=1,col=Region.Txt)  +
        geom_point() +  
          geom_smooth(method="loess") + 
        ggtitle( label=paste0(" Trend of ",input$Indicator, " of the world ")) + xlab("Year")+ylab(input$Indicator)
    }
    
    
    
  })
  
  #plot for the Scatter plot of Terrorist Attacks with respect to year (in 2nd tab)
  output$Indicator2Plot <- renderPlot({
    # check for the input variable
    if (input$country2!="All"){
      # ggplot to plot the graph for specific country provided in the input
      clean[clean$Country==input$country2,] %>% group_by(Country,get(input$Indicator),Year,group=1) %>%
        summarise(Terror_attack = length(Eventid)) %>% ggplot()+aes(x=as.factor(Year),y=Terror_attack)+
        geom_point(col="red")+
        geom_smooth(method="loess") + 
        ggtitle( label=paste0("Number of Terror attacks in ",input$country2)) + ylab("No. of Terror Attacks")+xlab("Year")
      
    }
    else {
      # ggplot to plot the graph for all the countries
      clean %>% group_by(Country,get(input$Indicator),Year,Region.Txt) %>%
        summarise(Terror_attack = length(Eventid)) %>% ggplot()+aes(x=as.factor(Year),y= Terror_attack,group=1,col=Region.Txt)+
        geom_point()+
        geom_smooth(method="loess") + 
        ggtitle( label=paste0("Number of Terror attacks in the World")) + ylab("No. of Terror Attacks")+xlab("Year")
    }
    
    
    
  })
  
  #plot for the Scatter plot of Prosperity Indicator with respect to Terrorist attacks (in 2nd tab)
  output$Indicator3Plot <- renderPlot({
    # check for the input variable
    if (input$country2!="All"){
      
      # ggplot to plot the graph for specific country provided in the input
      clean[clean$Country==input$country2,]%>%group_by(Country,Prosperity_Index,Business_Environment_Index,Economic_Quality_Index,Education_Index,Natural_Environment_Index,Governance_Index,Health_Index,Personal_Freedom_Index,Safety_And_Security_Index,Social_Capital_Index) %>%
        summarise(Terror_attack = length(Eventid)) %>% ggplot()+aes(x=get(input$Indicator),y=Terror_attack)+geom_point(col="green")+
        geom_smooth(method="loess") +
        ggtitle( label=paste0("Relationship of ",input$Indicator," with number of attacks in ", input$country2)) + xlab(input$Indicator)+ylab("Number of attacks")
      
    }
    else {
      # ggplot to plot the graph for all the countries
        clean %>%group_by(Country,Prosperity_Index,Business_Environment_Index,Economic_Quality_Index,Education_Index,Natural_Environment_Index,Governance_Index,Health_Index,Personal_Freedom_Index,Safety_And_Security_Index,Social_Capital_Index) %>%
        summarise(Terror_attack = length(Eventid)) %>%  ggplot()+aes(x=get(input$Indicator),y=Terror_attack)+geom_point(col="green")+
        geom_smooth(method="loess") +
        ggtitle( label=paste0("Relationship of ",input$Indicator," with number of attacks in the World ")) + xlab(input$Indicator)+ylab("Number of attacks")
    }
  })
  
  #plot for the globe indicating people affected
  output$globeplot <- renderGlobe({
    # earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
    globejs(img <- "file://earth.jpg",
            lat=clean[clean$Year %in% input$Year1, ]$Latitude, 
            long=clean[clean$Year %in% input$Year1, ]$Longitude, color = "red",atmosphere = TRUE,   pointsize=0.5, rotationlat=.5, rotationlong=-.05,fov=30,
            value=clean$Nkill+clean$Nwound)
  
  })
  
  #plot for the map in accordance with the colour and poppup from the graph in tab 1  
  output$mapPlot1 <- renderLeaflet({
    pal <- colorFactor(c("#F8766D", "#CD9600", "#7CAE00","#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"), clean$Gname)
    print(input$country)
    if (input$country!="All"){    
      #Plot for specific country
      dat=clean[(clean$Year %in%input$Year1)&(clean$Country== input$country),]
      leaflet(data = dat) %>% addTiles() %>%
        addCircles(~Longitude, ~Latitude, ~mean(dat$Nkill+dat$Nwound, na.rm = TRUE)^3,color = ~pal(dat$Gname),
                   popup = ~as.character(paste0(Gname," group killed ",Nkill," people.")))%>%
            addPopups(~Longitude, ~Latitude, ~as.character(paste0(Gname," group killed ",Nkill," people.")))
        
      
    }
    else{
      #Plot for all the countries
      
      dat=clean[(clean$Year %in%input$Year1),]
      leaflet(data = dat) %>% addTiles() %>%
        addCircles(~Longitude, ~Latitude, ~mean(dat$Nkill+dat$Nwound, na.rm = TRUE)^3,color = ~pal(dat$Gname),
                   popup = ~as.character(paste0(Gname," group killed ",Nkill," people.")))
      
    }
    
  })
  


  #Plot for spread of terrorism in the world
  output$world_motion <- renderImage({ 

     list(src = "WorldFile.gif",
          contentType = 'image/gif'
          # width = 400,
          # height = 300,
          # alt = "This is alternate text"
     )
     
   
  })
  
  #Plot for relationship of prosperity and terrorism 
  output$plot1 <- renderImage({
    if (input$country=="All"){
      #Plot for all the countries
    list(src = "Test_if.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
         )
    }
    else{
      #Plot for specific country
      Test_else <- tempfile(fileext='.gif')
      d = clean[clean$Country==input$country,] %>% group_by(Country,Prosperity_Index,Year) %>%
        summarise(Terror_attack = length(Eventid)) %>% ggplot()+ aes(Prosperity_Index,Terror_attack,frame=Year)  + 
        geom_point(col="red")+
        scale_color_brewer(type = 'div', palette = 'Spectral') + 
        ggtitle( paste0("Prosperity Index of ",input$country," vs Terror attacks in {frame_time}")) + ylab("No. of Terror Attacks")+xlab("Prosperity Index")+
        transition_time(Year) +
        ease_aes("linear") +
        enter_fade() +
        exit_fade()
      anim_save("Test_else.gif",animate(d))
      
      list(src = "Test_else.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
      
    }
    
    }, deleteFile = TRUE)
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
})
