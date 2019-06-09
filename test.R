library(ggplot2)
library(shiny)
library(leaflet)
library(datasets)
library(ggplot2)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(geojsonio)
library (dplyr)


clean = read.csv("cleaned.csv", header=TRUE, sep=',')
clean <- clean[1:200,]
clean

l= leaflet(data = clean) %>% addTiles() %>%
   addCircles(~Longitude, ~Latitude,radius = 0.001,
              popup = ~as.character(Country))

l$dependencies[[length(l$dependencies)+1]] <- htmlDependency(
  name = "leaflet-timeline",
  version = "1.0.0",
  src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
  script = "javascripts/leaflet.timeline.js",
  stylesheet = "stylesheets/leaflet.timeline.css"
)
power_geo <- geojson_json(clean,lat="Latitude",lon="Longitude")


l %>%
  setView(44.0665,23.74667,2) %>%
  onRender(sprintf(
  '
  function(el,x){
  var power_data = %s;
  var timeline = L.timeline(power_data, {
  pointToLayer: function(data, latlng){
  var hue_min = 120;
        var hue_max = 0;
        var hue = hue_min;
        return L.circleMarker(latlng, {
          radius: 10,
          color: "hsl("+hue+", 100%%, 50%%)",
          fillColor: "hsl("+hue+", 100%%, 50%%)"
        });
      },
      steps: 1000,
      duration: 10000,
      showTicks: true
    });
    timeline.addTo(this);
    }
  ',
  power_geo
  ))

# myGraph = ggplot ( data = clean, aes (x = Year,y = PI))
# myGraph+geom_point()+facet_grid(Year~Country)+ geom_smooth(aes(group = 1))
a <- "Nkill"
  
clean[clean$Year ==2009,][a]

clean[]
a= "Iraq"
cleaner=clean[clean$Country==a,]


myGraph

ggplot ( data = clean[clean$Country=="Iraq",], aes (x = PI,y = count_multiple(Eventid)))+geom_point()



myGraph+geom_point()+ facet_grid(location~coralType)+ geom_smooth(aes(group = 1), method = input$Smoothers)+labs(y="Percentage", x="Year")

clean[clean$Country=="Iraq",] %>% group_by(Country,Prosperity_Index,Year) %>%
  summarise(Terror_attack = length(Eventid)) %>% ggplot()+ geom_point(aes(x=as.factor(Year),y=Terror_attack),col="red")+
  ggtitle( paste0(" Prosperity Index vs Terror attacks of ","Iraq")) + ylab("No. of Terror Attacks")+xlab("Prosperity Index")


clean[clean$Country=="Afghanistan",] %>% group_by(Year,Country,PI) %>%
  summarise(Terror_attack = length(Eventid)) %>% ggplot(aes(x=as.factor(Year)))+ 
  geom_point(aes(y=Terror_attack, colour="A")) +
  geom_point(aes(y=PI, colour="B")) + 
  scale_y_continuous(sec.axis = sec_axis(~., name = "Prosperity Index"))+scale_colour_manual(values = c("blue", "red"))

  
  
  
  
  geom_point(  aes(PI,Terror_attack))+
  geom_smooth(method="loess") + 
  ggtitle( paste0(" Prosperity Index vs Terror attacks of ","Iraq")) + ylab("No. of Terror Attacks")+xlab("Prosperity Index")


b="PI"
ggplot(clean[clean$Country=="Iraq",])+ aes(as.factor(Year),get(b))  +
  geom_point(col="blue") +  
  geom_smooth(method="loess") + 
  ggtitle( paste0(" Trend of Prosperity Index of ","Iraq")) + xlab("Year")+ylab("Prosperity Index")



+ xlab(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)


head(clean[((clean$Year==2009)&(clean$Country=="Iraq")),])


dat=clean[(clean$Year==2010),]
leaflet(data = dat) %>% addTiles() %>%
  addCircles(~Longitude, ~Latitude, ~mean(dat$Nkill+dat$Nwound, na.rm = TRUE)^3, 
             popup = ~as.character(Gname))


# a <- %>% group_by (Country,PI,Econ,Busi,Gove,Educ,Heal,Safe,Pers,Soci,Envi)%>%
#   summarise (Terror_attack =length(Eventid))

c=clean  %>% group_by ("Country","Prosperity Index","Business Environment Index","Economic Quality Index","Education Index","Natural Environment Index","Governance Index","Health Index","Personal freedom Index","Safety And Security Index","Social Capital Index","Year") %>%
  summarise (Terror_attack =length(Country))

b = clean%>% group_by("Country","Year")
head(b)



a= "ProsperityIndex"
b="Econ"


ggplot(clean)+ aes_string(Year, "Prosperity_Index") +
  geom_point(col="blue") +  
  geom_smooth(method="loess") + 
  ggtitle( paste0(" Trend of ",a, " of the world ")) + xlab("Year")+ylab(a)

a="Afghanistan"
b= "Prosperity_Index"
c="2009"

clean[clean$Country==a,] %>% group_by(Country,get(b),Year) %>%
  summarise(Terror_attack = length(Eventid)) %>% ggplot()+ geom_point(aes(x=as.factor(Year)),y=Terror_attack,col="red")+
  geom_point()+ 
  geom_smooth(method="loess") 
  
  ggtitle( paste0(" Terror attacks of ",input$country," in ", input$Year1)) + ylab("No. of Terror Attacks")+xlab(input$Year1)
  
  
  img.file <- system.file(file.path("images", "test.png"),
                          package = "ggpubr")
  library(png)
  library(ggpubr)
  
  img <- png::readPNG("test.png")
  
  library(gifski)
  library(plotly)
  
  
  
  d = clean %>% group_by(Country,Prosperity_Index,Year) %>%
    summarise(Terror_attack = length(Eventid)) %>% ggplot()+ aes(Prosperity_Index,Terror_attack,frame=Year)  + 
    geom_point(col="red")+
    scale_color_brewer(type = 'div', palette = 'Spectral') + 
    ggtitle( "Prosperity Index vs Terror attacks in {frame_time}") + ylab("No. of Terror Attacks")+xlab("Prosperity Index")+
    transition_time(Year) +
    ease_aes("linear") +
    enter_fade() +
    exit_fade()
  d
  
  anim <- animate(d)
  anim_save("Test.gif",anim)
  
  magick::image_write(anim, path="myanimation.gif")  
  
  library(magick)
  
  
  p <- clean %>% group_by(Country,Prosperity_Index,Year,Nkill) %>%
    summarise(Terror_attack = length(Eventid)) %>% 
    plot_ly(
      x = ~Prosperity_Index, 
      y = ~Terror_attack, 
      frame = ~Year, 
      hoverinfo = "text",
      type = 'scatter',
      mode = 'markers'
    ) %>%
    layout(
      xaxis = list(
        type = "log"
      )
    )
  
  # Create a shareable link to your chart
  # Set up API credentials: https://plot.ly/r/getting-started
ggplotly(p)
  
  chart_link
  install.packages('httpuv')
  plotly:::verify("username")
  
  
  dat=clean
  leaflet(data = dat,frame(Year)) %>% addTiles() %>%
  addCircles(~Longitude, ~Latitude, ~mean(dat$Nkill+dat$Nwound, na.rm = TRUE)^3,
             popup = ~as.character(Gname))
  scale_color_brewer(type = 'div', palette = 'Spectral') + 
    
  transition_time(Year) +
    ease_aes("linear") +
    enter_fade() +
    exit_fade()
  
  
  
  
    ggtitle( paste0("Relationship of ",b," with number of attacks in the World ")) + xlab(b)+ylab("Number of attacks")
    
    
    a="Prosperuty_Index"
    b=get(a)
    colnames(clean)
    clean[(clean$rank.PI)&(clean$Year==2010),]$Country[1]
    
    clean[clean$Year==2010,][order(clean$rank.PI),]$Country
    
    clean[(clean$rank.PI==min(clean[clean$Year==2011,]$rank.PI))&(clean$Year==2011),]$Country[1]
    
    
    
    library(ggplot2)
    library(maps)
    library(ggthemes)
    
    world <- ggplot() +
      borders("world", colour = "gray85", fill = "gray80") +
      theme_map() 
    world
    map <- world +
      geom_point(aes(x = Longitude, y = Latitude, size = Nkill),
                 data = clean, 
                 colour = 'red', alpha = .5) +
      scale_size_continuous(range = c(1, 8), 
                            breaks = c(250, 500, 750, 1000)) +
      labs(size = 'Fatalities')
    map
    
    
    
    d = clean[clean$Country=="Iraq",] %>% group_by(Country,Year,Latitude,Longitude,Nkill) %>%
      summarise(Terror_attack = length(Eventid)) %>% ggplot()+ borders("world", colour = "gray85", fill = "gray80") +
      theme_map()+aes(x=Longitude,y= Latitude,frame=Year,size=Nkill)  + 
      geom_point(col="red")+
      scale_color_brewer(type = 'div', palette = 'Spectral') + 
      ggtitle( "Prosperity Index vs Terror attacks in {frame_time}") + ylab("No. of Terror Attacks")+xlab("Prosperity Index")+
      transition_time(Year) +
      ease_aes("linear") +
      enter_fade() +
      exit_fade()
    d
    
    anim <- animate(d)
    anim_save("Test.gif",anim)

    
    
    library(jpeg)
    library(threejs)
    library(imager)
    file <- system.file('E://books/sem2visualization\Assignment 5\v2/earth.jpg',package='imager')
    
    img <- readJPEG( "earth.jpg")
    
    globejs(img=ch,
            lat=clean[clean$Year ==2009, ]$Latitude, 
            long=clean[clean$Year ==2009, ]$Longitude, color = "red",atmosphere = TRUE,   pointsize=0.5, rotationlat=.5, rotationlong=-.05,fov=30,
            value=clean$Nkill+clean$Nwound)
    
    earth
    
    earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
    ch= ''
    globejs(img=earth, bg="white")
  
    globejs(
            lat=clean[clean$Year %in% input$Year, ]$Latitude, 
            long=clean[clean$Year %in% input$Year, ]$Longitude, color = Gname,atmosphere = TRUE,   pointsize=0.5, rotationlat=.5, rotationlong=-.05,fov=30,
            value=clean$Nkill+clean$Nwound)
    
    
    list(src = "tenor.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
    
    
    pal <- colorFactor( domain = clean$Gname)
    colorNumeric()
    
    
    dat=clean[clean$Year==2009,]
    
    leaflet(data = dat) %>% addTiles() %>%
      addCircles(~Longitude, ~Latitude, ~mean(dat$Nkill+dat$Nwound, na.rm = TRUE)^3,color = ~pal(dat$Gname),
                 popup = ~as.character(Gname))
    clean$re
    
    
    clean %>% group_by(Country,Year,Latitude,Longitude,Nkill,Region.Txt) %>%
      summarise(Terror_attack = length(Eventid)) %>% ggplot()+ borders("world", colour = "gray85", fill = "gray80") +
      theme_map()+aes(x=Longitude,y= Latitude,frame=Year,size=Nkill,color= Region.Txt)  + 
      geom_point()    +
      scale_color_brewer(type = 'div', palette = 'Spectral') + 
      ggtitle( "Number of Attacks and Deaths in the year {frame_time}") + 
      transition_time(Year) +
      ease_aes("linear") +
      enter_fade() +
      exit_fade()
    
    
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
