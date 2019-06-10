# ui.R
#Author Akshay Sapra: 29858186
# Task 3, 4 & 5
library(shiny)
library(leaflet)
library(threejs)
library (dplyr)
library(shinyjs)
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


# Define UI for the dashboard

#Loading SCreen specs
appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"
shinyUI(
  
  fluidPage(
    useShinyjs(),
    inlineCSS(appCSS),
    #Content for the loading screenm
    div(
      id = "loading-content",
      h2("Be Patient You Must.."), br(),
      h2("Loading..."),
      img(src = "./tenor.gif")
      
      
    ),
    
    
    
    # Application title
    headerPanel("Spread of Terrorism V/S Speread of Prosperity"),
    p("This Dashboard has some of the graphs to show how Terrorism has spread over the time and How likely is it for a country to get targetted by the attacks."),br(),p("Please wait if graphs take a little longer to load"),
    
    # Sidebar with controls to select the variable to plot against
    sidebarLayout(
      sidebarPanel(width=4,
                   
                   #creating tab to display views
                   tabsetPanel(
                     tabPanel("Terrorism", value = 1),
                     tabPanel("Prosperity", value = 2),
                     id ="tab"
                   ),
                   #Side pannel for first tab
                   conditionalPanel(condition= "input.tab == 1",
                                  br(),br(),
                                    sliderInput("Year1", "Year:",
                                                min = 2007, max = 2017,sep = '',
                                                value = c(2007,2017),step=1),
                                    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),

                                    selectInput("country","Please select country", c("All","Afghanistan" ,"Albania" ,"Algeria" ,"Angola" ,"Argentina" ,"Armenia" ,"Australia" ,"Austria" ,"Azerbaijan" ,"Bahrain" ,"Bangladesh" ,"Belarus" ,"Belgium" ,"Belize" ,"Benin" ,"Bolivia" ,"Brazil" ,"Bulgaria" ,"Burkina Faso" ,"Burundi" ,"Cambodia" ,"Cameroon" ,"Canada" ,"Central African Republic" ,"Chad" ,"Chile" ,"China" ,"Colombia" ,"Croatia" ,"Cyprus" ,"Czech Republic" ,"Denmark" ,"Djibouti" ,"Dominican Republic" ,"Ecuador" ,"Egypt" ,"Estonia" ,"Ethiopia" ,"Finland" ,"France" ,"Gabon" ,"Georgia" ,"Germany" ,"Ghana" ,"Greece" ,"Guatemala" ,"Guinea" ,"Guyana" ,"Honduras" ,"Hong Kong" ,"Hungary" ,"Iceland" ,"India" ,"Indonesia" ,"Iran" ,"Iraq" ,"Ireland" ,"Israel" ,"Italy" ,"Ivory Coast" ,"Jamaica" ,"Japan" ,"Jordan" ,"Kazakhstan" ,"Kenya" ,"Kuwait" ,"Kyrgyzstan" ,"Laos" ,"Latvia" ,"Lebanon" ,"Lesotho" ,"Liberia" ,"Libya" ,"Macedonia" ,"Madagascar" ,"Malawi" ,"Malaysia" ,"Mali" ,"Malta" ,"Mauritania" ,"Mexico" ,"Moldova" ,"Montenegro" ,"Morocco" ,"Mozambique" ,"Nepal" ,"Netherlands" ,"New Zealand" ,"Nicaragua" ,"Niger" ,"Nigeria" ,"Norway" ,"Pakistan" ,"Panama" ,"Paraguay" ,"Peru" ,"Philippines" ,"Poland" ,"Portugal" ,"Qatar" ,"Romania" ,"Russia" ,"Rwanda" ,"Saudi Arabia" ,"Senegal" ,"Serbia" ,"Sierra Leone" ,"South Africa" ,"South Korea" ,"Spain" ,"Sri Lanka" ,"Sudan" ,"Swaziland" ,"Sweden" ,"Switzerland" ,"Tajikistan" ,"Tanzania" ,"Thailand" ,"Trinidad and Tobago" ,"Tunisia" ,"Turkey" ,"Uganda" ,"Ukraine" ,"United Arab Emirates" ,"United Kingdom" ,"United States" ,"Uruguay" ,"Venezuela" ,"Vietnam" ,"Yemen" ,"Zambia" ,"Zimbabwe")
                                                ,selected="Australia"  ),
                   
                 br(),br(),
                   
                   
                   textOutput("key_rankings"),  
                  h3("Relationship of Prosperity and Terrorist attacks for the selected country"),
                  p("Please select the country from the drop down menu to choose appropriate Country"),

                   imageOutput("plot1"),

                   br(),br(),br(), br(),br(),

                   strong("NOTE:"), p(" Due to Large Data Set App might run a little slow and take some time to load")
),
              #Side pannel for second tab
              conditionalPanel(condition= "input.tab == 2",
                 selectInput("country2","Please select country", c("All","Afghanistan" ,"Albania" ,"Algeria" ,"Angola" ,"Argentina" ,"Armenia" ,"Australia" ,"Austria" ,"Azerbaijan" ,"Bahrain" ,"Bangladesh" ,"Belarus" ,"Belgium" ,"Belize" ,"Benin" ,"Bolivia" ,"Brazil" ,"Bulgaria" ,"Burkina Faso" ,"Burundi" ,"Cambodia" ,"Cameroon" ,"Canada" ,"Central African Republic" ,"Chad" ,"Chile" ,"China" ,"Colombia" ,"Croatia" ,"Cyprus" ,"Czech Republic" ,"Denmark" ,"Djibouti" ,"Dominican Republic" ,"Ecuador" ,"Egypt" ,"Estonia" ,"Ethiopia" ,"Finland" ,"France" ,"Gabon" ,"Georgia" ,"Germany" ,"Ghana" ,"Greece" ,"Guatemala" ,"Guinea" ,"Guyana" ,"Honduras" ,"Hong Kong" ,"Hungary" ,"Iceland" ,"India" ,"Indonesia" ,"Iran" ,"Iraq" ,"Ireland" ,"Israel" ,"Italy" ,"Ivory Coast" ,"Jamaica" ,"Japan" ,"Jordan" ,"Kazakhstan" ,"Kenya" ,"Kuwait" ,"Kyrgyzstan" ,"Laos" ,"Latvia" ,"Lebanon" ,"Lesotho" ,"Liberia" ,"Libya" ,"Macedonia" ,"Madagascar" ,"Malawi" ,"Malaysia" ,"Mali" ,"Malta" ,"Mauritania" ,"Mexico" ,"Moldova" ,"Montenegro" ,"Morocco" ,"Mozambique" ,"Nepal" ,"Netherlands" ,"New Zealand" ,"Nicaragua" ,"Niger" ,"Nigeria" ,"Norway" ,"Pakistan" ,"Panama" ,"Paraguay" ,"Peru" ,"Philippines" ,"Poland" ,"Portugal" ,"Qatar" ,"Romania" ,"Russia" ,"Rwanda" ,"Saudi Arabia" ,"Senegal" ,"Serbia" ,"Sierra Leone" ,"South Africa" ,"South Korea" ,"Spain" ,"Sri Lanka" ,"Sudan" ,"Swaziland" ,"Sweden" ,"Switzerland" ,"Tajikistan" ,"Tanzania" ,"Thailand" ,"Trinidad and Tobago" ,"Tunisia" ,"Turkey" ,"Uganda" ,"Ukraine" ,"United Arab Emirates" ,"United Kingdom" ,"United States" ,"Uruguay" ,"Venezuela" ,"Vietnam" ,"Yemen" ,"Zambia" ,"Zimbabwe")
                             ,selected="Australia"  ),
                 
                 selectInput("Indicator","Please select one of the Prosperity Indicator", c("Prosperity Index"="Prosperity_Index","Education Index"="Education_Index", "Economic Quality Index"="Economic_Quality_Index", "Business Environment Index"="Business_Environment_Index", "Governance Index"="Governance_Index", "Personal Freedom Index"="Personal_Freedom_Index", "Social Capital Index"="Social_Capital_Index", "Health Index"="Health_Index", "Safety And Security Index"="Safety_And_Security_Index", "Natural Environment Index"="Natural_Environment_Index")
                             ,selected="Prosperity"  )
                 
                 
)
                   
      ),
      
      
      # Show the captions and plot of the dashboard graphs
      mainPanel(
        hidden(
          div(
            id = "app-content",
            conditionalPanel(condition = "input.tab==1",
                             #graphs in First Tab
                              h3("Global Outreach of Terrorism"),
                             p("Global Map indicates people affected (which means people injured or killed) by terrorism across the globe."),
                             globeOutput("globeplot"),
                             
                             h3("Spread of Terrorism from 2007 to 2017"),
                             p("This Graph shows the spread of Terrorism around the world in different regions."),
                             p("Colour of the point indicates the region and size of the point indicate number of people affected."),
                             imageOutput("world_motion",height="500px",width ="1000px"),
                             
                             # splitLayout(cellWidths = c("50%", "50%"), imageOutput("world_motion",height = "600px"), imageOutput("plot1",height = "600px")),
                             h3("Attacks Carried out by different group in your Country"),
                             p("This map shows the attacks carried out by different groups where colour and popup shows the information about carried out attack and size of the circle reflects the amount of people killed."),
                             leafletOutput("mapPlot1"),br(),br(),br()
                             
            ),
            #graphs in Second Tab
            conditionalPanel(width=9,condition = "input.tab==2",
                             splitLayout(cellWidths = c("50%", "50%"), plotOutput("IndicatorlPlot"), plotOutput("Indicator2Plot")), 
                             plotOutput("Indicator3Plot")
                             )
            )
          )
        )
    
  )))
