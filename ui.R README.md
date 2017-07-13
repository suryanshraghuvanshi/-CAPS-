# -CAPS-
This project is based on machine learning using R and Shiny for predicting the crime trend from past dataset by training them and predict the future crime by testing the same dataset using linear regression from certain parameters such as location, time &amp; date, crime type etc. 
library(shiny)
library(shinydashboard)
library(rCharts)
library(leaflet)
library(ggmap)
library(ggplot2)
library(dplyr)
library(leaflet.extras)
library(rMaps)

d <- read.csv("del.csv")




names <- as.character(unique(unlist(d$CITY)))

ui <- dashboardPage(
  dashboardHeader(title = "Predicted Crime Rates", titleWidth = 300),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     #h5(strong("Choose Crime Type:")), 
                     selectInput("variable", label= "Choose Crime Type",
                                 choices = list("", "Violent Crime", "Property Crime"), selected=""),
                     #h5(strong("Choose Time Period:")),
                     selectInput("year", label= "Choose Time Period",
                                 choices = list("", "2017", "Change, 2015-2017"), selected=""),
                     # h5(strong("Choose City:")),
     #selectInput("city", label= "Choose City", choices = names[order(names)], selected="New Delhi"),
      selectInput("city", label="Choose City", choices = list("Adarsh Nagar" =  1,"Ambedkar Nagar" =  2,"Babarpur" =  3,"Badarpur" =  4
                                                    ,"Badli" =  5,"Ballimaran" =  6,"Bawana" =  7,"Burari" =  8,"Chandni Chowk" =  9,"Chhatarpur" =  10
                                                     ,"Delhi Cantt" =  11,"Deoli" =  12,"Dwarka" =  13,"Gandhi Nagar" =  14,"Ghonda" =  15,"Gokalpur" =  16
                                                     ,"Greater Kailash" =  17,"Hari Nagar" =  18,"Janakpuri" =  19,"Jangpura" =  20,"Kondli" =  21,"Karawal Nagar" =  22
                                                     ,"Karol Bagh" =  23,"Kasturaba Nagar" =  24,"Kalkaji" =  25,"Madipur" =  26,"Malviya Nagar" =  27,"Matiala" =  28
                                                     ,"Matia Mahal" =  29,"Mehrauli" =  30,"Mangol Puri" =  31,"Model Town" =  32,"Moti Nagar" =  33,"Mundka" =  34
                                                     ,"Mustafabad" =  35,"Najafgarh" =  36
                                                     ,"Nangloi Jat" =  37,"Narela" =  38,"Okhla" =  39,"Palam" =  40
                                                     ,"Patel Nagar" =  41,"Patparganj" =  42,"Rajinder Nagar" =  43,"Rajouri Garden" =  44
                                                     ,"Rithala" =  45,"R.K. Puram" =  46,"Rohtas Nagar" =  47,"Rohini" =  48,"Sadar Bazar" =  49
                                                     ,"Sangam Vihar" =  50,"Shahdara" =  51,"Shakur Basti" =  52,"Shalimar Bagh" =  53
                                                     ,"Seelam Pur" =  54,"Seemapuri" =  55,"Sultan Pur Majra" =  56,"Timarpur" =  57
                                                     ,"Tilak Nagar" =  58,"Trilokpuri" =  59,"Tri Nagar" =  60,"Tughlakabad" =  61
                                                     ,"Uttam Nagar" =  62,"Vishwas Nagar" =  63,"Vikaspuri" =  64,"Wazirpur" =  65
                                                     ,"Kirari" =  66,"Bijwasan" =  67,"Noida" =  68,"Faridabad" =  69,"Gaziabad" =  70
                                                     ,"Shamli" =  71,"Gurgaon" =  72,"New Delhi" =  73), selected = 73),
      
      br(),
      div(style="display:center-align;",actionButton("go", label = "Analyize", icon = icon("paper-plane"))),
      br(),
      p(strong("Data Notes:")),
      
      #p(textOutput("var_desc")),
      br(),
      #textOutput("yr_desc"),
      br(),
      #a("NOTES :"),
      br(),
      a("       "),
      br(),
      a("",href="http://www.delhipolice.nic.in/"),
      br(),
      img(src= 'ilssc.png', height=100, width=100)
      
    )),
  dashboardBody(
fluidRow(
  tabsetPanel(
    
    tabPanel("Introduction", textOutput("bhai ji "),
             tags$iframe(src = 'include.html', # put testdoc.html to /www
                         width = '100%', height = '800px', 
                         frameborder = 0, scrolling = 'auto')
             #p(strong("Data Notes:")),
             
             #p(textOutput("var_desc")),
             #br(),
             #textOutput("yr_desc"),
             #br(),
             #a("NOTES :"),
             #br(),
             #a("       "),
             #br(),
             #a("",href="http://www.delhipolice.nic.in/"),
             #br(),
             #img(src="ilssc.png", height=100, width=100)
    ),
      
      tabPanel( title = "Histogram", 
               h2("Cities in New Delhi- Histogram"),
               p(em("Predicted crime values of all cities in New Delhi are displayed below.")),  
               plotOutput("hist", height = 400),
               
               br(),
               p(".   Value for selected city:"),
               verbatimTextOutput("data1"),
               p(".   Values for all cities:"),
               verbatimTextOutput("data2")
               ),
      
    
    
    tabPanel("Heatmap", 
             h2("Cities in New Delhi- Heatmap"),
             
            p(em("Predicted crime location of all cities in New Delhi are displayed below.")),
             br(),
            plotOutput("del.csv"),
            chartOutput("my.map", "leaflet"),
            tags$style('.leaflet {height: calc(100vh -80px) !important; padding: 0; margin: 0; min-height: 500px}')
          # tags$head(tags$script(src="http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js"))
            ),
    
    
    tabPanel("Linear Regression", textOutput("this wiredsoft"),
             h2("Cities in New Delhi- Linear Regression"),
             br(),
             p(em("Linear regression for Cities vs Voilent Crime in 2017 in New Delhi are displayed below.")),
             plotOutput("mod1", height = 400),
             p(em("Linear regression for Cities vs Property Crime in 2017 in New Delhi are displayed below.")),
             plotOutput("mod2", height = 400),
             p(em("Linear regression for Cities vs Voilent Crime in 2015 in New Delhi are displayed below.")),
             plotOutput("mod3", height = 400),
             p(em("Linear regression for Cities vs Property Crime in 2015 in New Delhi are displayed below.")),
             plotOutput("mod4", height = 400)
             ),
    
    tabPanel("Predictive Values", textOutput("this values"),
             h2("Cities in New Delhi- Predictive Data"),
             p(em("Predicted crimes values of all cities in New Delhi are displayed below.")),
             h3(textOutput("predict")),
             br(),
             h3(p("Sum of squared errors (SSE) for calcuating R squared :")),
             h3(textOutput("SSET"))
    ),
    
    tabPanel("About", textOutput("this")
             
             
        )
       )
      )
     )
   )
