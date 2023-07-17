#Mobile Bay: Modeled vs Measured
#for NOAA RESTORE, award NA19NOS4510194
#R Shiny App: L.L. Lowe, 2023
#Model outputs from ROMS - grid by Z. Liu and J. Coogan
#CTD data from ARCOS, https://arcos.disl.org/

#ui.R is User Interface
#Libraries:
#The dashboard version of Shiny
library(shinydashboard)
#Leaflet is for the interactive map
library(leaflet)
#The error messages are usually due to time zone warnings.
options( warn = -1 )


#Begin entire displayed webpage
dashboardPage(
  
  #Header is title of the page
  dashboardHeader(title = "Mobile Bay - 2019"),   

  #A sidebar is default, need to disable for single page
  dashboardSidebar(disable = TRUE),
  
  #The Dashboard is contained in the Page
  dashboardBody(
    #Makes the map and plot take 65% and 50% vertical height of browser window
    tags$style(type = "text/css", "#map {height: calc(65vh) !important;}"),
    tags$style(type = "text/css", "#timeplot {height: calc(50vh) !important;}"),
    
    #Main row - entire window
    fluidRow(
      #Map fills the left half of the window, in a box
      #'primary' is code for blue color
      column(width=6,
        box(width=NULL, solidHeader=TRUE,status="primary",
        leafletOutput("map"))),
      #Right half of window is a column with two rows
      column(width=6,align="center",
        #First row the plot
        fluidRow(
          box(width=12,solidHeader=TRUE,status="primary",plotOutput("timeplot"))
         ),
        #Second row is the time slider
        fluidRow(                               
          #Add the slider, modify times as needed
          column(width=12,align="center",
                sliderInput("timeRange", label = "Time range", timeFormat="%F", 
                              min = as.POSIXct("2019-01-01 00:00:00",tz = 'GMT'),
                              max = as.POSIXct("2019-12-16 00:00:00",tz = 'GMT'),
                              value = c(as.POSIXct("2019-01-01 00:00:00",tz = 'GMT'),
                                        as.POSIXct("2019-12-16 00:00:00",tz = 'GMT'))))
     #close second row in left column, close left column, close first row
         ),),),
    
    #Second row - entire window
    fluidRow(
      #Display Lat/lon of chosen point
      column(width=2,align="center",strong("Location"),htmlOutput("plotwin"),br()),
      #Choose the variable
      column(width=2,align="center",
             radioButtons("var", "Variable",
                           choices = list("Salinity" = "Salinity", "Temperature" = "Temperature"),
                            selected = "Salinity")),
      #Color map circles, choose surface or bottom mean of chosen variable 
      column(width=2,align="center",
             radioButtons("colorby", "Color By",
                           choices = list("Surface(mean)" = "Surface", "Bottom(mean)" = "Bottom"),
                           selected = "Surface")),
      #Choose to display station data, default is yes
      column(width=3,strong("Station Data"),align="center",
             checkboxInput("checkbox", "Uncheck to remove CTD data.", value = TRUE)),
      #Plot limits
      column(width=3,align="center",
             radioButtons("radio", "Plot Limits",
                           choices = list("Model min-max" = 1, "Fixed, 0-35" = 2, "Fixed, 10-35" = 3),
                          selected = 1))
      )#End second row

  )#-- End dashboard Body
)#-- End dashboard Page