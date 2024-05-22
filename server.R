#Mobile Bay: Modeled vs Measured
#for NOAA RESTORE, award NA19NOS4510194
#R Shiny App: L.L. Lowe, 2023
#Model outputs from ROMS - grid by Z. Liu and J. Coogan
#CTD data from ARCOS, https://arcos.disl.org/

#server.R makes calculations based on values from the user interface
#The dashboard version of Shiny
library(shinydashboard)
#For colormap
library(RColorBrewer)
#To filter dataframes
library(dplyr)

#Sets output based on input from ui
function(input, output, session) {
  
  #---Initializations-----
  
  #Start an empty plot window with a 'click me' message
  output$timeplot <- renderPlot({
    plot(1,type="n",xlab="",ylab="",xaxt="n",yaxt="n")
    mtext("Click a station to start a plot.",side=3,line=-4,cex=1.5,col="#006CD1")
   })
  
  #Start the map
  output$map <- renderLeaflet({
                 leaflet()  %>%
                 addTiles() %>%
                 setView(lng = -87.98733, lat = 30.50355, zoom = 9) 
    })
  
  #---Observations----
  
  #Watch for changes in Variable and Color By and redraw the map
  observe({
    
    #Define plot variables based on temperature or salinity
    if(input$var == "Temperature"){ 
      
       df <- mb_stations %>% select(Station,Lat,Lon,ID)
       
       #Get values for surface or bottom
       if(input$colorby == "Surface") df$varmean <- mb_stations$Tsm
       if(input$colorby == "Bottom") df$varmean <- mb_stations$Tbm
       
       #Maps data values to colors using inferno pallete
       pal <- colorNumeric(palette = "inferno", domain = c(21,25))
       
       #For the legend
       groupname <- paste("Mean",input$colorby,"Temperature")
       legendtitle <- "T"
       
     }else if(input$var == "Salinity"){
       
       df <- mb_stations %>% select(Station,Lat,Lon,ID)
       
       #Get values for surface or bottom
       if(input$colorby == "Surface") df$varmean <- mb_stations$Ssm
       if(input$colorby == "Bottom") df$varmean <- mb_stations$Sbm
       
       #Maps data values to colors using viridis pallete
       pal <- colorNumeric(palette = "viridis", domain = c(0,35))
       
       #For the legend
       groupname <- paste("Mean",input$colorby,"Salinity")
       legendtitle <- "S"
     }
    
    
    #Redraw the map
    leafletProxy("map", data = df) %>%
          clearShapes() %>%
          clearControls() %>%
          addCircles(~Lon, ~Lat, radius=800, layerId=~ID,stroke=FALSE, 
                      fillOpacity=.8, fillColor=~pal(varmean),group="Model") %>%   
          addLegend("bottomright", pal = pal, values = ~varmean,
                      title = legendtitle,group=groupname,opacity = 1) %>%
          addLayersControl(overlayGroups = c("Model",groupname),
                            options = layersControlOptions(collapsed = FALSE))
        
    })

  #If the map is clicked, make a plot
  observe({
    
        event <- input$map_shape_click
        
        #If nothing is clicked, do nothing.
        if (is.null(event)) return()
        
        #Event id is a index that defines data and station name
        isolate({GetPlot(event$id)})
    })

  
  #If the map is clicked, display Lat/Lon for location
  observe({
    
        #Event id is a index that defines data and station name
        event <- input$map_shape_click
        
        isolate({
             #GetIndicies returns lat/lon
             gid <- GetIndicies(event$id)
             content <- as.character(HTML(sprintf("Lat = %01.2f Lon = %01.2f",gid$lat,gid$lon)))
         })
        
         if (is.null(event)) content <- "None Selected"
        
         #Display the location's lat/lon
         output$plotwin <- renderUI({HTML(content)})
         
      return()
         
    })
  #End observe

  
  #---Functions-------
  
  #Function to get lat/lon to display on dashboard  
  GetIndicies <- function(id) { 
    whichlon <- mb_stations$Lon[id]
    whichlat <- mb_stations$Lat[id]
    which <- list("lon"=whichlon,"lat"=whichlat)
    return(which)
  }      
  
  
  #Function to make the plot 
  GetPlot <- function(inode) { 
    #The Plot!
    output$timeplot <- renderPlot({
      #Extract data from clicked location 
      df <- mb_model[[inode]]
      dfo <- mb_obs[[inode]]
      #Station name
      idf <- mb_stations[mb_stations$ID==inode,]$Station
      #Filter based on time slider
      df <- df %>% filter(Date >=input$timeRange[1] & Date <=input$timeRange[2])
      dfo <- dfo %>% filter(Date >=input$timeRange[1] & Date <=input$timeRange[2])
      #Define 'Time'
      Time <- df$Date
      Time_ctd <- dfo$Date
      #For the chosen variable, define data and plot limits
      if(input$var == "Temperature"){
         Surf <- df$Tsurf 
         Bot <- df$Tbot
         mainlabel=paste("Temperature - ",idf) #idf is station name
         ylabel = "T"
         msurf = "Surface Temperature"
         mbot = "Bottom Temperature"
         ctd <- dfo$T  #measured data
         pal <- colorNumeric(palette = "inferno", domain = c(0,35))
      }else if(input$var == "Salinity") {
        Surf <- df$Ssurf
        Bot <- df$Sbot
        mainlabel=paste("Salinity - ",idf) #idf is station name
        ylabel = "S"
        msurf = "Surface Salinity"
        mbot = "Bottom Salinity"
        ctd <- dfo$S #measured data
        pal <- colorNumeric(palette = "viridis", domain = c(0,35))
      }
      #Hex codes for surface and bottom lines and text
      colsurf <- "#006CD1"    #Blue
      colbot <- "#994F00"     #Brown
      
      #Find Surface min,max,mean and format label
      is_min <- format(min(Surf),digits=3)
      is_max <- format(max(Surf),digits=3)
      is_mean <- format(mean(Surf),digits=3)
      surf_label <- paste("Surface: min=",is_min,"mean=",is_mean,"max=",is_max)
      #Find Bottom min,max,mean and format label
      ib_min <- format(min(Bot),digits=3)
      ib_max <- format(max(Bot),digits=3)
      ib_mean <- format(mean(Bot),digits=3)
      bot_label <- paste("Bottom:  min=",ib_min,"mean=",ib_mean,"max=",ib_max)
      #Ylimits for plot
      if(input$radio == "1"){  
        #choose range based on min/max of model values
        ymi <- min(min(Surf),min(Bot))
        yma <- max(max(Surf),max(Bot))
      }else if(input$radio == "2") {
        #fixed range, 0-35
        ymi <- 10
        yma <- 30
      }else if(input$radio == "3"){
        #fixed range, 10-35
        ymi <- 10
        yma <- 35
      }
      #Plot timeseries for surface   
      plot(Time,Surf,main=mainlabel,ylab=ylabel,cex=0.3,type="l",
            col=colsurf,bg=colsurf,ylim = c(ymi,yma),xlabel="")
      #Add timeseries for bottom
      lines(Time,Bot,pch=20,cex=0.3,type="l",col=colbot,bg=colbot)
      #Format time for x-axis
      axis.Date(1, Time,format="%b %d")
      #Add additional info as text
      mtext(msurf, side=3, line=1, col=colsurf, cex=1, adj=0)
      mtext(mbot, side=3, line=1, col=colbot, cex=1, adj=1)
      mtext(surf_label, side=1, line=3, col=colsurf, cex=1, adj=0)
      mtext(bot_label, side=1, line=4, col=colbot, cex=1, adj=0)
      #If checked, overlay the station data
      if(input$checkbox) points(Time_ctd,ctd,pch=23,col="black",bg="#D9CA4B",cex=1)
    }) #End of 'render plot'
  } #End function 'GetPlot'
  
}
