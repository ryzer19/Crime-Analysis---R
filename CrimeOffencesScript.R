install.packages("rvest")
install.packages("dplyr")
install.packages("plyr")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("writexl")
install.packages("ggplot2")
install.packages("scales")
install.packages("flextable")
install.packages("cluster")


library(tidyverse)
library(rvest)
library(dplyr)
library(plyr)
library(funModeling)
library(Hmisc)
library(leaflet)
library(writexl)
library(ggplot2)
library(easyGgplot2)
library(scales)
library(ggthemes)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(data.table)
library(DT)
library(flextable)
library(ggpubr)
library(factoextra)
library(cluster)
library(mclust)



#loading datasets
crimeOffences <- read.csv(file = '/Users/ryanjohnston/development/r/crime/Datasets/RecordedCrimeOffences.csv')
locations <- read.csv(file = '/Users/ryanjohnston/development/r/crime/Datasets/Locations.csv')



#Exploratory Data Analysis
  glimpse(crimeOffences)
  status(crimeOffences)


  
    #datatable of locations
    datatable(locations)
    
    
#changing column names
names(crimeOffences)[3] <- "Garda_Station"
names(crimeOffences)[4] <- "Type_Of_Offence"
names(crimeOffences)[5] <- "Unit"
names(crimeOffences)[6] <- "Value"

#creating variable to filter out rows - values in the variable are selected
criteria_crime_north_dublin <- c("62101 Bridewell Dublin, D.M.R. North Central Division", "62202 Fitzgibbon Street, D.M.R. North Central Division",
                                 "62203 Mountjoy, D.M.R. North Central Division","62301 Store Street, D.M.R. North Central Division","63101 Balbriggan, D.M.R. Northern Division",
                                 "63102 Garristown, D.M.R. Northern Division","63103 Lusk, D.M.R. Northern Division","63105 Skerries, D.M.R. Northern Division",
                                 "63201 Ballymun, D.M.R. Northern Division","63202 Dublin Airport, D.M.R. Northern Division","63203 Santry, D.M.R. Northern Division",
                                 "63301 Coolock, D.M.R. Northern Division","63302 Malahide, D.M.R. Northern Division","63303 Swords, D.M.R. Northern Division",
                                 "63401 Clontarf, D.M.R. Northern Division","63402 Howth, D.M.R. Northern Division","63403 Raheny, D.M.R. Northern Division",
                                 "66101 Blanchardstown, D.M.R. Western Division","66102 Cabra, D.M.R. Western Division","66103 Finglas, D.M.R. Western Division")

#creating crime dataset for data analysis tests - using criteria variable to select only necessary values
crimeOffences_northDublin <- crimeOffences[crimeOffences$Garda_Station %in% criteria_crime_north_dublin, ]
#exporting to be reformatted in excel & have income added
#write.csv(crimeOffences_northDublin, "/Users/ryanjohnston/development/r/crime/Datasets/crimeOffences_northDublin.csv", row.names = FALSE)
  #reading in excel file that was reformatted
  crimeOffences_northDublin_reformatted <- read.csv("/Users/ryanjohnston/development/r/crime/Datasets/crimeOffences_northDublin_reformatted.csv")

#removing rows with no income on new reformatted dataset (2003)
crimeOffences_northDublin_reformatted <- crimeOffences_northDublin_reformatted[-c(1:20),]
#removing columns which were redundant and created after importing (blank columns)
crimeOffences_northDublin_reformatted <- crimeOffences_northDublin_reformatted[ , -c(16:20)]

#filtering relevant columns, focusing on particular areas - North Dublin Areas
northDublin = data.frame(crimeOffences %>% filter(Garda_Station %in% c(
  "62101 Bridewell Dublin, D.M.R. North Central Division", "62202 Fitzgibbon Street, D.M.R. North Central Division",
  "62203 Mountjoy, D.M.R. North Central Division", "62301 Store Street, D.M.R. North Central Division", "63101 Balbriggan, D.M.R. Northern Division",
  "63102 Garristown, D.M.R. Northern Division", "63103 Lusk, D.M.R. Northern Division", "63105 Skerries, D.M.R. Northern Division",
  "63201 Ballymun, D.M.R. Northern Division", "63202 Dublin Airport, D.M.R. Northern Division", "63203 Santry, D.M.R. Northern Division",
  "63301 Coolock, D.M.R. Northern Division", "63302 Malahide, D.M.R. Northern Division", "63303 Swords, D.M.R. Northern Division",
  "63401 Clontarf, D.M.R. Northern Division", "63402 Howth, D.M.R. Northern Division", "63403 Raheny, D.M.R. Northern Division",
  "66101 Blanchardstown, D.M.R. Western Division", "66102 Cabra, D.M.R. Western Division", "66103 Finglas, D.M.R. Western Division"
  )))

  #south dublin filter
southDublin = data.frame(crimeOffences %>% filter(Garda_Station %in% c(
    "65203 Dun Laoghaire, D.M.R. Eastern Division",
    "65201 Cabinteely, D.M.R. Eastern Division",
    "65102 Dundrum, D.M.R. Eastern Division",
    "65101 Blackrock, Co Dublin, D.M.R. Eastern Division",
    "64302 Terenure, D.M.R. Southern Division",
    "64301 Rathmines, D.M.R. Southern Division",
    "64202 Tallaght, D.M.R. Southern Division",
    "64201 Rathfarnham, D.M.R. Southern Division",
    "64102 Sundrive Road, D.M.R. Southern Division",
    "64101 Crumlin, D.M.R. Southern Division",
    "61302 Kilmainham, D.M.R. South Central Division",
    "61301 Kevin Street, D.M.R. South Central Division",
    "61202 Pearse Street, D.M.R. South Central Division",
    "61101 Donnybrook, D.M.R. South Central Division",
    "61102 Irishtown, D.M.R. South Central Division",
    "66301 Lucan, D.M.R. Western Division",
    "66302 Ronanstown, D.M.R. Western Division",
    "66203 Rathcoole, D.M.R. Western Division",
    "66202 Clondalkin, D.M.R. Western Division",
    "66201 Ballyfermot, D.M.R. Western Division"
    )))

#gets total number of crimes between 2003-2019 North Dublin
totalNorthValue <- aggregate(northDublin$Value, by=list(northDublin$Garda_Station), FUN=sum)
datatable(totalNorthValue)

#gets total number of crimes between 2003-2019 South Dublin
totalSouthValue <- aggregate(southDublin$Value, by=list(southDublin$Garda_Station), FUN=sum)
datatable(totalSouthValue)

      locations$Total <- c(
103752,#BRIDEWELL
  39786,#FITZGIBBON
    43011,#MOUNTJOY
      157464,#STORE
        20962,#BALBRIGGAN
          1167,#GARRISTOWN
            10562,#LUSK
                5777,#SKERRIES
                  33956,#BALLYMUN
                      8137,#DUBLIN AIRPORT
                          39703,#SANTRY
                            50225,#COOLOCK
                                18856,#MALAHIDE
                                    44410,#SWORDS
                                        30160,#CLONTARF
                                            14309,#HOWTH
                                                17480,#RAHENY
                                                    95535,#BLANCHARDSTOWN
                                                      22536,#CABRA
                                                        50622,#FINGLAS
                                                      45383,#DUNLAOIGHRE
                                                    18832,#CABINTEELY
                                                49157,#DUNDRUM
                                              24996,#BLACKROCK
                                            22110,#TERENURE
                                          31606,#RATHMINES
                                        101189,#TALLAGHT
                                      42489,#RATHFARNHAM
                                    27738,#SUNDRIVE ROAD
                                  26063,#CRUMLIN
                                44117,#KILMAINHAM
                              55677,#KEVIN ST
                          171741,#PEARSE ST
                        28958,#DONNYBROOK
                      23081,#IRISHTOWN
                    27327,#LUCAN
                  43382,#RONANSTOWN
                10946,#RATHCOOLE
              44234,#CLONDALKIN
      35606)#BALLYFERMOT
      
      
      
      
      
#---------MAP OF NORTH & SOUTH - TOTAL CRIME------------------------------------
         #map visualisation total reported crimes of each garda station
          Total.40 <- locations[1:40,]
           
        #styling for icons displayed on map
            icons <- awesomeIcons(
              icon = 'ios-close',
              iconColor = 'black',
              library = 'ion',
              markerColor = getColor(Total.40) #uses function getColor to determine colours of the Total.20 variables on map
            )
          
          #function - sets colours for each pin icon, changes with values listed below
            getColor <- function(locations) {
              sapply(locations$Total, function(Total) {
                if(Total <= 31025) { #31000 is 5 crimes a day - green shows "safe"
                  "green" 
                } else if(Total <= 62000) { #62000 is 10 crimes a day - orange shows "slightly-unsafe
                  "orange"
                } else {
                  "red" #red shows "unsafe"
                } })
            }
                
            
          #code to show the map & markers on it using the Leaflet library
               allTimeMap <- leaflet()%>%addTiles()%>%addAwesomeMarkers(
                 data = locations, #locations dataframe used
                 lat = ~Latitude, #latitude coordinate taken from Latitude column in data frame
                 lng = ~Longitude, #longitude coordinate taken from Longitude column in data frame
                 icon=icons, #sets the icon style
                 label =  locations$Location, #adds locations to each label when hovered over the icon
                 popup = ~as.character(Total)) #displays popup of value of crimes in that area when icon pressed
              
               #displays map visualisation
               allTimeMap
               
               
#-------------------------------------------------------------------------------
               
               
               
               
               
             
           #adding garda station name to Total.40 df for join
              #making df for paste
               locations_paste <-  c("62101 Bridewell Dublin, D.M.R. North Central Division","62202 Fitzgibbon Street, D.M.R. North Central Division",
                                    "62203 Mountjoy, D.M.R. North Central Division","62301 Store Street, D.M.R. North Central Division", "63101 Balbriggan, D.M.R. Northern Division",
                                    "63102 Garristown, D.M.R. Northern Division", "63103 Lusk, D.M.R. Northern Division", "63105 Skerries, D.M.R. Northern Division", 
                                    "63201 Ballymun, D.M.R. Northern Division", "63202 Dublin Airport, D.M.R. Northern Division", "63203 Santry, D.M.R. Northern Division", 
                                    "63301 Coolock, D.M.R. Northern Division", "63302 Malahide, D.M.R. Northern Division", "63303 Swords, D.M.R. Northern Division", "63401 Clontarf, D.M.R. Northern Division", 
                                    "63402 Howth, D.M.R. Northern Division", "63403 Raheny, D.M.R. Northern Division", "66101 Blanchardstown, D.M.R. Western Division", "66102 Cabra, D.M.R. Western Division", 
                                    "66103 Finglas, D.M.R. Western Division", "65203 Dun Laoghaire, D.M.R. Eastern Division", "65201 Cabinteely, D.M.R. Eastern Division", "65102 Dundrum, D.M.R. Eastern Division", 
                                    "65101 Blackrock, Co Dublin, D.M.R. Eastern Division", "64302 Terenure, D.M.R. Southern Division", "64301 Rathmines, D.M.R. Southern Division", "64202 Tallaght, D.M.R. Southern Division", 
                                    "64201 Rathfarnham, D.M.R. Southern Division", "64102 Sundrive Road, D.M.R. Southern Division", "64101 Crumlin, D.M.R. Southern Division", "61302 Kilmainham, D.M.R. South Central Division",
                                    "61301 Kevin Street, D.M.R. South Central Division", "61202 Pearse Street, D.M.R. South Central Division", "61101 Donnybrook, D.M.R. South Central Division", "61102 Irishtown, D.M.R. South Central Division", 
                                    "66301 Lucan, D.M.R. Western Division", "66302 Ronanstown, D.M.R. Western Division", "66203 Rathcoole, D.M.R. Western Division", "66202 Clondalkin, D.M.R. Western Division", "66201 Ballyfermot, D.M.R. Western Division")
               locations_paste <- data_frame(locations_paste) #creates dataframe from variable values
                  names(locations_paste)[1] <- "Garda_Station" #changes column name
              Total.40$Garda_Station <- paste(locations_paste$Garda_Station) # pastes values above in beside corresponding location
                          

              #mean total of north & south each location
                   averageCrimesAllTimeNorth <- aggregate(Value~Garda_Station,northDublin,mean) #aggregation table of the mean values in North Dublin
                    datatable(averageCrimesAllTimeNorth)
                   averageCrimesAllTimeSouth <- aggregate(Value~Garda_Station,southDublin,mean) #aggregation table of the mean values in South Dublin
                    datatable(averageCrimesAllTimeSouth)
                       
                       
                                  #making yearly dataset for map animation
                                  north2003_2019 <- aggregate(Value~Garda_Station+Year,northDublin,sum) #aggregation of total crimes by gardastation & year (north)
                                  south2003_2019 <- aggregate(Value~Garda_Station+Year,southDublin,sum) #aggregation of total crimes by gardastation & year (south)
                                  
                                  #join locations(long&lat) to yearly data frame - NORTH
                                  north_location_2003_2019 <- left_join(north2003_2019, Total.40, 
                                                         by = c("Garda_Station" = "Garda_Station"))
                                        north_location_2003_2019 <- select(north_location_2003_2019, -7) #deleting total column

                                  #join locations(long&lat) to yearly data frame - SOUTH
                                  south_location_2003_2019 <- left_join(south2003_2019, Total.40, 
                                                                        by = c("Garda_Station" = "Garda_Station"))
                                        south_location_2003_2019 <- select(south_location_2003_2019, -7) #deleting total column
                                  
                                        
                                  all_location_2003_2019 <- rbind(north_location_2003_2019, south_location_2003_2019)
                                  
#------------------------------------------------------------------------------------------------------------------------
                                allIconsStyle <- awesomeIcons(
                                  icon = 'ios-close',
                                  iconColor = 'black',
                                  library = 'ion',
                                  markerColor = getColorShinyAll(all_location_2003_2019) #uses function getColorShinyNorth to determine colours of the variables on map
                                )
                                
         #ICON CREATION FOR SHINY MAP DASHBOARD
                                #function - sets colours for each pin icon, changes with values listed below
                                getColorShinyAll <- function(all_location_2003_2019) {
                                  sapply(all_location_2003_2019$Value, function(Value) {
                                    if(Value <= 1825) { #1825 is 5 crimes a day - green shows "safe"
                                      "green" 
                                    } else if(Value <= 3650) { #3650 is 10 crimes a day - orange shows "slightly-unsafe
                                      "orange"
                                    } else {
                                      "red" #red shows "unsafe" 16 crimes a day avg
                                    } })
                                }
                                
                                
              #SHINY MAP & DASHBOARD IMPLEMENTATION
                        ui <- dashboardPage(
                          skin = "red", #background colour
                          dashboardHeader(title = "Crime Dashboard"), #dashboard title
                          dashboardSidebar( #sidebar formation for slider bar
                            sliderInput("Year_Range", label = "Year Range", #slider bar
                                        min = min(all_location_2003_2019$Year), #gets min year from 'year' column
                                        max = max(all_location_2003_2019$Year), #gets max year from 'year' column
                                        value = c(min(all_location_2003_2019$Year, max(all_location_2003_2019$Year))), #copying values for slider
                                        sep = "", #if this wasn't here year 2003 would be 20,03
                                        step = 1
                                        )
                          ),
                          dashboardBody(
                            fluidRow(box(width = 12, leafletOutput(outputId = "map"))), #map width
                            fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table"))) #dashboard width (under map)
                          )
                        )
                        server <- function(input, output) {
                          data_input <- reactive({
                            all_location_2003_2019 %>%
                              filter(Year == input$Year_Range[1]) %>%
                              group_by(Garda_Station)
                          })

                          data_input_ordered <- reactive({
                            data_input()[order(match(data_input()$Location, all_location_2003_2019$Value)),]
                          })
                          
                          
                          labels <- reactive({ #reactive function changes labels as slider changes year
                            paste("<p>", data_input_ordered()$Location, "</p>", #location word stored in paragraph tag
                                  sep = ""
                              )
                          })
                          
                          output$map <- renderLeaflet(
                            leaflet()%>%addTiles()%>%addAwesomeMarkers(
                              data = all_location_2003_2019, #locations data frame used
                              lat = ~Latitude, #latitude coordinate taken from Latitude column in data frame
                              lng = ~Longitude, #longitude coordinate taken from Longitude column in data frame
                              icon = allIconsStyle, #sets the icon style
                              label =  lapply(labels(), HTML), #adds locations to each label when hovered over the icon
                              popup = ~as.character(data_input()$Value)) #displays pop-up of value of crimes in that area when icon pressed
                          )

                          output$summary_table <- renderDataTable(data_input()) #outputs summary table containing data below map on dashboard
                          }

                  shinyApp(ui = ui, server = server) #runs the shiny app, showing the dashboard & map

                          
                          
#-------------------------------------------------------------------------------

                          
                          
        #clusters
        #NORTH DUBLIN
        #writing to csv, then reading back in to have labels for cluster, col 1 = labels             
        #write.csv(averageCrimesAllTimeNorth, "/Users/ryanjohnston/development/r/crime/Datasets/averageNorthAllTime.csv", row.names = FALSE)
        
        #THIS DATASET HAS BEEN EDITED IN EXCEL AFTER WRITING FROM R, another column added to allow for cluster matrix ***
        averageNorthAllTime <- read.csv("/Users/ryanjohnston/development/r/crime/Datasets/averageNorthAllTime.csv", header = TRUE, row.names = 1, sep = ",")
        
        set.seed(1234)
        kmeans.ani(averageNorthAllTime[1:2], 2)
            #creates 3 clusters from data
            km.clus <- kmeans(averageNorthAllTime, 3) #creates cluster with 3 clusters(groups)
            fviz_cluster(km.clus, averageNorthAllTime, main="North Dublin Average Crimes")+theme_fivethirtyeight() #outputs visualisation of 3 clusters
        
        #SOUTH DUBLIN
        #write.csv(averageCrimesAllTimeSouth, "/Users/ryanjohnston/development/r/crime/Datasets/averageSouthAllTime.csv", row.names = FALSE)
        #THIS DATASET HAS BEEN EDITED IN EXCEL AFTER WRITING FROM R, another column added to allow for cluster matrix ***
        averageSouthAllTime <- read.csv("/Users/ryanjohnston/development/r/crime/Datasets/averageSouthAllTime.csv", header = TRUE, row.names = 1, sep = ",")
        
        set.seed(1234)
        kmeans.ani(averageSouthAllTime[1:2], 2)
            #creates 3 clusters from data
            km.clus <- kmeans(averageSouthAllTime, 5) #creates cluster with 3 clusters(groups)
            fviz_cluster(km.clus, averageSouthAllTime, main="South Dublin Average Crimes")+theme_fivethirtyeight() #outputs visualisation of 3 clusters
        
        #NORTH & SOUTH DUBLIN CLUSTER
        #THIS DATASET HAS BEEN *CREATED* IN EXCEL AFTER WRITING FROM R, another column added to allow for cluster matrix ***
        averageAllTime <- read.csv("/Users/ryanjohnston/development/r/crime/Datasets/averageAllTime.csv", header = TRUE, row.names = 1, sep = ",")
        
        set.seed(1234)
        kmeans.ani(averageAllTime[1:2], 2)
            #creates 3 clusters from data
            km.clus <- kmeans(averageAllTime, 3) #creates cluster with 3 clusters(groups)
            fviz_cluster(km.clus, averageAllTime, main="Dublin Average Crimes")+theme_fivethirtyeight() #outputs visualisation of 3 clusters
        
                              
                              
                              
             #-----------BARPLOT
                   #mean of all north vs all south together
                       crimesNorth <- mean(averageCrimesAllTimeNorth$Value)
                       crimesSouth <- mean(averageCrimesAllTimeSouth$Value)
                       
                       names(totalNorthValue)[1] <- "Garda_Station"
                       names(totalNorthValue)[2] <- "Value"
                       names(totalSouthValue)[1] <- "Garda_Station"
                       names(totalSouthValue)[2] <- "Value"
                       
                       
                       #barplot North locations - all time crime
                       northLabels <- c("Bridewell","Fitzgibbon", "Mountjoy", 
                                        "Store St", "Balbriggan", "Garristown", 
                                        "Lusk", "Skerries", "Ballymun", 
                                        "Dublin Airport", "Santry", "Coolock", 
                                        "Malahide", "Swords", "Clontarf", 
                                        "Howth", "Raheny", "Blanchardstown", "Cabra", "Finglas")
                       
                    #barplot North locations
                       ggplot(totalNorthValue, aes(x = northLabels, y = Value))+
                         geom_bar(stat="identity", fill = "#CC8899", color = "black") +
                         labs( title = "Total Value of Crime 2003-2019 - North Dublin"
                         )+
                         scale_y_continuous(breaks=seq(0,160000,20000))+
                         theme_fivethirtyeight() +
                         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
                       
                       
                       #barplot South locations - all time crime
                       southLabels <- c("Donnybrook","Irishtown", "Pearse St", 
                                        "Kevin St", "Kilmainham", "Crumlin", 
                                        "Sundrive Road", "Rathfarnham", "Tallaght", 
                                        "Rathmines", "Terenure", "Blackrock", "Dundrum", 
                                        "Cabinteely", "Dun Laoighre", "Ballyfermot", 
                                        "Clondalkin", "Rathcoole", "Lucan", "Ronanstown")
                       
                       ggplot(totalSouthValue, aes(x = southLabels, y = Value))+
                         geom_bar(stat="identity", fill = "#228B22", color = "black") +
                         labs( title = "Total Value of Crime 2003-2019 - South Dublin"
                         )+
                         scale_y_continuous(breaks=seq(0,180000,20000))+
                         theme_fivethirtyeight() +
                         theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
                       
                       
                       
                       
                       
                       
         #top 5 areas North & South
         #finglas, blanch, store st, bridewell, coolock
         #tallaght, dundrum, dun laoighre, pearse st, kevin st
         top5 <- c("Finglas","Tallaght","Blanch","Dundrum","Store St","Pearse St","Bridewell","Kevin St", "Coolock","Dun Laoighre")
         top5Area <- c("North", "South","North", "South","North", "South","North", "South","North", "South")
         top5North <- data.frame(50622,95535,157464,103752,50225)
         top5South <- data.frame(101189,49157,171741,55677,45383)
         top5All <- c(50622,101189,95535,49157,157464,171741,103752,55677,50225,45383)
         
         #creating dataframe
         top5All <- data.frame(top5, top5All, top5Area)
                names(top5All)[1] <- "Location"
                names(top5All)[2] <- "Crimes"
                names(top5All)[3] <- "Area"
                    
      #bar plot of north & south values
      ggplot(top5All, aes(x = Location, y = Crimes, fill = Area))+
      geom_bar(stat="identity") +
        labs( title = "Total Value of Crime (ALL TIME) - Top 5 Areas North & South"
        )+
      scale_x_discrete(limits = top5) + #orders based on top5 variable
      scale_y_continuous(breaks=seq(0,180000,20000))+
        theme_fivethirtyeight() +
        theme(axis.title = element_text())
        
                       
      
      
      
                       #average crimes by year 2009-2019 -  FOR EDUCATION COMPARISON
                                #removing rows 2003-2008(education data only starts at 2009)
                                avgCrimesByYear_from2009 <- avgCrimesByYear[-c(1:5),]
                        #plots avg crimes by year 2009-2019
                       ggplot(avgCrimesByYear_from2009, aes(x = factor(Year), y = Value, label=Value)) +
                         geom_point(size = 10, color = 2) +
                         labs( title = "Average Crimes by Year",
                               x = "Year",  
                               y = "Crimes"
                         )+
                         theme_fivethirtyeight() +
                         theme(axis.title = element_text())
                       
                       
                      
                       
          #top 5 types of crime North Dublin
                       #find top 5 types
          top5Crime <- aggregate(Value~Type_Of_Offence,northDublin,sum)
          
              #keep Year value too for visualisation
          top5Crime_year <- aggregate(Value~Type_Of_Offence+Year,northDublin,sum) 
                top5_crimeType <- top5Crime_year %>%
                    filter(Type_Of_Offence %in% c("Theft and related offences",
                                                  "Public order and other social code offences",
                                                  "Damage to property and to the environment",
                                                  "Offences against government, justice procedures and organisation of crime",
                                                  "Burglary and related offences"))
                
                datatable(top5_crimeType)
                write.csv(top5_crimeType, "/Users/ryanjohnston/development/r/crime/Datasets/top5_crimeType.csv", row.names = FALSE)
                
                #line & plot graph showing top 5 crimes & values by year
                top5_crimeType %>% ggplot( (aes(x= Year, y = Value, color=Type_Of_Offence))) +
                  geom_line(lwd = 1)+ geom_point(size=2)+
                  scale_x_continuous(breaks=seq(2004, 2020,2))+
                  scale_y_continuous(breaks=seq(0, 20000,2500))+
                  theme_dark()
                       
                       
                       
#data analysis tests
     #data table of structure of reformatted dataset to be used for tests
     datatable(crimeOffences_northDublin_reformatted)
     
     #creating linear model
     fullmodel_northDublin_incomeAndCrime <- lm(Income ~ Crime.1 + Crime.2 + Crime.3 + Crime.4 + Crime.5 + Crime.6 + Crime.7 + Crime.8 + Crime.9 + Crime.10 + Crime.11 + Crime.12 ,data=crimeOffences_northDublin_reformatted)
     #summary to check significance for correllation
     summary(fullmodel_northDublin_incomeAndCrime) #R^2 = 36% data can only be explained by model
     plot(fullmodel_northDublin_incomeAndCrime)
     
     #creating linear model for columns with '*' - suggest level of statistical significance with regression coefficient
     halfmodel_northDublin_incomeAndCrime <- lm(Income ~ Crime.1 + Crime.2 + Crime.4 + Crime.5 + Crime.10, data = crimeOffences_northDublin_reformatted )
     #summary to check significance for correllation
     summary(halfmodel_northDublin_incomeAndCrime) #R^2 = 30% data can only be explained by model
     plot(halfmodel_northDublin_incomeAndCrime)
     
     
  #flextable of models
    #fulltable
    table_fullModel <- as_flextable(fullmodel_northDublin_incomeAndCrime)
      table_fullModel <- theme_box(table_fullModel)
        table_fullModel <- bold(table_fullModel, bold = TRUE, part = "all")
          table_fullModel <- bg(table_fullModel, bg = "lightgray", part = "all")
            table_fullModel <- color(table_fullModel, color= "blue", part = "header")
              table_fullModel <- color(table_fullModel, color= "navy", part = "body")
                table_fullModel <- color(table_fullModel, color = "black", part = "footer")
    table_fullModel #output of table with styles applied above
    
    #halfmodel table
    table_halfModel <- as_flextable(halfmodel_northDublin_incomeAndCrime)
      table_halfModel <- theme_box(table_halfModel)
        table_halfModel <- bold(table_halfModel, bold = TRUE, part = "all")
          table_halfModel <- bg(table_halfModel, bg = "lightgray", part = "all")
            table_halfModel <- color(table_halfModel, color= "blue", part = "header")
              table_halfModel <- color(table_halfModel, color= "navy", part = "body")
                table_halfModel <- color(table_halfModel, color = "black", part = "footer")
    table_halfModel #output of table with styles applied above
     
    
    #testing for normality
        #QQPlots to visualise result
          ggqqplot(crimeOffences_northDublin_reformatted$Crime.1)
          ggqqplot(crimeOffences_northDublin_reformatted$Crime.3)
          ggqqplot(crimeOffences_northDublin_reformatted$Crime.6)
          ggqqplot(crimeOffences_northDublin_reformatted$Crime.10)
          
        #shapiro-wilk test for normality with random samples
          shapiro.test(crimeOffences_northDublin_reformatted$Crime.1)
          shapiro.test(crimeOffences_northDublin_reformatted$Crime.3)
          shapiro.test(crimeOffences_northDublin_reformatted$Crime.6)
          shapiro.test(crimeOffences_northDublin_reformatted$Crime.10)
                  #p value < 0.05 in all columns showing the data is NOT-NORMAL
