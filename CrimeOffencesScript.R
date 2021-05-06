install.packages("rvest")
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("writexl")
install.packages("ggplot2")
install.packages("scales")

library(tidyverse)
library(rvest)
library(dplyr)
library(plyr)
library(leaflet)
library(writexl)
library(ggplot2)
library(scales)
library(ggthemes)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(data.table)

#loading datasets
crimeOffences <- read.csv(file = '/Users/ryanjohnston/development/r/crime/Datasets/RecordedCrimeOffences.csv')
locations <- read.csv(file = '/Users/ryanjohnston/development/r/crime/Datasets/Locations.csv')

#changing column names
names(crimeOffences)[3] <- "Garda_Station"
names(crimeOffences)[4] <- "Type_Of_Offence"
names(crimeOffences)[5] <- "Unit"
names(crimeOffences)[6] <- "Value"

criteria_crime_north_dublin <- c("62101 Bridewell Dublin, D.M.R. North Central Division", "62202 Fitzgibbon Street, D.M.R. North Central Division",
                                 "62203 Mountjoy, D.M.R. North Central Division","62301 Store Street, D.M.R. North Central Division","63101 Balbriggan, D.M.R. Northern Division",
                                 "63102 Garristown, D.M.R. Northern Division","63103 Lusk, D.M.R. Northern Division","63105 Skerries, D.M.R. Northern Division",
                                 "63201 Ballymun, D.M.R. Northern Division","63202 Dublin Airport, D.M.R. Northern Division","63203 Santry, D.M.R. Northern Division",
                                 "63301 Coolock, D.M.R. Northern Division","63302 Malahide, D.M.R. Northern Division","63303 Swords, D.M.R. Northern Division",
                                 "63401 Clontarf, D.M.R. Northern Division","63402 Howth, D.M.R. Northern Division","63403 Raheny, D.M.R. Northern Division",
                                 "66101 Blanchardstown, D.M.R. Western Division","66102 Cabra, D.M.R. Western Division","66103 Finglas, D.M.R. Western Division")

#creating crime dataset for data analysis tests
crimeOffences_northDublin <- crimeOffences[crimeOffences$Garda_Station %in% criteria_crime_north_dublin, ]
#exporting to be reformatted in excel & have income added
write.csv(crimeOffences_northDublin, "/Users/ryanjohnston/development/r/crime/Datasets/crimeOffences_northDublin.csv", row.names = FALSE)
#reading in excel file that was reformatted
crimeOffences_northDublin_reformatted <- read.csv("/Users/ryanjohnston/development/r/crime/Datasets/crimeOffences_northDublin_reformatted.csv")

#removing columns with no income on new reformatted dataset (2003)
crimeOffences_northDublin_reformatted <- crimeOffences_northDublin_reformatted[-c(1:20),]
#removing columns which were redundant and created after importing (blank columns)
crimeOffences_northDublin_reformatted <- crimeOffences_northDublin_reformatted[ , -c(16:20)]

#filtering relevant columns, focusing on particular areas - North Dublin Areas
northDublin = data.frame(crimeOffences %>% filter(Garda_Station %in% c(
  "62101 Bridewell Dublin, D.M.R. North Central Division", 
  "62202 Fitzgibbon Street, D.M.R. North Central Division",
  "62203 Mountjoy, D.M.R. North Central Division",
  "62301 Store Street, D.M.R. North Central Division",
  "63101 Balbriggan, D.M.R. Northern Division",
  "63102 Garristown, D.M.R. Northern Division",
  "63103 Lusk, D.M.R. Northern Division",
  "63105 Skerries, D.M.R. Northern Division",
  "63201 Ballymun, D.M.R. Northern Division",
  "63202 Dublin Airport, D.M.R. Northern Division",
  "63203 Santry, D.M.R. Northern Division",
  "63301 Coolock, D.M.R. Northern Division",
  "63302 Malahide, D.M.R. Northern Division",
  "63303 Swords, D.M.R. Northern Division",
  "63401 Clontarf, D.M.R. Northern Division",
  "63402 Howth, D.M.R. Northern Division",
  "63403 Raheny, D.M.R. Northern Division",
  "66101 Blanchardstown, D.M.R. Western Division",
  "66102 Cabra, D.M.R. Western Division",
  "66103 Finglas, D.M.R. Western Division"
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

#gets total number of crimes between 2003-2019 South Dublin
totalSouthValue <- aggregate(southDublin$Value, by=list(southDublin$Garda_Station), FUN=sum)

      locations$Total <- c(
        #BRIDEWELL
        103752,
          #FITZGIBBON
          39786,
            #MOUNTJOY
            43011,
             #STORE
              157464,
                #BALBRIGGAN
                20962,
                  #GARRISTOWN
                  1167,
                   #LUSK
                    10562,
                        #SKERRIES
                        5777,
                          #BALLYMUN
                          33956,
                              #DUBLIN AIRPORT             
                              8137,
                                  #SANTRY
                                  39703,
                                     #COOLOCK
                                      50225,
                                          #MALAHIDE
                                          18856,
                                             #SWORDS
                                              44410,
                                                 #CLONTARF
                                                  30160,
                                                     #HOWTH
                                                      14309,
                                                          #RAHENY
                                                          17480,
                                                              #BLANCHARDSTOWN
                                                              95535,
                                                                    #CABRA
                                                                    22536,
                                                                        #FINGLAS
                                                                        50622,
                                                                  #DUNLAOIGHRE
                                                                  45383,
                                                                #CABINTEELY
                                                                18832,
                                                             #DUNDRUM
                                                            49157,
                                                          #BLACKROCK
                                                          24996,
                                                         #TERENURE
                                                        22110,
                                                      #RATHMINES
                                                      31606,
                                                    #TALLAGHT
                                                    101189,
                                                    #RATHFARNHAM
                                                    42489,
                                                  #SUNDRIVE ROAD
                                                  27738,
                                                #CRUMLIN
                                                26063,
                                              #KILMAINHAM
                                              44117,
                                             #KEVIN ST
                                            55677,
                                         #PEARSE ST
                                        171741,
                                      #DONNYBROOK
                                      28958,
                                    #IRISHTOWN
                                    23081,
                                  #LUCAN
                                  27327,
                                #RONANSTOWN
                                43382,
                              #RATHCOOLE
                              10946,
                            #CLONDALKIN
                            44234,
                      #BALLYFERMOT
                      35606)
#---------MAP
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

                       ###---------------------------------------------------------------               
                         
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
                           locations_paste <- data_frame(locations_paste)
                              names(locations_paste)[1] <- "Garda_Station" #changes column name
                          Total.40$Garda_Station <- paste(locations_paste$Garda_Station) # pastes values above in beside corresponding location
                          

                  #mean total of north & south each location
                       averageCrimesAllTimeNorth <- aggregate(Value~Garda_Station,northDublin,mean) #aggregation table of the mean values in North Dublin
                       averageCrimesAllTimeSouth <- aggregate(Value~Garda_Station,southDublin,mean) #aggregation table of the mean values in South Dublin
                       
                       
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
                                  
                                        iconsNorthStyle <- awesomeIcons(
                                          icon = 'ios-close',
                                          iconColor = 'black',
                                          library = 'ion',
                                          markerColor = getColorShinyNorth(north_location_2003_2019) #uses function getColorShinyNorth to determine colours of the variables on map
                                        )
                                        
                 #ICON CREATION FOR SHINY MAP DASHBOARD
                                        #function - sets colours for each pin icon, changes with values listed below
                                        getColorShinyNorth <- function(north_location_2003_2019) {
                                          sapply(north_location_2003_2019$Value, function(Value) {
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
                                  dashboardHeader(title = "Crime Dashboard"),
                                  dashboardSidebar( #sidebar formation for slider bar
                                    sliderInput("Year_Range", label = "Year Range", #slider bar
                                                min = 2003, #gets min year from 'year' column
                                                max = 2019, #gets max year from 'year' column
                                                value = c(min(north_location_2003_2019$Year, max(north_location_2003_2019$Year))),
                                                sep = "", #if this wasn't here year 2003 would be 20,03
                                                step = 1
                                                )
                                  ),
                                  dashboardBody(
                                    fluidRow(box(width = 12, leafletOutput(outputId = "north_map"))),
                                    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
                                  )
                                )
                                server <- function(input, output) {
                                  data_input <- reactive({
                                    north_location_2003_2019 %>%
                                      filter(Year >= input$Year_Range[1]) %>%
                                      group_by(Garda_Station)
                                  })

                                  data_input_ordered <- reactive({
                                    data_input()[order(match(data_input()$Location, north_location_2003_2019$Value)),]
                                  })
                                  
                                  
                                  labels <- reactive({ #reactive function changes labels as slider changes year
                                    paste("<p>", data_input_ordered()$Location, "</p>", #location word stored in paragraph tag
                                          sep = ""
                                      )
                                    
                                  })

                                  output$north_map <- renderLeaflet(
                                    leaflet()%>%addTiles()%>%addAwesomeMarkers(
                                      data = north_location_2003_2019, #locations data frame used
                                      lat = ~Latitude, #latitude coordinate taken from Latitude column in data frame
                                      lng = ~Longitude, #longitude coordinate taken from Longitude column in data frame
                                      icon=iconsNorthStyle, #sets the icon style
                                      label =  lapply(labels(), HTML), #adds locations to each label when hovered over the icon
                                      popup = ~as.character(data_input()$Value)) #displays pop-up of value of crimes in that area when icon pressed
                                  )
                                  
                                  output$summary_table <- renderDataTable(data_input()) #outputs summary table containing data below map on dashboard
                                }

                          shinyApp(ui = ui, server = server) #runs the shiny app, showing the dashboard & map
                          
                          #clusters
                          #NORTH DUBLIN
                          #writing to csv, then reading back in to have labels for cluster, col 1 = labels             
                          write.csv(averageCrimesAllTimeNorth, "/Users/ryanjohnston/development/r/crime/Datasets/averageNorthAllTime.csv", row.names = FALSE)
                          
                          #THIS DATASET HAS BEEN EDITED IN EXCEL AFTER WRITING FROM R, another column added to allow for cluster matrix ***
                          averageNorthAllTime <- read.csv("/Users/ryanjohnston/development/r/crime/Datasets/averageNorthAllTime.csv", header = TRUE, row.names = 1, sep = ",")
                          
                          set.seed(1234)
                          kmeans.ani(averageNorthAllTime[1:2], 2)
                          
                          #creates 3 clusters from data
                          km.clus <- kmeans(averageNorthAllTime, 3) #creates cluster with 3 clusters(groups)
                          fviz_cluster(km.clus, averageNorthAllTime, main="North Dublin Average Crimes")+theme_fivethirtyeight() #outputs visualisation of 3 clusters
                          
                          #SOUTH DUBLIN
                          write.csv(averageCrimesAllTimeSouth, "/Users/ryanjohnston/development/r/crime/Datasets/averageSouthAllTime.csv", row.names = FALSE)
                          
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
                       
                       northLabels <- c("Bridewell","Fitzgibbon", "Mountjoy", "Store St", "Balbriggan", "Garristown", "Lusk", "Skerries", "Ballymun", "Dublin Airport", "Santry", "Coolock", "Malahide", "Swords", "Clontarf", "Howth", "Raheny", "Blanchardstown", "Cabra", "Finglas")
                       barplot(totalNorthValue$Value,
                               ylab = "Values",
                               main = "North Dublin Total Crimes 2003-2019", 
                               names.arg = northLabels,
                               las = 2, 
                               cex.lab = 1,
                               cex.axis = 0.5, 
                               cex.names = 0.6, 
                               font.axis = 2,
                               space=c(0),
                               col = c("#3CA0D0")
                             )
                      
                      #barplot 5 year top 5 north vs south
                       #finglas, blanch, store st, bridewell, coolock
                       #tallaght, dundrum, dun laoighre, pearse st, kevin st
                       top5 <- c("Finglas/Tallaght","Blanch/Dundrum","Store St/Pearse St","Bridewell/Kevin St", "Coolock/Dun Laoighre")
                       top5North <- data.frame(50622,95535,157464,103752,50225)
                       top5South <- data.frame(101189,49157,171741,55677,45383)

                       write.xlsx(northDublin, file, sheetName = "Sheet1", 
                                  col.names = TRUE, row.names = TRUE, append = FALSE)
                       
                       #average crimes by year 2009-2019 -  FOR EDUCATION COMPARISON
                       ggplot(avgCrimesByYear, aes(x = factor(Year), y = Value)) +
                         geom_point() +
                         labs( title = "Average Crimes by Year",
                               x = "Crimes",  
                               y = "Year"
                         )+
                         theme_fivethirtyeight() +
                         theme(axis.title = element_text())
                       
              #data analysis tests
                       #creating linear model
                       fullmodel_northDublin_incomeAndCrime <- lm(Income ~ Crime.1 + Crime.2 + Crime.3 + Crime.4 + Crime.5 + Crime.6 + Crime.7 + Crime.8 + Crime.9 + Crime.10 + Crime.11 + Crime.12 ,data=crimeOffences_northDublin_reformatted)
                       #summary to check significance for correllation
                       summary(fullmodel_northDublin_incomeAndCrime)
                       plot(fullmodel_northDublin_incomeAndCrime)
                       
                       #creating linear model for columns with '*' - suggest level of statistical significance with regression coefficient
                       halfmodel_northDublin_incomeAndCrime <- lm(Income ~ Crime.1 + Crime.2 + Crime.4 + Crime.5 + Crime.10, data = crimeOffences_northDublin_reformatted )
                       summary(halfmodel_northDublin_incomeAndCrime)
                       plot(halfmodel_northDublin_incomeAndCrime)
                       
                       #data is NORMAL so parametric tests are to be done
                          #pearsons R correlation
                       #value = -1(perfectly negative association), 0(no association) , 1(perfectly positive association)
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.1, alternative = "greater")  #0.2351258 - correlation - positive association
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.2, alternative = "greater")  #-0.1579969
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.3, alternative = "greater")  #0.03016411 
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.4, alternative = "greater")  #-0.1341515
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.5, alternative = "greater")  #0.03577719
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.6, alternative = "greater")  #0.1255371 - correlation - positive association
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.8, alternative = "greater")  #0.07597163
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.7, alternative = "greater")  #0.03234386
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.9, alternative = "greater")  #-0.02089501
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.10, alternative = "greater") #-0.1074566
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.11, alternative = "greater") #-0.004724964
                       cor.test(crimeOffences_northDublin_reformatted$Income, crimeOffences_northDublin_reformatted$Crime.12, alternative = "greater") #0.004500105
                      