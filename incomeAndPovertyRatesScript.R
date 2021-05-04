install.packages("animation")	
install.packages("mclust")
install.packages("factoextra")
install.packages("ggthemes")


library(dplyr)
library(animation)
library(mclust)
library(cluster)
library(factoextra)
library(writexl)
library(ggplot2)
library(ggthemes)

#creates data frame from csv file
incomeAndPovertyRates <- read.csv(file = '/Users/ryanjohnston/development/r/crime/Datasets/Income&PovertyRates.csv')


#changing dataframe column names
names(incomeAndPovertyRates)[1] <- "Statistic"
names(incomeAndPovertyRates)[3] <- "Age_Range"
names(incomeAndPovertyRates)[4] <- "Unit"
names(incomeAndPovertyRates)[5] <- "Value"

#getting mean real real disposable income from full data frame
incomeDF <- data.frame(ageGroup = c(incomeAndPovertyRates$Age_Range), year = c(incomeAndPovertyRates$Year),meanRealDispInc = c(incomeAndPovertyRates$Statistic == "Mean Real Household Disposable Income"),incomeAndPovertyRates$Value)

#filtering data to new data frame based on the values that came up as TRUE
incomeDFfiltered <- filter(incomeDF, meanRealDispInc == "TRUE")

#removes logical(containing TRUE) column from data frame
incomeDFfiltered = subset(incomeDFfiltered, select = -meanRealDispInc)

#changing dataframe names
names(incomeDFfiltered)[1] <- "Age_Group"
names(incomeDFfiltered)[2] <- "Year"
names(incomeDFfiltered)[3] <- "Value"

#removes age group to have only values for clustering
incomeDFforCluster = subset(incomeDFfiltered, select = -Age_Group)

    #avg income by year - all ages(no age group)
        #remove age group 0-17 as it contains NA's
        avgIncome <- incomeDFfiltered[-c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46),]
  
        #gets mean of values by year for people 18+
        avgIncome <- aggregate(avgIncome$Value, by=list(avgIncome$Year), FUN=mean)
              names(avgIncome)[1] = "Year"
                names(avgIncome)[2] = "Value"
                
        #dataframe creation for avg crimes by year north & south
          averageCrimesByYearNorth <- aggregate(northDublin$Value, by=list(northDublin$Year), FUN=mean)
          averageCrimesByYearSouth <- aggregate(southDublin$Value, by=list(southDublin$Year), FUN=mean)
          
          #joins two data frames - Year|South|North
          averageCrimesByYearBoth <- left_join(averageCrimesByYearSouth, averageCrimesByYearNorth, 
                                 by = c("Group.1" = "Group.1"))
          #renaming columns for visualisation
          names(averageCrimesByYearBoth)[1] = "Year"
            names(averageCrimesByYearBoth)[2] = "South"
              names(averageCrimesByYearBoth)[3] = "North"
            
              #plot showing avg crimes & year - without income
              ggplot(averageCrimesByYearBoth, aes(x = Year, y = South)) +
                geom_point()
              
                    #getting mean of both areas by year
                    avgCrimesByYear <- (averageCrimesByYearBoth[2] + averageCrimesByYearBoth[3])/2
                    #pastes year value as new column
                    avgCrimesByYear$Year <- paste(averageCrimesByYearBoth$Year)
                      names(avgCrimesByYear)[1] = "Value"

                      
                #adding income to avg crimes dataframe
                      #both dataframes must start from 2004- (removing 2003 year of crime)
                      avgCrimesByYear <- avgCrimesByYear[-c(1),]
                    
                       #changing 'year' in avgCrimesByYear(char) to numeric for join
                             avgCrimesByYear$Year = as.numeric(as.character(avgCrimesByYear$Year))
                              
                            #joining df's
                              avgCrimesWithIncome <- left_join(avgCrimesByYear, avgIncome, 
                                                                 by = c("Year" = "Year"))
                                       names(avgCrimesWithIncome)[1] = "CrimeValue"
                                       names(avgCrimesWithIncome)[3] = "IncomeValue"
                              
            
              

#cluster
set.seed(1234)

kmeans.ani(incomeDFfiltered[2:3], 3)

#creates 3 clusters from data
km.clus <- kmeans(incomeDFforCluster, 3)
fviz_cluster(km.clus, incomeDFforCluster) #outputs visualisation of 3 clusters
res.km <- eclust(incomeDFforCluster, "kmeans") #shows data plotted

#clustering
income_cluster1 <- Mclust(incomeDFfiltered)
plot(income_cluster1)
summary(income_cluster1)

                #VISUALISATIONS#
          #AGE GROUP INCOME BY YEAR
          ggplot(incomeDFfiltered, aes(x = Year, y = Value, color = Age_Group)) +
            geom_line(size=1) + geom_point(size=2) +
            labs(title = "Age Group Income",
              subtitle = "       by Year",
              xlab = "Year",
              ylab = "Income",
              color = "Age"
            ) +
            xlim (2004, 2020) +
            ylim (20000, 60000) +
            theme_fivethirtyeight() +
            theme(axis.title = element_text())
          
          #average income plot
          ggplot(avgIncome, aes(x = Year, y = Value)) +
            geom_line(size=0.5, color=1)+ geom_point(size=2.5,color=4)+
            labs( title = "Average income by Year",
                  x = "Year",  
                  y = "Income"
            )+
            scale_x_continuous(breaks=seq(2004,2020,2))+
            scale_y_continuous(breaks=seq(35000,50000,2000))+
            theme_fivethirtyeight() +
            theme(axis.title = element_text())

          
          #AVG CRIMES, AVG INCOME, YEAR
          ggplot(avgCrimesWithIncome, aes(x = Year, y = CrimeValue, size = IncomeValue)) +
            geom_point() +
            labs( title = "Average Crimes by Year",
                  subtitle = "        with Average Income",
              x = "Crimes",  
              y = "Year",
              size = "Income"
            )+
            xlim (2004, 2020) +
            ylim (170, 242) +
            theme_fivethirtyeight() +
            theme(axis.title = element_text())