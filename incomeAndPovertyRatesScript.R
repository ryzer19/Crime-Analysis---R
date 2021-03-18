install.packages("animation")	
install.packages("mclust")
install.packages("factoextra")

library(dplyr)
library(animation)
library(mclust)
library(cluster)
library(factoextra)
library(writexl)
library(ggplot2)

#creates data frame from csv file
incomeAndPovertyRates <- read.csv(file = '/Users/ryanjohnston/development/r/crime/Datasets/Income&PovertyRates.csv')


#changing dataframe column names
names(incomeAndPovertyRates)[1] <- "Statistic"
names(incomeAndPovertyRates)[3] <- "Age_Range"
names(incomeAndPovertyRates)[4] <- "Unit"
names(incomeAndPovertyRates)[5] <- "Value"

#getting mean equivalised real disposable income from full dataframe
incomeDF <- data.frame(ageGroup = c(incomeAndPovertyRates$Age_Range), year = c(incomeAndPovertyRates$Year),meanEquivRealDisInc = c(incomeAndPovertyRates$Statistic == "Mean Equivalised Real Disposable Income"),incomeAndPovertyRates$Value)
#filtering data to new data frame based on the values that came up as TRUE
incomeDFfiltered <- filter(incomeDF, meanEquivRealDisInc == "TRUE")
#changing dataframe names
names(incomeDFfiltered)[1] <- "Age_Group"
names(incomeDFfiltered)[2] <- "Year"
names(incomeDFfiltered)[4] <- "Value"

#removes logical(containing TRUE) column from data frame
incomeDFfiltered = subset(incomeDFfiltered, select = -meanEquivRealDisInc) 

#removes age group to have only values for clustering
incomeDFforCluster = subset(incomeDFfiltered, select = -Age_Group) 

#plotting values on geom point graph
ggplot(incomeDFfiltered, aes(x = Year, y = Value, color = Age_Group)) +
    geom_point() +
    ylim (15000, 30000) +
    xlim (2004, 2020)

    #dataframe creation for avg crimes by year
      averageCrimesByYear <- aggregate(southDublin$Value, by=list(southDublin$Garda_Station), FUN=sum)
      


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
