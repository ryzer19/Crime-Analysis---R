install.packages("animation")	
install.packages("mclust")
install.packages("factoextra")

library(dplyr)
library(animation)
library(mclust)
library(cluster)
library(factoextra)
library(writexl)

#creates data frame from csv file
incomeAndPovertyRates <- read.csv(file = '/Users/ryanjohnston/OneDrive - National College of Ireland/- YEAR 4 -/Project Info/Datasets/Income&PovertyRates.csv')


#changing dataframe column names
names(incomeAndPovertyRates)[1] <- "Statistic_ID"
names(incomeAndPovertyRates)[5] <- "Participants"
names(incomeAndPovertyRates)[6] <- "Age_Range"
names(incomeAndPovertyRates)[8] <- "Value"

#TLIST column removed as it was identical to 'year' column
incomeAndPovertyRates = subset(incomeAndPovertyRates, select = -TLIST.A1.) 


#getting mean equivalised real disposable income from full dataframe
incomeDF <- data.frame(ageGroup = c(incomeAndPovertyRates$Age_Range), year = c(incomeAndPovertyRates$Year),meanEquivRealDisInc = c(incomeAndPovertyRates$Statistic == "Mean Equivalised Real Disposable Income"),incomeAndPovertyRates$VALUE)
#filtering data to new data frame based on the values that came up as TRUE
incomeDFfiltered <- filter(incomeDF, meanEquivRealDisInc == "TRUE")
#changing dataframe names
names(incomeDFfiltered)[1] <- "Age_Group"
names(incomeDFfiltered)[2] <- "Year"
names(incomeDFfiltered)[4] <- "Value"

#removes logical column from data frame
incomeDFfiltered = subset(incomeDFfiltered, select = -Logical) 

#removes age group to have only values for clustering
incomeDFforCluster = subset(incomeDFfiltered, select = -Age_Group) 
incomeDFforCluster = subset(incomeDFforCluster, select = -meanEquivRealDisInc)

#plotting values on geom point graph
ggplot(incomeDFfiltered, aes(x = Year, y = Value, color = Age_Group)) +
    geom_point() +
    ylim (15000, 30000) +
    xlim (2004, 2020)


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

str(northDublin)