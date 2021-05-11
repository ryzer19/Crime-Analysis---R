#######################################################
#MAKE SURE TO RUN CRIMEOFFENCESSCRIPT.R LIBRARIES FIRST
#######################################################
#libraries can be read for crime dataset first & used on this

#importing dataset
levelOfEducation <- read.csv(file = '/Users/ryanjohnston/OneDrive - National College of Ireland/-YEAR4-/Project Info/Datasets/LevelOfEducation.csv')



#Exploratory Data Analysis
glimpse(levelOfEducation)
status(levelOfEducation)




      #removing 'quarter (Q2)' from yearly value to make it an int
      #creating stopword variable to look for the Q2 at the end
      stopwords_education = c("Q2")
      #making variable with stopword removed
      levelOfEducation_years <- gsub(paste0(stopwords_education,collapse = "|"),"", levelOfEducation$Quarter)
      #pasting variable into the quarter column
      levelOfEducation$Quarter <- paste(levelOfEducation_years)
      
      #changing value from char to numeric(double)
      levelOfEducation$Quarter <- as.numeric(levelOfEducation$Quarter)
      




#renaming columns
names(levelOfEducation)[2] <- "Year"
names(levelOfEducation)[3] <- "Age_Group"
names(levelOfEducation)[5] <- "Education_Level"
names(levelOfEducation)[6] <- "Unit"
names(levelOfEducation)[7] <- "Value"

#removing rows of year 2020 as they are unfinished
levelOfEducation <- levelOfEducation[-c(2377:2592),]


#male and female education dataframe creation - REMOVING BOTH SEXES ROWS
educationMaleAndFemale <- levelOfEducation[!(levelOfEducation$Sex=="Both sexes"),]

#dataframe - average of each type of education
educationTypeAvg <- aggregate(educationMaleAndFemale$Value, by=list(educationMaleAndFemale$Education_Level,educationMaleAndFemale$Year), FUN = mean)
names(educationTypeAvg)[1] = "Education_Level"
names(educationTypeAvg)[2] = "Year"
names(educationTypeAvg)[3] = "Value"

#creating dataframe only including top 5 avg types of education
criteria_education_top5 <- c("Third level", "Upper secondary", "Lower secondary", "Honours bachelor degree/professional qualification or both", "Post leaving cert")
education_top5 <- educationTypeAvg[educationTypeAvg$Education_Level %in% criteria_education_top5, ]

#PLOT SHOWING TOP 5 EDUCATION TYPE - AVG EDUCATION & TYPE - BY YEAR
ggplot(education_top5, aes(x = Year, y = Value, color = Education_Level)) +
  geom_point(size = 3)+
  scale_y_continuous(breaks=seq(0, 40, 5))+
  scale_x_continuous(breaks=seq(2009, 2019, ))+
  theme_fivethirtyeight() + theme_dark()

#cluster
#aggregating all time average values of each education type
education_aggregate <- aggregate(Value~Education_Level,educationTypeAvg,sum)

#writing to csv, then reading back in to have labels for cluster, col 1 = labels (each type of education as labels on cluster)   
#write.csv(education_aggregate, "/Users/ryanjohnston/development/r/crime/Datasets/education_aggregate.csv", row.names = FALSE)

#edited in excel, another column added to allow for cluster matrix ***
education_aggregate_cluster <- read.csv("/Users/ryanjohnston/development/r/crime/Datasets/education_aggregate.csv", header = TRUE, row.names = 1, sep = ",")

set.seed(1234)

#creates 3 clusters from data
km.clus <- kmeans(education_aggregate_cluster, 3) #creates cluster with 3 clusters(groups)
fviz_cluster(km.clus, education_aggregate_cluster, main="Average Education Type - All Time")+theme_fivethirtyeight() #outputs visualisation of 3 clusters
