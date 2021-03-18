#importing dataset
levelOfEducation <- read.csv(file = '/Users/ryanjohnston/OneDrive - National College of Ireland/-YEAR4-/Project Info/Datasets/LevelOfEducation.csv')

#renaming columns
names(levelOfEducation)[3] <- "Age_Group"
names(levelOfEducation)[5] <- "Education_Level"
names(levelOfEducation)[6] <- "Unit"
names(levelOfEducation)[7] <- "Value"

#removing duplicate column
levelOfEducation = subset(levelOfEducation, select = -TLIST.Q1.) 

#removing rows of year 2020 as they are unfinished
levelOfEducation<-levelOfEducation[!(levelOfEducation$Quarter=="2020Q2"),]

      #casting numbers to each education





