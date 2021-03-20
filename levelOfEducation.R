#importing dataset
levelOfEducation <- read.csv(file = '/Users/ryanjohnston/OneDrive - National College of Ireland/-YEAR4-/Project Info/Datasets/LevelOfEducation.csv')

#renaming columns
names(levelOfEducation)[3] <- "Age_Group"
names(levelOfEducation)[5] <- "Education_Level"
names(levelOfEducation)[6] <- "Unit"
names(levelOfEducation)[7] <- "Value"

#removing rows of year 2020 as they are unfinished
levelOfEducation <- levelOfEducation[!(levelOfEducation$Quarter=="2020Q2"),]

    #male and female education dataframe creation - REMOVING BOTH SEXES ROWS
    educationMaleAndFemale <- levelOfEducation[!(levelOfEducation$Sex=="Both sexes"),]
    
          #dataframe - average of each type of education
             educationTypeAvg <- aggregate(educationMaleAndFemale$Value, by=list(educationMaleAndFemale$Education_Level,educationMaleAndFemale$Quarter), FUN = mean)
                names(educationTypeAvg)[1] = "Education_Level"
                names(educationTypeAvg)[2] = "Quarter"
                names(educationTypeAvg)[3] = "Value"
                
            #PLOT SHOWING AVG EDUCATION & TYPE - BY YEAR
          ggplot(educationTypeAvg, aes(x = Quarter, y = Value, color = Education_Level)) +
            geom_point()+
            theme_dark()
