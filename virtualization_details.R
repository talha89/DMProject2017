

## load the data

job_data <- read.csv("C:/Users/adefisayo.akande/Documents/Personal/second semester/Data_Mining/project/new_project/DMProject2017/filtered_data - merged columns.csv", header =  TRUE )
head(job_data)
dim(job_data)
str(job_data)


# #randomly assign either 0 or 1 to the data column to show if the applicant either likes the job or dislikes it 
# job_data$like.dislike <- sample(0:1, size = nrow(job_data), replace = TRUE)

# #check the distribution 
# table(job_data$like.dislike)

levels(job_data$expert_country)

#Distribution of the Project Education
library(plyr)
print(count(job_data, 'expert_country'))
print(count(job_data, 'project_education'))
print(count(job_data, 'project_title'))

#convert education data to numeric
job_data$project_education1 <- as.numeric(factor(job_data$project_education))

summary(job_data$project_education1)

# Plots

par(ask = FALSE, mfrow = c(1,1) )

#Educational qualification ditribution per country
# Expert years of work expreince per country

summary(job_data$project_education)

# library(ggplot2)
# ggplot(job_data,  aes("Education Dsitribution", fill = project_education) ) +
#   geom_bar()

ggplot(job_data) +
  geom_bar( aes(project_education, fill = project_education) )


ggplot(job_data, aes(liked, fill = project_education) ) +
  geom_bar(position = "stack")


# expert working location per education
ggplot(job_data, aes(expert_work_from_home, fill = project_education) ) +
  geom_bar(position = "dodge")

#expert years of experince 
ggplot(job_data, aes(expert_experience,fill = project_education) ) +
  geom_histogram(binwidth = 3)

#project years of expereince and work location
ggplot(job_data, aes(project_experience, fill = expert_work_from_home) ) +
  geom_bar()


#project experince vs expertise and shows project lanaguage language
library(ggplot2)
expert_experience
 ggplot(job_data, aes(x=project_experience, y=expert_experience, fill=project_lang)) + 
  geom_violin()


 qplot(expert_experience, data = job_data, geom = "density",
       color = expert_work_from_home, linetype = expert_work_from_home)
 
 
 qplot(expert_experience, data = job_data, geom = "density",
       color = expert_work_from_home, size = 0.3)
 
 
 
#most spoken langauge
ggplot(job_data) +
  geom_bar( aes(expert_country, fill = project_lang) )

ggplot(job_data) +
  geom_bar( aes(project_country, fill = project_education) ) 

#expert project experince and langauge
ggplot(job_data, aes(project_experience, fill = project_lang) ) +
  geom_bar(position = "dodge")


#Expert years of expereince and work location
# library(plotly)
# library(ggplot2)
#ggplotly(x)

ggplot(job_data, aes(expert_experience, fill = expert_work_from_home) ) +
  geom_bar()

#QQ plot of project experince
qqnorm(job_data$project_experience) 
 + qqline(job_data$project_experience)

#countries and working location 
 ggplot(job_data, aes(expert_country, fill = expert_work_from_home) ) +
  geom_bar() 
 

 ##plotting project country locations on map of the world
 
 library(ggmap)
 library(maptools)
 library(maps)
 
 uniqueCountries = unique(job_data$project_country)
 geocodeCountries <- geocode(as.character(uniqueCountries))
 uniqueCountries.x <- geocodeCountries$lon
 uniqueCountries.y <- geocodeCountries$lat
 
 mp <- NULL
 mapWorld <- borders("world", colour="cornsilk", fill="cornsilk")
 mp <- ggplot() +   mapWorld
 
 mp <- mp+ geom_point(aes(x=uniqueCountries.x, y=uniqueCountries.y) ,color="red", size=1) 
 mp
 
 #############
 
 uniqueprojecte = unique(job_data$project_experience)
 geocodeCountries <- geocode(as.character(uniqueCountries))
 uniqueCountries.x <- geocodeCountries$lon
 uniqueCountries.y <- geocodeCountries$lat
 
 mp <- NULL
 mapWorld <- borders("world", colour="cornsilk", fill="cornsilk")
 mp <- ggplot() +   mapWorld
 
 mp <- mp+ geom_point(aes(x=uniqueCountries.x, y=uniqueCountries.y) ,color="red", size=1) 
 mp
 
##########################################
 
 

#Project experince vs expert experince. No coreelation betwwen project experince and expert experince 
plot(job_data$project_experience,job_data$expert_experience )
abline(lm(job_data$project_experience ~ job_data$expert_experience))


plot(job_data$expert_experience,job_data$project_experience )
abline(lm( job_data$project_experience~job_data$expert_experience , data= ))


#Expert experince amongst the various countries 
plot(job_data$expert_country,job_data$expert_experience,  ylab = "Experince" )

# Project langauge and exper experince
plot(job_data$project_lang,job_data$expert_experience, xlab = "Language", ylab = "Expert Experince" )


#project experince distribution. decline in number of expert with long project experienced 
ggplot(job_data, aes(project_experience) ) +
  geom_bar()


#project experince density plot # more experinced expert are between 10 and 15 years of experince 
ggplot(job_data, aes(expert_experience)) +
  geom_density()


#box plot

plot(job_data$project_education1~job_data$expert_work_from_home, xlab= "expert_work_from_home" ,ylab="project_education1" , main="boxplot")

#disribution count per country
plot(job_data$expert_country, , xlab= "expert_country", ylab= "count ", main= "Distribution")

################################################################

#ggplot(job_data,aes(job_data$expert_province,job_data$expert_experience ))

plot(job_data$applied,job_data$liked )


plot(job_data$project_education,job_data$expert_work_from_home)




















```

