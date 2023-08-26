
#test to push 
squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

install.packages("renv")
install.packages("tidytuesdayR")

install.packages("ggplot2")
install.packages('tidyverse')   
install.packages(c('pacman', 'here'))
install.packages(c('tinytex', 'rmarkdown', 'knitr')) 
tinytex::install_tinytex()  
 
install.packages(c('tidycensus','tigris')) 
install.packages(c('sp', 'sf', 'raster', 'RColorBrewer', 'rgeos', 'rgdal', 'maptools', 'OpenStreetMap'))  
install.packages(c('tmap', 'tmaptools')) 
install.packages(c('spdep', 'CARBayes', 'sparr', 'spatialreg',  'DCluster', 'SpatialEpi', 'smerc'))
install.packages(c('GWmodel', 'spgwr') )
help('census_api_key','tidycensus')
# this function installs the tinytex LaTex on your computer which is necessary for rendering (creating) PDF's

library(gtsummary)
library(broom)
library(readr)
library(here)
library(renv)
library(dplyr)


squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')
#don't need this once i download squirrel dataset, can just use the code below

data <- read.csv(here::here("data","squirrel_data.csv"))

# Create a summary table (this is the correct code for the summary table)
summary_table <- squirrel_data %>%
  select(Age, `Tail twitches`, Location, Shift, Approaches) %>%
  tbl_summary(by = Approaches)

#create a function (this is the correct code)
average_adults_approaching<-function(data){mean(data$Approaches[squirrel_data$AgeNumeric == 1], na.rm = TRUE)}

# Calculate average number of juveniles that approach 
average_juveniles_approaching <- mean(squirrel_data$Approaches[squirrel_data$AgeNumeric == 0], na.rm = TRUE)

#summary table code from class  (tweak it a little)

inline_text.tbl_summary(variables = c("Age", "Tail twitches", "Location","Shift", "Approaches"))
#use inline text for summary or logistic table, if categorical add the level function


# Fit a logistic regression model (this is correct)
logistic_model <- glm(Approaches ~ Age + `Tail twitches` + Location + Shift, data = squirrel_data, family = "binomial")


#logistic regression code from class (update the label names)
tbl_regression(logistic_model,
intercept = TRUE,
label = list(
Approaches_data ~ "Approaches"
Age_data ~ "Age",
Tail twitches_data ~ "Tail twitches",
Location_data ~ "Location"
Shift_data ~ "Shift"
))
  

#create a bar plot (update code)
histogram <- hist(data$age)

#create a quarto document file>new file> quarto document. change from visual to source. copy and paste code into r chunk (double check github for examples)
#`r nrow(data)` when i want to enter code . ex: there are nrow observations in the dataset



renv::int()
#at the end of all the code do renv::int()
