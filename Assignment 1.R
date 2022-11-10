library(readr)
library(rio)
install_formats()

#setting working directory
getwd()
setwd('/Users/abhi/Desktop')

#importing comorbidity data set from files 
comorbidity <- read.csv(file = 'comorbidities.csv')

head(comorbidity)

summary(comorbidity)

typeof(comorbidity) 
# comorbidity is a list

# creating data frame from the list 
comorbidity.data <- as.data.frame(comorbidity)
print(comorbidity.data)
summary(comorbidity.data)

#deleting column 1 : comorbidity as it does not help with analysis 
comorbidity.data <- subset(comorbidity.data, select = -c(comorbidity))

library(tidyverse)

#get column names
colnames(comorbidity.data)

#renaming column names 
#cases to no_cases 
#deaths to no_of_deaths
colnames(comorbidity.data)[4] <- "no_of_cases"
colnames(comorbidity.data)[5] <- "no_of_deaths"

# converting sex, ethnicity and race to factor from character as it will be helpful to use group by analysis
comorbidity.data$sex = as.factor(comorbidity.data$sex)
comorbidity.data$ethnicity = as.factor(comorbidity.data$ethnicity)
comorbidity.data$race = as.factor(comorbidity.data$race)
comorbidity.data

#checking the structure of dataframe
str(comorbidity.data)
# 3 levels of sex 
# 4 levels of ethnicity 
# 8 levels of race 


# trying gsub on a temp variable 
temp <- comorbidity.data
temp
temp$sex<-gsub("Unknown","NA",as.character(temp$sex))

# replacing sex column Unknowns to NA using gsub
comorbidity.data$sex<-gsub("Unknown","NA",as.character(comorbidity.data$sex))
comorbidity.data

# trying ifelse to replace ethnicity Unknown to NA 
temp$ethnicity <- ifelse(temp$ethnicity %in% c('Unknown'), 'NA', temp$ethnicity)
temp

# replacing ethnicity column Unknowns to NA using ifelse 
comorbidity.data$ethnicity <- ifelse(comorbidity.data$ethnicity %in% c('Unknown'), 'NA', comorbidity.data$ethnicity)
comorbidity.data

# replacing race column Unknowns to NA using gsub
comorbidity.data$race<-gsub("Unknown","NA",as.character(comorbidity.data$race))
comorbidity.data

# removing rows which has Total as value under race and/or ethnicity columns 
# as it just contains the total for that category 
# and will not be useful for analysis
comorbidity.data<-comorbidity.data[!(comorbidity.data$ethnicity=="Total" | comorbidity.data$race=="Total"),]
comorbidity.data

# Add no of recovered cases using dplyr after no_of_deaths column 
# no_of_cases - no_of_deaths 
comorbidity.data <- comorbidity.data %>%
  add_column(no_of_recovered = comorbidity.data$no_of_cases - comorbidity.data$no_of_deaths,
             .after = "no_of_deaths") 
comorbidity.data

# Add proportion column using dplyr at the end 
# mortality_rate column to have the ratio between no_of_deaths to no_of_cases
comorbidity.data <- comorbidity.data %>%
  add_column(mortality_rate = comorbidity.data$no_of_deaths/comorbidity.data$no_of_cases,
             .after = "no_of_recovered") 
comorbidity.data

# replacing Infinite values/Nan to NA in mortality_rate column 
comorbidity.data$mortality_rate <- ifelse(comorbidity.data$mortality_rate %in% c('Inf','NaN'), 0, comorbidity.data$mortality_rate)
comorbidity.data


# change mortality_rate from character to numeric 
comorbidity.data$mortality_rate <- as.numeric(as.character(comorbidity.data$mortality_rate))

# format/round mortality_rate to have 2 decimal places for easy calculations 
comorbidity.data[, 'mortality_rate'] <- round(comorbidity.data[, 'mortality_rate'], digits = 2)

#comorbidity.data <- subset(comorbidity.data, select = -c(proportion))

str(comorbidity.data)


# Analysis of total number of cases and total number of deaths 
total_cases <- sum(comorbidity.data$no_of_cases)
total_cases

total_deaths <- sum(comorbidity.data$no_of_deaths)
total_deaths

# proportion of total cases over total deaths 
#total_proportion <- total_cases / total_deaths 
#total_proportion


# Analysis based on sex using groupby and filter 
# grouping does not chnage the struture of the dataframe, but just how it looks.
sex_analysis <- comorbidity.data %>% group_by(sex)
sex_analysis

# number of different sex values available 
table(sex_analysis$sex)

# summarizing and getting the mean of number of cases and number of deaths based on the sex
sex_analysis %>% summarise(
  no_of_cases = mean(no_of_cases),
  no_of_deaths = mean(no_of_deaths),
  no_of_recovered = mean(no_of_recovered)
)

# What proportion of deaths are among black population?
race_analysis <- comorbidity.data %>% group_by(race)
race_analysis

race_analysis_black_pop <- filter(comorbidity.data, comorbidity.data$race == 'African-American/ Black')
race_analysis_black_pop

total_black_pop_cases <- sum(race_analysis_black_pop$no_of_cases)
total_black_pop_cases

total_black_pop_deaths <- sum(race_analysis_black_pop$no_of_deaths)
total_black_pop_deaths

#proportion of deaths from black population in comparison with total number of deaths from all population 
proportion_of_black_pop_deaths <- total_black_pop_deaths / total_deaths
proportion_of_black_pop_deaths
# 35.14% 

# How would you examine the possible relationship between race/ethnicity and the probability of death given that someone has been diagnosed with COVID-19?
race_analysis <- race_analysis %>%
  add_column(death_rate = race_analysis$no_of_deaths/total_deaths,
             .after = "mortality_rate") 
race_analysis

# All race analysis 
#Asian
race_analysis_asian_pop <- filter(comorbidity.data, comorbidity.data$race == 'Asian')
race_analysis_asian_pop

total_asian_pop_cases <- sum(race_analysis_asian_pop$no_of_cases)
total_asian_pop_cases

total_asian_pop_deaths <- sum(race_analysis_asian_pop$no_of_deaths)
total_asian_pop_deaths

#American Indian/ Alaska Native
race_analysis_indian_pop <- filter(comorbidity.data, comorbidity.data$race == 'American Indian/ Alaska Native')
race_analysis_indian_pop

total_indian_pop_cases <- sum(race_analysis_indian_pop$no_of_cases)
total_indian_pop_cases

total_indian_pop_deaths <- sum(race_analysis_indian_pop$no_of_deaths)
total_indian_pop_deaths


#Native Hawaiian/ Pacific Islander
race_analysis_hawaiian_pop <- filter(comorbidity.data, comorbidity.data$race == 'Native Hawaiian/ Pacific Islander')
race_analysis_hawaiian_pop

total_hawaiian_pop_cases <- sum(race_analysis_hawaiian_pop$no_of_cases)
total_hawaiian_pop_cases

total_hawaiian_pop_deaths <- sum(race_analysis_hawaiian_pop$no_of_deaths)
total_hawaiian_pop_deaths

#Other
race_analysis_other_pop <- filter(comorbidity.data, comorbidity.data$race == 'Other')
race_analysis_other_pop

total_other_pop_cases <- sum(race_analysis_other_pop$no_of_cases)
total_other_pop_cases

total_other_pop_deaths <- sum(race_analysis_other_pop$no_of_deaths)
total_other_pop_deaths

#White 
race_analysis_white_pop <- filter(comorbidity.data, comorbidity.data$race == 'White')
race_analysis_white_pop

total_white_pop_cases <- sum(race_analysis_white_pop$no_of_cases)
total_white_pop_cases

total_white_pop_deaths <- sum(race_analysis_white_pop$no_of_deaths)
total_white_pop_deaths


# All ethnicity analysis 
#Hispanic/ Latino
ethnicity_analysis_hispanic_pop <- filter(comorbidity.data, comorbidity.data$ethnicity == 'Hispanic/ Latino')
ethnicity_analysis_hispanic_pop

total_hispanic_pop_cases <- sum(ethnicity_analysis_hispanic_pop$no_of_cases)
total_hispanic_pop_cases

total_hispanic_pop_deaths <- sum(ethnicity_analysis_hispanic_pop$no_of_deaths)
total_hispanic_pop_deaths

#Non-Hispanic/ Latino
ethnicity_analysis_non_hispanic_pop <- filter(comorbidity.data, comorbidity.data$ethnicity == 'Non-Hispanic/ Latino')
ethnicity_analysis_non_hispanic_pop

total_non_hispanic_pop_cases <- sum(ethnicity_analysis_non_hispanic_pop$no_of_cases)
total_non_hispanic_pop_cases

total_non_hispanic_pop_deaths <- sum(ethnicity_analysis_non_hispanic_pop$no_of_deaths)
total_non_hispanic_pop_deaths

#NA 
ethnicity_analysis_na_pop <- filter(comorbidity.data, comorbidity.data$ethnicity == 'NA')
ethnicity_analysis_na_pop

total_na_pop_cases <- sum(ethnicity_analysis_na_pop$no_of_cases)
total_na_pop_cases

total_na_pop_deaths <- sum(ethnicity_analysis_na_pop$no_of_deaths)
total_na_pop_deaths

# Creating dataframe with race data to find relationship between race and probability of death
race_list <- c('African-American/ Black', 'American Indian/ Alaska Native', 'Asian', 'Native Hawaiian/ Pacific Islander', 'Other', 'White')
race_no_of_cases <- c(total_black_pop_cases,total_indian_pop_cases,total_asian_pop_cases,total_hawaiian_pop_cases,total_other_pop_cases,total_white_pop_cases)
race_no_of_deaths <- c(total_black_pop_deaths,total_indian_pop_deaths,total_asian_pop_deaths,total_hawaiian_pop_deaths,total_other_pop_deaths,total_white_pop_deaths)
race_df <- data.frame(race_list,race_no_of_cases,race_no_of_deaths)
race_df
str(race_df)

# totak number of cases across all races 
total_cases_race_df <- sum(race_df$race_no_of_cases)
total_cases_race_df

# adding probability column to each race 
race_df <- race_df %>%
  add_column(probability_of_death = race_df$race_no_of_deaths / total_cases_race_df,
             .after = "race_no_of_deaths") 
race_df
str(race_df)

# format probability_of_death to 2 decimal places 
race_df[, 'probability_of_death'] <- round(race_df[, 'probability_of_death'], digits = 2)


# creating data frame for ethnicity data to find relationship between ethnicity and probability of death

ethnicity_list <- c('Hispanic/ Latino', 'Non-Hispanic/ Latino', 'Unknown')
et_no_of_cases <- c(total_hispanic_pop_cases,total_non_hispanic_pop_cases,total_na_pop_cases)
et_no_of_deaths <- c(total_hispanic_pop_deaths,total_non_hispanic_pop_deaths,total_na_pop_deaths)
ethnicity_df <- data.frame(ethnicity_list,et_no_of_cases,et_no_of_deaths)
ethnicity_df
str(ethnicity_df)

# totak number of cases across all ethnicities 
total_cases_et_df <- sum(ethnicity_df$et_no_of_cases)
total_cases_et_df

# adding probability column to each race 
ethnicity_df <- ethnicity_df %>%
  add_column(probability_of_death = ethnicity_df$et_no_of_deaths / total_cases_et_df,
             .after = "et_no_of_deaths") 
ethnicity_df
str(ethnicity_df)

# format probability_of_death to 2 decimal places 
ethnicity_df[, 'probability_of_death'] <- round(ethnicity_df[, 'probability_of_death'], digits = 4)

summary(race_df)
summary(ethnicity_df)

#installing writexl 
install.packages("writexl")
library("writexl")

#writing race and ethnicity data frames into a sharable excel. 
write_xlsx(race_df,"/Users/abhi/Desktop/race_table.xlsx")
write_xlsx(ethnicity_df,"/Users/abhi/Desktop/ethnicity_table.xlsx")

# histogram of number of cases based on race 
race_no_of_cases_hist <- race_df$race_no_of_cases
hist(race_no_of_cases_hist,
     main="Maximum number of cases based on race",
     xlab="Number of cases",
     col="darkmagenta")


# create data for histogram of number of cases 
comorbidity_prob <- comorbidity.data$no_of_cases
hist(comorbidity_prob,
     border="blue", 
     col="green",
     xlab="Number of cases",
     main="Histogram of number of cases in comorbidity dataset")

# bar chart 
barplot(height = ethnicity_df$et_no_of_cases, names = ethnicity_df$ethnicity_list, col = rainbow(3))

et_table <- table(et_no_of_cases, et_no_of_deaths)

barplot(et_table,
        main = "Number of deaths based on ethnicity",
        xlab = "Ethnicity", ylab = "Frequency",
        col = c("darkgrey", "darkblue", "red"),
        legend.text = ethnicity_df$ethnicity_list,
        beside = TRUE) # Grouped bars

