###################CA2- Predictive Modeling###############################################################################################################################################################################

#Initial load of the data is performed
covid_df <- read.csv('covid.csv', na="")

# The head function displays the first 15 rows
#ncol fuctions gives the number of columns the dataset has
#nrow displays the number of rows the Covid dataset has
#The name function gives the variables names in the Covid dataset 
head(covid_df, 15)
ncol(covid_df)                                                                                                                                                                                                  
nrow(covid_df)
names(covid_df)

#--------------------------------DATA PREPARATION-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#The data is Cleaned and transformed to build a model
#A subgroup is created with the variables
#VIM Library is used to find out the anomalies in the data
attach(covid_df)

covid <- subset.data.frame(covid_df,continent == 'Europe', select = c(continent, total_cases, new_cases,iso_code,
                                                                           total_deaths, new_deaths, total_cases_per_million,  #the subset is created with europe data where thevariables here are subgrouped
                                                                           total_deaths_per_million, new_cases_per_million,
                                                                           reproduction_rate, icu_patients,
                                                                           icu_patients_per_million, 
                                                                           new_tests, new_tests_per_thousand,
                                                                           total_tests, total_tests_per_thousand,
                                                                           positive_rate, total_vaccinations, 
                                                                           total_vaccinations_per_hundred, people_vaccinated, people_fully_vaccinated,
                                                                           stringency_index, population, population_density,
                                                                           median_age, aged_65_older, aged_70_older,
                                                                           gdp_per_capita, handwashing_facilities, 
                                                                           hospital_beds_per_thousand,
                                                                           female_smokers, male_smokers, 
                                                                           diabetes_prevalence, extreme_poverty, 
                                                                            cardiovasc_death_rate))

detach(covid_df)


#VIM introduces tools for visualization of missing and imputed values. 
#Furthermore, methods to impute missing values are featured.
#VIM Library is used to find out the anomalies in the data.
#aggr- aggregates the missing values and denotes them
#the Prop is turned false when the proportion of frequencies of different combinations should not be represented by numbers.
# The summary gives the statics of the missing values like mean etc.
library(VIM)
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
summary(missing_values)
summary(covid)


#There are many columns with less data or no data recorded, so replacing them with zero
#$ variable lets us to access the particular variable within the dataset
#is.na- is null replaced with zero
attach(covid)
covid$people_vaccinated[is.na(people_vaccinated)] <- 0
covid$total_cases[is.na(total_cases)] <- 0
covid$total_cases_per_million[is.na(total_cases_per_million)] <- 0
covid$new_deaths[is.na(new_deaths)] <- 0
covid$new_cases[is.na(new_cases)] <- 0
covid$total_deaths[is.na(total_deaths)] <- 0
covid$people_fully_vaccinated[is.na(people_fully_vaccinated)] <- 0
covid$total_tests_per_thousand[is.na(total_tests_per_thousand)] <- 0
covid$total_vaccinations[is.na(total_vaccinations)] <- 0
covid$new_cases_per_million[is.na(new_cases_per_million)] <- 0
covid$total_tests[is.na(total_tests)] <- 0
covid$total_deaths_per_million[is.na(total_deaths_per_million)] <- 0
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
summary(covid)

#A model to when it is to be good should have minimal NA's so the variables which has more number of Na's are removed.
#lets us find the variables which has more number of NA's
#using Summary function we can easily denoted the Na's in the dataset
#By Summarizing we can say 19 columns have higest Na's
#Creating a subset for the variables having higest NA's and removing them as they are not suitable to build a model
summary(covid)
covid <- subset.data.frame(covid,!is.na(population_density), select = -c(icu_patients,
                                                                         new_tests, 
                                                                         new_tests_per_thousand,
                                                                         positive_rate, 
                                                                         total_vaccinations_per_hundred,
                                                                         handwashing_facilities,
                                                                         icu_patients_per_million,
                                                                         extreme_poverty,
                                                                         reproduction_rate, 
                                                                         male_smokers,
                                                                         female_smokers, 
                                                                         new_tests,
                                                                         total_vaccinations,
                                                                         new_deaths,
                                                                         total_deaths_per_million,
                                                                         icu_patients_per_million, 
                                                                         total_cases_per_million, 
                                                                         new_cases_per_million, 
                                                                         hospital_beds_per_thousand))





#The data which has more than 5 Na's is also removed
#Checking for missing values again to check if the missing values are aggregated and removed
#Using summary function we determine the basic stasis of the covid data
#checking for missing values again after using of omit function
#finally detachment of covid data
covid <- covid[rowSums(is.na(covid) ) < 4,]
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
summary(covid)
covid <- na.omit(covid)
missing_values <- aggr(covid, prop = FALSE, numbers = TRUE)
summary(covid)
detach(covid)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#To find the correlation between the variables

#In the code the ggploting is made to know more about the scattering of the data,
#firstly the new_cases is selected as independent variable with ten other dependent variable.

##### 1. Correlation for new_cases and total_cases:#####
one <- lm(total_cases ~ new_cases, data = covid)
summary(one)
summary(covid)

install.packages("ggplot2")
library(ggplot2)

ggplot(data = covid, aes(x = new_cases, y = total_cases)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  theme(panel.background = element_rect(fill = "grey"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Prediction of total cases based on new cases")



# The correlation for new cases with total cases is 0.65, R-sq value is 0.37 and P-value is less that 2.2e-16.
#Which says there is strong correlation and if new cases increase the total cases also increase. 

paste("correltion for new_cases and total_cases: ", cor(new_cases, total_cases))      # 0.65150878614081




#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##### 2.Correlation for new_cases and total_cases:#####

#A graph is plotted with New_cases that is independent variable on X-axis with total_deaths as dependent variabtle on Y-axis 

two <- lm(total_deaths ~ new_cases, data = covid)
summary(two)

ggplot(data = covid, aes(x = new_cases, y = total_deaths)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  theme(panel.background = element_rect(fill = "brown"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of total deaths based on new cases")

#The correlation for new cases with total_deaths is 0.61, R-sq value is 0.37 and P-value is less than 2.2e-16. 
#Which says there is strong correlation and if new cases increase the total_deaths also increase. 

paste("correltion for new_cases and total_deaths: ", cor(new_cases, total_deaths))  # 0.65150878614081



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



##### 3.Correlation for total_deaths and total_cases::#####

 
#A graph is plotted with total_cases that is independent variable on X-axis with total_deaths as dependent variabtle on Y-axis

three <- lm(total_deaths ~ total_cases, data = covid)
summary(three)

ggplot(data = covid, aes(x = total_cases, y = total_deaths)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Prediction of total deaths based on total cases")

#The correlation for total_cases with total_deaths is 0.934, R-sq value is 0.87 and P-value is less than 2.2e-16.
#Which says there is very strongly related correlation and if total_cases increase the total_deaths also increase.


paste("correltion for total_casess and total_deaths: ", cor(total_cases, total_deaths))  # 0.934026835126423



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

######4.  Correlation for new_cases and aged_65_older:#######
#A grapgh is plotted with New_cases that is independent variable on X-axis with aged_65_older as dependent variabtle on Y-axis 
four <- lm(aged_65_older ~ new_cases, data = covid)
summary(four)

ggplot(data = covid, aes(x = new_cases, y = aged_65_older)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  theme(panel.background = element_rect(fill = "brown"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of aged_65_older based on new cases")

#Which says there is no correlation and if new cases increase the aged_65_older has no effect 
#The new cases and aged_65_older are not proportional to each other.
paste("correltion for aged_65_older and new_cases: ", cor(aged_65_older, new_cases)) #0.111802796667657



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

########### 5.  Correlation for new_cases and aged_70_older:########
#A graph is plotted with New_cases that is independent variable on X-axis with aged_70 older as dependent variabtle on Y-axis
five <- lm(aged_70_older ~ new_cases, data = covid)
summary(five)

ggplot(data = covid, aes(x = new_cases, y = aged_70_older)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  theme(panel.background = element_rect(fill = "brown"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of aged_70_older based on new cases")

#Which says there is no correlation and if new cases increase the aged_70_older has no great change. 
#The new cases and aged_70_older are not proportional to each other
paste("correltion for aged_70_older and new_cases: ", cor(aged_70_older, new_cases)) #0.132586710485861



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 ############ 6.  Correlation for new_cases and gdp_per_capita:##########
#A grapgh is plotted with New_cases that is independent variable on X-axis with gdp_per_capita as dependent variabtle on Y-axis



six <- lm(gdp_per_capita ~ new_cases, data = covid)
summary(six)

ggplot(data = covid, aes(x = new_cases, y = gdp_per_capita)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  theme(panel.background = element_rect(fill = "brown"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of gdp_per_capita based on new cases")

#Which says there is weak correlation and if new cases increase the gdp_per_capita decreases 
#The new cases and gdp_per_capita are inversely proportional to each other.


paste("correltion for gdp_per_capita and new_cases: ", cor(gdp_per_capita, new_cases)) #0.132586710485861


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###########  7.  Correlation for new_cases and diabetes_prevalence:#######

#A graph is plotted with New_cases that is independent variable on X-axis with diabetes_prevalence as dependent variabtle on Y-axis 


seven <- lm(diabetes_prevalence ~ new_cases, data = covid)
summary(seven)

ggplot(data = covid, aes(x = new_cases, y = diabetes_prevalence)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  theme(panel.background = element_rect(fill = "blue"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of diabetes_prevalence based on new cases")

#Which says there is weak correlation and if new cases increase the diabetes_prevalence decreases 
#The new cases and diabetes_prevalence is inversely proportional to each other.
paste("correltion for diabetes_prevalence and new_cases: ", cor(diabetes_prevalence, new_cases)) #-0.0527819072467536



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
###### 8.  Correlation for new_cases and people_vaccinated:######

#A grapgh is plotted with New_cases that is independent variable on X-axis with people_vaccinatedas dependent variabtle on Y-axis 
eight <- lm( ~ new_cases, data = covid)
summary(eight)

ggplot(data = covid, aes(x = new_cases, y = people_vaccinated)) +
  geom_point() +people_vaccinated
stat_smooth(method = "lm", col = "green") +
  theme(panel.background = element_rect(fill = "violet"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of people_vaccinated based on new cases")

#Which says there is weak correlation and if new cases increase the people_vaccinated decreases. 
#The new cases and people_vaccinated are inversely proportional to each other.

paste("correltion for people_vaccinated and new_cases: ", cor(people_vaccinated, new_cases)) #0.220262479369432


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
########9.  Correlation for new_cases and population:########3
#A grapgh is plotted with New_cases that is independent variable on X-axis with population as dependent variable on Y-axis 
nine <- lm(population ~ new_cases, data = covid)
summary(nine)

ggplot(data = covid, aes(x = new_cases, y = population)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  theme(panel.background = element_rect(fill = "pink"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of population based on new cases")

#Which says there is strong correlation and if new cases increase the population increases. 
#The new cases and population are directly proportional to each other.

paste("correltion for population and new_cases: ", cor(population, new_cases)) #0.468499572376311


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#########10.  Correlation for new_cases and total_tests:######
#A grapgh is plotted with New_cases that is independent variable on X-axis with total_tests as dependent variable on Y-axis 

ten <- lm(total_tests ~ new_cases, data = covid)
summary(ten)

ggplot(data = covid, aes(x = new_cases, y = total_tests)) +
  geom_point() +
  stat_smooth(method = "lm", col = "violet") +
  theme(panel.background = element_rect(fill = "lavender"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("prediction of total_tests based on new cases")

#Which says there is weak correlation and if new cases increase the total_tests decreases. 
#The new cases and total_tests are inversely proportional to each other
paste("correltion for total_tests and new_cases: ", cor(total_tests, new_cases)) #0.37009373852408


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
######Building a predictive model:#####
#To build a predictive model the data is diving into training and test data which can be 70% and 30% respectively.
#Set the seed of R‘s random number generator, which is useful for creating simulations or random objects that can be reproduced.
#Nrow of covid data that is number of rows are selected and stored in another dataframe.
set.seed(1)
data_df <- nrow(covid)


model_df <- model_df(1:data_df, size = round(0.7 * data_df), replace =FALSE)
training_data <- covid[model_df, ]
testing_data <- covid[-model_df, ]

#A model is fitted with independent variable i.e. new_cases on X-axis and dependent variables on Y-axis.
#Total cases, total deaths, aged 65 older, aged 70 older, gdp percapita, diabetes prevalence and cardiovasc death rate are dependent variables.
fit <- lm(new_cases ~ total_cases  +
            total_deaths + aged_65_older + 
            aged_70_older + gdp_per_capita + diabetes_prevalence + cardiovasc_death_rate + population + total_tests + people_vaccinated , data=training_data)

#the fuction sumamry gives the summarisation of fit dataframe
summary(fit)

#to install required package
#this function gvlma takes a linear model object and compute the tests for assessing the model assumptions.

install.packages("gvlma")
library(gvlma)
gvlma_model <- gvlma(fit)
summary(gvlma_model)


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
########## Model Forecasting##################################
#The time series data is forecasts with the below code,
#The actual to predicted values are drawn and traning data actual values to predicted data actual value is 1.

covid <- predict(fit, testing_data)


actual_Predicted_values <- data.frame(cbind(actuals = testing_data$new_cases, 
                                     predicted = new_cases))


head(actual_Predicted_values )


correlation_accuracy <- cor(actual_Predicted_values )
correlation_accuracy

#The following shows the values with predicted to actual
#using the MAE function the min and max values are shown with the actual and predicted values.
function_minmax<- mean(apply(actual_Predicted_values , 1, min) / apply(actual_Predicted_values , 1, max))
function_minmax














