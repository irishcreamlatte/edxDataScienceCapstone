#################################
###### LOAD PACKAGES ############ 
#################################

library(tidyverse)
library(caret)
library(lubridate)
library(countrycode)
library(broom)
library(car)
library(randomForest)
library(ranger)

#################################
###### DATA SET-UP   ############ 
#################################

## DATA SOURCE: ##
## https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016 ## 
## Download the file to the Capstone directory ## 

list.files() ### Confirm the presence of the zip file ###

## Unzip the file ## 

file <- unzip("./suicide-rates-overview-1985-to-2016.zip") ## Unzipped file is a CSV file (master.csv) ## 

list.files() ## Check that the .csv file is in the directory ## 

## Load the dataset ## 

suicides <- read.csv("./master.csv")

#################################
## EXPLORATORY DATA ANALYSIS #### 
#################################

glimpse(suicides)

## 27820 observations of 12 variables 
## VARIABLES: 
## 1. ï..country (Factor) --> need to fix the variable name; also add column for CONTINENT  
## 2. year (Integer) --> parse into date format 
## 3. sex (Factor)
## 4. age (Factor) 
## 5. suicides_no (integer) --> Variable of interest
## 6. population (integer)
## 7. suicides.100k.pop (numeric) 
## 8. country.year (Factor) 
## 9. HDI.for.year (numeric) --> 70% of observations are NAs, thus dropping this variable 
## 10. gdp_for_year (factor) 
## 11. gdp_per_capita (integer)
## 12. generation (factor)

summary(suicides)   ## There are NAs for HDI.for.year 

head(suicides)
tail(suicides)

### TRENDS IN NUMBER OF SUICIDES ### 

summary(suicides$suicides_no) 
sd(suicides$suicides_no) ## var is larger than mean == quasipoisson ## 
IQR(suicides$suicides_no) ## there are outliers ## 

suicides %>% 
    ggplot(aes(1, suicides_no)) + geom_boxplot()

quantile(suicides$suicides_no) 

suicides %>%  ## remove outliers 
    filter(suicides_no <= 150) %>% 
    ggplot(aes(1, suicides_no)) + geom_boxplot()

suicides %>% 
  ggplot(aes(suicides_no)) + geom_histogram()

suicides %>% 
  ggplot(aes(log(suicides_no + 1))) + geom_histogram() ## log transform because of skew

### TRENDS IN SUICIDES PER YEAR ### 

## Convert year to date format ## 

suicides$year <- as.Date(as.character(suicides$year), format = "%Y")
suicides$year <- year(suicides$year)

suicides %>% 
  ggplot(aes(year, suicides_no)) + 
  geom_point(alpha = 0.01) + geom_smooth() ## no clear trend with year 

### TRENDS PER COUNTRY ###

## Fix country variable name ## 

colnames(suicides)[1] <- "country"
names(suicides)

levels(suicides$country) ## 101 countries included ## 
                         ## will group by continents instead ## 

suicides %>% 
  group_by(country) %>% 
  summarize(total = sum(suicides_no)) %>% 
  arrange(desc(total)) %>% 
  top_n(10) ## Top 10 countries, by total suicides 

## Add a column for continent ## 

suicides$continent <- countrycode(sourcevar = suicides[,"country"], 
                                  origin = "country.name", 
                                  destination = "continent")

str(suicides)

suicides$continent <- factor(suicides$continent)
levels(suicides$continent)

## Group by continent, summarize mean and median ## 

suicides %>% 
    group_by(continent) %>% 
    summarize(mean = mean(suicides_no), 
              median = median(suicides_no), 
              sd = sd(suicides_no), 
              n = n()) %>% 
    arrange(desc(mean))

suicides %>% 
    group_by(year, continent) %>% 
    summarize(total = sum(suicides_no)) %>% 
    ggplot(aes(year, total)) + 
    geom_line() + facet_wrap(~continent)

suicides %>% 
  ggplot(aes(continent, suicides_no)) + 
  geom_boxplot()

## ADD: Suicides per capita ## 

suicides <- suicides %>% 
  group_by(year, country) %>% 
  mutate(suicides.per.capita = sum(suicides_no)/sum(population))

suicides %>% 
  ggplot(aes(year, suicides.per.capita)) + 
  geom_point(alpha = 0.01) + geom_smooth() ## still no clear trend ## 

### TRENDS PER SEX ###

levels(suicides$sex)

suicides %>% 
    group_by(sex) %>% 
    summarize(total = sum(suicides_no)) ## Higher for males ##

suicides %>% 
  ggplot(aes(sex, suicides_no)) + 
  geom_boxplot()

suicides %>% 
    group_by(year, continent, sex) %>% 
    summarize(total = sum(suicides_no)) %>% 
    ggplot(aes(year, total, color = sex)) +
    geom_point() + facet_wrap(~continent)

suicides %>% 
  group_by(year, sex, country) %>%
  summarize(pop.per.100k = sum(population)/100000,
            suicides = sum(suicides_no), 
            suicides.per.100k = suicides/pop.per.100k) %>% 
  ggplot(aes(year, suicides.per.100k)) + 
  geom_point(alpha = 0.2, position = "jitter") + 
  geom_smooth() +
  facet_wrap(~ sex) ## no clear trend per year, but higher numbers for males ## 

### TRENDS PER AGE ### 

levels(suicides$age) ## age is divided into 6 levels ## 

suicides %>% 
    group_by(age) %>% 
    summarize(total = sum(suicides_no)) %>% 
    arrange(desc(total)) ## highest among 35-54, 55-74, 25-34 --> among adults

suicides %>% 
    ggplot(aes(age, suicides_no)) + 
    geom_boxplot()

suicides %>% 
    group_by(year, age) %>% 
    summarize(total = sum(suicides_no)) %>% 
    ggplot(aes(year, total, color = age)) + geom_point()

suicides %>% 
    group_by(year, age, sex) %>% 
    summarize(total = sum(suicides_no)) %>% 
    ggplot(aes(year, total, color = sex)) + 
    geom_point() + facet_wrap(~ age) + 
    labs(title = "By Age and Sex", x = "Year", 
         y = "Total Suicides")

### TRENDS BY POPULATION ### 

suicides %>% 
  ggplot(aes(population)) + geom_histogram() + 
  labs(title = "Population", x = "Population") ## heavily right skewed too ## 

suicides %>% 
  ggplot(aes(log(population))) + geom_histogram() + 
  labs(title = "Log Transformed Population", x = "Log(Population)") ## log transform to address skew ## 

suicides %>% 
    select(year, suicides_no, population) %>% 
    group_by(year) %>% 
    mutate(total_pop = sum(population), 
           total_suicides = sum(suicides_no)) %>% 
    distinct(year, total_pop, total_suicides) %>% 
    ggplot(aes(total_pop, total_suicides)) + 
    geom_point() + geom_smooth() + 
    labs(title = "Suicides and Population", 
         x = "Total Population", 
         y = "Total Suicides") ## positive association ##

suicides %>% 
    group_by(year, country) %>% 
    mutate(total_pop = sum(population), total_suicides = sum(suicides_no)) %>% 
    ggplot(aes(total_pop, total_suicides)) +
    geom_point(alpha = 0.01, position = "jitter") + 
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Suicides and Population", 
       x = "Total Population", 
       y = "Total Suicides")

suicides %>% 
    group_by(year, country) %>% 
    mutate(total_pop = sum(population), total_suicides = sum(suicides_no)) %>% 
    ungroup() %>% 
    distinct(country, year, total_pop, total_suicides) %>% 
    mutate(suicides_percapita = total_suicides/total_pop) %>% 
    arrange(desc(suicides_percapita)) %>% 
    top_n(20) ## Lithuania with highest annual suicides per capita 

suicides %>% 
    group_by(year) %>% 
    summarize(total_pop = sum(population), 
              total_suicides = sum(suicides_no)) %>% 
    ggplot(aes(cut(total_pop, breaks = 5), total_suicides)) +
    geom_boxplot()

suicides %>% 
    ggplot(aes(cut(population, breaks = 5), suicides_no)) + 
    geom_boxplot()

### TRENDS PER GENERATION ### 

levels(suicides$generation) ## 6 levels for generation ## 

suicides %>% 
    group_by(generation) %>% 
    summarize(total = sum(suicides_no)) %>% 
    arrange(desc(total)) ## Boomers, Silent Gen, Gen X ## 

suicides %>% 
    ggplot(aes(generation, suicides_no)) + 
    geom_boxplot()

suicides %>% 
    group_by(year, generation) %>% 
    summarize(total = sum(suicides_no)) %>% 
    ggplot(aes(year, total)) + 
    geom_line() + facet_wrap(~generation) 

suicides %>% 
    group_by(year, generation, sex) %>% 
    summarize(total = sum(suicides_no)) %>% 
    ggplot(aes(year, total, color = sex)) +
    geom_line() + facet_wrap(~generation) + 
    labs(title = "Suicides by Generation and Sex",
         x = "Year", y = "Total Suicides")

### TRENDS BY GDP ### 

## gdp_for_year (factor), gdp_per_capita(integer)## 

## Fix the variable names 

colnames(suicides)[10] <- "gdp_per_year"
colnames(suicides)[11] <- "gdp_per_capita"
names(suicides)

## Convert gdp_per_year from factor to numeric 

levels(suicides$gdp_per_year)

suicides$gdp_per_year <- gsub(",", "", suicides$gdp_per_year)
suicides$gdp_per_year <- as.numeric(suicides$gdp_per_year)

head(suicides$gdp_per_year)

suicides %>% 
  ggplot(aes(gdp_per_year, suicides_no)) + 
  geom_point(alpha = 0.5, position = "jitter") + 
  geom_smooth() + 
  labs(title = "Suicides and GDP", 
       x = "GDP per Year", 
       y = "Suicides")

suicides %>% 
  ggplot(aes(cut(gdp_per_year, breaks = 5), suicides_no)) +
  geom_boxplot() + 
  labs(title = "Suicides and GDP", 
       x = "GDP per Year (Breaks = 5)",
       y = "Suicides")

quantile(suicides$gdp_per_year)
IQR(suicides$gdp_per_year)

suicides %>% 
  filter(gdp_per_year < IQR(gdp_per_year)) %>% 
  ggplot(aes(gdp_per_year, suicides_no)) + 
  geom_point() + geom_smooth()

suicides %>% 
  filter(suicides_no < 6000) %>% 
  ggplot(aes(gdp_per_year, suicides_no)) + 
  geom_point(alpha = 0.2, position = "jitter") +
  geom_smooth()


##### OBSERVATIONS ##### 
## 1. Suicide numbers are heavily right skewed 
## 2. Russia and Western countries have highest total suicides. 
##    From Asia, Japan and Korea with highest total suicides. 
## 3. Males have higher rates than females. 
## 4. Rates are higher among adults (24-75 years). 
## 5. There seems to be a positive association between suicides and population.
## 6. Lithuania has highest annual rates of suicides per capita. 
## 7. Rates are highest among the Boomer generation. 
## 8. Rates are highest in Europe, and lowest in Africa. 
## 9. There are outliers. 
## 10. No clear trend with year.
## 11. No clear trend with GDP.
######################## 

#################################
############ MODELS ############# 
#################################

### Remove unneeded variables ### 
##
## Analysis using DEMOGRAPHIC VARIABLES == 
## continent, sex, age, generation, population
## RESPONSE == suicides_no
##

suicides <- suicides %>% 
  select(suicides_no, sex, age, population, generation, continent)

colnames(suicides)[1] <- "suicides"
names(suicides)

### Create test and train sets ### 

set.seed(725, sample.kind = "Rounding")

y <- suicides$suicides
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- suicides %>% slice(test_index)
train_set <- suicides %>% slice(-test_index)

RMSE <- function(actual_suicides, predicted_suicides){
  sqrt(mean((actual_suicides - predicted_suicides)^2))
}

actual_suicides <- test_set$suicides

### Using the Mean ### 

mu <- mean(train_set$suicides)

RMSE_mean <- RMSE(actual_suicides, mu)

options(pillar.sigfig = 6)
rmse_results <- tibble(Method = "Mean", 
                       RMSE = RMSE_mean)
rmse_results


##### LINEAR REGRESSION ##### 

## Model Selection using nested models ##

summary(lm(suicides ~ age, train_set)) 
  ## all significant, but R2 = 0.03209

summary(lm(suicides ~ sex, train_set))
  ## significant, R2 = 0.02011

summary(lm(suicides ~ population, train_set))
  ## significant, R2 = 0.3812 

summary(lm(suicides ~ continent, train_set))
  ## Americas, Asia and Europe significant, R2 = 0.006127

summary(lm(suicides ~ generation, train_set))
  ## All significnat, R2 = 0.01841

## Whether demographic variables determine suicide rates ## 
## 1. sex and age ## 

fit1 <- lm(suicides ~ sex, train_set)
fit2 <- update(fit1, suicides ~ sex + age, train_set)
vif(fit2) ## to check the variance inflation ## 
anova(fit1, fit2) ## model 2's terms are necessary over model 1 

summary(fit2) ## Adj. R-squared = 0.05223

## 2. with generation ## 

fit3 <- update(fit1, suicides ~ 
                 sex + age + generation, train_set)
vif(fit3) ## variance increased, for age and generation 
anova(fit2, fit3) ## model 2 over model 3, generation not necessary ## 

summary(fit3) ## Adj. R-squared = 0.05241 ## 

## 3. with population ## 

fit4 <- update(fit1, suicides ~ 
                   sex + age + 
                   population, train_set)
vif(fit4) ## less variance
anova(fit2, fit4) ## model 4 over model 2 ## 

summary(fit4) ## Adj. R-squared = 0.4219 ## 

## 4. with continent ## 

fit5 <- update(fit1, suicides ~ 
                 sex + age +
                 population + continent, train_set)
vif(fit5)
anova(fit4, fit5) ## model 5 over model 4 ## 

summary(fit5) ## Adj. R-squared = 0.4299 ## 

## 5. interactions? ## 

fit6 <- update(fit1, suicides ~ sex + age +  
                 population + continent + population:sex, 
               train_set)
vif(fit6)
anova(fit5, fit6) ## with interactions
summary(fit6) ## adj. r2 = 0.5587

fit7 <- update(fit1, suicides ~ sex + age + 
                 population + continent + population:sex +  
               population:age, train_set)
vif(fit7) ## increased variance with interaction 
anova(fit6, fit7)
summary(fit7) ## adj. r2 = 0.6418

fit8 <- update(fit1, suicides ~ sex + age +  
                 population + continent + population:sex +  
                 population:age + population:generation, train_set)
vif(fit8)
anova(fit7, fit8)
summary(fit8) ## adj. r2 = 0.6427

fit9 <- update(fit1, suicides ~ sex + age +  
                 population + continent + population:sex +  
                 population:age + population:generation + 
                 population:continent, train_set)
vif(fit9) ## increased variance
anova(fit8, fit9)
summary(fit9) ## adj. r2 = 0.69


## LINEAR REGRESSION MODELS ## 
## No interactions ## 

lm_fit1 <- lm(suicides ~ sex + age + 
               population + continent, train_set)

summary(lm_fit1) ## adj. R2 = 0.4299

y_hat1 <- predict(lm_fit1, test_set)

RMSE_lm <- RMSE(actual_suicides, y_hat1)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Linear Model", 
                       RMSE = RMSE_lm))
rmse_results

## With interactions ## 

lm_fit2 <- lm(suicides ~ sex + age + 
                population + continent + 
                population:sex + population:age + 
                population:generation + population:continent, 
              train_set)

summary(lm_fit2) ## Adj. R2 = 0.69

y_hat2 <- predict(lm_fit2, test_set)

RMSE_lm_int <- RMSE(actual_suicides, y_hat2)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Linear Model, Interactions", 
                                 RMSE = RMSE_lm_int))
rmse_results


##### POISSON REGRESSION ##### 
## Since Y is count data with many 0s ## 
## From earlier, saw that variance was larger than mean## 
## Thus, use quasipoisson ## 

suicides %>% 
  ggplot(aes(population, suicides)) + 
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))

suicides %>% 
  ggplot(aes(population, suicides, color = sex)) + 
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))

suicides %>% 
  ggplot(aes(population, suicides, color = age)) + 
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))

suicides %>% 
  ggplot(aes(population, suicides, color = generation)) + 
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))

suicides %>% 
  ggplot(aes(population, suicides, color = age)) + 
  facet_wrap(~ sex) +
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))

suicides %>% 
  ggplot(aes(population, suicides, color = generation)) + 
  facet_wrap(~ sex) +
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))

suicides %>% 
  ggplot(aes(population, suicides)) + 
  facet_wrap(~ continent) + 
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))


glm_fit <- glm(suicides ~ sex, train_set,
               family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.0832665

glm_fit <- glm(suicides ~ sex + age, train_set,
               family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.229009

glm_fit <- glm(suicides ~ sex + age + 
                 generation, train_set,
               family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.230428

glm_fit <- glm(suicides ~ sex + age + 
                 generation + population, 
               train_set, family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.602333

glm_fit <- glm(suicides ~ sex + age +  
                 generation + population + 
                 continent, train_set, family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.602333

y_hat3 <- predict(glm_fit, test_set, type = "response")

RMSE_glm <- RMSE(actual_suicides, y_hat3)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Quasipoisson Model", 
                                 RMSE = RMSE_glm))
rmse_results


### With Interactions ### 

glm_fit <- glm(suicides ~ sex + age +  
                 generation + population + 
                 continent + population:age, 
               train_set, family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.791399

glm_fit <- glm(suicides ~ sex + age +  
                 generation + population + 
                 continent + population:age + 
                 population:continent, train_set, family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.837181 

glm_fit <- glm(suicides ~ sex + age +  
                 generation + population + 
                 continent + population:age + 
                 population:continent + population:sex, 
               train_set, family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.837947

glm_fit <- glm(suicides ~ sex + age +  
                 generation + population + 
                 continent + population:age + 
                 population:continent + population:sex + 
                 population:generation, train_set, family = "quasipoisson")
summary(glm_fit)
glance(glm_fit) %>% 
  summarize(pseudoR2 = 1 - (deviance/null.deviance)) ## equivalent to R2 ## 
  ## pseudoR2 = 0.841450  

y_hat4 <- predict(glm_fit, test_set, type = "response")

RMSE_glm_int <- RMSE(actual_suicides, y_hat4)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Quasipoisson Model, Interactions", 
                                 RMSE = RMSE_glm_int))
rmse_results


##### K-NEAREST NEIGHBORS ##### 

set.seed(123, sample.kind = "Rounding")
train_knn <- train(suicides ~ sex + age + generation + 
                     population + continent, 
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(1, 51, 2)))

ggplot(train_knn, highlight = TRUE)
train_knn$bestTune ## k = 45

y_hat5 <- predict(train_knn, test_set)

RMSE_knn <- RMSE(actual_suicides, y_hat5)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "K-Nearest Neighbors", 
                                 RMSE = RMSE_knn))
rmse_results


##### RANDOM FOREST ##### 

## Using ranger package ## 

set.seed(123, sample.kind = "Rounding")
fit_rf1 <- ranger(suicides ~ sex + age + generation + 
                    population + continent, data = train_set,
                    num.trees = 500, respect.unordered.factors = "order", 
                  seed = 1234)

y_hat6 <- predict(fit_rf1, test_set)$predictions

RMSE_rf_ranger <- RMSE(actual_suicides, y_hat6)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Random Forest - Ranger", 
                                 RMSE = RMSE_rf_ranger))
rmse_results

fit_rf1$variable.importance %>% 
  tidy() %>% 
  dplyr::arrange(desc(x)) %>% 
  ggplot(aes(reorder(names, x), x)) +
  geom_col() + 
  coord_flip() + 
  ggtitle("Variable Importamce")

## Random Forest package ## 

set.seed(1234, sample.kind = "Rounding")

fit_rf2 <- randomForest(suicides ~ sex + age + generation + 
                          population + continent, 
                        data = train_set, 
                        ntree = 500)
which.min(fit_rf2$mse)

y_hat7 <- predict(fit_rf2, test_set)

RMSE_rf_1 <- RMSE(actual_suicides, y_hat7)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Random Forest - Not Tuned", 
                                 RMSE = RMSE_rf_1))
rmse_results

### Tuned mtry ### 

features <- setdiff(names(train_set), "suicides")

set.seed(1234, sample.kind = "Rounding")

mtry <- tuneRF(x = train_set[features],
                 y = train_set$suicides,
                 ntreeTry = 500, 
                 mtryStart = 3, 
                 stepFactor = 2, 
                 improve = 0.01, 
                 trace = FALSE)

best_m <- mtry[mtry[,2] == min(mtry[,2]), 1]
print(mtry)
print(best_m)

set.seed(123, sample.kind = "Rounding")
fit_rf3 <- randomForest(suicides ~ sex + age + generation + 
                          population + continent,
                        data = train_set, 
                        mtry = best_m, 
                        importance = TRUE, 
                        ntree = 500)

y_hat8 <- predict(fit_rf3, test_set)

RMSE_rf_2 <- RMSE(actual_suicides, y_hat8)

rmse_results <- bind_rows(rmse_results, 
                          tibble(Method = "Random Forest - Tuned", 
                                 RMSE = RMSE_rf_2))
rmse_results

varImpPlot(fit_rf3, main ="Variable Importance")
varImp(fit_rf3)

### BEST MODEL: Tuned Random Forest 