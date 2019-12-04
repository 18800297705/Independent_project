# Independent_project
---
title: "Independent project"
author: "Chenyue Zhu"
date: "11/19/2019"
output: beamer_presentation
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Import the raw data set.
```{r}
require(tidyverse)
require(readr)
```

Import the morality rate raw data set.
```{r}
morality_rate <- read_csv("Data/WHO_morality_rate.csv", 
    col_names = FALSE)
```

Clean the raw data set.
```{r}
morality_rate_1 <-
  morality_rate %>%
  select(country=X1, year=X2, morality_rate=X3) %>%
  group_by(country, year) #ensure the unit of analysis is country, year
morality_rate_2 <- morality_rate_1 [3:3133,]
head(morality_rate_2)
```


Import the GDP raw data downloaded from the United Nations website.
```{r}
GDP_raw <- read_delim("Data/UN_GDP.txt", 
    ";", escape_double = FALSE, col_names = FALSE, 
    trim_ws = TRUE)
```


Clean the raw GDP data.
```{r}
GDP_1 <-
  GDP_raw %>%
  select(country=X1, year=X2, GDP=X4) %>%
  group_by(country, year) #ensure the unit of analysis is country, year
GDP_2 <- GDP_1 [2:9657,]
head(GDP_2)
```

Import the HIV data.
```{r}
Life_Expectancy<- read_csv("Data/WHO_life_expectancy.csv", col_names = FALSE)
head(Life_Expectancy)
```

Clean the life expectancy data.

```{r}
Life_Expectancy_1 <-
  Life_Expectancy %>%
  select(country=X1, year=X2, Life_Expectancy=X3) %>%
  group_by(country, year) #ensure the unit of analysis is country, year
Life_Expectancy_2 <- Life_Expectancy_1 [3:3133, ]
head(Life_Expectancy_2)
```

Import other predictors.

```{r}
Complementary_Data <- read_csv("Data/Complementary_Data.csv", 
    col_names = FALSE)
```


Select the predictors I would like to use in the project.
```{r}
Complementary_1 <- Complementary_Data %>%
  select(country=X1, year=X2,Alcohol=X7, Hepatitis_B=X9, Measles= X10, education=X22)
head(Complementary_1)
```



# Combine the data set.

```{r}
dat_1<-full_join(Life_Expectancy_2, GDP_2,
               by=c('country',
                    'year'))
dat_2<-full_join(dat_1, morality_rate_2,
               by=c('country',
                    'year')) 
dat_3<-full_join(dat_2, Complementary_1,
               by=c('country',
                    'year')) 
head(dat_3)
```


```{r}
dat_clean <-
  dat_3 %>% 
  filter(!is.na(Life_Expectancy)) %>% 
  filter(!is.na(Life_Expectancy))  %>%
  filter(!is.na(GDP))  %>% 
  filter(!is.na(morality_rate) ) %>% 
  filter(!is.na(Alcohol) ) %>% 
  filter(!is.na(Hepatitis_B) ) %>% 
  filter(!is.na(Measles) ) %>% 
  filter(!is.na(education))
head(dat_clean)
```

#Regression model
Split the Sample: Training and test data

```{r}
require(caret)
set.seed(123)
index = createDataPartition(dat_clean$Life_Expectancy,p=.8,list=F) 
train_data = dat_clean[index,] # Use 80% of the data as training data 
test_data = dat_clean[-index,] # holdout 20% as test data 

dim(train_data)
```

```{r}
dim(test_data) 
```

Examine the Data
```{r}
summary(train_data)
```

```{r}
sum(is.na(train_data))
```
As we have cleaned up all the missing values in the previous steps, there is no need to impute any data.


```{r}
# First, turn the character variables into numeric variable
gen_cats = . %>% 
  mutate(Life_Expectancy = as.numeric(Life_Expectancy)) %>%
  mutate(GDP = as.numeric(GDP)) %>%
  mutate(morality_rate = as.numeric(morality_rate))%>%
  mutate(Alcohol = as.numeric(Alcohol))%>%
  mutate(Hepatitis_B = as.numeric(Hepatitis_B))%>%
  mutate(Measles = as.numeric(Measles))%>%
  mutate(education = as.numeric(education))
```



```{r}
# Generate our recipe to preprocess the data 
require(recipes)
rcp <- 
  recipe(Life_Expectancy~.,train_data %>% gen_cats) %>% 
  step_range(all_numeric()) %>%  # Normalize scale
  prep()


# Apply the recipe to the training and test data
train_data2 <- bake(rcp,train_data %>% gen_cats)
test_data2 <- bake(rcp,test_data%>% gen_cats) # Need to transform the character 
```

```{r}
train_data2 %>% glimpse()
```

```{r}
train_data3 <-train_data2 %>% filter(!is.na(Life_Expectancy))
```



```{r}
train_data3 %>% 
  select(Life_Expectancy,GDP,morality_rate,Alcohol,Hepatitis_B,Measles,education) %>% 
  gather(var,val) %>% 
  ggplot(aes(val,group=var)) +
  geom_histogram(bins = 30) +
  facet_wrap(~var,scales="free",ncol=3)
```


Use the k-fold cross-validation method with 5 folds in the data.
```{r}
set.seed(1988) # set a seed for replication purposes 

folds <- createFolds(train_data3$Life_Expectancy, k = 5) # Partition the data into 5 equal folds

sapply(folds,length)
```


Use trainControl() to establish the settings.
```{r}
control_conditions <- 
  trainControl(method='cv', # K-fold cross validation
               index = folds # The indices for our folds (so they are always the same)
  )
```

##Linear Regression(lm)
```{r}
mod_lm <-
  train(Life_Expectancy ~ .,          # Equation (outcome and everything else)
        data=train_data3, # Training data 
        method = "lm",    # linear model
        metric = "RMSE",   # mean squared error
        trControl = control_conditions # Cross validation conditions
  )
mod_lm
```

## KNN model
```{r}
mod_knn <-
  train(Life_Expectancy ~ .,           # Equation (outcome and everything else)
        data=train_data3,  # Training data 
        method = "knn",    # K-Nearest Neighbors Algorithm
        metric = "RMSE",   # mean squared error
        trControl = control_conditions # Cross validation conditions
  )
mod_knn
```

```{r}
plot(mod_knn)
```


Classification and Regression Trees (CART)
```{r}
mod_cart <-
   train(Life_Expectancy ~ .,            # Equation (outcome and everything else)
        data=train_data3,    # Training data 
        method = "rpart",    # Regression tree
        metric = "RMSE",     # mean squared error
        trControl = control_conditions # Cross validation conditions
  )
mod_cart
```

Note that the main tuning parameter here is the complexity parameter (cp): this reflect how deep we let the tree grow. It looks like the more complex models perform worse on out-of-sample data.
```{r}
plot(mod_cart)
```

Random Forest
```{r}
mod_rf <-
  train(Life_Expectancy ~ ., # Equation (outcome and everything else)
        data=train_data3, # Training data 
        method = "ranger", # random forest (ranger is much faster than rf)
        metric = "RMSE",     # mean squared error
        trControl = control_conditions
  )
mod_rf
```

```{r}
plot(mod_rf)
```

```{r}
mod_rf$bestTune
```


Model Comparison
```{r}
# Organize all model imputs as a list.
mod_list <-
  list(
    lm = mod_lm,
    knn = mod_knn,
    rf = mod_rf,
    cart = mod_cart
  )

# Resamples allows us to compare model output
resamples(mod_list)
```


```{r}
dotplot(resamples(mod_list),metric = "RMSE")
```

Examine the fit across each of the models.
```{r}
dotplot(resamples(mod_list),metric = "Rsquared")
```


Test the Predictive Accuracy of the Best Model

```{r}
#Of the models we ran, it looks like the Random Forest model did the best. Let’s now see how well it does on predicting the test data.
pred <- predict(mod_rf,newdata = test_data2)
mse = sum(test_data2$Life_Expectancy-pred^2)/nrow(test_data2)
mse 
```

