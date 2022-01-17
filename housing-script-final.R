# File > Import dataset
library(readxl)
housing <- read_excel("~/GitHub/edna-r-pbi-exploration/data/housing.xlsx")

# Summarize
summary(housing)
head(housing)

# View the data 
View(housing)

# install.packages('skimr')
library(skimr)

# Profile the data
skim(housing)

# Any questions? Get the documentation
?skim

# Visualize relationships b/w numeric variables
library(tidyverse)
library(GGally)

# Pairplot
housing %>% 
  select(price, lotsize, prefarea) %>% 
  ggpairs(aes())

# Fill in by prefarea
housing %>% 
  select(price, lotsize, prefarea) %>% 
  ggpairs(aes(color=prefarea))

# Now to modeling
housing_lm <- lm(price ~ lotsize, data = housing)
summary(housing_lm)

# Predict values
library(tidymodels)
housing_lm_tidy <- tidy(housing_lm)

housing_pred <- add_predictions(housing, housing_lm)
head(housing_pred)

library(ggplot2)
ggplot(data = housing_pred, aes(y=price, x=lotsize, color=prefarea)) +
  geom_point() +
  geom_smooth(method='lm')

# Expand the predicted values
lotsize <- seq(min(housing$lotsize),max(housing$lotsize), by=500)
lotsize

housing_pred_grid <- expand.grid(lotsize = lotsize)
dim(pred_grid)

housing_pred_grid$pred <-predict(housing_lm, new = pred_grid)
View(pred_grid)

saveRDS(housing_pred, "housing_pred.rds") # The actual data 
saveRDS(housing_pred_grid, "housing_pred_grid.rds") # The expanded grid

