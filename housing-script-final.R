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

# Go ahead and try this on
# install.packages('Computers')
library(Ecdat)
Computers

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
library(tidymodels)

housing_lm <- lm(price ~ lotsize + prefarea, data = housing)
summary(housing_lm)

# install.packages('report')
library(report)
report(housing_lm)

library(modelr)
housing_lm_tidy <- tidy(housing_lm)

housing_pred <- add_predictions(housing, housing_lm)
View(housing)

library(ggplot2)
ggplot(data = housing_pred, aes(y=price, x=lotsize, color=prefarea)) +
  geom_point() +
  geom_smooth(method='lm')

# Save this to a special RDS file
saveRDS(housing_lm_tidy, "housing_lm_tidy.rds")
housing_lm_tidy
