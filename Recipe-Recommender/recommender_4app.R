library(readr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(data.table)
library(mltools)
library(mlbench)
library(caret)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(DataExplorer)
library(parallel)
library(iterators)
library(foreach)
library(htmlTable)
library(anytime)
library(rworldmap)
library(naniar)
library(ggthemes)
library(VIM)
library(stringr)
library(readr)
library(wordcloud)
library(recommenderlab)

df <- readRDS("burger_recipes.rds")

#Creating rating matrix
#from df, selecting the necessary columns, recipe_id and ingredients
rating_matrix <- df %>%
  select(recipe_id, ingredients) %>%
  mutate(value = 1) %>%   #Adding a column of just 1's
  spread(ingredients, value, fill = 0) %>%  # spreading into user-item format, and removing the recipe-id column
  select(-recipe_id) %>%
  as.matrix() %>%     #convert to a matrix
  as("binaryRatingMatrix")   #recommender class

#scheme
scheme <- rating_matrix %>%
  evaluationScheme(method = "cross",
                   k = 5,
                   train = 0.8,
                   given = -1)

#Recommender
recomm1 <- Recommender(getData(scheme, 'train'), 
                       method = "RANDOM",  
                       param = list(k = 5))

