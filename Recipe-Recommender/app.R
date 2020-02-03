##############   CSDA1040 - PROJECT 1 - RECOMMENDER APP  ##############
############## APP INTERFACE BUILT BY LEESANN SUTHERLAND ##############

library(shiny)
library(shinyjs)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(DT)
library(Matrix)
library(slam)
library(recommenderlab)

## Loading necessary files
list_ingredients <- readRDS("list_ingredients.rds")
meats <- readRDS("meats.rds")
cheeses <- readRDS("cheeses.rds")
veggies <- readRDS("veggies.rds")
breads <- readRDS("breads.rds")
sides <- readRDS("sides.rds")
df <- readRDS("burger_recipes.rds")

###############  INTERFACE  #####################
ui <- fluidPage(
    # App Title
    titlePanel("Delicious Burger Recipe Recommender"),
    
    #Navigation Panel:
    navlistPanel("Navigate Recipes",
                 fluid = TRUE,
                 widths = c(2,10),
                 
                 ### WELCOME PAGE ###
                 tabPanel(title = "Home",
                          wellPanel(
                              h1("Welcome to Delicious Burger Recipe Recommender!"),
                              h3("Where we take your preferences and recommend delicious burgers for you to make at home")
                              ),
                         
                          
                          ###  MEAT SELECTION  ###
                          hr(),
                          selectInput(inputId = "meat", 
                                      label = "Choose your meat(s) or meat substitute(s):", 
                                      choices = c(unique(as.character(meats$Selection))),
                                      selected = NULL,
                                      multiple = TRUE,
                                      selectize = TRUE),
                          
                          ###  CHEESE SELECTION   ###
                          selectInput(inputId = "cheese", 
                                      label = "Choose you cheese(s) or cheese substitute(s):", 
                                      choices = c(unique(as.character(cheeses$Selection))),
                                      selected = NULL,
                                      multiple = TRUE,
                                      selectize = TRUE),
                          
                         ###   VEGETABLE CHOICE   ###
                          selectInput(inputId = "veggie", 
                                      label = h4("Choose your vegetables:"), 
                                      choices = c(unique(as.character(veggies$Selection))),
                                      selected = NULL,
                                      multiple = TRUE,
                                      selectize = TRUE),
                          
                          ###   BREAD CHOICE   ###
                          selectInput(inputId = "bread", 
                                      label = h4("Choose your bread:"), 
                                      choices = c(unique(as.character(breads$Selection))),
                                      selected = NULL,
                                      multiple = TRUE,
                                      selectize = TRUE),
                          
                          ###   SIDE DISH   ###
                          selectInput(inputId = "side", 
                                      label = h4("Choose your side(s):"), 
                                      choices = c(unique(as.character(sides$Selection))),
                                      selected = NULL,
                                      multiple = TRUE,
                                      selectize = TRUE),
                          
                          
                          ###  ACTION BUTTON  ###
                          actionButton(inputId = "action", 
                                       label = strong("Bring on the Recipes!"),
                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                          h6("After clicking the button, head over to the 'Our Recommendations' tab."),
                          
                          
                          hr(),
                          
                          ###  Page Footer  ###
                          wellPanel(
                            tags$footer("This Shiny App is part of an assignment for our Advanced Methods in Data Analysis course at York University (2020)"),
                            tags$footer("Contributors: Leesann Sutherland, Stan Taov and Maryan Mahamed"), 
                            tags$footer("Hosted by: shinyapps.io by RStudio")
                            )
                          ),
                          
                 
                 
                 
                 ### RECOMMENDER PANEL ###
                 tabPanel(title = "Our Recommendations",
                          h2("These are our burger recipe recommendations based on your input."),
                          h4("Keep in mind that these recipes may not contain all of your ingredient selections"),
                          
                          hr(),
                          
                          DT::dataTableOutput(outputId = "recomms"),
                            
                          
                          hr(),
                          wellPanel(
                          tags$footer("This Shiny App is part of an assignment for our Advanced Methods in Data Analysis course at York University (2020)"),
                          tags$footer("Contributors: Leesann Sutherland, Stan Taov and Maryan Mahamed"), 
                          tags$footer("Hosted by: shinyapps.io by RStudio"))
                          )
    )
)
                  
                        
              
######################   SERVER   ###############################
source("recommender_4app.R")
server <- function(input, output, session) {

 output$recomms <- DT::renderDataTable(DT::datatable({
   
   input$action
  
# Input strings to make into a matrix
    
    user_input <- isolate(
      unique(c(input$meat, input$cheese, input$veggie, input$bread, input$side))
    )
  
    #User input should feed into this
    user_input_rm <- df %>% 
      select(ingredients) %>% 
      unique() %>%
      mutate(value = as.numeric(ingredients %in% user_input)) %>% 
      spread(key = ingredients, value = value) %>% 
      as.matrix() %>%
      as("binaryRatingMatrix")
    
    #Model - Random Item
    recomm1 <- Recommender(getData(scheme, 'train'), 
                           method = "RANDOM",  
                           param = list(k = 5))
    
    #Predictions based on model
    user_pred <- predict(recomm1, 
                         newdata = user_input_rm, 
                         n = 10)
    
    #Extracting the top 3 sets to create an ingredients vector
    top_ingred <- do.call(c, strsplit(getList(user_pred)[[1]][1:3], ", "))
    top_ingred <- c(top_ingred, user_input)  #Appends this vector to the user's original input ingredients
    
    #Creating output dataframe
    df2 <- df
    
    df2$matches <- numeric(dim(df2)[1]) #Adds a column to the full recipes dataframe to initiate to match count function below
    
    #This loop runs those the df2$ingredients and count occurances of each recommended ingredient, adding 1 for each hit
    for (i in top_ingred) {
      df2$matches <- df2$matches + as.numeric(str_detect(df2$ingredient, i, negate = FALSE))
    }
    
    #Naming the final results, sorted by top count, and displaying the top 10 recipes to the user
    best_matches_all <- df2 %>% 
                        select(recipe_id, name, ingredients, minutes, rating, matches) %>% 
                        arrange(desc(matches))
    
    best_matches_output <- head(best_matches_all %>%
                                  select(-matches), 
                                n = 10)
    
    best_matches_output
    
     }))
    
}

        
#################   LAUNCH   ####################
shinyApp(ui = ui, server = server)
