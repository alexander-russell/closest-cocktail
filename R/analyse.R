#Inspired by Julia Silge's analysis on this dataset
#https://juliasilge.com/blog/cocktail-recipes-umap/

library(embed)
library(tidyverse)

#Load data (Note: This data is from TidyTuesday, 2020-05-26)
cocktailsRaw <- read_csv("bostonCocktailsOriginal.csv")

cocktails <- cocktailsRaw %>%
  #Clean up ingredient names
  mutate(
    #Lowercase ingredient names
    ingredient = str_to_lower(ingredient),
    #Strip anything after a comma (and remove the comma)
    ingredient = str_remove(ingredient, ",.*"),
    #Strip anything after "or" (and remove the " or ")
    ingredient = str_remove(ingredient, " or .*"),
    #Strip anything after " (" (and remove the " (")
    ingredient = str_remove(ingredient, " \\(.*"),
    #Strip anything after " and " (and remove the " and "), except "peeled and chopped fresh ginger"
    ingredient = ifelse(str_detect(ingredient, "peeled and chopped"), ingredient, str_remove(ingredient, " and .*")),
    #Strip "each " from ingredient
    ingredient = str_remove(ingredient, "^each "),
    #Strip "fresh " from ingredient
    ingredient = str_remove(ingredient, "^fresh "),
    #Strip "old mr. boston " from ingredient
    ingredient = str_remove(ingredient, "^old mr. boston "),
    #Strip brand names from vodka, rum etc
    ingredient = case_when(
      str_detect(ingredient, "vodka") ~ "vodka",
      str_detect(ingredient, "rum") ~ "rum",
      str_detect(ingredient, "absinthe") ~ "absinthe",
      str_detect(ingredient, "tequila") ~ "tequila",
      str_detect(ingredient, "brandy") ~ "brandy",
      str_detect(ingredient, "bourbon") ~ "bourbon",
      str_detect(ingredient, "rye") ~ "bourbon",
      str_detect(ingredient, "whiskey") ~ "whiskey",
      str_detect(ingredient, "whisky") ~ "whiskey",
      str_detect(ingredient, "bitters") ~ "bitters",
      str_detect(ingredient, "champagne") ~ "champagne",
      str_detect(ingredient, "cucumber") ~ "cucumber",
      str_detect(ingredient, "creme de cacao") ~ "creme de cacao",
      str_detect(ingredient, "sherry") ~ "sherry",
      str_detect(ingredient, "amaretto") ~ "amaretto",
      str_detect(ingredient, "vermouth") ~ "vermouth",
      str_detect(ingredient, "ginger ale") ~ "ginger ale",
      str_detect(ingredient, "whole milk") ~ "milk",
      str_detect(ingredient, "canned coconut milk") ~ "coconut milk",
      str_detect(ingredient, "gin") & !str_detect(ingredient, "ginger") ~ "gin",
      str_detect(ingredient, "ginger") & !str_detect(ingredient, "ale") ~ "ginger",
      str_detect(ingredient, "juice") & str_detect(ingredient, "lemon") ~ "lemon juice",
      str_detect(ingredient, "juice") & str_detect(ingredient, "lime") ~ "lime juice",
      str_detect(ingredient, "juice") & str_detect(ingredient, "grapefruit") ~ "grapefruit juice",
      str_detect(ingredient, "juice") & str_detect(ingredient, "orange") ~ "orange juice",
      str_detect(ingredient, "juice") & str_detect(ingredient, "grape") ~ "grape juice",
      str_detect(ingredient, "juice") & str_detect(ingredient, "passion fruit") ~ "grape juice",
      TRUE ~ ingredient
    )
  ) %>%
  mutate(
    #Fix error in bitters units (probably not meant to have 3oz bitters in a drink)
    measure = case_when(
      str_detect(ingredient, "bitters") ~ str_replace(measure, "oz$", "dash"),
      TRUE ~ measure
    )
  ) %>%
  mutate(
    #Create new weight variable for modelling importance of ingredient
    weight = measure,
    #Remove anything after an "or", also remove the "or"
    weight = str_remove(weight, " or.*"),
    #Convert from fraction to decimal
    weight = str_replace(weight, " ?1/2", ".5"),
    weight = str_replace(weight, " ?1/3", ".33"),
    weight = str_replace(weight, " ?2/3", ".66"),
    weight = str_replace(weight, " {0,2}3/4", ".75"),
    weight = str_replace(weight, " ?1/4", ".25"),
    #Convert to same 'unit', using ounces as basis
    weight = case_when(
      str_detect(weight, "oz") ~ str_remove(weight, " oz"),
      str_detect(weight, "splash") ~ "0.5",
      str_detect(weight, "2 -  3 drops") ~ "0.5",
      str_detect(weight, "For glass") ~ "1",
      str_detect(weight, "Fresh") ~ str_remove(weight, " Fresh"),
      str_detect(weight, "2 bottles") ~ "10",
      str_detect(weight, "1c") ~ "1",
      str_detect(weight, "dash") ~ str_remove(weight, " .*"),
      str_detect(weight, "slices") ~ str_remove(weight, " slices"),
      str_detect(weight, "2 750-ml") ~ "8",
      str_detect(weight, "1 750-ml") ~ "4",
      str_detect(weight, "1 tsp") ~ "0.5",
      str_detect(weight, ".5 tsp") ~ "0.25",
      TRUE ~ weight
    ),
    #Convert string to number
    weight = parse_number(weight)
  ) %>%
  #Remove duplicates (not confident but problems with pivot_wider otherwise)
  distinct(row_id, ingredient, .keep_all = TRUE)

#Note: I kept cocktailsReduced separate so I can give UMAP only the popular 
#      ingredients but keep the full, processed, cocktail recipe for
#      later display

cocktailsReduced <- cocktails %>%
  #Remove any ingredients used 15 or less times
  add_count(ingredient) %>%
  filter(n > 15) %>%
  select(-n)

#Pivot to wide format for UMAP analysis
cocktailsWide <- cocktailsReduced %>%
  select(-ingredient_number, -row_id, -category, -measure) %>%
  pivot_wider(names_from = ingredient, values_from = weight, values_fill = 0) %>%
  janitor::clean_names()
  
#Create plan for UMAP analysis
cocktailsUmapRecipe <- recipe( ~ ., data = cocktailsWide) %>%
  update_role(name, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

#Conduct UMAP analysis and extract results
cocktailsUmapPrep <- prep(cocktailsUmapRecipe)
cocktailsUmap <- juice(cocktailsUmapPrep)