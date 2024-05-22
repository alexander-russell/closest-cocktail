#Note: This file contains a series of helper functions to use the 
#      UMAP analysis to generate recommendations

#Get n recommended cocktails based on UMAP similarity and ingredients available
recommend <- function(cocktailsUmap, cocktails, selectedName, ingredients, extraThreshold, n) {
  #Convert name from factor to string
  cocktailsUmap <- cocktailsUmap %>%
    mutate(name = as.character(name))
  
  #Get data for selected cocktail, get data for all other cocktails
  selected <- cocktailsUmap %>% filter(name == selectedName)
  options <- cocktailsUmap %>% filter(name != selectedName)
  
  #Calculate how many extra ingredients you need, filter by threshold
  options <- options %>%
    mutate(
      extraNeeded = map_dbl(name, ~length(ingredientsNeeded(cocktails, .x, ingredients)))
    ) %>%
    filter(extraNeeded <= extraThreshold)
  
  #Sort by smallest euclidean distance in 2D UMAP space
  options <- options %>%
    mutate(
      distance = (UMAP1 - selected$UMAP1)^2 + (UMAP2 - selected$UMAP2)^2,
    ) %>%
    arrange(distance)
  
  #Get only the top n results
  options <- options %>%
    slice_head(n = n)
  
  return(options)
}

#Gets subset of ingredients needed to make <cocktailName>, not 
ingredientsNeeded <- function(cocktails, cocktailName, ingredients) {
  #Get list of ingredients in recipe
  cocktail <- cocktails %>% 
    filter(name == cocktailName)
  
  #Filter to ingredients not in owned ingredients list
  if (length(ingredients) > 0) {
    cocktail <- cocktail %>%
      filter(!(ingredient %in% ingredients$name))
  }
  
  #Send it back as a list of ingredient names
  return(cocktail$ingredient)
}

#Calculates number of ingredients needed
ingredientsNeededCount <- function(cocktails, cocktailName, ingredients) {
  #Count results from ingredientsNeeded
  return(length(ingredientsNeeded(cocktails, cocktailName, ingredients)))
}

#Gets data corresponding to a named cocktail
getCocktailByName <- function(cocktails, cocktailName) {
  #Filter and return
  cocktail <- cocktails %>% filter(name == cocktailName)
  return(cocktail)
}
