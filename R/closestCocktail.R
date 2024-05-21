#recommend(juice(umap_prep), cocktails_parsed, "Amaretto Sour", tibble(name = c("Brandy", "Cointreau", "Amaretto")), 2, 5)

recommend <- function(cocktailsUmap, cocktails, selectedName, ingredients, extraThreshold, n) {
  #^Where cocktails has name,UMAP1,UMAP2, ingredients has name
  
  #Get rid of factored name
  cocktailsUmap <- cocktailsUmap %>%
    mutate(name = as.character(name))
  
  #Get information on selected cocktail by name, get all other cocktails
  selected <- cocktailsUmap %>% filter(name == selectedName)
  options <- cocktailsUmap %>% filter(name != selectedName)
  
  #Calculate how many extra ingredients you need, filter by threshold
  # if (is_empty(ingredients)) {
    # return(ingredients)
    options <- options %>%
      mutate(
        extraNeeded = map_dbl(name, ~length(ingredientsNeeded(cocktails, .x, ingredients)))
      ) %>%
      filter(extraNeeded <= extraThreshold)
  # } else {
    # return(tibble(n=c(1,2,3)))
  # }
  
  # return(options)
  #Sort by euclidean distance in 2D UMAP space
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

ingredientsNeeded <- function(cocktails, cocktailName, ingredients) {
  # print(cocktailName)
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

ingredientsNeededCount <- function(cocktails, cocktailName, ingredients) {
  return(length(ingredientsNeeded(cocktails, cocktailName, ingredients)))
}

getCocktailByName <- function(cocktails, cocktailName) {
  #Filter and return
  cocktail <- cocktails %>% filter(name == cocktailName)
  return(cocktail)
}
