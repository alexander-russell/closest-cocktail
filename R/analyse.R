library(embed)
library(tidyverse)

raw <- read_csv("bostonCocktailsOriginal.csv")

cocktails <- raw %>%
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
  #Remove duplicates (not confident but problems with pivot wider otherwise)
  distinct(row_id, ingredient, .keep_all = TRUE)
  #Get first hundred (debug)
  # slice_head(n = 100)


cocktailsReduced <- cocktails %>%
  #Remove any ingredients used 15 or less times
  add_count(ingredient) %>%
  filter(n > 15) %>%
  select(-n)

cocktailsWide <- cocktailsReduced %>%
  select(-ingredient_number, -row_id, -category, -measure) %>%
  pivot_wider(names_from = ingredient, values_from = weight, values_fill = 0) %>%
  janitor::clean_names()
  
cocktailsUmapRecipe <- recipe( ~ ., data = cocktailsWide) %>%
  update_role(name, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

cocktailsUmapPrep <- prep(cocktailsUmapRecipe)
cocktailsUmap <- juice(cocktailsUmapPrep)

# 
# cocktailsUmap %>%
#   mutate(
#     highlight = ifelse(str_detect(name, "artini"), TRUE, FALSE)
#   ) %>%
#   ggplot(aes(UMAP1, UMAP2, label = name)) +
#   # Add big point
#   # geom_point(data = selected(), size = 8, shape = 4, stroke = 0.5) +
#   #Add points
#   geom_point(alpha = 0.7, size = 2, aes(colour = highlight)) +
#   #Add text
#   geom_text(check_overlap = TRUE, hjust = "inward") +
#   labs(color = NULL)
# 
# 
# v <- cocktails %>%
#   #select(ingredient) %>%
#   add_count(ingredient) %>%
#   distinct(ingredient, .keep_all = TRUE) %>%
#   arrange(ingredient) #%>%
#   #select(n, ingredient)# %>%
#   # filter(n > 1)
#   # filter(str_detect(ingredient, "lemon") & str_detect(ingredient, "juice"))
# # view(v)
# 
# v <- cocktails %>%
#   select(weight) %>%
#   add_count(weight) %>%
#   distinct(weight, .keep_all = TRUE)
# view(v)
#   
# 
# boston_cocktails <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv")
# cocktails_parsed <-  boston_cocktails %>%
#   mutate(
#     ingredient = str_to_lower(ingredient),
#     ingredient = str_replace_all(ingredient, "-", " "),
#     ingredient = str_remove(ingredient, " liqueur$"),
#     ingredient = str_remove(ingredient, " (if desired)$"),
#     ingredient = case_when(
#       str_detect(ingredient, "bitters") ~ "bitters",
#       str_detect(ingredient, "lemon") ~ "lemon juice",
#       str_detect(ingredient, "lime") ~ "lime juice",
#       str_detect(ingredient, "grapefruit") ~ "grapefruit juice",
#       str_detect(ingredient, "orange") ~ "orange juice",
#       TRUE ~ ingredient
#     ),
#     measure = case_when(
#       str_detect(ingredient, "bitters") ~ str_replace(measure, "oz$", "dash"),
#       TRUE ~ measure
#     ),
#     measure = str_replace(measure, " ?1/2", ".5"),
#     measure = str_replace(measure, " 3/4", ".75"),
#     measure = str_replace(measure, " ?1/4", ".25"),
#     measure_number = parse_number(measure),
#     measure_number = if_else(str_detect(measure, "dash$"),
#                              measure_number / 50,
#                              measure_number
#     )
#   ) %>%
#   add_count(ingredient) %>%
#   filter(n > 15) %>%
#   select(-n) %>%
#   distinct(row_id, ingredient, .keep_all = TRUE) %>%
#   na.omit()
# 
# cocktails_df <- cocktails_parsed %>%
#   select(-ingredient_number, -row_id, -measure) %>%
#   pivot_wider(names_from = ingredient, values_from = measure_number, values_fill = 0) %>%
#   janitor::clean_names() %>%
#   na.omit()
# 
# umap_rec <- recipe(~., data = cocktails_df) %>%
#   update_role(name, category, new_role = "id") %>%
#   step_normalize(all_predictors()) %>%
#   step_umap(all_predictors())
# 
# umap_prep <- prep(umap_rec)
# result <- juice(umap_prep)
