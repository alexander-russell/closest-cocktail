#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(glue)

# Define server logic required to draw a scatterplot
function(input, output, session) {
    #Get the selected cocktail
    # selected <- reactive({
    #   getCocktailByName(result, input$selectedName)
    # })
    
    #Create a scatterplot
    output$umapPlot <- renderPlot({
      cocktailsUmap %>%
        ggplot(aes(UMAP1, UMAP2, label = name)) +
        #Add points
        geom_point(alpha = 0.7, size = 2) +
        #Add text
        geom_text(check_overlap = TRUE, hjust = "inward") +
        labs(color = NULL) +
        #Add big point
        geom_point(data = (cocktailsUmap %>% filter(name == input$selectedName)), colour = "red", size = 5)
    })
    
    recommended <- reactive({
      recommend(
        cocktailsUmap = cocktailsUmap, 
        cocktails = cocktails, 
        selectedName = input$selectedName, 
        ingredients = tibble(name = input$ingredients), 
        extraThreshold = input$extraThreshold, 
        n = input$n
      )
    })
    
    selected <- reactive({
      cocktailsUmap %>% filter(name == input$selectedName)
    })
      
    output$recTable <- renderTable({
      recommended()
    })
    
    output$recPlot <- renderPlot({
      recommended() %>%
        ggplot(aes(UMAP1, UMAP2, label = name)) +
        #Add big point
        geom_point(data = selected(), size = 8, shape = 4, stroke = 0.5) +
        #Add points
        geom_point(alpha = 0.7, size = 2, aes(colour = as.factor(extraNeeded))) +
        #Add text
        geom_text(check_overlap = TRUE, hjust = "inward") +
        labs(color = NULL)
    })
    
    output$selectedInfo <- renderUI({
      container <- div(class = "info")
      
      #Mention the selected drink
      ingredientList <- getCocktailByName(cocktails, input$selectedName)$ingredient
      infoElement <- p(glue("You've selected {input$selectedName}. For reference, it contains: {toString(ingredientList)}."))
      
      container <- tagAppendChild(container, infoElement)
      
      #Mention what the following is
      # guideElement <- p("Listed below are", input$n, "cocktail recommendations based on your selected cocktail and the ingredients you have access to.")
      guideElement <- p(glue("Listed below are {input$n} cocktail recommendations based on your selected cocktail and the ingredients you have access to."))
      
      container <- tagAppendChild(container, guideElement)
      
      return(container)
    })
    
    output$recList <- renderUI({
      #debug make the border red
      container <- div(class = "rec-list")
      recs <- recommended()
      for (i in 1:nrow(recs)) {
        #Make a wrapper element (just in case)
        cocktailWrapper <- div(class="cocktail-wrapper")
        
        #Make the actual element
        cocktailElement <- div(class="cocktail")
        
        #Make a title and append it
        cocktailTitle <- h2(recs[i,]$name, class="cocktail-title")
        cocktailElement <- tagAppendChild(cocktailElement, cocktailTitle)
        
        #Make a paragraph stating how many extra ingredients needed
        extraNeeded <- recs[i,]$extraNeeded
        if (extraNeeded >= 1) {
          cocktailDescription <- p(glue("You need an extra {extraNeeded} ingredient{ifelse(extraNeeded > 1, 's', '')} to make this one."))
        } else {
          cocktailDescription <- p("You don't need anything extra")
        }
        
        cocktailElement <- tagAppendChild(cocktailElement, cocktailDescription)
        
        #Add in an ingredients subheading
        ingredientsTitle <- h3("Ingredients", class="ingredients-title")
        cocktailElement <- tagAppendChild(cocktailElement, ingredientsTitle)
        
        #Add in each of the ingredients
        ingredientsList <- getCocktailByName(cocktails, recs[i,]$name)$ingredient
        ingListElement <- tags$ul()
        
        for (j in 1:length(ingredientsList)) {
          ingredientElement <- tags$li(ingredientsList[j], class = "ingredient-item")
          ingListElement <- tagAppendChild(ingListElement, ingredientElement)
          
        }
        cocktailElement <- tagAppendChild(cocktailElement, ingListElement)
        
        
        #Add the element to the wrapper and the wrapper to the container
        cocktailWrapper <- tagAppendChild(cocktailWrapper, cocktailElement)        
        container <- tagAppendChild(container, cocktailWrapper)
      }

      return(container)
    })
    
    #Automatically select ingredients in selected cocktail
    observe({
      #Get currently selected cocktail
      selectedName <- input$selectedName
      
      #In the ingredients checkbox form, select any of the ingredients in the selected cocktail
      updateCheckboxGroupInput(
        session, "ingredients",
        selected = getCocktailByName(cocktails, selectedName)$ingredient
      )
    })
}
