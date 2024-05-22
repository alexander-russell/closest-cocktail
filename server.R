library(shiny)
library(glue)

#Define server logic required to 
function(input, output, session) {
  #Get recommendations based on input
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
    
    #Output information on the selected cocktail, assembled with html
    output$selectedInfo <- renderUI({
      #Create information panel
      container <- div(class = "info")
      
      #Name and describe the selected drink
      ingredientList <- getCocktailByName(cocktails, input$selectedName)$ingredient
      infoElement <- p(glue("You've selected {input$selectedName}. For reference, it contains: {toString(ingredientList)}."))
      container <- tagAppendChild(container, infoElement)
      
      #Describe recommendation list
      guideElement <- p(glue("Listed below are {input$n} cocktail recommendations based on your selected cocktail and the ingredients you have access to."))
      container <- tagAppendChild(container, guideElement)
      
      return(container)
    })
    
    #Create a series of panels displaying each recommendation
    output$recList <- renderUI({
      #Make a container to store the whole set of recommendations
      container <- div(class = "rec-list")
      recs <- recommended()
      
      #Go through each recommendation
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
}
