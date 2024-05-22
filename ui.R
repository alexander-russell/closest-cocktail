library(shiny)

#Define UI for application that draws a histogram
fluidPage(
  #Include a stylesheet
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "master.css")
  ),

  #Set application title
  titlePanel("Closest Cocktail"),

  #Configure input sidebar
  sidebarLayout(
    sidebarPanel(
      #Dropdown to select favourite cocktail
      selectInput(
        "selectedName",
        "Favourite cocktail:",
        sort(cocktailsUmap$name),
        selected = "Amaretto Sour"
      ),
      #Input to select how many recommendations you want
      numericInput(
        "n",
        "Number of cocktails:",
        min = 1,
        max = 10,
        value = 5
      ),
      #Input to select how many new ingredients you'd buy
      numericInput(
        "extraThreshold",
        "How many new ingredients are you willing to buy:",
        min = 0,
        max = 10,
        value = 1
      ),
      #Input to select what ingredients you have on hand
      checkboxGroupInput(
        "ingredients",
        "Ingredients you already have:",
        choices = sort(unique(cocktailsReduced$ingredient)),
      )
    ),

    #Configure output panel
    mainPanel(
      #Text information on selected cocktail
      htmlOutput("selectedInfo"),
      #Displays recommended cocktails as boxes
      htmlOutput("recList")
    )
  )
)
