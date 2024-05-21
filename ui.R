#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    #Include a stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "master.css")
    ),

    # Application title
    titlePanel("Closest Cocktail"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput(
            "selectedName",
            "Favourite cocktail:",
            sort(cocktailsUmap$name),
            selected = "Amaretto Sour"
          ),
            numericInput("n",
                        "Number of cocktails:",
                        min = 1,
                        max = 10,
                        value = 5),
            numericInput("extraThreshold",
                        "How many new ingredients are you willing to buy:",
                        min = 0,
                        max = 10,
                        value = 1),
          checkboxGroupInput(
            "ingredients",
            "Ingredients you already have:",
            choices = sort(unique(cocktailsReduced$ingredient)),
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          htmlOutput("selectedInfo"),
          htmlOutput("recList")#,
          # plotOutput("umapPlot")
          # tableOutput("recTable"),
          # plotOutput("recPlot")
        )
    )
)
