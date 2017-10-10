library(shiny)
library(plotly)

shinyUI(fluidPage(

  # Application title
  titlePanel("Project Gutenberg books analyzer"),

  # Sidebar with the upload panel
  sidebarLayout(
    sidebarPanel(
      textInput("book_title", label = h3("Title")),
      textInput("book_author", label = h3("Author's surname")),
      numericInput("percentage", label = h3("Choose percentage"), min=1, max=99, value = 20),
      hr(),
      actionButton("action", label = "Search")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      textOutput("textStats")
    )
  )
))
