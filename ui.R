library(shiny)
library(shinyjs)
library(plotly)

shinyUI(fluidPage(

  # Application title
  titlePanel("Project Gutenberg books analyzer"),

  # Sidebar with the upload panel
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      textInput("book_title", label = h3("Book title"), value="Christmas Carol"),
      textInput("book_author", label = h3("Author"), value="Dickens"),
      actionButton("action", label = "Search!"),
      hr(),
      uiOutput("coverage_slider")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      textOutput("textStats")
    )
  )
))
