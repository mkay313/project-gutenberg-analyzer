library(shiny)
library(shinyjs)
library(plotly)

shinyUI(fluidPage(

  # Application title
  titlePanel("Project Gutenberg books analyzer"),
  p("How many different words appear in a book? Can you measure the book difficulty by calculating how many unique words appear in it?
    Search for a volume from the Project Gutenberg to display graphs and statistical info."),

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
