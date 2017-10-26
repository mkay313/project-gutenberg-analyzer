library(shiny)
library(shinyjs)
library(ggplot2)
library(ggthemes)
library(plotly)

shinyServer(function(input, output) {
  
  reactive_parse_book <- reactive({
    req(input$action)
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        result <- parse_book(input$book_title, input$book_author)
        validate(
          need(
            result != "no results",
            "Book not found. Please look for something else."
          ),
          need(
            result != "several results",
            "More than one book found. Please provide more info."
          )
        )
        return(result)
      })
    })
  })
  
  observeEvent(input$action, {
    show("coverage_slider")
  })
  
  #serving the slider on the server side to be able to change max depending on the book length
  output$coverage_slider <- renderUI({
    max_pages <- reactive_parse_book()[[2]]$pages
    sliderInput(
      inputId = "coverage",
      label = "Which pages of the book you'd like to read?",
      min = 1,
      max = max_pages,
      value = c(1,20),
      step = 5
    )
  })
  
  reactive_calculate_words <- reactive({
    req(input$coverage)
    calculate_words(reactive_parse_book()[[1]], input$coverage[1], input$coverage[2])
  })
  
  #plot
  output$distPlot <- renderPlotly({
    ggplotly(
      ggplot(data = reactive_calculate_words(),
             aes(x = Pages, y = Uniques)) +
        xlab("# of pages") +
        ylab("# of new unique words") +
        geom_point() +
        geom_smooth(method = "auto") +
        theme_few()
    )
  })
  
  #text statistics under the plot
  output$textStats <- renderText({
    stats_frame <- reactive_parse_book()[[2]]
    sprintf(
      "%s by %s consists of %s words over %s pages. There are %s different words, of which %s appear only once.",
      stats_frame$title, stats_frame$author,
      stats_frame$words, stats_frame$pages,
      stats_frame$different_words, stats_frame$unique_words
    )
  })
})