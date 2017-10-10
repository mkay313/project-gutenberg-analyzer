library(shiny)
library(ggplot2)
library(plotly)

shinyServer(function(input, output) {
  
  terms <- reactive({
    req(input$action)
    isolate({
      withProgress({
        setProgress(message= "Processing...")
        result <- parse.book(input$book_title, input$book_author, input$percentage)
        validate(
          need(result != "no results", "Book not found. Please look for something else."),
          need(result != "several results", "More than one book found. Please provide more info.")
          )
        return(result)
      })
    })
  })
  
  #plot
  output$distPlot <- renderPlotly({
    v <- terms()
    ggplotly(ggplot(data=v[[1]], 
           aes(x=Pages, y=Uniques)) + xlab("# of pages") + ylab("# of new unique words") + geom_point() + geom_smooth(model=lm))
  })
  
  #text statistics under the plot
  output$textStats <- renderText({ 
    v <- terms()
    sprintf("After reading %s percent (%s pages) of the book you will have encountered %s out of %s unique words, 
            which is %s percent of total %s different words.
            The Average Readability Grade of %s by %s is %s.",
            input$percentage,
            v[[2]][['pages_at_percentage']], v[[2]]['unique_words_at_percentage'], 
            v[[2]]['total_unique_words'], v[[2]]['ratio'], 
            v[[2]]['number_of_different_words'], v[[2]][,'title'],
            v[[2]][,'author'],v[[2]]['readability'])
    })
})