library(tidyverse)
library(tm) #text manipulation
library(gutenbergr) #project gutenberg api


#fetch the book from PGutenberg API and 
 
parse_book <- function(book_title = "",
                       book_author = "") {
  
  #collecting the list of available books
  books <- gutenberg_metadata
  
  #modifying the input if provided in the format "name surname"
  if (grepl(" ", book_author) & !grepl(", ", book_author)) {
    book_author <- strsplit(book_author, split = " ")
    book_author <- paste(book_author[[1]][2], book_author[[1]][1], sep = ", ")
  }
  
  #searching for a book
  book_data <-
    filter(
      books,
      grepl(book_title, title, ignore.case = T) &
        grepl(book_author, author, ignore.case = T) &
        has_text == T & language == "en"
    )
  
  #throwing 'exceptions' if there are no books or too many books
  if (nrow(book_data) == 0) {
    return("no results")
  }
  else if (nrow(book_data) > 1  &
           (book_title == "" | book_author == "")) {
    return("several results")
  }
  
  #if none of the conditions above are fulfilled there still may be several copies of the same book;
  #since gutenberg retrieves them from the lowest id, we just select the first book from the list
  
  if (nrow(book_data) > 1) {
    book_data <- book_data[1, ]
  }
  
  #saving the book data
  title <- book_data$title
  author <- book_data$author %>%
    strsplit(split = ", ")
  author <- paste(author[[1]][2], author[[1]][1], sep = " ")
  book_id <- book_data$gutenberg_id
  book <- gutenberg_download(book_id)$text
  
  #reading and formatting the text
  text <- strsplit(paste(book, collapse = " "), ' ')[[1]]
  text <- text %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    removeNumbers() %>%
    stripWhitespace() %>%
    tolower()
    
  #saving the number of words
  total_number_of_words <- length(text)
  
  #saving the number of pages
  total_number_of_pages <- ceiling(total_number_of_words / 250)
  
  #save the number of different words
  number_of_different_words <- nrow(data.frame(table(text)))
  
  total_unique_recurring_df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("uniques", "recurring"))
  
  for (i in 1:total_number_of_pages) {
    #parse an increasing number of pages and put those words into a list
    words <- table(text[1:(i * 250)])
    
    #change the table into a data frame and add column names
    temporary_df <- data.frame(words)
    colnames(temporary_df) <- c("Words", "Freq")

    #filter out empty spaces
    temporary_df <- filter(temporary_df, Words != " ")
    
    #filter out the words that occur only once in a given range from those recurring
    unique_temporary <- nrow(filter(temporary_df, Freq == 1))
    recurring_temporary <- nrow(filter(temporary_df, Freq > 1))
    
    #create a temporary data frame with results 
    temporary_df <-
      data.frame(uniques = unique_temporary, recurring = recurring_temporary)
    
    #attach it to the main frame
    total_unique_recurring_df[i, ] <- temporary_df

  }
  
  stats <-
    data.frame(title = title,
               author = author,
               pages = total_number_of_pages,
               words = total_number_of_words,
               different_words = number_of_different_words,
               unique_words = total_unique_recurring_df$uniques[total_number_of_pages])
  
  return(list(
    total_unique_recurring_df,
    stats
  ))
}

calculate_words <- function(a_data_frame, a_start_page, an_end_page) {
  
  #only grab the part of df we need to seve as a graph
  a_data_frame <- a_data_frame[a_start_page:an_end_page,]

  number_of_pages <- an_end_page - a_start_page + 1
  
  #since we've only counted the total number of unique words in a book,
  #now we calculate how the number of unique words changes across the book
  uniques <-
    vector(mode = "numeric", length = number_of_pages)
  for (i in 1:(number_of_pages - 1)) {
    uniques[i] <- a_data_frame$uniques[i + 1] - a_data_frame$uniques[i]
  }
  
  #lets do the same for the recurring words
  recurrences <-
    vector(mode = "numeric", length = number_of_pages)
  for (i in 1:(number_of_pages - 1)) {
    recurrences[i] <-
      a_data_frame$recurring[i + 1] - a_data_frame$recurring[i]
  }
  
  #lets put that data into one data frame
  plot_df <-
    data.frame(
      Pages = a_start_page:(an_end_page),
      Uniques = uniques,
      Recurrences = recurrences
    )
  
  return(plot_df)
}