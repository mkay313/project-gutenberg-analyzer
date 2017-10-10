library(dplyr) #filter
library(tm) #text manipulation
library(magrittr) #piping
library(scales) #percent()
library(memoise) #caching the results
library(readability) #readability score
library(gutenbergr) #project gutenberg api


parse.book <- function(book_title="", book_author="", percentage) {
  
#collecting the list of available books
  books <- gutenberg_metadata

#searching for a book
  book_data <- filter(books, grepl(book_title, title, ignore.case=T) & grepl(book_author, author, ignore.case=T) & has_text==T & language=="en")

#throwing 'exceptions' if there are no books or too many books  
    if(nrow(book_data)==0) {
    return("no results") 
  } else if(nrow(book_data)>1  & (book_title=="" | book_author=="")) {
    return("several results")
  }
  
#if none of the conditions above is fulfilled there still may be several copies of the same book;
#since gutenberg retrieves them from the lowest id, we just select the first book from the list
  
#  book_data <- book_data[[1,]]
  
#saving the book data  
  title <- book_data$title
  author <- book_data$author
  book_id <- book_data$gutenberg_id
  book <- gutenberg_download(book_id)$text
  
#reading and formatting the text
  text <- strsplit(paste(book, collapse = " "),' ')[[1]]
  text <- text %>% 
  removePunctuation(preserve_intra_word_dashes = TRUE) %>% 
  removeNumbers() %>% 
  stripWhitespace() %>%
  tolower() %>% 
  removeWords(" ")
  
#calculating the average grade level readability (2 decimal places)
  readability_score <- round(readability(text, NULL)$Average_Grade_Level,2)
  
#saving the number of words
  total_number_of_words <- length(text)
  
#saving the number of pages
  total_number_of_pages <- ceiling(total_number_of_words/250)
  
#save the number of different words
  number_of_different_words <- nrow(data.frame(table(text)))

  total_unique_recurring_df <- data.frame()
  for (i in 1:total_number_of_pages) {
  #parse an increasing number of pages and put those words into a list
    words <- table(text[1:(i*250)])
  #change the table into a data frame and add column names
    temporary_df <- data.frame(words)
    colnames(temporary_df)[1] <- "Words"
    colnames(temporary_df)[2] <- "Freq"
  #filter out the words that occur only once in a given range from those recurring
    unique_temporary <- nrow(filter(temporary_df, Freq==1))
    recurring_temporary <- nrow(filter(temporary_df, Freq>1))
  #create a temporary data frame with results and attach it to the main frame
    temporary_df <- data.frame(uniques=unique_temporary, recurring=recurring_temporary)
    total_unique_recurring_df <- rbind(total_unique_recurring_df,temporary_df)
    if(i==ceiling(percentage/100*total_number_of_pages)) {
      after_percentage_df <- temporary_df
      }
    }

#removing unnecessary stuff
  rm(temporary_df,unique_temporary,recurring_temporary)

#since we've only counted the total number of unique words in a book, now we calculate how the number of unique words changes across the book
  uniques <- vector(mode = "numeric", length = total_number_of_pages-1)
  for (i in 1:(total_number_of_pages-1)) {
    uniques[i] <- total_unique_recurring_df$uniques[i+1] - total_unique_recurring_df$uniques[i]
    }

#lets do the same for the recurring words
  recurrences <- vector(mode = "numeric", length = total_number_of_pages-1)
  for (i in 1:(total_number_of_pages-1)) {
    recurrences[i] <- total_unique_recurring_df$recurring[i+1] - total_unique_recurring_df$recurring[i]
    }

#lets put that data into one data frame
  plot_df <- data.frame(Pages = 1:(total_number_of_pages-1), Uniques = uniques, Recurrences = recurrences)

#calculate some text stats    
  pages_at_percentage <- floor(percentage/100*total_number_of_pages)
  unique_words_at_percentage <- after_percentage_df$uniques
  total_unique_words = total_unique_recurring_df$uniques[total_number_of_pages]
  ratio = round(unique_words_at_percentage/number_of_different_words*100,2)
  
#returning text stats and the plot df to access the results in our reactive function: 
 
  text_stats <- data.frame(
    pages_at_percentage = pages_at_percentage,
    unique_words_at_percentage = unique_words_at_percentage,
    total_unique_words = total_unique_words,
    ratio = ratio,
    number_of_different_words = number_of_different_words,
    title = title,
    author = author,
    readability = readability_score)
  return(list(plot_df,text_stats))
}