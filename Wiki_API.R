
#############################
### Packages& Parameters  ###
#############################

library(tidyverse)            # Data Manipulation
library(WikipediR)            # Wikipedia Queries
library(rvest)                # Web Scraping

category_list <- "Ice hockey" # Set to starting category name
filename <- "data.txt"        # Output filename
total_pages <- c()
categories <- c()
text_data <- as.null()

########################################
### Retrieve Category and Page List  ###
########################################

while (!is.null(category_list)) {

  # initialize list for next layer of sub-categories
  next_category_list <- c()
  
  # retrieve pages and categories
  for (category in category_list) {
    pages <- pages_in_category("en", "wikipedia", categories = category,  properties = c("title"), type = c("page"))
    sub_cats <- pages_in_category("en", "wikipedia", categories = category,  properties = c("title"), type = c( "subcat"))
  
    # add pages to list  
    if (length(pages$query$categorymembers) > 0 ) {
      for (i in 1:length(pages$query$categorymembers)) {
        total_pages <- c(total_pages, pages$query$categorymembers[[i]]$title)
      }
    }
  
    # add sub categories to list
    if (length(sub_cats$query$categorymembers) > 0 ) {
      for (i in 1:length(sub_cats$query$categorymembers)) {
        sub_cat = gsub("Category:", "", sub_cats$query$categorymembers[[i]]$title)
        categories <- c(categories, sub_cat)
        next_category_list <- c(next_category_list, sub_cat)
      }
    }
  }
  category_list <- next_category_list
}

print(paste0("Number of Categories: ", length(categories)))
print(paste0("Number of Pages: ", length(total_pages)))

#################################################
### Retrieve Page Text, Pre-process and Save  ###
#################################################

# read all page paragraph data
for (i in 1:length(total_pages)) {
  page = gsub(" ", "_", total_pages[i])
  print(paste0("Loading Page: ", page))
  web_address <- paste0("https://en.wikipedia.org/wiki/",page)
  page_html <- read_html(web_address)
  page_paragraphs <- html_nodes(page_html,"p")
  page_text <- paste(html_text(page_paragraphs), sep = '', collapse = '')
  for (j in 1:length(page_text)) {
    if (is.null(text_data)) { text_data <- page_text }
    else { text_data <- paste(text_data, page_text, sep = '', collapse = '') }
  }
}

# text pre-processing
text_data <- gsub("\n", " ", text_data)       # remove the existing line breaks
text_data <- gsub("\\.", "\r\n", text_data)   # add line breaks
text_data <- gsub("\\[\\d\\]"," ", text_data) # remove citations
text_data <- gsub("[[:punct:]]"," ", text_data) # remove punctuation
text_data <- text_data %>% tolower()

# save text file
write(text_data, filename)




