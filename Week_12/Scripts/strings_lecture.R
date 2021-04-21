### working with text
##created by Emily Rukstales
### created on 2021-04-19

### load libraries
library(here)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(janeaustenr)



### WORDS! 

words<-"This is a string"
words

words_vector<-c("Apples", "Bananas","Oranges")
words_vector



### Manipulation

#paste words together
paste("High temp", "Low pH")
paste("High temp", "Low pH", sep = "-") #add dash between words
paste0("High temp", "Low pH") #remove space between words

shapes <- c("Square", "Circle", "Triangle")
paste("My favorite shape is a", shapes)

two_cities <- c("best", "worst")
paste("It was the", two_cities, "of times.")

str_length(shapes)

seq_data <- c("ATCCCGTC")
str_sub(seq_data, start = 2, end = 4)

str_sub(seq_data, start = 3, end = 3) <-"A"
seq_data

str_dup(seq_data, times = c(2, 3))



### White space

badtreatments <- c("High", " High", "High ", "Low", "Low")
badtreatments

str_trim(badtreatments) #remove extra whitespaces

str_trim(badtreatments, side = "left")

str_pad(badtreatments, 5, side = "right") #pads items by adding spaces on the right until there are 5 total characters

str_pad(badtreatments, 5, side = "right", pad = "1") #pads with a 1 instead of a space



### Locale sensitive

x <- "I love R!"

str_to_upper(x)
str_to_lower(x)
str_to_title(x)



### Pattern matching functions

data <- c("AAA", "TATA", "CTAG", "GCTT")

str_view(data, pattern = "A") #shows strings that contain an A

str_detect(data, pattern = "AT") #shows a logical vector (T/F) saying whether each row contains patern or not

str_locate(data, pattern = "AT")



### Regular expressions

vals <- c("a.b", "b.c", "c.d")

#escape metacharacters: put 2 backslashes in front of character
str_replace(vals, "\\.", " ") #replaces first period with space

vals <- c("a.b.c", "b.c.d", "c.d.e")

str_replace_all(vals, "\\.", " ") #replaces every period with a space


#find a certain type of sequence
val2 <- c("test 123", "test 456", "test")

str_subset(val2, "\\d") #searches for rows that contain digits


#character class
str_count(val2, "[aeiou]") #counts how many vowels are in each string

str_count(val2, "[0-9]")



### Quantifiers
strings <- c("550-153-7578",
             "banana",
             "435.114.7586",
             "home: 672-442-6739")

#Make a regex that finds all the strings that contain a phone number. We know there is a specific pattern (3 numbers, 3 numbers, 4 numbers and it can have either a "." or "-" to separate them). Let's also say we know that the first number cannot be a 1
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
# subset only the strings with phone numbers
test<-str_subset(strings, phone)
test

#think pair share
test %>%
  str_replace_all(pattern = "\\.", replacement = "-") %>% # replace periods with -
  str_replace_all(pattern = "[a-zA-Z]|\\:", replacement = "") %>% # remove all the things we don't want
  str_trim()



### tidytext

head(austen_books())

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", #carrot means starting with "chapter"
                                                 ignore_case = TRUE)))) %>%
  ungroup()
head(original_books)

tidy_books <- original_books %>%
  unnest_tokens(output = word, input = text) #takes each word ("token") from text column and puts them into a new column (word) with each word as an observation
head(tidy_books)

head(get_stopwords()) #shows stopwords (meaningless words)

#remove stopwords
cleaned_books <- tidy_books %>%
  anti_join(get_stopwords())
head(cleaned_books)

cleaned_books %>%
  count(word, sort = TRUE) #sort by most used words

sent_word_counts <- tidy_books %>%
  inner_join(get_sentiments()) %>% #keep only positive or negative words
  count(word, sentiment, sort = TRUE) #sort by most used pos/neg words
head(sent_word_counts)

sent_word_counts %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")



### Wordclouds
words <- cleaned_books %>%
  count(word) %>%
  arrange(desc(n)) %>%
  slice(1:100)
wordcloud2(words, shape = "triangle", size = 0.3)

