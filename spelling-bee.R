# load libraries
library(tidyverse)

# create word list
allwords <- read.delim('~/Documents/spelling-bee/words_alpha.txt',
                       header = F,
                       col.names = 'word') %>%
  rowwise() %>%
  mutate(nchar = nchar(word),
         unique_char = length(unique(str_split_1(word, ''))),
         # filter word list to 4 chars or greater, 7 or fewer unique letters
         valid = nchar >= 4 & unique_char <= 7,
         # create subset of words that are pangrams
         pangram = unique_char == 7)

# select 7 random letters and pick one as the middle letter
letterlist <- sample(letters, 7, replace = F)
middle <- letterlist[1]

# create list of possible words
solutionlist <- allwords %>%
  filter(valid) %>%
  filter(sum(!(str_split_1(word, '') %in% letterlist)) == 0) %>%
  filter(grepl(middle, word)) 
