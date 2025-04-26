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

# initialize counters of valid words and pangrams - resample if too low
validwords <- 0
pangrams <- 0
i <- 1

while (validwords < 10 | pangrams == 0) {
  print(paste0('i =  ', i))
  # select 7 random letters and pick one as the middle letter
  letterlist <- sample(letters, 7, replace = F)
  middle <- letterlist[1]
  
  # create list of possible words
  solutionlist <- allwords %>%
    filter(valid) %>%
    filter(sum(!(str_split_1(word, '') %in% letterlist)) == 0) %>%
    filter(grepl(middle, word)) 
  
  validwords <- nrow(solutionlist)
  pangrams <- sum(solutionlist$pangram)
  print(validwords)
  print(pangrams)
  i <- i + 1
}
