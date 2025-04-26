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

validwords <- allwords %>%
  filter(valid)

# initialize counters of valid words and pangrams - resample if too low
validwordcount <- 0
pangramcount <- 0
i <- 1

while (validwordcount < 10 | pangramcount == 0) {
  print(paste0('i =  ', i))
  # select 7 random letters and pick one as the middle letter
  # make sure at least one is a vowel
  vowel <- sample(c('a', 'e', 'i', 'o', 'u'), 1)
  letterlist <- c(vowel, sample(letters[letters != vowel], 6, replace = F))
  middle <- letterlist[sample(c(1:7), 1)]
  
  # create list of possible words
  solutionlist <- validwords %>%
    filter(valid) %>%
    filter(grepl(middle, word)) %>%
    filter(sum(!(str_split_1(word, '') %in% letterlist)) == 0) 
  
  validwordcount <- nrow(solutionlist)
  pangramcount <- sum(solutionlist$pangram)
  print(validwordcount)
  print(pangramcount)
  i <- i + 1
}
