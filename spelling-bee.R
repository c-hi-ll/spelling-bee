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

# sample letters
while (validwordcount < 10 | pangramcount == 0) {
  # select 7 random letters and pick one as the middle letter
  # make sure at least one is a vowel
  vowel <- sample(c('a', 'e', 'i', 'o', 'u'), 1)
  letterlist <- c(vowel, sample(letters[letters != vowel], 6, replace = F))
  middle <- letterlist[sample(c(1:7), 1)]
  
  # create list of possible words
  solutionlist <- validwords %>%
    filter(grepl(middle, word)) %>%
    filter(all(str_split_1(word, '') %in% letterlist)) 
  
  validwordcount <- nrow(solutionlist)
  pangramcount <- sum(solutionlist$pangram)
  i <- i + 1
}

# set scoring
solutionlist <- solutionlist %>%
  mutate(score = case_when(pangram    ~ 7 + nchar,
                           nchar == 4 ~ 1,
                           nchar > 4  ~ nchar))

maxscore <- sum(solutionlist$score)

# set up interactive prompting
currentscore <- 0
gameend <- F
letterlist_display <- toupper(c(letterlist[letterlist != middle][1:3],
                                paste0('[', middle, ']'),
                                letterlist[letterlist != middle][4:6]))
                        

while (!gameend) {
  cat(letterlist_display, '\n')
  cat('Current score: ', currentscore, '\n')
  cat('Enter "stop" to end the game. \n')
  input <- tolower(readline('Your guess: '))
  
  # end game
  if (input == 'stop') {
    gameend <- T
  }
  
  # checks
  chk_middle   <- grepl(middle, input)
  chk_length   <- nchar(input) >= 4
  chk_letters  <- all(str_split_1(input, '') %in% letterlist)
  chk_word     <- input %in% allwords$word
  chk_solution <- input %in% solutionlist$word
  chk_pangram  <- input %in% solutionlist$word[solutionlist$pangram]

}
