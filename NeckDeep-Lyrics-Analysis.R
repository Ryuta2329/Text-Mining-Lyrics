library(readxl)
library(dplyr)
library(tidytext)
library(textclean)
library(stringr)
library(ggplot2)

# Loading data: Database created in NeckDeep-SongsScrapped.R
lyrics <- read_excel(here::here("data", "Neck-Deep-Lyrics-Data.xlsx"),
    sheet = "lyrics", na = "Not Available", n_max = 2715) 

# Tokenizing: unigram analysis...and taking out contractions
# Taking out stop-words
data(stop_words)

tidy_lyrics <- lyrics %>%
  # Deleting stop_words contracted
  mutate(line = replace_contraction(line, 
    contraction.key = lexicon::key_contractions)
  ) %>% 
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%

# Data is prepared so now i begin to do sentiment analysis
# Counting words per album
tidy_lyrics %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
