library(geniusr)
library(dplyr)
library(purrr)
library(magrittr)
library(stringr)
library(openxlsx)

source("get_lyrics_genius.R")

# Taking Artist info, album and songs lyrics
artist_info <- search_artist("Neck Deep") 
songs <- get_artist_songs_df(artist_info$artist_id) 

album_info <- songs %>% 
  .$song_id %>% map(., function(x) {
    z <- get_song_df(x); Sys.sleep(5); z
  }) %>%
  bind_rows() %>%  
  select(album_id, album_name, 
    song_id, song_name, song_lyrics_url,
    artist_id, artist_name) %>%
  filter(
    !(album_name %in% c("Serpents - EP", "In Bloom: Versions", 
      "Green Day: The Early Years (Covers & New Classics)", 
      "Kerrang! Does Green Day’s American Idiot ",
      "December"))
  )

# Obtaining Lyrics from songs 
# Scrapping takes place
lyrics <- album_info %>% 
  .$song_lyrics_url %>%
  get_lyrics_genius()

# There was some failing so i processed them...
# Repeat as long is needed to empty fails... 
# (this should be a function)
lyrics <- bind_rows(lyrics, 
 fails %>% 
    .$song_lyrics_url %>%
   get_lyrics_genius())

# Antes de guardar los datos modifico los apostrofes
album_info %<>% 
  mutate(
    song_name = ifelse(str_detect(song_name, "’"), 
      str_replace(song_name, "’", "'"), song_name),
    album_name = ifelse(str_detect(album_name, "’"), 
      str_replace(album_name, "’", "'"), album_name)) 

# En el caso de lyrics, coloco los features y 
# modifico los apostrofes en las letras
slyrics %<>%
  mutate(
    song_name = ifelse(str_detect(song_name, "’"), 
      str_replace(song_name, "’", "'"), song_name),
    line = ifelse(str_detect(line, "’"), 
      str_replace(line, "’", "'"), line)) %>%
  mutate_at(vars(song_name), 
    ~case_when(
      str_detect(song_name, "A Part of Me") ~ 
        str_c(., " (Ft. Laura Whiteside)"),
      str_detect(song_name, "Don't Wait") ~ 
        str_c(., " (Ft. Sam Carter)"),
      str_detect(song_name, "Kali Ma") ~ 
        str_c(., " (Ft. Jeremy McKinnon)"),
      str_detect(song_name, "(.)") ~ .
    )) %>% 
  select(-song_id, -song_lyrics_url) %>%
  left_join(album_info)

# Crea el archivo de datos Excel
wb <- createWorkbook()

addWorksheet(wb, "lyrics")
header_style <- createStyle(halign = "center", textDecoration = "bold")
writeData(wb, "lyrics", lyrics, 
  keepNA = T, na.string = "Not Available", 
  headerStyle = header_style)
setColWidths(wb, "lyrics", cols = 1:ncol(lyrics), widths = "auto")

saveWorkbook(wb, 
  file = here::here("data", "Neck-Deep-Lyrics-Data.xlsx"), 
  overwrite = TRUE)