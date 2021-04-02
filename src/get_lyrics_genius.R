# This tibble will retain scrapping failures
fails <- tibble(setNames(
  data.frame(integer(), character(),
    character(), integer(),
    integer(), character(), character()),
  colnames(songs)))

# Esta funcion hace el scrapping. 
# La puedo necesitar para procesar las fallas sin tanto typing
get_lyrics_genius <- function(urls_vec)
{
  urls_vec %>% 
    map(., function(x) 
    {
      z <- get_lyrics_url(x)
      if (nrow(z) != 0) 
        cat(paste(unique(z$song_name), "succesfully scrapped!"), sep = "\n")
      else {
        if (length(album_info %>%
              filter(song_lyrics_url == x) %>%
              .$song_name) > 0) 
        {
          cat(paste("Couldn't get ", 
            album_info %>% filter(song_lyrics_url == x) %>% .$song_name, 
              ". Retrying...", sep = ""), sep = " ")
          Sys.sleep(2)
          z <- get_lyrics_id(album_info$song_id[album_info$song_lyrics_url == x])

          if (nrow(z) != 0)
            cat("Succes!", sep = "\n")
          else 
          {
            cat("Retrying...", sep = " ")
            z <- get_lyrics_search(
              album_info$artist_name[album_info$song_lyrics_url == x], 
              album_info$song_name[album_info$song_lyrics_url == x])

            if(nrow(z) != 0)
              cat("Succes!", sep = "\n")
            else {
              assign("fails", fails %>% 
                  bind_rows(album_info[album_info$song_lyrics_url == x,]), 
                envir=globalenv())
              cat("Culd not be Scrapped! :(", sep = "\n")
            }
          }
        }
      }
      Sys.sleep(2)
      z
    }) %>%
  keep(function(x) nrow(x) != 0 ) %>%
  bind_rows()
}