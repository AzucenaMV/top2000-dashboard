library(tidyverse)

df <- read_csv("Top2000_extra_columns_spotify_genre.csv")
df$...1 <- NULL
colnames(df) <- tolower(colnames(df))

audio_features <- c("tempo","danceability","energy","valence","loudness","instrumentalness")
col_years <- colnames(df)[grepl("[0-9]{4}", colnames(df))]
non_col_years <- colnames(df)[!grepl("[0-9]{4}", colnames(df))]

get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

df[df$artist == 'Pearl Jam',c('genre_groups')] <- "rock"

df_unique_genres <- df %>% 
  count(artist, genre_groups) %>%
  mutate(n = ifelse(genre_groups == 'unknown', 0, n)) %>%
  arrange(artist, desc(n)) %>%
  distinct(artist, .keep_all = TRUE) %>%
  select(artist,genre_groups)

df$genre_groups <- NULL

df <- df %>% left_join(df_unique_genres, by = 'artist')
