library(tidyverse)

df <- read_csv("Top2000_extra_columns_spotify_genre.csv")
df$...1 <- NULL
colnames(df) <- tolower(colnames(df))

audio_features <- c("tempo","danceability","energy","valence","loudness","instrumentalness")
col_years <- colnames(df)[grepl("[0-9]{4}", colnames(df))]
non_col_years <- colnames(df)[!grepl("[0-9]{4}", colnames(df))]

#df %>%
#  select(col_years)
# df_genre <- df %>%
#   count(genre_groups, name = 'Counts') %>%
#   arrange(desc(Counts)) %>%
#   mutate(proportion = /sum()) 
# 
# 
# df_country <- df_selected() %>%
#   replace_na(list(artist_country_name = "Unknown")) %>%
#   count(artist_country_name, continent) %>%
#   mutate(only_one = ifelse(n < 10, TRUE, FALSE)) %>%
#   mutate(artist_country_name = ifelse(only_one,"Others",artist_country_name)) %>%
#   group_by(artist_country_name, continent) %>%
#   summarize(sum = sum(n)) %>%
#   ungroup %>%
#   mutate(key = paste(artist_country_name,continent, sep = "-")) %>%
#   mutate(artist_country_name = ifelse(artist_country_name == "Others", paste0(artist_country_name,": ",continent),artist_country_name),
#          continent = ifelse(continent == "other", "Unkown", continent))
