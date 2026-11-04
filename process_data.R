library(tidyverse)

# billboard variables:
# - artist: name of the artist
# - track: song title
# - date.entered: date the song entered the Billboard Top 100
# - wk1 through wk76: weekly rankings from first week on chart, lower is better

# Step 1: Calculate week offset and find ranks in early July 2000 (approx. week 26)
billboard_data <- billboard |> 
  mutate(date.entered = as.Date(date.entered),
         week_offset = as.numeric(difftime(date.entered, as.Date("2000-01-01"), units = "weeks")))

# Approximate July 1, 2000 (week 26 from Jan 1)
jul_week <- 26

# Find the rank of each song around July 1, 2000
billboard_jul <- billboard_data |> 
  filter(date.entered <= as.Date("2000-07-01")) |> 
  mutate(wk_idx = jul_week - week_offset,
         wk_idx = if_else(wk_idx >= 1 & wk_idx <= 76, wk_idx, NA_real_),
         jul_rank = NA_real_)

# Compute jul_rank by extracting values from wk columns
for (i in seq_len(nrow(billboard_jul))) {
  wk_idx <- billboard_jul$wk_idx[i]
  if (!is.na(wk_idx)) {
    col_name <- paste0("wk", wk_idx)
    billboard_jul$jul_rank[i] <- as.numeric(billboard_jul[[col_name]][i])
  }
}

# Select top 5 songs by jul_rank
top_5_songs <- billboard_jul |> 
  filter(!is.na(jul_rank)) |> 
  arrange(jul_rank) |> 
  slice(1:5) |> 
  pull(track)

# Step 2: Filter data to only the top 5 songs for 2000
billboard_filtered <- billboard_data |> 
  filter(track %in% top_5_songs)

# Step 3: Reshape data for plotting, limiting to 2000 (weeks 0 to 52)
top_songs <- billboard_filtered |> 
  select(artist, track, wk1:wk76) |> 
  pivot_longer(cols = starts_with("wk"),
               names_to = "week_num",
               values_to = "rank") |> 
  mutate(week_num = as.numeric(gsub("wk", "", week_num))) |> 
  left_join(billboard_filtered |> select(track, week_offset), by = "track") |> 
  mutate(week_num = week_num + week_offset) |> 
  filter(week_num >= 0 & week_num <= 52) |> 
  filter(!is.na(rank)) |> 
  mutate(song_artist = paste0(track, " (", artist, ")"))

write_rds(top_songs, file = "clean_data.rds")
