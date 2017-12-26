library(tidyverse)
library(lubridate)

urlfile <- "https://raw.githubusercontent.com/batpigandme/nba_xmas/master/data/nba_schedule_1718.csv"

nba_schedule_1718 <- read_csv(url(urlfile), col_types = cols(started_at = col_character()))

# get the scheduled games
christmas_games <- nba_schedule_1718 %>%
  filter(date == "2017-12-25")

# because I'm lying about tz, some lubridate will be necessary
christmas_games <- christmas_games %>%
  separate(started_at, c("startdate", "starttime"), sep = "T", remove = FALSE) %>%
  separate(starttime, c("starttime", "team_tz"), sep = "-") %>%
  unite("datetime", c("startdate", "starttime"), sep = " ") %>%
  mutate(starttime = ymd_hms(datetime, tz = "America/New_York")) %>%
  mutate(utc_less = as.integer(str_sub(team_tz, start = 2L, end = 2L))) %>%
  mutate(diff_est = utc_less - 5) %>%
  mutate(start_est = starttime + dhours(diff_est))

# use average duration from games played this year
urlfile <- "https://raw.githubusercontent.com/batpigandme/nba_xmas/master/data/nba_games.csv"
nba_games <- read_csv(url(urlfile))

avg_duration <- summarise(nba_games, avg_secs = mean(duration, na.rm = TRUE))

avg_duration <- avg_duration %>%
    mutate(avg_secs = lubridate::dseconds(avg_secs))

# average duration
avg_duration$avg_secs

# let's give a buffer
christmas_games <- christmas_games %>%
  mutate(proj_end = start_est + dhours(2.5))

christmas_games$proj_end

# see https://github.com/daattali/timevis
# devtools::install_github("daattali/timevis")
library(timevis)

timevis_games <- christmas_games %>%
  select(c("id", "label", "start_est", "proj_end")) %>%
  rename(content = "label", start = "start_est", end = "proj_end")

id <- as.character("the-movie")
content <- as.character("I, Tonya.")
start <- ymd_hms("2017-12-25 15:15:00", tz = "America/New_York")
end <- start + dminutes(121)

movie_row <- tibble("id" = id, "content" = content, "start" = start, "end" = end)

timevis_df <- bind_rows(timevis_games, movie_row)

timevis(timevis_df)
