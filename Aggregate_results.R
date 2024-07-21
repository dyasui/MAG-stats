# aggregating results from major national and international competitions
library(tidyverse)

# Selected Olympic teams:
olympic_teams <- read_csv("./olympic_teams.csv")
olympians <- olympic_teams %>% 
  select(NAME) %>% 
  pull()

nc1_df <- read_csv("./OlympicTrials2024/natnls-d1.csv")
nc2_df <- read_csv("./OlympicTrials2024/natnls-d2.csv")
ot1_df <- read_csv("./OlympicTrials2024/trials-day1.csv")
ot2_df <- read_csv("./OlympicTrials2024/trials-day2.csv")
usa_df <- bind_rows(list(nc1_df, nc2_df, ot1_df, ot2_df)) %>% 
  mutate(TEAM = "USA")

allJA_df <- read_csv("./Japan2024/AllJapan_results.csv")
nhkJA_df <- read_csv("./Japan2024/NHK_results.csv")
jpn_df <- bind_rows(list(allJA_df, nhkJA_df)) %>% 
  select(!Total) %>% 
  mutate(TEAM = "JPN")

results_files <- str_c("./results/", list.files("./results"))
results_other <- read_csv(results_files)

# merge all results into one dataframe
results_df <- bind_rows(list( usa_df, jpn_df, results_other)) %>% 
  filter(NAME %in% olympians)

library(ggthemes)
eventsdist <- results_df %>% 
  filter(NAME %in% olympians) %>% 
  select(c(NAME, TEAM, FX, PH, SR, VT, PB, HB)) %>%
  pivot_longer(
    cols = c(FX, PH, SR, VT, PB, HB),
    names_to = "Event",
    values_to = "Score",
  ) %>% filter(Score > 1) %>%
  ggplot(aes(x=Score)) +
  geom_density() +
  xlim(11, 16) +
  facet_wrap(~Event, ncol = 2) +
  theme_pander()

event_averages <- results_df %>% select(!AA) %>%
  filter(NAME %in% olympians) %>% 
  pivot_longer(
    cols = c(FX, PH, SR, VT, PB, HB),
    names_to = "Event",
    values_to = "Score",
  ) %>% 
  group_by(Event) %>% 
  summarise(mean = mean(Score, na.rm = T),
            sd   = sd(Score, na.rm = T))

eventsdist + 
  geom_vline(data = event_averages, aes(xintercept = mean)) +
  geom_vline(data = event_averages, aes(xintercept = mean + sd), linetype = "dashed") +
  geom_vline(data = event_averages, aes(xintercept = mean - sd), linetype = "dashed")

indv.averages_df <- results_df %>% 
  group_by(NAME, TEAM) %>% 
  summarise_at(vars(FX:AA), mean, na.rm=TRUE) %>% 
  arrange(desc(AA)) 

indv.averages_df <- indv.averages_df %>% 
  filter(NAME %in% olympians)

# function to pick top three scores on each event from a team
team_top_scores <- function(names, scores = indv.averages_df, perevent = 3) {
  scores %>% 
    filter(NAME %in% names) %>%
    select(NAME, FX, PH, SR, VT, PB, HB) %>% 
    pivot_longer(
      -NAME,
      names_to = "Event",
      values_to = "Score",
    ) %>% 
    # take top 3 on each event dropping extra tied scores
    group_by(Event) %>% 
    slice_max(n = perevent, order_by = Score, with_ties = FALSE) %>% 
    mutate(
      teamscore = sum(Score),
      # names     = c(unique(NAME))
      )  %>%
    group_by(Event) %>% 
    summarise(
      Score   = first(teamscore),
      OnEvent = list((NAME))
      ) %>% ungroup()
}

# function to take output of select_top_scores and calculate total
team_total <- function(top_scores) {
  top_scores %>% 
    summarise(x=sum(Score)) %>% 
    pull()
}


results_df %>% 
  group_by(TEAM) %>% 
  summarise(score = team_total(team_top_scores(unlist(list(unique(NAME)))))) %>% 
  arrange(desc(score))

# TODO: 
#   - prevent doubling up on same athlete in team_top_scores function
#   - fix club names instead of nations
#   - fix Igor Radivlov showing up as on team DEU not UKR

results_df %>% 
  filter(TEAM == "USA") %>% 
  pull(NAME) %>% 
  unique() %>% 
  team_top_scores() %>% kableExtra::kable()
  
indv.averages_df %>% 
  filter(TEAM == "GBR") %>% 
  pull(NAME) %>% 
  unique() %>% 
  team_top_scores()
