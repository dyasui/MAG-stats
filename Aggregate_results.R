# aggregating results from major national and international competitions
library(tidyverse)

nc1_df <- read_csv("./USA2024/natnls-d1.csv")
nc2_df <- read_csv("./USA2024/natnls-d2.csv")
ot1_df <- read_csv("./USA2024/trials-day1.csv")
ot2_df <- read_csv("./USA2024/trials-day2.csv")
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
  select(MEET, ROUND, NAME, TEAM, FX:AA, FX_D:HB_D)

write_csv(results_df, "./ParisOlympics2024/2024-aggregate-results.csv")
