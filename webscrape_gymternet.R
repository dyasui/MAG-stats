# Load the packages
library(rvest)
library(tidyverse)
library(countrycode)

# URL of the webpage
url <- 'https://thegymter.net/2024/04/29/2024-pacific-rim-championships-mens-results/'
meet_name <- "pacrim_champs"

# Read the HTML content from the webpage
webpage <- read_html(url)

# extract event and round info from headers above each table
headers <- (webpage %>% html_elements("p") %>% html_text2() )
headers <- headers[2:(length(headers)-2)] %>% 
  str_replace_all(c(
    "All-Around" = "AA",
    "Floor" = "FX",
    "Pommels" = "PH",
    "Rings" = "SR",
    "Vault" = "VT",
    "P-Bars" = "PB",
    "High Bar" = "HB",
    "Final Results" = "F",
    "Qualification Results" = "Q"
  )) %>% 
  as_tibble() %>% 
  mutate(
    junior = str_detect(value, "Junior"),
  ) %>% filter(junior==FALSE) %>% 
  separate(value, 
           into = c(
             # "age",
             "event", 
             "round"),
           sep = " ") %>% 
  # filter(age == "Senior") %>% 
  select("event", "round") %>% 
  unite("header", c(event, round), sep = " ") %>% 
  pull()
# make sure they look right:
headers

# initialize the loop:
i = 1; j = 1; k = 1
table <- webpage %>% html_table(header = TRUE)
table_events <- list()
table_aa <- list()
# loop over all tables (indexed by their headers):
for (header in headers) {
  if(str_detect(header, "Junior")) { break }
  else if (str_detect(header, "All-Around")) {
    table_aa[[j]] <- table[[i]][, -1] %>% 
      mutate(NAME = str_split_i(Athlete, "\n", 1), 
             TEAM = str_split_i(Athlete, "\n", 2),
             ROUND = header,
      ) %>% 
      mutate(across( # convert "DNS" to NA and convert to numeric
        where(is.character) & matches(c("FX","PH","SR","VT","PB","HB")),
        ~as.numeric(na_if(., "——"))
      )) %>% 
      select(!c(Athlete))
    j = j + 1
  } else if (str_detect(header, c("FX|PH|SR|VT|PB|HB"))) {
    table_events[[k]] <- table[[i]][, -1] %>% 
      mutate( 
        across( # convert "DNS" to NA and convert to numeric
          where(is.character) & matches(c("D", "Total")),
          ~as.numeric(na_if(., "DNS"))
        ),
        
        EVENT = str_split_i(header, " ", 1),
        ROUND = str_split_i(header, " ", 2),
      ) %>% 
      rename( NAME = Athlete, TEAM = Nation) %>% 
      select(EVENT, ROUND, NAME, TEAM, D, Total)
    
    k = k + 1
  } else {
    break 
  }
  i = i + 1
}

aa_results <- bind_rows(table_aa) %>% 
  mutate(across( # convert "DNS" to NA and convert to numeric
      where(is.character) & matches(c("FX","PH","SR","VT","PB","HB")),
      ~as.numeric(na_if(., "DNS"))
      ))

event_results <- bind_rows(table_events) %>% 
  pivot_wider(
    id_cols = c(ROUND, NAME, TEAM),
    names_from = EVENT,
    values_from = c(D, Total), 
    values_fn = max # two vaults are annoying to keep track of
    ) %>% 
  rename(FX = Total_FX, PH = Total_PH, SR = Total_SR, VT = Total_VT, PB = Total_PB, HB = Total_HB,
    FX_D = D_FX, PH_D = D_PH, SR_D = D_SR, VT_D = D_VT, PB_D = D_PB, HB_D = D_HB) %>% 
  mutate(AA = NA)

results <- bind_rows(list(aa_results, event_results)) %>% 
  mutate(
    MEET = meet_name,
    TEAM = countrycode(TEAM, origin = 'country.name', destination = 'iso3c')
    ) %>% 
  select(MEET, ROUND, NAME, TEAM, FX, PH, SR, VT, PB, HB, AA,
         FX_D, PH_D, SR_D, VT_D, PB_D, HB_D)

# check to makee sure everything looks okay:
View(results)

# save to file
filename <- paste("./results/", meet_name, "-2024.csv", sep = "")
write_csv(results, file = filename)
