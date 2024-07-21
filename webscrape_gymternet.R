# Load the packages
library(rvest)
library(tidyverse)
library(countrycode)

# function to read results from an All-around competition in gymternet's format
read_gymternet_AA <- function(table, label = "AA") {
  table %>% 
    mutate( # record athlete name and team name, separated by newline
      NAME = str_split_i(Athlete, "\n", 1), 
      TEAM = str_split_i(Athlete, "\n", 2),
    ) %>% 
    mutate(across( # convert DNS, —— to NA and make numeric for event results
      where(is.character) & matches(c("FX","PH","SR","VT","PB","HB")),
      ~as.numeric(na_if(., "——"))
    )) %>% 
    mutate(ROUND = label) %>% # save the name of the round of competition
    select(ROUND, NAME, TEAM, FX:AA)
}

# function to read results from an event-final competition in gymternet's format
read_gymternet_EV <- function(table, label) {
  table %>% 
    mutate( 
      across( # convert DNS, —— to NA and make numeric for event results
        where(is.character) & matches(c("D", "Total")),
        ~as.numeric(na_if(., "DNS")) ),
      EVENT = case_when(
        str_detect(label, "Floor|FX") ~ "FX",
        str_detect(label, "Pommel|PH") ~ "PH",
        str_detect(label, "Rings|SR") ~ "SR",
        str_detect(label, "Vault|VT") ~ "VT",
        str_detect(label, "P-Bars|Parrallel|PB") ~ "PB",
        str_detect(label, "High Bar|HB") ~ "HB"
      ),
      ROUND = label
    ) %>% 
    rename( NAME = Athlete, TEAM = Nation) %>% 
    select(EVENT, ROUND, NAME, TEAM, D, E, Total)
}

read_gymternet_international <- function(url, meet_name) {
  webpage <- read_html(url)
  # save all results tables in a list
  table_list <- webpage %>% html_table(header = TRUE)
  
  # extract event and round info from headers above each table
  headers <- (webpage %>% html_elements("p") %>% html_text2() )
  # drop the first element of headers (the description),
  # and the last two which are just formatting
  headers <- headers[2:(length(headers)-2)]
  
  # initialize the loop:
  i = 1; j = 1; k = 1 # index for all tables, aa tables, and ev tables
  table_aa = list(); table_ev = list()
  table_list <- webpage %>% html_table(header = TRUE)
  # loop over all tables (indexed by their headers):
  for (header in headers) {
    if(str_detect(header, "Junior")) { 
      break # don't include results for juniors
    } 
    else if (str_detect(header, "All-Around")) {
      table_aa[[j]] <- read_gymternet_AA(table_list[[i]], header)
      j = j + 1
    } else if (
      str_detect(header, 
                 c("Event|Floor|Pommel|Rings|Vault|P-Bars|High Bar|FX|PH|SR|VT|PB|HB")
      ) ) {
      table_ev[[k]] <- read_gymternet_EV(table_list[[i]], header)
      k = k + 1
    } else {
      break 
    }
    i = i + 1
  }
  
  aa_results <- bind_rows(table_aa)
  
  event_results <- bind_rows(table_ev) %>% 
    pivot_wider(
      id_cols = c(ROUND, NAME, TEAM),
      names_from = EVENT,
      values_from = c(D, E, Total), 
      values_fn = max # two vaults are annoying to keep track of
    ) %>% 
    rename(
      FX = Total_FX, PH = Total_PH, SR = Total_SR, VT = Total_VT, PB = Total_PB, HB = Total_HB,
      FX_D = D_FX, PH_D = D_PH, SR_D = D_SR, VT_D = D_VT, PB_D = D_PB, HB_D = D_HB,
      FX_E = E_FX, PH_E = E_PH, SR_E = E_SR, VT_E = E_VT, PB_E = E_PB, HB_E = E_HB) %>% 
    mutate(AA = NA)
  
  results <- bind_rows(list(aa_results, event_results)) %>% 
    mutate(
      MEET = meet_name,
      TEAM = countrycode(TEAM, origin = 'country.name', destination = 'iso3c')
    ) %>% 
    select(MEET, ROUND, NAME, TEAM, FX, PH, SR, VT, PB, HB, AA,
           FX_D, PH_D, SR_D, VT_D, PB_D, HB_D)
  
  results
}

save_gym_results <- function(results, year = "2024") {
  meet_name <- results %>% 
    select(MEET) %>% head(n=1) %>% pull()
  filename <- paste("./results/", meet_name, "-", year, ".csv", sep = "")
  write_csv(results, file = filename)
}

read_gymternet_international(
  "https://thegymter.net/2024/04/29/2024-european-mens-championships-results/",
  "euro_champs"
  ) %>% 
  save_gym_results()
