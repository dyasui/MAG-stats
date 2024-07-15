# Load the packages
library(rvest)
library(tidyverse)
library(countrycode)

# URL of the webpage
url <- 'https://thegymter.net/2024/02/18/2024-cairo-world-cup-mens-results/'
meet_name <- "cairo"

# Read the HTML content from the webpage
webpage <- read_html(url)

# Extract the table data (assume the table is the first table on the webpage)
table <- webpage %>% html_table(fill = TRUE)

# check which table corresponds to which event
table
headers <- webpage %>% html_elements("p") %>% html_text2()
headers <- headers[2:(length(headers)-2)] %>% 
  str_replace_all(c(
    "Floor" = "FX",
    "Pommels" = "PH",
    "Rings" = "SR",
    "Vault" = "VT",
    "P-Bars" = "PB",
    "High Bar" = "HB",
    "Final Results" = "F",
    "Qualification Results" = "Q"
  ))

i = 1
for (header in headers) {
  table[[i]] <- table[[i]][-1, -1] %>% 
    mutate( EVENT = header) %>% 
    rename( NAME = X2, TEAM = X3, D = X4, Score = X7 ) %>% 
    select(EVENT, NAME, TEAM, D, Score)
  i = i + 1
}

results <- bind_rows(table) %>% 
  separate(EVENT, c("EVENT", "ROUND")) %>% 
  mutate(D = na_if(D, "DNS"), Score = na_if(Score, "DNS")) %>% 
  pivot_wider(
    id_cols = c(ROUND, NAME, TEAM),
    names_from = EVENT,
    values_from = c(D, Score), 
    values_fn = max # two vaults are annoying to keep track of
    ) %>% 
  rename(FX = Score_FX, PH = Score_PH, SR = Score_SR, VT = Score_VT, PB = Score_PB, HB = Score_HB,
    FX_D = D_FX, PH_D = D_PH, SR_D = D_SR, VT_D = D_VT, HB_D = D_HB) %>% 
  mutate(
    MEET = meet_name,
    TEAM = countrycode(TEAM, origin = 'country.name', destination = 'iso3c')
    )

write_csv(results, file = "./results/cairo_cup-2024.csv")
