library(tidyverse)
library(here)

NLSY97 <- read_csv(here("Data/NLSY97_raw.csv"))

head(NLSY97)

NLSY97 <- NLSY97 %>% 
  mutate(across(starts_with("E"), ~case_when(
  .x < 0 ~ NA_real_,
  .x == 99 ~ NA_real_,
  TRUE ~ .x
  ))) %>% 
  filter(if_any(starts_with("E"), ~!is.na(.x))) %>% 
  rowwise() %>% 
  mutate(total_arrests = sum(c_across(starts_with("E")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gender = if_else(R0536300 == 1, "Male", "Female")) %>% 
  mutate(race = case_when(
    R1482600 == 1 ~ "Black",
    R1482600 == 2 ~ "Hispanic",
    R1482600 == 3 ~ "Mixed Race (Non-Hispanic)",
    R1482600 == 4 ~ "Non-Black / Non-Hispanic",
  )) %>% 
  select(race, gender, total_arrests)
  
write_csv(NLSY97,"Data/NLSY97_clean.csv")

