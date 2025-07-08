library(tidyverse)

setwd("C:/Users/Subhayan/Work/visual-cues-project/")

# given the number of elites, facial expressions, and news types
# and assuming the mturk has a Democrat bias
# how many participants do we need so that every condition has at least X participants

# five elites
elites <- paste0("E", 1:5) 

# three levels of facial expression (positive, negative, no face)
face_exp <- paste0("F", 1:3)

# 3 levels of news (factual, positive opinion, negative opinion)
news_type <- paste0("N", 1:3) 

# each participant sees 3 elites
elites_per_person <- 3

# assuming mturk labour pool is 80% liberal (to be safe)
prolific_dem_bias <- 0.5

# anticipated number of participants
n_participants <- 750

set.seed(9)

all_participant_conditions <- tibble(
  p_id = numeric(),
  p_partisanship = character(),
  elite = character(),
  condition = character()
)

for(p in 1:n_participants) {
  p_e_tracker <- NULL
  for(e in 1:elites_per_person) {
    
    p_party <- sample(c("D", "R"),
                      size = 1,
                      prob = c(prolific_dem_bias, 1-prolific_dem_bias))
    
    p_elite <- sample(elites[!elites %in% p_e_tracker], 1)
    
    p_e_tracker <- c(p_e_tracker, p_elite)
      
    p_face_exp <- sample(face_exp, 1)
    p_news_type <- sample(news_type, 1)
    
    p_condition <- paste(p_face_exp, p_news_type)
    
    all_participant_conditions <- all_participant_conditions %>%
      add_row(p_id = p, 
              p_partisanship = p_party,
              elite = p_elite,
              condition = p_condition)
  }
}

condition_counts <- all_participant_conditions |>
  count(condition, p_partisanship) |>
  pivot_wider(names_from = p_partisanship, values_from = n) |>
  mutate(total = D + R) |>
  select(condition, total, D, R)

# check the condition with the minimum number of Republicans and adjust
# n_participants accordingly

condition_counts |>
  filter(R == min(R))
