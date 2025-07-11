# demographic stats

library(tidyverse)
library(glue)
library(lfe)
library(lubridate)
library(modelsummary)
library(patchwork)

scale_variable <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

dat <- read_csv("data/visual-cues_anonymised-data.csv")

glimpse(dat)

# create a column batch based on StartDate

dat <- dat |>
  mutate(Batch = case_when(
    as.Date(StartDate) == "2024-10-16" ~ 1,
    as.Date(StartDate) == "2024-10-24" ~ 2,
    as.Date(StartDate) == "2024-10-30" ~ 3
  ))

# experimental response columns
# contain underscores
# but not "_A" (which are attention checks)
# also remove "PROFLICI_PID" column
# also remove"Q93_1" column (attention check)

exp_cols <- dat |>
  select(contains("_")) |>
  select(!contains("_A")) |>
  select(-c(PROLIFIC_PID, Q93_1)) |>
  names()

# demographic columns

other_relevant_cols <- dat |>
  select(Batch, Duration, Q94:Q99) |>
  names()

reqd_cols <- c(other_relevant_cols, exp_cols)

dat <- dat |>
  select(all_of(reqd_cols)) |>
  rename(Age = Q94,
         Gender = Q95,
         Familiarity = Q98,
         Race = Q99)

dat <- dat |>
  mutate(Partisanship = case_when(
    paste(Q96, Q97) == "Democrat NA" ~ "Strong Dem",
    paste(Q96, Q97) == "Independent Democrat" ~ "Weak Dem",
    paste(Q96, Q97) == "Independent Prefer not to lean in either direction" ~ "Independent",
    paste(Q96, Q97) == "Independent Republican" ~ "Weak Rep",
    paste(Q96, Q97) == "Republican NA" ~ "Strong Rep"
  )) |>
  select(-c(Q96, Q97)) |>
  mutate(PID = row_number()) |>
  mutate(Race = case_when(
    Race == "Black or African American" ~ "Black",
    Race == "Mixed Race" ~ "Mixed",
    Race == "American Indian or Alaska Native" ~ "Native American",
    Race == "Native Hawaiian or Other Pacific Islander" ~ "Pacific Islander",
    .default = Race
  )) |>
  mutate(Partisanship = factor(Partisanship, 
                               levels = c("Strong Dem", "Weak Dem", "Independent", "Weak Rep", "Strong Rep"))) |>
  select(PID, Batch, Duration, Age, Gender, Familiarity, Partisanship, Race, everything())

# PID data
pid_dat <- dat |>
  select(PID,
         Batch,
         Duration,
         Age,
         Gender,
         Familiarity,
         Partisanship,
         Race)

# distribution of time taken to complete survey
duration_plot <- ggplot(pid_dat, aes(Duration/60)) +
  geom_histogram(bins = 30, fill = "grey", color = "black") +
  labs(title = "Distribution of time taken \nto complete survey",
       x = "Duration (in minutes)",
       y = "Frequency") +
  # add a vertical line showing the median
  geom_vline(aes(xintercept = median(Duration/60)), color = "red", linetype = "dashed") +
  theme_minimal()

pid_dat |>
  summarise(median_duration = median(Duration/60),
            mean_duration = mean(Duration/60))

# median duration is 4 minutes 34 seconds
# medan duration is 5 minutes 51 seconds

# bar-graph showing age distribution
age_plot <- ggplot(pid_dat, aes(Age)) +
  geom_bar(fill = "grey", color = "black") +
  labs(title = "Age distribution",
       x = "Age",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# proportions of different ages
pid_dat |>
  count(Age) |>
  mutate(prop = 100*n/sum(n)) |>
  arrange(desc(prop))

# number of participants in each batch
pid_dat |>
  count(Batch) |>
  mutate(prop = 100*n/sum(n))

# bar-graph showing gender distribution
gender_plot <- ggplot(pid_dat, aes(Gender)) +
  geom_bar(fill = "grey", color = "black") +
  labs(title = "Gender distribution",
       x = "Gender",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# proportions of different gender
pid_dat |>
  count(Gender) |>
  mutate(prop = 100*n/sum(n)) |>
  arrange(desc(prop))

# partisanship
partisanship_plot <- ggplot(pid_dat, aes(Partisanship, fill = as.factor(Partisanship))) +
  geom_bar(fill = "grey", color = "black") +
  labs(title = "Partisanship distribution",
       x = "Partisanship",
       y = "Frequency",
       fill = "Partisanship") +
  theme_minimal() +
  guides(fill="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# familiarity
familiarity_plot <- ggplot(pid_dat, aes(Familiarity)) +
  geom_bar(fill = "grey", color = "black") +
  labs(title = "Familiarity distribution",
       x = "Familiarity with politics",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# race
race_plot <- ggplot(pid_dat, aes(Race)) +
  geom_bar(fill = "grey", color = "black") +
  labs(title = "Race distribution",
       x = "Race",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

aligned_plot <-
  age_plot + gender_plot + race_plot +
  partisanship_plot + familiarity_plot + duration_plot +
  plot_layout(nrow = 3, guides = "collect", axis_titles = "collect") + 
  plot_annotation(tag_levels = "A")  # Adds labels A, B, C, D, E

ggsave(aligned_plot,
       filename = "results/figures/descriptives.svg",
       width = 8, height = 8, units = "in")


# Age distribution grouped by gender
# exclude Other
age_gender_plot <- ggplot(pid_dat[pid_dat$Gender != "Other",], aes(x = Age, fill = Gender)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Male" = "darkseagreen",
                               "Female" = "rosybrown")) +
  labs(title = "Age grouped by\nGender",
       x = "Age",
       y = "Frequency",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

# group partisanship into Democrat and Republican
# exclude centrists
pid_dat2 <- pid_dat |>
  mutate(Partisanship2 = case_when(
    Partisanship %in% c("Strong Dem", "Weak Dem") ~ "Democrat",
    Partisanship %in% c("Strong Rep", "Weak Rep") ~ "Republican"
  )) |>
  filter(Partisanship != "Independent")

# Age distribution grouped by partisanship
age_partisanship_plot <- ggplot(pid_dat2, aes(x = Age, fill = factor(Partisanship2))) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Democrat" = "#56B4E9",
                               "Republican" = "salmon")) + 
  labs(title = "Age grouped by\nPartisanship",
       x = "Age",
       y = "Frequency",
       fill = "Partisanship") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

# Gender grouped by partisanship
# Partisanship distribution grouped by gender
partisanship_gender_plot <- ggplot(pid_dat2, aes(x = Gender, fill = Partisanship2)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Democrat" = "#56B4E9",
                               "Republican" = "salmon")) + 
  labs(title = "Gender grouped by\nPartisanship",
       x = "Gender",
       y = "Frequency",
       fill = "Partisanship") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

partisanship_race_plot <- ggplot(pid_dat2, aes(x = Race, fill = Partisanship2)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Democrat" = "#56B4E9",
                               "Republican" = "salmon")) + 
  labs(title = "Race grouped by\nPartisanship",
       x = "Race",
       y = "Frequency",
       fill = "Partisanship") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

race_age_plot <- ggplot(pid_dat2, aes(x = Race, fill = Age)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("18-24" = "grey91",
                               "25-34" = "grey77",
                               "35-44" = "grey63",
                               "45-54" = "grey49",
                               "55-64" = "grey35",
                               "65-74" = "grey21",
                               "75 or older" = "grey7")) + 
  labs(title = "Race grouped by\nAge",
       x = "Race",
       y = "Frequency",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

race_gender_plot <- ggplot(pid_dat2[pid_dat2$Gender != "Other",], aes(x = Race, fill = Gender)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("Male" = "darkseagreen",
                               "Female" = "rosybrown")) + 
  labs(title = "Race grouped by\nGender",
       x = "Race",
       y = "Frequency",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")


aligned_plot2 <- age_gender_plot + age_partisanship_plot + race_gender_plot + partisanship_race_plot +
partisanship_gender_plot + race_age_plot +
  guide_area() +
  plot_layout(nrow = 4, guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave(aligned_plot2,
       filename = "results/figures/demographic_breakdown.svg",
       width = 6, height = 12, units = "in")
