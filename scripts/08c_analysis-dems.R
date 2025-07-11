# sub-group analysis for democrats (partisanship < 3)

library(tidyverse)
library(glue)
library(lfe)
library(lubridate)
library(modelsummary)
library(pandoc)

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
    paste(Q96, Q97) == "Democrat NA" ~ 1,
    paste(Q96, Q97) == "Independent Democrat" ~ 2,
    paste(Q96, Q97) == "Independent Prefer not to lean in either direction" ~ 3,
    paste(Q96, Q97) == "Independent Republican" ~ 4,
    paste(Q96, Q97) == "Republican NA" ~ 5
  )) |>
  select(-c(Q96, Q97)) |>
  mutate(PID = row_number()) |>
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

# experimental data
exp_dat  <- dat |>
  select(PID, contains("_"))

# make it longer
exp_dat_long <- exp_dat |>
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = contains("_"),
               names_to = "Q",
               values_to = "Response") |>
  filter(!is.na(Response)) |>
  mutate(Response = case_when(
    Response == "Strongly distrust" ~ "1",
    Response == "Distrust" ~ "2",
    Response == "Neutral" ~ "3",
    Response == "Trust" ~ "4",
    Response == "Strongly trust" ~ "5",
    .default = Response
  )) |>
  mutate(Response = as.numeric(Response))

# read question condition mapping

q_details <- read_csv("auxiliary/question-condition-mapping.csv")

# merge with exp_dat_long

exp_dat_long <- exp_dat_long |>
  left_join(q_details, by = c("Q" = "Question"))

# trust tibble

trust_tibble <- exp_dat_long |>
  filter(DV == "trust") |>
  mutate(PID = as.numeric(PID)) |>
  select(PID, Elite, News_Type, Facial_Cue, Response) |>
  inner_join(pid_dat, by = "PID") |>
  filter(Partisanship < 3) |>
  arrange(PID)

# feeling tibble

feeling_tibble <- exp_dat_long |>
  filter(DV == "feeling") |>
  mutate(PID = as.numeric(PID)) |>
  select(PID, Elite, News_Type, Facial_Cue, Response) |>
  inner_join(pid_dat, by = "PID")|>
  filter(Partisanship < 3) |>
  arrange(PID)

# all trust models
# trust models

# Ensure Facial_Cue is a factor and set "none" as the reference level
trust_tibble <- trust_tibble %>%
  mutate(Facial_Cue = relevel(factor(Facial_Cue), ref = "none"),
         News_Type = relevel(factor(News_Type), ref = "factual"))

# H1. Visual cues conveyed through facial expressions of elites influence the extent to which people trust headlines. Positive facial expressions increase the level of trust that people put in the headline.

m_trust_h1 <- lfe::felm(Response ~ # DV
                          Facial_Cue | # IVs
                          Batch + PID + Elite | # fixed effects
                          0 | # no instruments
                          + Batch, # cluster
                        data = trust_tibble)


m_trust_h1_s <- lfe::felm(scale_variable(Response) ~ # DV
                            Facial_Cue | # IVs
                            Batch + PID + Elite | # fixed effects
                            0 | # no instruments
                            + Batch, # cluster
                          data = trust_tibble)


# H2. The effect of visual cues on trust is conditioned on whether the news headline pertains to an in-party political elite, out-party political elite or a non-political elite.

trust_tibble <- trust_tibble |>
  filter(Partisanship != 3) |> # remove neutrals
  mutate(Elite_Party = case_when(
    Elite == "Biden" ~ "D",
    Elite == "Musk" ~ "R",
    Elite == "Taylor" ~ "D",
    Elite == "Trump" ~ "R",
    Elite == "Kardashian" ~ NA_character_
  )) |>
  mutate(P_Party = case_when(
    Partisanship %in% c(1, 2) ~ "D",
    Partisanship %in% c(4, 5) ~ "R"
  )) |>
  mutate(Elite_InParty = Elite_Party == P_Party)

# remove elite from FE  because matrix is rank deficient
m_trust_h2 <- lfe::felm(Response ~ # DV
                          Facial_Cue * Elite_InParty | # IVs
                          Batch + PID | # fixed effects
                          0 | # no instruments
                          + Batch, # cluster
                        data = trust_tibble)

m_trust_h2_s <- lfe::felm(scale_variable(Response) ~ # DV
                            Facial_Cue * Elite_InParty | # IVs
                            Batch + PID | # fixed effects
                            0 | # no instruments
                            + Batch, # cluster
                          data = trust_tibble)

# H3a. For political elites, the effect of visual cues on trust is conditioned on whether the headline is biased towards the in-party, biased towards the out-party or if it is unbiased and factual.

trust_tibble <- trust_tibble |>
  mutate(News_Bias = case_when(
    Elite_InParty & News_Type == "positive" ~ "BiasedTowardsInParty",
    Elite_InParty & News_Type == "negative" ~ "BiasedAgainstInParty",
    !Elite_InParty & News_Type == "positive" ~ "BiasedTowardsOutParty",
    !Elite_InParty & News_Type == "negative" ~ "BiasedAgainstOutParty",
    News_Type == "factual" ~ "Factual"
  )) |>
  mutate(News_Bias = relevel(factor(News_Bias), ref = "Factual"))

m_trust_h3a_1 <- lfe::felm(Response ~ # DV
                             Facial_Cue * News_Bias | # IVs
                             Batch + PID + Elite | # fixed effects
                             0 | # no instruments
                             + Batch, # cluster
                           data = trust_tibble)

m_trust_h3a_1_s <- lfe::felm(scale_variable(Response) ~ # DV
                               Facial_Cue * News_Bias | # IVs
                               Batch + PID + Elite | # fixed effects
                               0 | # no instruments
                               + Batch, # cluster
                             data = trust_tibble)

trust_tibble <- trust_tibble |>
  mutate(News_Bias2 = case_when(
    Elite_InParty & News_Type == "positive" ~ "BiasedTowardsInParty",
    Elite_InParty & News_Type == "negative" ~ "BiasedTowardsOutParty",
    !Elite_InParty & News_Type == "positive" ~ "BiasedTowardsOutParty",
    !Elite_InParty & News_Type == "negative" ~ "BiasedTowardsInParty",
    News_Type == "factual" ~ "Factual"
  )) |>
  mutate(News_Bias2 = relevel(factor(News_Bias2), ref = "Factual"))

m_trust_h3a_2 <- lfe::felm(Response ~ # DV
                             Facial_Cue * News_Bias2 | # IVs
                             Batch + PID + Elite | # fixed effects
                             0 | # no instruments
                             + Batch, # cluster
                           data = trust_tibble)

m_trust_h3a_2_s <- lfe::felm(scale_variable(Response) ~ # DV
                               Facial_Cue * News_Bias2 | # IVs
                               Batch + PID + Elite | # fixed effects
                               0 | # no instruments
                               + Batch, # cluster
                             data = trust_tibble)

# H3b. For non-political elites, the effect of visual cues on trust is conditioned on whether the headline is biased in favour of the elite, biased against the elite or if it is unbiased and factual.

# doesn't work
# non_political_elites <- c("Kardashian")
# 
# m_trust_h3b <- lfe::felm(Response ~ # DV
#                            Facial_Cue * News_Type | # IVs
#                            Batch + PID + Elite | # fixed effects
#                            0 | # no instruments
#                            + Batch, # cluster
#                          data = trust_tibble[trust_tibble$Elite %in% non_political_elites,])
# 
# summary(m_trust_h3b)


# feeling models

feeling_tibble <- feeling_tibble %>%
  mutate(Facial_Cue = relevel(factor(Facial_Cue), ref = "none"),
         News_Type = relevel(factor(News_Type), ref = "factual"))

# H4. Visual cues conveyed through facial expressions influence how people feel  (as measured by a feeling thermometer)  about the person the news is about. Positive facial expressions increase feelings of warmth towards the person.

m_feeling_h4 <- lfe::felm(Response ~ # DV
                            Facial_Cue | # IVs
                            Batch + PID + Elite | # fixed effects
                            0 | # no instruments
                            + Batch, # cluster
                          data = feeling_tibble)

m_feeling_h4_s <- lfe::felm(scale_variable(Response) ~ # DV
                              Facial_Cue | # IVs
                              Batch + PID + Elite | # fixed effects
                              0 | # no instruments
                              + Batch, # cluster
                            data = feeling_tibble)

# H5.  The effect of visual cues on feelings (as measured by a feeling thermometer) is conditioned on whether the news headline pertains an in-party political elite, out-party political elite or a non-political elite.

feeling_tibble <- feeling_tibble |>
  filter(Partisanship != 3) |> # remove neutrals
  mutate(Elite_Party = case_when(
    Elite == "Biden" ~ "D",
    Elite == "Musk" ~ "R",
    Elite == "Taylor" ~ "D",
    Elite == "Trump" ~ "R",
    Elite == "Kardashian" ~ NA_character_
  )) |>
  mutate(P_Party = case_when(
    Partisanship %in% c(1, 2) ~ "D",
    Partisanship %in% c(4, 5) ~ "R"
  )) |>
  mutate(Elite_InParty = Elite_Party == P_Party)


# Elite fixed effect removed otherwise matrix is rank deficient
m_feeling_h5 <- lfe::felm(Response ~ # DV
                            Facial_Cue * Elite_InParty | # IVs
                            Batch + PID  | # fixed effects
                            0 | # no instruments
                            + Batch, # cluster
                          data = feeling_tibble)

m_feeling_h5_s <- lfe::felm(scale_variable(Response) ~ # DV
                              Facial_Cue * Elite_InParty | # IVs
                              Batch + PID | # fixed effects
                              0 | # no instruments
                              + Batch, # cluster
                            data = feeling_tibble)


# H6a. For political elites, the effect of visual cues on feelings (as measured by a feeling thermometer) is conditioned on whether the headline is biased towards the in-party, biased towards the out-party or if it is unbiased and factual.

feeling_tibble <- feeling_tibble |>
  mutate(News_Bias = case_when(
    Elite_InParty & News_Type == "positive" ~ "BiasedTowardsInParty",
    Elite_InParty & News_Type == "negative" ~ "BiasedAgainstInParty",
    !Elite_InParty & News_Type == "positive" ~ "BiasedTowardsOutParty",
    !Elite_InParty & News_Type == "negative" ~ "BiasedAgainstOutParty",
    News_Type == "factual" ~ "Factual"
  )) |>
  mutate(News_Bias = relevel(factor(News_Bias), ref = "Factual"))

m_feeling_h6a_1 <- lfe::felm(Response ~ # DV
                               Facial_Cue * News_Bias | # IVs
                               Batch + PID + Elite | # fixed effects
                               0 | # no instruments
                               + Batch, # cluster
                             data = feeling_tibble)

m_feeling_h6a_1_s <- lfe::felm(scale_variable(Response) ~ # DV
                                 Facial_Cue * News_Bias | # IVs
                                 Batch + PID + Elite | # fixed effects
                                 0 | # no instruments
                                 + Batch, # cluster
                               data = feeling_tibble)

feeling_tibble <- feeling_tibble |>
  mutate(News_Bias2 = case_when(
    Elite_InParty & News_Type == "positive" ~ "BiasedTowardsInParty",
    Elite_InParty & News_Type == "negative" ~ "BiasedTowardsOutParty",
    !Elite_InParty & News_Type == "positive" ~ "BiasedTowardsOutParty",
    !Elite_InParty & News_Type == "negative" ~ "BiasedTowardsInParty",
    News_Type == "factual" ~ "Factual"
  )) |>
  mutate(News_Bias2 = relevel(factor(News_Bias2), ref = "Factual"))

m_feeling_h6a_2 <- lfe::felm(Response ~ # DV
                               Facial_Cue * News_Bias2 | # IVs
                               Batch + PID + Elite | # fixed effects
                               0 | # no instruments
                               + Batch, # cluster
                             data = feeling_tibble)

m_feeling_h6a_2s <- lfe::felm(scale_variable(Response) ~ # DV
                                Facial_Cue * News_Bias2 | # IVs
                                Batch + PID + Elite | # fixed effects
                                0 | # no instruments
                                + Batch, # cluster
                              data = feeling_tibble)

# H6b. For non-political elites, the effect of visual cues on feelings (as measured by a feeling thermometer) is conditioned on whether the headline is biased in favour of the elite, biased against the elite or if it is unbiased and factual. 

# doesn't work
# 
# non_political_elites <- c("Kardashian")
# 
# m_feeling_h6b <- lfe::felm(Response ~ # DV
#                            Facial_Cue * News_Type | # IVs
#                            Batch + PID | # fixed effects
#                            0 | # no instruments
#                            + Batch, # cluster
#                          data = feeling_tibble[feeling_tibble$Elite %in% non_political_elites,])
# 
# summary(m_feeling_h6b)

modelsummary(list("Trust H1" = m_trust_h1_s,
                  "Feel H2" = m_feeling_h4_s,
                  "Trust H3a" = m_trust_h2_s,
                  "Feel H3b" = m_feeling_h5_s,
                  "Trust H4a" = m_trust_h3a_1_s,
                  "Feel H4b" = m_feeling_h6a_1_s),
             stars = c('*' = .05, '**' = .01, '***' = .001),
             estimate  = c("{estimate}{stars} ({std.error})"),
             statistic = NULL,
             coef_omit = "Intercept",
             notes = list('p-values: * < 0.05 ** < 0.01 *** < 0.001'),
             output = "results/model_outputs/04_democrats.png")

modelsummary(list("Trust H1" = m_trust_h1_s,
                  "Feel H2" = m_feeling_h4_s,
                  "Trust H3a" = m_trust_h2_s,
                  "Feel H3b" = m_feeling_h5_s,
                  "Trust H4a" = m_trust_h3a_1_s,
                  "Feel H4b" = m_feeling_h6a_1_s),
             stars = c('*' = .05, '**' = .01, '***' = .001),
             estimate  = c("{estimate}{stars} ({std.error})"),
             statistic = NULL,
             coef_omit = "Intercept",
             notes = list('p-values: * < 0.05 ** < 0.01 *** < 0.001'),
             output = "results/model_outputs/04_democrats.docx")


