library(tidyverse)
library(glue)

setwd("C:/Users/Subhayan/Work/visual-cues-project/")

data_folder <- "C:/Users/Subhayan/Dropbox/Current projects/Visual Cues/pilot study 2/"

mturk_dat <- read_csv(glue("{data_folder}/mturk-Batch_5237375_batch_results.csv"))
qualtrics_dat <- read_csv(glue("{data_folder}/pilot study 2_July 6, 2024_11.06.csv"))

# Age:(Q7)
# 1 - 18- 24
# 2 - 25-34
# 3 - 35-44
# 4 - 45-54
# 5 - 65-74
# 6 - 75+

# Gender: (Q8)
# 1 - Male
# 2 - Female
# 3 - Other

# Party ID: (Q15)
# 1 - Democrat
# 2 - Republican
# 3 - Neither

# Q16 is MTurk worker ID

# Q17 should be 21 or 12+9

male <- qualtrics_dat |>
  filter(Q8 == 1) |>
  pull(Q16)

female <- qualtrics_dat |>
  filter(Q8 == 2) |>
  pull(Q16)

democrats <- qualtrics_dat |>
  filter(Q15 == 1) |>
  pull(Q16)

republicans <- qualtrics_dat |>
  filter(Q15 == 2) |>
  pull(Q16) 

passed_attn_chk <- qualtrics_dat |>
  filter(`Q3#1_26` == 1 & `Q3#2_26` == 3) |>
  pull(Q16)

all_workers <- mturk_dat |>
  pull(WorkerId)

correct_survey_code <- mturk_dat |>
  filter(Answer.surveycode %in% c("21", "12+9")) |>
  pull(WorkerId)

not_too_fast <- mturk_dat |>
  filter(WorkTimeInSeconds >= 120) |>
  pull(WorkerId)

duplicate_IPs <- qualtrics_dat |>
  group_by(IPAddress) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  filter(n > 1) |>
  pull(IPAddress)

duplicate_IP_workers <- qualtrics_dat |>
  filter(IPAddress %in% duplicate_IPs,
         !is.na(Q16)) |> # Q11 is workerID
  pull(Q16)

# preprocess data by keeping only responses from mturkers and removing Don't Know/Can't Say responses

qualtrics_dat <- qualtrics_dat |>
  filter(Q16 %in% all_workers) |>
  
  # replace the value 6 (Don't Know/Can't Say) in columns starting with "Q4_" with NA
  mutate(across(starts_with("Q4_"), ~ replace(., . == 6, NA)))  |>

  # replace the value 6 (Don't Know/Can't Say)in columns starting with "Q6_" with NA
  mutate(across(starts_with("Q6_"), ~ replace(., . == 6, NA)))

# all responses

all_means <- qualtrics_dat |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "All") |>
  select(Responses, everything())

all_annotations <- qualtrics_dat |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "All") |>
  select(Responses, everything())

# responses of only those who passed attn check

attnchkpass_means <- qualtrics_dat |>
  filter(Q16 %in% passed_attn_chk) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check") |>
  select(Responses, everything())

attnchkpass_annotations <- qualtrics_dat |>
  filter(Q16 %in% passed_attn_chk) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check") |>
  select(Responses, everything())

# responses of only those who passed attn check and gave the right survey code

attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q16 %in% intersect(passed_attn_chk, correct_survey_code)) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code") |>
  select(Responses, everything())

attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q16 %in% intersect(passed_attn_chk, correct_survey_code)) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only those who passed attn check and gave the right survey code and were not too fast

attncheckpass_correctsurveycode_nottoofast_means <- qualtrics_dat |>
  filter(Q16 %in% intersect(passed_attn_chk, intersect(correct_survey_code, not_too_fast))) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Too Fast") |>
  select(Responses, everything())

attncheckpass_correctsurveycode_nottoofast_annotations <- qualtrics_dat |>
  filter(Q16 %in% intersect(passed_attn_chk, intersect(correct_survey_code, not_too_fast))) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Too Fast") |>
  select(Responses, everything())

# responses of only those who passed attn check and gave the right survey code and were not duplicate IP workers

attncheckpass_correctsurveycode_nottoofast_notduplicateIPs_means <- qualtrics_dat |>
  filter(Q16 %in% intersect(passed_attn_chk, intersect(correct_survey_code, not_too_fast))) |>
  filter(!Q16 %in% duplicate_IP_workers) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Too Fast and Non-duplicate IPs") |>
  select(Responses, everything())

attncheckpass_correctsurveycode_nottoofast_notduplicateIPs_annotations <- qualtrics_dat |>
  filter(Q16 %in% intersect(passed_attn_chk, intersect(correct_survey_code, not_too_fast))) |>
  filter(!Q16 %in% duplicate_IP_workers) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Extremely Fast and Non-duplicate IPs") |>
  select(Responses, everything())

# responses of only males who passed attn check and gave the right survey code

male_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q16 %in% intersect(male, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Male Passed Attention Check and Correct Code") |>
  select(Responses, everything())

male_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q16 %in% intersect(male, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Male Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only females who passed attn check and gave the right survey code

female_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q16 %in% intersect(female, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Female Passed Attention Check and Correct Code") |>
  select(Responses, everything())

female_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q16 %in% intersect(female, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Female Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only democrats who passed attn check and gave the right survey code

democrats_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q16 %in% intersect(democrats, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Democrat Passed Attention Check and Correct Code") |>
  select(Responses, everything())

democrats_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q16 %in% intersect(democrats, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Democrat Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only republicans who passed attn check and gave the right survey code

republicans_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q16 %in% intersect(republicans, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Republican Passed Attention Check and Correct Code") |>
  select(Responses, everything())

republicans_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q16 %in% intersect(republicans, intersect(passed_attn_chk, correct_survey_code))) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Republican Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# aggregated means

bind_rows(
  all_means,
  attnchkpass_means,
  attncheckpass_correctsurveycode_means,
  attncheckpass_correctsurveycode_nottoofast_means,
  attncheckpass_correctsurveycode_nottoofast_notduplicateIPs_means,
  male_attncheckpass_correctsurveycode_means,
  female_attncheckpass_correctsurveycode_means,
  democrats_attncheckpass_correctsurveycode_means,
  republicans_attncheckpass_correctsurveycode_means
) |>
  pivot_longer(-1) |>
  pivot_wider(names_from = 1, values_from = value) |>
  write_csv("results/pilot2_means.csv")

# annotatation counts

bind_rows(
  all_annotations,
  attnchkpass_annotations,
  attncheckpass_correctsurveycode_annotations,
  attncheckpass_correctsurveycode_nottoofast_annotations,
  attncheckpass_correctsurveycode_nottoofast_notduplicateIPs_annotations,
  male_attncheckpass_correctsurveycode_annotations,
  female_attncheckpass_correctsurveycode_annotations,
  democrats_attncheckpass_correctsurveycode_annotations,
  republicans_attncheckpass_correctsurveycode_annotations
) |>
  pivot_longer(-1) |>
  pivot_wider(names_from = 1, values_from = value) |>
  write_csv("results/pilot2_annotation-counts.csv")
