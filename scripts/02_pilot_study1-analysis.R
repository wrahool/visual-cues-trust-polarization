library(tidyverse)
library(glue)

setwd("C:/Users/Subhayan/Work/visual-cues-project/")

data_folder <- "C:/Users/Subhayan/Dropbox/Current projects/Visual Cues/pilot study 1/"

mturk_dat <- read_csv(glue("{data_folder}/mturk-Batch_5226248_batch_results.csv"))
qualtrics_dat <- read_csv(glue("{data_folder}/pilot study1_May 27-2024_08_17.csv"))

# Age:(Q8)
# 1 - 18- 24
# 2 - 25-34
# 3 - 35-44
# 4 - 45-54
# 5 - 65-74
# 6 - 75+

# Gender: (Q9)
# 1 - Male
# 2 - Female
# 3 - Other

# Party ID: (Q10)
# 1 - Democrat
# 2 - Republican
# 3 - Neither

male <- qualtrics_dat |>
  filter(Q9 == 1) |>
  pull(Q11)

female <- qualtrics_dat |>
  filter(Q9 == 2) |>
  pull(Q11)

democrats <- qualtrics_dat |>
  filter(Q10 == 1) |>
  pull(Q11)

republicans <- qualtrics_dat |>
  filter(Q10 == 2) |>
  pull(Q11) 

failed_attn_chk <- mturk_dat |>
  filter(AttentionChk == "FAIL") |>
  pull(WorkerId)

wrong_survey_code <- mturk_dat |>
  filter(!Answer.surveycode %in% c("22", "17+5")) |>
  pull(WorkerId)

too_fast <- mturk_dat |>
  filter(WorkTimeInMins < 5) |>
  pull(WorkerId)

extremely_fast <- mturk_dat |>
  filter(WorkTimeInMins < 3) |>
  pull(WorkerId)

duplicate_IPs <- qualtrics_dat |>
  group_by(IPAddress) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  filter(n > 1) |>
  pull(IPAddress)

duplicate_IP_workers <- qualtrics_dat |>
  filter(IPAddress %in% duplicate_IPs,
         !is.na(Q11)) |> # Q11 is workerID
  pull(Q11)

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
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% failed_attn_chk) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check") |>
  select(Responses, everything())

attnchkpass_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% failed_attn_chk) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check") |>
  select(Responses, everything())

# responses of only those who passed attn check and gave the right survey code

attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code") |>
  select(Responses, everything())

attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only those who passed attn check and gave the right survey code and were not too fast

attncheckpass_correctsurveycode_nottoofast_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code, too_fast)) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Too Fast") |>
  select(Responses, everything())

attncheckpass_correctsurveycode_nottoofast_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code, too_fast)) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Too Fast") |>
  select(Responses, everything())

# responses of only those who passed attn check and gave the right survey code and were not extremely fast

attncheckpass_correctsurveycode_notextremelyfast_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code, too_fast, extremely_fast)) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Extremely Fast") |>
  select(Responses, everything())

attncheckpass_correctsurveycode_notextremelyfast_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code, too_fast, extremely_fast)) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Extremely Fast") |>
  select(Responses, everything())

# responses of only those who passed attn check and gave the right survey code and were not extremely fast

attncheckpass_correctsurveycode_notextremelyfast_notduplicateIPs_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code, too_fast, extremely_fast, duplicate_IP_workers)) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Extremely Fast and Non-duplicate IPs") |>
  select(Responses, everything())

attncheckpass_correctsurveycode_notextremelyfast_notduplicateIPs_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code, too_fast, extremely_fast, duplicate_IP_workers)) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Passed Attention Check and Correct Code and Not Extremely Fast and Non-duplicate IPs") |>
  select(Responses, everything())

# responses of only males who passed attn check and gave the right survey code

male_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% male) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Male Passed Attention Check and Correct Code") |>
  select(Responses, everything())

male_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% male) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Male Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only females who passed attn check and gave the right survey code

female_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% female) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Female Passed Attention Check and Correct Code") |>
  select(Responses, everything())

female_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% female) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Female Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only democrats who passed attn check and gave the right survey code

democrats_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% democrats) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Democrat Passed Attention Check and Correct Code") |>
  select(Responses, everything())

democrats_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% democrats) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Democrat Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# responses of only republicans who passed attn check and gave the right survey code

republicans_attncheckpass_correctsurveycode_means <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% republicans) |>
  summarise(across(contains("_"), ~mean(.x, na.rm = T))) |>
  mutate("Responses" = "Republican Passed Attention Check and Correct Code") |>
  select(Responses, everything())

republicans_attncheckpass_correctsurveycode_annotations <- qualtrics_dat |>
  filter(Q11 %in% unique(mturk_dat$WorkerId)) |>
  filter(!Q11 %in% c(failed_attn_chk, wrong_survey_code)) |>
  filter(Q11 %in% republicans) |>
  summarise(across(contains("_"), ~mean(sum(!is.na(.))))) |>
  mutate("Responses" = "Republican Passed Attention Check and Correct Code") |>
  select(Responses, everything())

# aggregated means

bind_rows(
  all_means,
  attnchkpass_means,
  attncheckpass_correctsurveycode_means,
  attncheckpass_correctsurveycode_nottoofast_means,
  attncheckpass_correctsurveycode_notextremelyfast_means,
  attncheckpass_correctsurveycode_notextremelyfast_notduplicateIPs_means,
  male_attncheckpass_correctsurveycode_means,
  female_attncheckpass_correctsurveycode_means,
  democrats_attncheckpass_correctsurveycode_means,
  republicans_attncheckpass_correctsurveycode_means
) |>
  pivot_longer(-1) |>
  pivot_wider(names_from = 1, values_from = value) |>
  write_csv("results/pilot1_means-v2.csv")

# annotatation counts

bind_rows(
  all_annotations,
  attnchkpass_annotations,
  attncheckpass_correctsurveycode_annotations,
  attncheckpass_correctsurveycode_nottoofast_annotations,
  attncheckpass_correctsurveycode_notextremelyfast_annotations,
  attncheckpass_correctsurveycode_notextremelyfast_notduplicateIPs_annotations,
  male_attncheckpass_correctsurveycode_annotations,
  female_attncheckpass_correctsurveycode_annotations,
  democrats_attncheckpass_correctsurveycode_annotations,
  republicans_attncheckpass_correctsurveycode_annotations
) |>
  pivot_longer(-1) |>
  pivot_wider(names_from = 1, values_from = value) |>
  write_csv("results/pilot1_annotation-counts-v2.csv")
