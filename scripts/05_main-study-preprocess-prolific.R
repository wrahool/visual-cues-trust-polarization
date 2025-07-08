# pre-process prolific qualtrics files ad create a cleaned file

library(tidyverse)
library(glue)
library(lfe)

setwd("C:/Users/Subhayan/Work/visual-cues-project/")

data_location <- "C:/Users/Subhayan/NUS Dropbox/Subhayan Mukerjee/Research Projects/Visual Cues"

# get all the Prolific export files
# list all files in all subfolders within the data folder that begin with "prolific_export"
all_files <- list.files(glue("{data_location}/main study"),
                        recursive = TRUE, full.names = TRUE) |>
  str_subset("prolific_export")

# concatenate all the available prolific files
message(glue("Available Prolific files: {glue_collapse(all_files, sep = ', ')}"))

batch <- 1
all_prolific_batches <- NULL
for(prolific_file in all_files) {
  
  # add a batch column to the prolific data
  prolific_dat <- read_csv(prolific_file) |>
    mutate(batch = batch)
  
  all_prolific_batches <- all_prolific_batches %>%
    bind_rows(prolific_dat)
  
  batch = batch + 1
}
  
# read latest qualtrics file
qualtrics_file <- glue("{data_location}/main study/prolific/") |>
  list.files(pattern = "*qualtrics.csv",
             recursive = TRUE, full.names = TRUE)
  
# Load data
qualtrics_dat <- read_csv(qualtrics_file)
  
# Worker IDs with survey code = C1APXW4W if batch = 1 and
# survey code = CG3BQ32I if batch = 2 or 3

code_pass <- all_prolific_batches |>
  filter((batch == 1 & `Completion code` == "C1APXW4W") |
         (batch == 2 & `Completion code` == "CG3BQ32I") |
         (batch == 3 & `Completion code` == "CG3BQ32I")) |>
  pull(`Participant id`)
  
# shouldn't be paid
# workers with wrong survey code
wrong_code <- all_prolific_batches |>
  filter((batch == 1 & (is.na(`Completion code`) | `Completion code` != "C1APXW4W")) |
         (batch == 2 & (is.na(`Completion code`) | `Completion code` != "CG3BQ32I")) |
         (batch == 3 & (is.na(`Completion code`) | `Completion code` != "CG3BQ32I"))) |>
  pull(`Participant id`)
  
# check the status of those who entered the wrong survey code
all_prolific_batches |>
  filter(`Participant id` %in% wrong_code) |>
  count(batch, `Completion code`, `Status`)

# reject the workers in wrong_code_status who are awaiting review
wrong_code_rejects <- all_prolific_batches |>
  filter(`Participant id` %in% wrong_code) |>
  filter(`Status` == "AWAITING REVIEW") |>
  select(batch, `Participant id`)

# there are some participants with the correct completion code
# they are in the data because they returned
# the first batch, but went through the second
# they should be approved, but their responses should be removed before analysis

approve_but_remove <- wrong_code_rejects |>
  filter(`Participant id` %in% code_pass) |>
  pull(`Participant id`)

# these are the ones who should really be rejected
wrong_code_rejects <- wrong_code_rejects |>
  filter(!`Participant id` %in% approve_but_remove) |>
  select(batch, `Participant id`)

wrong_code_rejects |>
  write_csv("auxiliary/wrong_code_rejects.csv")

# now we want to know who entered the wrong Prolific ID in qualtrics
# to do this, we will match the PROLIFIC_PID sent by the URL with the
# response to Q100 in qualtrics (which should have been pre-populated, but they can edit)
# we want to keep only those for whom the two columns match
ID_mismatch <- qualtrics_dat |>
  mutate(ID_check = Q100 == `PROLIFIC_PID`) |>
  filter(is.na(ID_check) | ID_check == F) |>
  filter(!str_detect(PROLIFIC_PID, "PROLIFIC_PID")) |>
  pull(PROLIFIC_PID)

# investigate those IDs
all_prolific_batches |>
  filter(`Participant id` %in% ID_mismatch) |>
  select(batch, `Participant id`, Status)
  
# again, one participant who returned the study in wave 1 completed the study in wave 2
# should be paid

qualtrics_dat |> 
  filter(PROLIFIC_PID == "66a3db47903a771878c6e0c6") |>
  select(Q100, PROLIFIC_PID) |>
  mutate(check = Q100 == PROLIFIC_PID)

ID_mismatch <- all_prolific_batches |>
  filter(`Participant id` %in% ID_mismatch) |>
  filter(`Participant id` != "66a3db47903a771878c6e0c6") |>
  select(batch, `Participant id`)

ID_mismatch |>
  write_csv("auxiliary/ID_mismatch.csv")

# workers who failed attn check
attn_fail <- qualtrics_dat |>
  filter(Q93_1 != "Strongly distrust") |>
  pull(Q100)
  
# what batch are they in?
attn_fail <- all_prolific_batches |>
  filter(`Participant id` %in% attn_fail) |>
  select(batch, `Participant id`, Status)

attn_fail |>  count(batch)

attn_fail |>
  select(`Participant id`, batch) |>
  write_csv("auxiliary/attn_fail.csv")

# workers who failed attn check and duration < 300 seconds (prolific rejection criteria)
prolific_attn_fail <- qualtrics_dat |>
  filter(Q93_1 != "Strongly distrust", as.numeric(`Duration (in seconds)`) < 300) |>
  pull(Q100)
  
# what batch are they in?
prolific_attn_fail <- all_prolific_batches |>
  filter(`Participant id` %in% prolific_attn_fail) |>
  select(batch, `Participant id`, Status)

prolific_attn_fail |>  count(batch)

prolific_attn_fail |>
  select(`Participant id`, batch) |>
  write_csv("auxiliary/prolific_attn_fail.csv")

# attention check answers:
attn_chk_answers <- read_csv("auxiliary/attention-checks.csv")
  
attn_chk2_cols <- qualtrics_dat |>
select(Q100, contains("_A"))
 
attn_chk_scores <- attn_chk2_cols |>
  # keep rows with at least one NA to remove useless qualtrics rows 
  filter(if_any(everything(), is.na)) |>
  pivot_longer(cols = contains("_A"),
               names_to = "q",
               values_to = "answer",
              values_drop_na = TRUE) |>
  left_join(attn_chk_answers, by = c("q" = "q")) |>
  mutate(correct_answer = as.numeric(answer == elite)) |>
  group_by(Q100) |>
  summarise(attn_check2_score = sum(correct_answer))

attn_fail2 <- attn_chk_scores |>
  filter(attn_check2_score < 3) |>
  pull(Q100)

# which batch are they in?
all_prolific_batches |>
  filter(`Participant id` %in% attn_fail2) |>
  count(batch)

attn_fail2 <- all_prolific_batches |> 
  filter(`Participant id` %in% attn_fail2) |>
  select(batch, `Participant id`)

attn_fail2 |>
  write_csv("auxiliary/attn_fail2.csv")

# workers who hoarded/reserved HITs
duplicate_workerIDs <- qualtrics_dat |>
  group_by(Q100) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  pull(Q100)

# investigate
all_prolific_batches |> 
  filter(`Participant id` %in% duplicate_workerIDs) |>
  select(batch, `Participant id`, Status)

# another participant who returned the study in wave 1 completed the study in wave 2
# should be paid

duplicates_IDs <- all_prolific_batches |>
  filter(`Participant id` %in% duplicate_workerIDs) |>
  select(`Participant id`, batch)

duplicates_IDs |>
  write_csv("auxiliary/duplicates_IDs.csv")

# too fast, based on Prolific's criteria of duration < 3 standard deviations from the mean
mean_duration <- qualtrics_dat$`Duration (in seconds)` |>
  as.numeric() |>
  mean(na.rm = TRUE)

sd_duration <- qualtrics_dat$`Duration (in seconds)` |>
  as.numeric() |>
  sd(na.rm = TRUE)

too_fast <- qualtrics_dat |>
  filter(`Duration (in seconds)` < mean_duration - 3 * sd_duration) |>
  pull(Q100)

# to be rejected
wrong_code_rejects <- wrong_code_rejects |>
  mutate(Reason = "Wrong Code")

attn_fail |>
  select(-Status) |>
  mutate(Reason = "Failed Attention Check") |>
  mutate(Reason = ifelse(`Participant id` %in% prolific_attn_fail$`Participant id`, "Failed Attention Check and Duration", Reason)) |>
  bind_rows(wrong_code_rejects) |>
  arrange(batch, Reason) |>
  write_csv("auxiliary/prolific_rejects.csv")

# to be removed from study
ID_mismatch
wrong_code_rejects
attn_fail
attn_fail2
duplicates_IDs
too_fast # this is NA

removeIDs <- c(
  ID_mismatch$`Participant id`,
  wrong_code_rejects$`Participant id`,
  attn_fail$`Participant id`,
  attn_fail2$`Participant id`,
  duplicates_IDs$`Participant id`
) |> unique()

final_qualtrics_dat_prolific <- qualtrics_dat |>
  filter(!is.na(as.numeric(`Duration (in seconds)`))) |>
  filter(!Q100 %in% removeIDs) |>
  filter(!is.na(Q100)) |>
  mutate(Duration = as.numeric(`Duration (in seconds)`))

# test that all have passed attention check  
final_qualtrics_dat_prolific |>
  count(Q93_1)

# verify that all have passed the more difficult attention test
# first get the columns ending with _A
attn_chk2_cols <- final_qualtrics_dat_prolific |>
  select(contains("_A"))

# for each of these columns, look at the unique values that are not NA
answers <- attn_chk2_cols |>
  pivot_longer(cols = contains("_A"),
               names_to = "q",
               values_to = "answer",
               values_drop_na = TRUE) |>
  arrange(q)

correct_answers <- attn_chk_answers |>
  arrange(q)

# check that all answers are correct
answers |>
  left_join(correct_answers, by = c("q" = "q")) |>
  mutate(answer_is_correct = answer == elite) |>
  count(answer_is_correct)

#  looks like they are all correct

# now save the file
final_qualtrics_dat_prolific |>
  write_csv("auxiliary/preprocessed-final-prolific-for-analysis.csv")

# histogram of duration
final_qualtrics_dat_prolific |>
  ggplot(aes(Duration/60)) +
  geom_histogram(colour = "black", fill = "white", binwidth = 1) +
  labs(title = "Duration of the study",
       x = "Duration (in minutes)",
       y = "Count") +
  theme_bw()

# get IDs of those who need to be approved (this list of IDs is larger than
# the list of IDs in the dataset that will be analyzed as some participants
# are being paid despite not meeting all the attention check criteria

final_qualtrics_dat_prolific |> 
  right_join(all_prolific_batches, by = c("Q100" = "Participant id")) |>
  select(batch, Q100) |>
  filter(!Q100 %in% attn_fail$`Participant id`) |>
  arrange(batch, Q100) |>
  write_csv("auxiliary/prolific_approved.csv")
