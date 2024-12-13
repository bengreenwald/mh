############################
#### RESEARCH QUESTIONS ####
############################

# ---- load packages ---- 
pacman::p_load(
  here, # project management
  tidyverse, # data wrangling
  lubridate, # date cleaning
  broom # tidying up statistical data
)

# ---- load functions ----
source(here("functions.R"))

# ---- load & clean data ----
member <- read_csv(here("data", "mh_member.csv")) %>% 
  # remove rows in CSV file with no data
  filter(!is.na(id)) %>% 
  # create age groups for easier comparisons
  mutate(age_grouped = case_when(
    between(age, 18, 24) ~ "18_24",
    age < 35 ~ "25_34",
    age < 45 ~ "35_44",
    age < 55 ~ "45_54",
    age < 65 ~ "55_64",
    age >= 65 ~ "65 or older",
    TRUE ~ '17 or younger'))

activity <- read_csv(here("data", "mh_activity.csv")) %>% 
  # convert activity date from string --> date
  mutate(activity_date = mdy(date)) %>%
  # filter out rows where all three visit types are FALSE (unclear what that means)
  filter(!(coach_visit == FALSE & therapy_visit == FALSE & digital == FALSE)) %>% 
  select(-date)

pre_assessment <- read_csv(here("data", "mh_pre_assessment.csv")) %>% 
  # rename columns to identify pre-assessment data when joining with post
  rename_with(~paste0("pre_", .x), everything()) %>% 
  # convert pre-assessment date from string --> date
  mutate(pre_date = mdy(pre_date)) 

post_assessment <- read_csv(here("data", "mh_post_assessment.csv")) %>% 
  # rename columns to identify post-assessment data when joining with pre
  rename_with(~paste0("post_", .x), everything()) %>% 
  # convert post-assessment date from string --> date
  mutate(post_date = mdy(post_date)) 

# ---- join member and assessment data ----
mh_data <- member %>% 
  # join all data together
  left_join(., pre_assessment, by = c("id" = "pre_member_id")) %>% 
  left_join(., post_assessment, by = c("id" = "post_member_id")) %>% 
  # calculate change in scores between pre and post assessments
  mutate(
    change_dts_total = post_dts_total - pre_dts_total,
    change_phq9_total = post_phq9_total - pre_phq9_total
  )

# check out the dataset
glimpse(mh_data)

# ---- Question 2.1 ----
# What is the association between the type of care used and changes in distress tolerance?

# counting up each member's aggregated care by type between pre and post assessments
member_care_agg <- activity %>% 
  group_by(member_id) %>% 
  summarise(
    coach_visit = sum(coach_visit[coach_visit == TRUE]),
    therapy_visit = sum(therapy_visit[therapy_visit == TRUE]),
    digital_visit = sum(digital[digital == TRUE]),
    .groups = "drop"
  )

# join care aggregates to member data for full data
mh_1 <- mh_data %>% 
  left_join(., member_care_agg, by = c("id" = "member_id")) %>% 
  # replace members with NA activity counts with 0
  mutate(across(contains("visit"), ~replace_na(., 0))) %>% 
  select(
    id,
    contains("dts"),
    contains("visit")
  )

# linear regression formula & run linear regressions
formula_1 <- as.formula("change_dts_total ~ coach_visit + therapy_visit + digital_visit")
reg_1 <- lm(formula_1, data = mh_1)

# summarize, tidy, and view results
q1_results <- reg_1 %>% tidy_results()
q1_results
summary(reg_1)$adj.r.squared

# save results
results_csv(q1_results)

# ---- Question 2.2 ----
# Does the average change in distress tolerance differ between members who recovered from depression compared to those who did not recover from depression?

# build member data to compare groups based on depression recovery
mh_2 <- mh_data %>% 
  # only include members classified as having moderate to severe depression in the pre-assessment 
  filter(pre_phq9_total > 10) %>% 
  # determine whether those members are classified as having depression or not in the post-assessment
  mutate(recovered = if_else(post_phq9_total <= 10, "yes", "no")) %>% 
  select(
    id, 
    recovered, 
    contains("dts"),
    contains("phq9")
  )

# t test formula & run t test to compare group means
formula_2 <- as.formula("change_dts_total ~ recovered")
t_2 <- t.test(formula_2, data = mh_2)

# summarize, tidy, and view results
q2_results <- t_2 %>% tidy_results()
q2_results

# save results
results_csv(q2_results)

# ---- Question 2.3 ----
# What are the associations between the demographic variables 
# presented and improvement in depression scores?

# anova formula & run 4-way anova with demographic variables
formula_3 <- as.formula("change_phq9_total ~ age_grouped + gender + education + race")
anova_3 <- aov(formula_3, data = mh_data)

# summarize, tidy, and view results
q3_results <- anova_3 %>% tidy_results()
q3_results

# TukeyHSD post-hoc analysis to dig deeper into pairwise comparisons
q3_posthoc <- TukeyHSD(anova_3, c("education", "race")) %>% 
  tidy() %>% 
  # include only pairwise comparisons that have significant differences
  filter(adj.p.value <= 0.05) %>% 
  arrange(term, contrast) %>% 
  select(
    term,
    contrast,
    adj.p.value
  )

# calculate group means
q3_means_education <- group_means(var = education)
q3_means_race <- group_means(var = race)

# save results
results_csv(q3_results)
results_csv(q3_posthoc)
results_csv(q3_means_education)
results_csv(q3_means_race)

# ---- Question 2.4 ----
# Is there anything else interesting that you can glean from this dataset?

# how many members engaged on more than 1 day?
activity %>% 
  group_by(member_id) %>% 
  summarise(dates_engaged = n_distinct(activity_date)) %>% 
  mutate(return_member = if_else(dates_engaged > 1, TRUE, FALSE)) %>% 
  group_by(return_member) %>% 
  summarise(members = n_distinct(member_id)) %>% 
  mutate(perc_return = members / sum(members))

# how many total care activities happen on members' initial day?
first_day <- activity %>% 
  group_by(member_id) %>% 
  mutate(first_date = min(activity_date)) %>% 
  ungroup() %>% 
  mutate(on_first_day = if_else(activity_date == first_date, TRUE, FALSE)) 

first_day %>% 
  group_by(on_first_day) %>%
  summarise(
    total_visits = n(),
    coach_visit = sum(coach_visit[coach_visit == TRUE]),
    therapy_visit = sum(therapy_visit[therapy_visit == TRUE]),
    digital_visit = sum(digital[digital == TRUE]),
    .groups = "drop"
  ) %>% 
  mutate(across(where(is.integer), ~ . / sum(.), .names = "perc_{.col}"))

# how many members complete more than 1 type of activity on their initial day?
first_day %>% 
  filter(on_first_day == TRUE) %>%
  group_by(member_id) %>% 
  summarise(
    coach_visit = sum(coach_visit[coach_visit == TRUE]),
    therapy_visit = sum(therapy_visit[therapy_visit == TRUE]),
    digital_visit = sum(digital[digital == TRUE]),
    .groups = "drop"
  ) %>% 
  count(
    (
      (coach_visit > 0 & therapy_visit > 0) |
      (coach_visit > 0 & digital_visit > 0) |
      (therapy_visit > 0 & digital_visit > 0)
    ),
    name = "member_count") %>% 
  mutate(perc = member_count / sum(member_count))

# how many members eventually completed each care type at least once?
first_day %>% 
  # filter(on_first_day == TRUE) %>% 
  group_by(member_id) %>% 
  summarise(
    coach_visit = sum(coach_visit[coach_visit == TRUE]),
    therapy_visit = sum(therapy_visit[therapy_visit == TRUE]),
    digital_visit = sum(digital[digital == TRUE]),
    .groups = "drop"
  ) %>% 
  count(
    coach_visit > 0 & therapy_visit > 0 & digital_visit > 0,
    name = "member_count"
  ) %>% 
  mutate(perc = member_count / sum(member_count))
