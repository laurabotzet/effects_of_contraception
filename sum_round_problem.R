data_diary_means = data_diary %>%
  group_by(session, number_of_days) %>%
  summarise(diary_sex_active_mean = mean(sex_active, na.rm = T),
            diary_sex_active_sex_mean = mean(sex_activity_sex, na.rm = T),
            diary_masturbation_mean = mean(sex_activity_masturbation, na.rm = T),
            diary_sex_active_sum = sum(sex_active, na.rm = T),
            diary_sex_active_sex_sum = sum(sex_activity_sex, na.rm = T),
            diary_masturbation_sum = sum(sex_activity_masturbation, na.rm = T))

data_diary_means = data_diary_means %>%
  mutate(diary_sex_active_sex_sum_false = diary_sex_active_sex_mean*number_of_days,
    diary_sex_active_sex_sum_false_round = as.integer(diary_sex_active_sex_mean*number_of_days))

data_diary_means = data_diary_means %>%
  ungroup() %>%
  select(number_of_days, diary_sex_active_sex_mean, diary_sex_active_sex_sum,
         diary_sex_active_sex_sum_false,
         diary_sex_active_sex_sum_false_round)

data_diary_means = data_diary_means %>%
  filter(diary_sex_active_sex_sum != diary_sex_active_sex_sum_false_round)

save(data_diary_means, data_diary_means, file = "sum_round_problem.rdata")
