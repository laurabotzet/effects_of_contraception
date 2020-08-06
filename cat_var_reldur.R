model = lm(age ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_age =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Age") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(education_years ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_education =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Education") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_extra ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_extra =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Extraversion") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_neuro ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_neuro =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Neuroticism") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_agree ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_agree =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Agreeableness") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


model = lm(bfi_consc ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_consc =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Conscientiousness") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_open ~ relationship_duration_factor, data = data)
summary(model)

tidy(model, conf.int = T)
reldur_open =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Openness") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(religiosity ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_religiosity =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Religiosity") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(attractiveness_partner ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_attractiveness =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Attractiveness of Partner") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(relationship_satisfaction ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_relsat =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Relationship Satisfaction") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(satisfaction_sexual_intercourse ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_sexsat =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Sexual Satisfaction") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(diary_libido_mean ~ relationship_duration_factor, data = data)
summary(model)
tidy(model, conf.int = T)
reldur_libido =
  effect_plot(model, pred = relationship_duration_factor, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Relationship Duration", y = "Libido") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = glm(diary_sex_active_sex_sum ~ relationship_duration_factor + offset(log(number_of_days)),
            data = data,
            family = "poisson")
summary(model)
anova(model, test = "Chisq")
tidy(model, conf.int = T)
reldur_sexfreq =
  effect_plot(model, pred = relationship_duration_factor, data = data, colors = "CUD") +
  labs(x = "Relationship Duration", y = "Sexual Frequency") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = glm(diary_masturbation_sum ~ relationship_duration_factor + offset(log(number_of_days)),
            data = data,
            family = "poisson")
summary(model)
anova(model, test = "Chisq")
reldur_masfreq =
  effect_plot(model, pred = relationship_duration_factor, data = data, colors = "CUD") +
  labs(x = "Relationship Duration", y = "Masturbation Frequency") +
  scale_x_discrete(labels = c("Single", "0-12 months", "13-28 months", "29-52 months", "> 52 months")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

relationship_duration_factor =
  ggarrange(reldur_age + rremove("xlab"),
            reldur_education + rremove("xlab"),
            reldur_extra + rremove("xlab"),
            reldur_neuro + rremove("xlab"),
            reldur_agree + rremove("xlab"),
            reldur_consc + rremove("xlab"),
            reldur_open + rremove("xlab"),
            reldur_religiosity + rremove("xlab"),
            reldur_attractiveness + rremove("xlab"),
            reldur_relsat + rremove("xlab"),
            reldur_sexsat + rremove("xlab"),
            reldur_libido + rremove("xlab"),
            reldur_sexfreq,
            reldur_masfreq,
            common.legend = TRUE, legend = "bottom",
            ncol = 3, nrow = 5,
            align = "v")

jpeg('Reldur.jpg',
     width = 800, height = 1200, quality = 100)
relationship_duration_factor
dev.off()
