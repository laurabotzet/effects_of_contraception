model = lm(age ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_age =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Age") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


model = lm(education_years ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_education =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Education") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_extra ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_extra =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Extraversion") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_neuro ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_neuro =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Neuroticism") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_agree ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_agree =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Agreeableness") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


model = lm(bfi_consc ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_consc =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Conscientiousness") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(bfi_open ~ net_income, data = data)
summary(model)

tidy(model, conf.int = T)
income_open =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Openness") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(religiosity ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_religiosity =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Religiosity") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(attractiveness_partner ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_attractiveness =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Attractiveness of Partner") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(relationship_satisfaction ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_relsat =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Relationship Satisfaction") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(satisfaction_sexual_intercourse ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_sexsat =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Sexual Satisfaction") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = lm(diary_libido_mean ~ net_income, data = data)
summary(model)
tidy(model, conf.int = T)
income_libido =
  effect_plot(model, pred = net_income, colors = "CUD", plot.points = FALSE, point.color = "grey",
              jitter = 0.2) +
  labs(x = "Income", y = "Libido") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = glm(diary_sex_active_sex_sum ~ net_income + offset(log(number_of_days)),
            data = data,
            family = "poisson")
summary(model)
anova(model, test = "Chisq")
tidy(model, conf.int = T)
income_sexfreq =
  effect_plot(model, pred = net_income, data = data, colors = "CUD") +
  labs(x = "Income", y = "Sexual Frequency") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

model = glm(diary_masturbation_sum ~ net_income + offset(log(number_of_days)),
            data = data,
            family = "poisson")
summary(model)
anova(model, test = "Chisq")
income_masfreq =
  effect_plot(model, pred = net_income, data = data, colors = "CUD") +
  labs(x = "Income", y = "Masturbation Frequency") +
  scale_x_discrete(labels = c("< 500€", "500-1000€", "1000-2000€", "2000-3000€",
                              "> 3000€", "do not want to tell")) +
  apatheme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

net_income =
  ggarrange(income_age + rremove("xlab"),
            income_education + rremove("xlab"),
            income_extra + rremove("xlab"),
            income_neuro + rremove("xlab"),
            income_agree + rremove("xlab"),
            income_consc + rremove("xlab"),
            income_open + rremove("xlab"),
            income_religiosity + rremove("xlab"),
            income_attractiveness + rremove("xlab"),
            income_relsat + rremove("xlab"),
            income_sexsat + rremove("xlab"),
            income_libido + rremove("xlab"),
            income_sexfreq,
            income_masfreq,
            common.legend = TRUE, legend = "bottom",
            ncol = 3, nrow = 5,
            align = "v")

jpeg('Income.jpg',
     width = 800, height = 1200, quality = 100)
net_income
dev.off()
