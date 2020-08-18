source("0_helpers.R")
load("data/cleaned_selected_wrangled.rdata")

data_plot = data.frame(
  Statistic = c(rep("R2y~D|X", 4),
                rep("RVq=1", 4),
                rep("RVq=1, a=0.05", 4)),
  Outcome = rep(c(rep("Sexual\nFrequency", 2),
                  rep("Masturbation\nFrequency", 2)),
                3),
  Predictor = rep(c("Use of HC",
              "Use of HC + Observed Confounders"),
            6),
  Percentage = c(19.8, 7.5,
               16.9, 8.7,
               38.8, 24.7,
               36.1, 26.4,
               34.9, 19.8,
               32.0, 21.6
               )
)



plot =
  ggplot(data_plot, aes(x = factor(Outcome,
                                   levels = c("Sexual\nFrequency", "Masturbation\nFrequency")),
                        y = Percentage,
                        fill = Predictor), color = "black") +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Statistic) +
  scale_fill_manual(values = c("#1B9E77", "#D95F02")) +
  apatheme +
  theme(legend.position = "bottom") +
  labs(x = "Outcome", fill = "Predictor(s)")

jpeg('Plot Sensitivity Analyses.jpg',
     width = 800, height = 600, quality = 100)
plot
dev.off()
