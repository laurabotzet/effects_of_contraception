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
  Percentage = c(10.2, 4.3,
               19.2, 10.9,
               28.5, 19.1,
               38.3, 29.5,
               23.7, 13.5,
               34.3, 24.6
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

