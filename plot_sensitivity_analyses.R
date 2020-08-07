source("0_helpers.R")
load("data/cleaned_selected_wrangled.rdata")

data_plot = data.frame(
  Statistic = c(rep("R2y~D|X", 8),
                rep("RVq=1", 8),
                rep("RVq=1, a=0.05", 8)),
  Outcome = rep(c(rep("Sexual\nFrequency", 4),
                  rep("Masturbation\nFrequency", 4)),
                3),
  Model = rep(c("Use of HC",
              "Use of HC + Observed Confounder",
              "Use of HC, Congruency, and Interaction",
              "Use of HC, Congruency, and Interaction + Observed Confounder"),
            6),
  Percentage = c(19.8, 7.5, 3.4, 1.8,
               16.9, 8.7, 6.3, 6.7,
               38.8, 24.7, 17.1, 12.8,
               36.1, 26.4, 22.8, 23.4,
               34.9, 19.8, 10.4, 5.6,
               32.0, 21.6, 16.6, 17.1
               )
)



plot =
  ggplot(data_plot, aes(x = factor(Outcome,
                                   levels = c("Sexual\nFrequency", "Masturbation\nFrequency")),
                        y = Percentage,
                        fill = Model), color = "black") +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Statistic) +
  scale_fill_manual(values = c("#7570B3", "#D95F02", "#1B9E77", "#E7298A"),
                    guide = guide_legend(nrow = 4)) +
  apatheme +
  theme(legend.position = "bottom") +
  labs(x = "Outcome")

jpeg('Plot Sensitivity Analyses.jpg',
     width = 800, height = 600, quality = 100)
plot
dev.off()

