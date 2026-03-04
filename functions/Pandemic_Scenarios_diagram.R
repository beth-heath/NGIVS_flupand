#install.packages("ggplot2")  # if not installed
library(ggplot2)
library(patchwork)
source(here::here('setup','aesthetics.R'))

data <- data.frame(
  pandemic_example = c("1918-like", "1957-like", "2009-like"),
  value = c(0.08588, 0.08588, 0.08588),
  lower = c(0.07249, 0.07249, 0.07249),
  upper = c(0.09834, 0.09834, 0.09834)
)

data$pandemic_example <- factor(data$pandemic_example, levels = c("1918-like", "1957-like", "2009-like"))

ggplot(data, aes(x = pandemic_example, y = value)) +
  geom_point(size = 3, color = c('red','blue','grey' )) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Transmissibility Parameters", y = "Estimate", x = "Pandemic Scenarios")

plot1 <- ggplot(data, aes(x = pandemic_example, y = value)) +
  geom_point(size = 3, color = c('#FFC1C2','#C1C3FF','grey' )) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Transmissibility Parameters", y = "Estimate", x = "Pandemic Scenarios")


df <- data.frame(
  group = rep(c("0-4", "5-19", "20-64", "65+"), each = 3),
  category = rep(c("1918-like", "1958-like", "2009-like"), 4),
  value = c(2.5, 1, 0.03, 0.7, 0.02, 0.01, 2.2, 0.09, 0.04, 4.5, 2.5, 1)
)
df$group <- factor(df$group, levels= c("0-4", "5-19", "20-64", "65+"))

ggplot(df, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1918-like" = "red", "1958-like" = "blue", "2009-like" = "grey")) +
  facet_wrap(~ group, nrow = 1) +
  scale_fill_manual(
    values = c("1918-like" = "red", "1958-like" = "blue", "2009-like" = "grey"),
    name = "Pandemic Scenarios",
    labels = c("x" = "Type X", "y" = "Type Y", "z" = "Type Z")
  ) +
  theme_minimal() +
  labs(title = "Case Fatality Ratio Estimates", y='Estimate (%)', x='Pandemic Scenarios')

plot2 <- ggplot(df, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1918-like" = "#FFC1C2", "1958-like" = "#C1C3FF", "2009-like" = "grey")) +
  facet_wrap(~ group, nrow = 1) +
  scale_fill_manual(
    values = c("1918-like" = "#FFC1C2", "1958-like" = "#C1C3FF", "2009-like" = "grey"),
    name = "Pandemic Scenarios",
    labels = c("x" = "Type X", "y" = "Type Y", "z" = "Type Z")
  ) +
  theme_minimal() +
  labs(title = "Case Fatality Ratio Estimates", y='Estimate (%)', x='Pandemic Scenarios')


df <- data.frame(
  scenario = c(rep("1918-like", 21), rep("1958-like", 21), rep("2009-like", 21)),
  step = rep(seq(0,100,5),3),
  lower = c(rep(0.84,1), rep(0.8,3),rep(0.8,17),
            rep(0.76,1), rep(0.70,3),rep(0.6,17),
            rep(0.84,1), rep(0.8,3),rep(0.5,17)),
  upper = c(rep(0.96,1), rep(0.95,3),rep(0.95,17),
            rep(0.92,1), rep(0.90,3),rep(0.8,17),
            rep(0.96,1), rep(0.95,3),rep(0.7,17))
)


ggplot(df, aes(x = step, ymin = lower, ymax = upper, fill = scenario)) +
  geom_ribbon(alpha = 0.3) +
  geom_step(aes(y = lower), color = "black") +
  geom_step(aes(y = upper), color = "black") +
  scale_fill_manual(values = c("1918-like" = "red", "1958-like" = "blue", "2009-like" = "grey")) +
  facet_wrap(~ scenario) +
  theme_minimal() +
  labs(title = "Susceptibility Parameter Intervals by Age", y='Estimate', x='Age')

plot3 <- ggplot(df, aes(x = step, ymin = lower, ymax = upper, fill = scenario)) +
  geom_ribbon(alpha = 0.3) +
  geom_step(aes(y = lower), color = "black") +
  geom_step(aes(y = upper), color = "black") +
  scale_fill_manual(values = c("1918-like" = "red", "1958-like" = "blue", "2009-like" = "grey"),
                    name = "Pandemic Scenarios") +
  facet_wrap(~ scenario) +
  theme_minimal() +
  labs(title = "Susceptibility Parameter Intervals by Age", y='Estimate', x='Age')


combined_plot <- plot1 / plot3 / plot2

combined_plot


### the different types of mechanisms part

data <- data.frame(
  category = c("Without Vaccine", "With Vaccine"),
  count = c(1, 0.7),
  vaccination_status = c("Without Vaccine", "With Vaccine")
)

# Updated ggplot with fill mapped to 'vaccination_status'
ggplot(data, aes(x = category, y = count, fill = vaccination_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Without Vaccine" = "#FFC1C2", "With Vaccine" = "#C1C3FF")) +
  labs(title = "CFR by vaccination status", x = "Vaccination Status", y = "CFR (%)") +
  theme_minimal()  +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 15),
    strip.text = element_text(size = 14)
  ) 


df <- data.frame(
  Scenario = c(rep("Pandemic Scenario 1", 21), rep("Pandemic Scenario 2", 21), rep("Pandemic Scenario 3", 21)),
  step = rep(seq(0,100,5),3),
  lower = c(rep(84,1), rep(80,3),rep(80,17),
            rep(76,1), rep(70,3),rep(60,17),
            rep(84,1), rep(80,3),rep(30,17)),
  upper = c(rep(96,1), rep(95,3),rep(95,17),
            rep(92,1), rep(90,3),rep(80,17),
            rep(96,1), rep(95,3),rep(50,17))
)

df <- data.frame(
  Scenario = c(rep("Scenario 1", 101), rep("Scenario 2", 101), rep("Scenario 3", 101)),
  step = rep(seq(0,100,1),3),
  lower = c(rep(100,1), rep(80,17),rep(80,83),
            rep(100,1), rep(70,17),rep(60,83),
            rep(100,1), rep(80,17),rep(30,83)),
  upper = c(rep(100,1), rep(95,17),rep(95,83),
            rep(100,1), rep(90,17),rep(80,83),
            rep(100,1), rep(95,17),rep(50,83))
)

pandemic_colors_scen <- pandemic_colors
names(pandemic_colors_scen) <- paste0('Scenario ', 1:3)

plot1 <- ggplot(df, aes(x = step, ymin = lower, ymax = upper, fill = Scenario)) +
  geom_ribbon(alpha = 0.3) +
  geom_step(aes(y = lower, color = Scenario), lwd = 0.8) +
  geom_step(aes(y = upper, color = Scenario), lwd = 0.8) +
  scale_color_manual(values = pandemic_colors_scen) +
  scale_fill_manual(values = pandemic_colors_scen) +
  facet_wrap(~ Scenario) +
  theme_minimal() +
  ylim(c(0,100)) + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 14),
    legend.position = 'none'
  ) +
  labs(y='Susceptibility (%)', x='Age (years)'); plot1


df <- data.frame(
  scenario = c(rep("Scenario 1", 101), rep("Scenario 2", 101), rep("Scenario 3", 101)),
  step = rep(seq(0,100,1),3),
  cfr = c(rep(2.5,5), rep(0.7,13),rep(2.2,47),rep(4.5,36),
          rep(1.1,5), rep(0.03,13),rep(0.07,47),rep(1.8,36),
          rep(0.03,5), rep(0.01,13),rep(0.04,47),rep(1,36))
)

plot2 <- ggplot(df, aes(x = step)) +
  geom_step(aes(y = cfr, color = scenario), lwd = 0.8) +
  facet_wrap(~ scenario) +
  scale_color_manual(values = pandemic_colors_scen) +
  theme_minimal() +
  # scale_y_log10(breaks = c(0.01,0.03,0.1,0.3,1,3,10),
  #               labels = c(0.01,0.03,0.1,0.3,1,3,10)) +
  labs(y = 'IFR (%)', x = 'Age (years)', col = 'Scenario') +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = 'none'
  )

library(magick)
img1 <- magick::image_read(here::here("Graphs_included","seeiir.png")) %>% magick::image_ggplot()

combined <- img1 / (plot1 + plot2) + plot_layout(heights = c(1, 2)) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')
combined <- img1 + plot1 + plot2 + plot_layout(nrow = 3, heights = c(3,4,4)) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')
combined

ggsave(here::here('Graphs_included','Figure_1_patchwork.png'),
       height = 8, width = 8)

