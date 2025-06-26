#install.packages("ggplot2")  # if not installed
#library(ggplot2)
library(patchwork)

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
  count = c(1, 0.5),
  vaccination_status = c("Without Vaccine", "With Vaccine")
)

# Updated ggplot with fill mapped to 'vaccination_status'
ggplot(data, aes(x = category, y = count, fill = vaccination_status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Without Vaccine" = "#FFC1C2", "With Vaccine" = "#C1C3FF")) +
  labs(title = "CFR by vaccination status", x = "Vaccination Status", y = "CFR (%)") +
  theme_minimal()




