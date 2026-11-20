# Sample project analysis

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

data <- read_csv("homework/sample-project-data.csv")

head(data)

# Plot distributions


# Easy but not as well designed
data |>
  select(-Date) |>
  pivot_longer(everything(), names_to = "var", values_to = "value") |>
ggplot(aes(x = value)) + geom_histogram() + facet_wrap(~var, scales = "free")


# Better to create specific distribution graphics
data |>
  ggplot(aes(x = Chocolate_Consumption_g)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "grey") + 
  theme_bw() + 
  labs(y = "# Observations", 
       x = "Chocolate consumption (to nearest 5g)", 
       title = "Distribution of Chocolate Consumption") 


data |>
  ggplot(aes(x = Mood)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  theme_bw() + 
  labs(y = "# Observations", 
       x = "Mood (1 = bad, 5 = awesome)", 
       title = "Distribution of Mood") 


data |>
  ggplot(aes(x = Minutes_Exercise)) + 
  geom_histogram(binwidth = 5, color = "black", fill = "grey") + 
  theme_bw() + 
  labs(y = "# Observations", 
       x = "Minutes of Exercise", 
       title = "Distribution of Exercise Time") 


data |>
  ggplot(aes(x = Minutes_Studying)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "grey") + 
  theme_bw() + 
  labs(y = "# Observations", 
       x = "Minutes Spent Studying", 
       title = "Distribution of Study Time") 


# Relationships between variables

data |> 
  ggplot(aes(x = Mood, y = Minutes_Exercise)) + geom_jitter() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(y = "Minutes of Exercise", 
       x = "Mood", 
       title = "Exercise and Overall Daily Mood") 

data |> 
  ggplot(aes(x = Mood, y = Minutes_Studying)) + geom_jitter() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(y = "Minutes Spent Studying", 
       x = "Mood", 
       title = "Studying and Overall Daily Mood") 

data |> 
  ggplot(aes(x = Mood, y = Chocolate_Consumption_g)) + geom_jitter() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(y = "Chocolate Consumption (g)", 
       x = "Mood", 
       title = "Chocolate Consumption and Overall Daily Mood") 

data |> 
  ggplot(aes(x = Minutes_Exercise)) + 
  geom_point(aes(y = jitter(Chocolate_Consumption_g))) + 
  geom_smooth(aes(y = Chocolate_Consumption_g), method = "lm") + 
  theme_bw() + 
  labs(y = "Chocolate Consumption (g)", 
       x = "Minutes of Exercise", 
       title = "Chocolate Consumption and Exercise Time") 


data |> 
  ggplot(aes(x = Minutes_Exercise, y = Minutes_Studying)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(y = "Minutes Studying", 
       x = "Minutes of Exercise", 
       title = "Study and Exercise Time") 


data |> 
  ggplot(aes(x = Minutes_Studying, 
             y = Chocolate_Consumption_g)) + 
  geom_point(aes(y = jitter(Chocolate_Consumption_g))) + 
  geom_smooth(aes(y = Chocolate_Consumption_g), method = "lm") + 
  theme_bw() + 
  labs(y = "Chocolate Consumption (g)", 
       x = "Minutes Studying", 
       title = "Chocolate Consumption and Study Time") 


# Statistical Tests

# Compare "bad" mood days to "good" mood days
data <- data |>
  mutate(good_mood = factor(if_else(Mood <= 2, "bad", "good")))

data |> 
  ggplot(aes(x = good_mood, y = Chocolate_Consumption_g)) +
  geom_jitter() + 
  geom_boxplot(fill = "transparent")

t.test(Chocolate_Consumption_g ~ good_mood, data = data)


# Compare levels of mood
data |> 
  ggplot(aes(x = factor(Mood), 
             y = Chocolate_Consumption_g)) +
  geom_jitter() + 
  geom_boxplot(fill = "transparent")

## Treating Mood as a numeric variable
reg_mood_numeric <- lm(Chocolate_Consumption_g ~ Mood, data = data)
summary(reg_mood_numeric)

## Treating Mood as a categorical variable
reg_mood_cat <- lm(Chocolate_Consumption_g ~ factor(Mood), data = data)
summary(reg_mood_cat)
anova(reg_mood_cat)


# Examine Minutes Studying vs. Exercise
ggplot(data, aes(x = Minutes_Exercise, y = Minutes_Studying)) + geom_point()

reg_study_exercise <- lm(Minutes_Studying ~ Minutes_Exercise, data = data)
summary(reg_study_exercise)

# Examine Minutes Exercise vs. Chocolate Consumption
ggplot(data, aes(x = Minutes_Exercise, y = Chocolate_Consumption_g)) + geom_point() + geom_smooth(method = "lm")


reg_choc_exercise <- lm(Chocolate_Consumption_g ~ Minutes_Exercise, data = data)
summary(reg_choc_exercise)

