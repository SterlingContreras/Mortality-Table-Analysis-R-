# Load packages
library(ggplot2)

# Import data
mortality <- read.csv("MortalityTable.csv", skip = 2)

# Rename columns
colnames(mortality) <- c(
  "Age",
  "Male_qx",
  "Male_lx",
  "Male_ex",
  "Female_qx",
  "Female_lx",
  "Female_ex"
)

mortality$Male_lx <- as.numeric(gsub(",", "", mortality$Male_lx))
mortality$Female_lx <- as.numeric(gsub(",", "", mortality$Female_lx))

# Male vs Female Mortality
p2 <- ggplot() +
  geom_line(
    data = mortality,
    aes(Age, Male_qx, color = "Male")
  ) +
  geom_line(
    data = mortality,
    aes(Age, Female_qx, color = "Female")
  ) +
  labs(
    title = "Male vs Female Mortality Rates",
    x = "Age",
    y = "Probability of Death",
    color = "Sex"
  )

ggsave(
  "male_vs_female_mortality.png",
  plot = p2,
  width = 8,
  height = 5
)
# male mortality curve
p1 <- ggplot(mortality,
             aes(Age, Male_qx)) +
  geom_line() +
  labs(
    title = "Male Mortality Rates by Age",
    x = "Age",
    y = "Probability of Death"
  )
ggsave(
  "male_mortality_curve.png",
  plot = p1,
  width = 8,
  height = 5
)
# Life expectancy calculations
male65 <- mortality$Male_ex[mortality$Age == 65]

female65 <- mortality$Female_ex[mortality$Age == 65]

male65
female65

difference <- female65 - male65

difference

# Survival probabilities
p3 <- ggplot(mortality,
             aes(Age, Male_lx)) +
  geom_line() +
  labs(
    title = "Male Survival Function",
    x = "Age",
    y = "Number Surviving"
  )

ggsave(
  "male_survival_curve.png",
  plot = p3,
  width = 8,
  height = 5
)
# Log mortality curve
mortality <- mortality[mortality$Male_qx > 0, ]
p4 <- ggplot(mortality,
             aes(Age, Male_qx)) +
  geom_line() +
  scale_y_log10() +
  labs(
    title = "Male Mortality Rates (Log Scale)",
    x = "Age",
    y = "Log Probability of Death"
  )

ggsave(
  "male_mortality_log_curve.png",
  plot = p4,
  width = 8,
  height = 5
)

p5 <- ggplot() +
  geom_line(
    data = mortality,
    aes(Age, Male_ex, color = "Male")
  ) +
  geom_line(
    data = mortality,
    aes(Age, Female_ex, color = "Female")
  ) +
  labs(
    title = "Remaining Life Expectancy by Age",
    x = "Age",
    y = "Expected Remaining Years",
    color = "Sex"
  )

ggsave(
  "remaining_life_expectancy.png",
  plot = p5,
  width = 8,
  height = 5
)

# Life expectancy comparison at different ages
selected_ages <- mortality[mortality$Age %in% c(0, 25, 45, 65), ]
selected_ages[, c("Age", "Male_ex", "Female_ex")]

mortality$Male_Sx <- mortality$Male_lx / mortality$Male_lx[1]
mortality$Female_Sx <- mortality$Female_lx / mortality$Female_lx[1]

p6 <- ggplot() +
  geom_line(
    data = mortality,
    aes(Age, Male_Sx, color = "Male")
  ) +
  geom_line(
    data = mortality,
    aes(Age, Female_Sx, color = "Female")
  ) +
  labs(
    title = "Survival Probability by Age",
    x = "Age",
    y = "Probability of Survival",
    color = "Sex"
  )

ggsave(
  "survival_probability_comparison.png",
  plot = p6,
  width = 8,
  height = 5
)

# Probability of surviving to age 65
male_survive_65 <- mortality$Male_lx[mortality$Age == 65] /
  mortality$Male_lx[mortality$Age == 0]

female_survive_65 <- mortality$Female_lx[mortality$Age == 65] /
  mortality$Female_lx[mortality$Age == 0]

male_survive_65
female_survive_65

# Median survival age 
male_median_age <- mortality$Age[
  which(mortality$Male_Sx <= 0.5)[1]
]

female_median_age <- mortality$Age[
  which(mortality$Female_Sx <= 0.5)[1]
]

male_median_age
female_median_age

adult <- subset(
  mortality,
  Age >= 30 &
    Age <= 90 &
    Male_qx > 0
)

gompertz_model <- lm(
  log(Male_qx) ~ Age,
  data = adult
)

summary(gompertz_model)

adult$Predicted_qx <- exp(
  predict(gompertz_model)
)

p7 <- ggplot() +
  geom_line(
    data = adult,
    aes(Age, Male_qx, color = "Observed")
  ) +
  geom_line(
    data = adult,
    aes(Age, Predicted_qx, color = "Gompertz Model")
  ) +
  labs(
    title = "Observed vs Gompertz Mortality Model",
    x = "Age",
    y = "Probability of Death",
    color = ""
  )

ggsave(
  "gompertz_model_fit.png",
  plot = p7,
  width = 8,
  height = 5
)

results <- list(
  male65 = male65,
  female65 = female65,
  difference = difference,
  male_survive_65 = male_survive_65,
  female_survive_65 = female_survive_65,
  male_median_age = male_median_age,
  female_median_age = female_median_age,
  gompertz_r2 = summary(gompertz_model)$r.squared
)

print(results)
