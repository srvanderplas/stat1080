library(tidyverse)

set.seed(1841)

# ==========================================================
# Whale shape generator
# ==========================================================

whale <- read_csv("lab/ahab-data.csv")
n <- length(whale$x)

ggplot(whale, aes(x = x, y = y)) + geom_point()

# ==========================================================
# Predictor variables
# ==========================================================

sightings <- whale$x 
fog <- sample(1:10, size = n, replace = T)
error <- whale$y


ggplot(tibble(x = sightings, y = error), aes(x = x, y = y)) + geom_point()
# ==========================================================
# Construct response
# ==========================================================
#
# Whale pattern = residual structure
#
# Simple model:
# distance ~ sightings
#
# residuals follow whale_y
#
# Multiple model:
# distance ~ sightings + fog
#
# fog explains most of whale_y,
# leaving a smaller whale behind.
# ==========================================================

distance <-
  1.5 * sightings +
  -.8 * fog +
  error 


voyage <- tibble(
  day = seq_len(n),
  distance,
  sightings,
  fog
)

ggplot(voyage, aes(x = distance, y = sightings)) + geom_point()

write.csv(voyage,
          "lab/regression_ahab.csv",
          row.names = FALSE)


reg1 <- lm(distance ~ sightings, data = voyage)
voyage$resid1 <- resid(reg1)

ggplot(voyage, aes(x = distance, y = resid1)) + geom_point()

reg2 <- lm(distance ~ sightings + fog, data = voyage)
voyage$resid2 <- resid(reg2)
summary(reg2)
ggplot(voyage, aes(x = distance, y = resid2)) + geom_point()

