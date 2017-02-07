library(dplyr)

# get the total not white population for a city
total.not.white <- function(df) {
  return (sum(df$pop.not.white))
}

# get the total population for a city
total.pop <- function(df) {
  return (sum(df$pop))
}

# get the proportion of not white population in a city
total.not.white.prop <- function(df) {
  total.not.white(df)/total.pop(df)
}

# caculate the dissimilarity index
diss.index <- function(df) {
  filtered <- df %>% select(pop, pct.not.white)
  numerator <- sum(apply(filtered,1,function(x) { x[1] * abs(x[2] - total.not.white.prop(df))}))
  denominator <- 2 * total.pop(df) * total.not.white.prop(df) * ( 1 - total.not.white.prop(df))
  return (numerator / denominator)
}

# caculate the interaction index
inter.index <- function(df) {
  subset <- df %>% select(pop.not.white, pct.white)
  result <- sum(apply(subset, 1, function(x) { 
    (x[1]/total.not.white(df)) * x[2]}))
  return (result)
}

# caculate the isolation index
isolation.index <- function(df) {
  subset <- df %>% select(pop.not.white, pct.not.white)
  result <- sum(apply(subset, 1, function(x) {
    (x[1]/total.not.white(df)) * (x[2])}))
  return (result)
}

# caculate the correlation ratio
correlation.index <- function(df) {
  isolation <-isolation.index(df)
  result <- (isolation - total.not.white.prop(df)) /(1 -total.not.white.prop(df))
  return (result)
}

# caculate the standard deviation index
standard.dev <- function(df) {
  sd <- sd(df$pct.not.white)
  return (sd)
}
