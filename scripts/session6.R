
### Series 5 (Split plots)

library(tidyverse)
library(multcomp)
library(crossdes)

# Exercise 2 --------------------------------------------------------------


# read data
df1 = read.table(
  "https://stat.ethz.ch/Teaching/Datasets/cas-das/ibd.dat",
  header = TRUE) |> 
  as_tibble() |> janitor::clean_names()

# factor 
df1$block = df1$block |> as_factor()
df1$treatment = df1$treatment |> as_factor()

# visualise
xtabs(~ block + treatment, data = df1)
# considerable variation between blocks, low variation within, thus blocking helpful
stripchart(y ~ block, data = df1, vertical = TRUE)
# a lot of variation within most treatments (caused by blocks); unclear if trt has 
# large influence
stripchart(y ~ treatment, data = df1, vertical = TRUE)

# fit model
model1 = aov(y ~ block + treatment, data = df1)
summary(model1)

# treatment not significant after having controlled for blocks

# residual check -> TA and QQ seem to be okay
par(mfrow = c(1, 2))
plot(model1, which = 1:2)

# test all pairwise comparisons
summary(glht(model1, linfct = mcp(treatment = "Tukey")))
