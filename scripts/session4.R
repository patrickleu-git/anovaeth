
### Series 4

library(tidyverse)
library(lmerTest)

# Exercise 1 --------------------------------------------------------------


# read data
df1 = read.table(
  "https://stat.ethz.ch/Teaching/Datasets/cas-das/alloy.dat",
  header = TRUE) |> 
  as_tibble() |> janitor::clean_names()

# change casting method to factor
df1$casting = as_factor(df1$casting)

# visualise stripchart
stripchart(strength ~ casting, vertical = TRUE, pch = 1, data = df1)

# variation between groups seems to be smaller than the variation within groups

# use random effects for casting
model.rand = lmer(strength ~ (1 | casting), data = df1)
summary(model.rand)

# can directly read off varaince components:
# error var = 5.819 ; var of random effect = 6.812

# getting confidence intervals
confint(model.rand, oldNames = FALSE)

# quite large on scale of sd... (too few observations)

# residuals check
par(mfrow = c(1,2))
qqnorm(resid(model.rand), main = "residuals")
qqnorm(ranef(model.rand)$casting[, 1], main = "casting")

# QQ plot for residuals about okay, for casting just not enough observations...



# Exercise 2 --------------------------------------------------------------

# read new bacteria data
df2 = read.table(
  "http://stat.ethz.ch/Teaching/Datasets/cas-das/pasteurization.dat",
  header = TRUE) |> 
  as_tibble() |> janitor::clean_names() 

# factor cols
df2$lab = as_factor(df2$lab)
df2$sample = as_factor(df2$sample)

# variation seems to increase with large values for bacteria
with(df2, interaction.plot(x.factor = sample, trace.factor = lab, response = bacteria))

# see also with stripchart
stripchart(bacteria ~ interaction(sample, lab), data = df2, vertical = TRUE, pch = 1)

# log transform
stripchart(log(bacteria) ~ interaction(sample, lab), data = df2, vertical = TRUE, pch = 1)
# error variance in lab number 4 (and maybe 3) seems to be larger than for  other labs

# fit random effect model
model2 = lmer(log(bacteria) ~ (1 | lab * sample), data = df2)
summary(model2)

# corresponding confident intervals
confint(model2, oldNames = FALSE)

# sample-to-sample variation is large, followed by lab-to-lab variation 
# already observed this when inspecting the interaction plot above

# check on residuals 
plot(model2, pch = as.character(df2$lab)) 
# again, mainly lab 4 has large residuals...

# QQ plots okay for residuals too little observations for rest
par(mfrow = c(2, 2))
qqnorm(resid(model2), main = "residuals")
qqnorm(ranef(model2)$lab[,1], main = "lab")
qqnorm(ranef(model2)$sample[,1], main = "sample")
qqnorm(ranef(model2)$"lab:sample"[,1], main = "lab:sample")

# removing lab 4 makes it more conform to assumptions
df_reduced = subset(df2, lab != 4)
model3 = update(model2, data = df_reduced)

# TA plot
plot(model3, pch = as.character(df_reduced$lab))

# QQ plots
par(mfrow = c(2, 2))
qqnorm(resid(model3), main = "residuals")
qqnorm(ranef(model3)$lab[,1], main = "lab")
qqnorm(ranef(model3)$sample[,1], main = "sample")
qqnorm(ranef(model3)$"lab:sample"[,1], main = "lab:sample")

# now all plots look better

# waht about variance components?
summary(model3)
confint(model3, oldNames = FALSE)

# variance of the interaction now very small, and the corresponding null 
# hypothesis is not being; hence mainly lab number 4 which “caused” interaction

