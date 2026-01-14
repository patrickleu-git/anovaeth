
### Series 3

library(tidyverse)
library(car)

# Exercise 2 --------------------------------------------------------------


# read data --> unbalanced data
df1 = read.table(
  "http://stat.ethz.ch/Teaching/Datasets/cas-das/unbalanced.dat",
  header = TRUE) |> 
  as_tibble() |> janitor::clean_names()

# change data type to factors
df1$a = factor(df1$a)
df1$b = factor(df1$b)

# set options to contr.sum, otherwise no meaningful result for type III sum of sq
options(contrasts = c("contr.sum", "contr.poly"))
model1 = aov(y ~ a * b, data = df1)

# type I SS
anova(model1)

# type II SS
Anova(model1, type = "II")

# type III SS
Anova(model1, type = "III")

# SS for the interaction is the same for all types of SS; interaction is 
# not significant for any SS type

# compare main effect model with null model
fit.null = aov(y ~ 1, data = df1)
fit.main = aov(y ~ a + b, data = df1)
anova(fit.null, fit.main)

# highly significant, i.e., cannot drop both A and B simultaneously

# inspect main effects (type II or III are equivalent)
Anova(fit.main, type = "II")

# no significant main effect but whole model is significant?
# -->  unbalancedness, A and B are highly correlated; one factor already 
# accounts for most of the variation 



# Exercise 3 --------------------------------------------------------------


# read coffee data
df2 = read.table(
  "http://stat.ethz.ch/Teaching/Datasets/cas-das/coffee.dat",
  header = TRUE) |> 
  as_tibble() |> 
  janitor::clean_names()

df2$location = as_factor(df2$location)
df2$density = as_factor(df2$density)

# interaction plot
with(df2, interaction.plot(x.factor = density, trace.factor = location, response = coffee))

# observations from the same location tend to have similar responses
# except for density level 90, location 1 has the highest coffee yield

# fit one way ANOVA with block on location
coffee.model = aov(coffee ~ location + density, data = df2)
anova(coffee.model)

# blocking was efficient, MSBlock / MSE = 3.3 > 1
# yet, no evidence that tree density has an effect on coffee yield

# check on residuals
par(mfrow = c(1, 2))
plot(coffee.model, which = 1:2)

# TA and QQ about alright (few observations...)
# maybe slight funnel shape for TA plot

# take log transform to address funnelling issue
log.coffee.model = aov(log(coffee) ~ location + density, data = df2)
par(mfrow = c(1, 2))
plot(log.coffee.model, which = 1:2)

# hardly any changes...