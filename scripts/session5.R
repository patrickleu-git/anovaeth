
### Series 5 (Split plots)

library(tidyverse)
library(lmerTest)

# Exercise 3 --------------------------------------------------------------


# read data
df1 = read.table(
  "https://stat.ethz.ch/Teaching/Datasets/cas-das/breaking_exp.dat",
  header = TRUE) |> 
  as_tibble() |> janitor::clean_names()

# factors
df1$alloy <- factor(df1$alloy)
df1$temp <- factor(df1$temp)
df1$day <- factor(df1$day)

# visualise data
stripchart(breaking ~ temp, data = df1, vertical = TRUE)
with(df1, interaction.plot(x.factor = temp, trace.factor = alloy, response = breaking))
plot.design(df1)

# can hardly see an interaction, especially alloys 1 and 3 are parallel
# in the design plot see that the effect of day is quite small 

# fit a model with parameters:
#   - fixed effect of day
#   - fixed effect of temperature
#   - fixed effect of alloy
#   - interaction temperature and alloy
#   - random interaction day and temperature (whole plot error)
model1 = lmer(breaking ~ day + temp * alloy + (1 | day:temp), data = df1)
anova(model1)

# yet there is a significant interaction effect
# main effects highly significant as well

# TA plot seems okay, no funnel shape observable
plot(model1)

# QQ plot for whole plot error (i.e. from the interaction day:temp)
qqnorm(as_vector(ranef(model1)[[1]]))

# QQ plot for residuals (split plot error) looks okay
qqnorm(resid(model1))




# Exercise 4 --------------------------------------------------------------

df2 <- read.table(
  "http://stat.ethz.ch/Teaching/Datasets/WBL/nitrogen.dat",
  header = TRUE) |> 
  as_tibble() |> 
  janitor::clean_names()

# factor
df2$nitrogen = df2$nitrogen |> as_factor()
df2$block = df2$block |> as_factor()
df2$thatch = df2$thatch |> as_factor()

# visualise again 
with(
  df2, 
  interaction.plot(
    x.factor = thatch, 
    trace.factor = nitrogen, 
    response = chlorophyll
    )
  )

# seems to be a (weak) interaction between thatch and nitrogen; lines in the 
# interaction plot are not parallel, most pronounced for `ibdu` nitrogen level

# or ggplot to visualise with block factor
ggplot(data = df2) +
  geom_line(aes(x = thatch, y = chlorophyll, colour = nitrogen, group = nitrogen))+
  facet_wrap(~block)

# fit model
model2 = lmer(chlorophyll ~ block + nitrogen*thatch + (1 | block:nitrogen), data = df2)
anova(model2)

# main effects of nitrogen and thatch are significant, interaction not

# check residuals (TA - looks good)
plot(model2)

# QQ plots also okay
par(mfrow = c(1,2))
qqnorm(as_vector(ranef(model2)[[1]]))
qqnorm(resid(model2))





