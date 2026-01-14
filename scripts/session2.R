
### Series 2

library(tidyverse)

# Exercise 1 --------------------------------------------------------------

# read data
df1 = read.table(
  "http://stat.ethz.ch/Teaching/Datasets/cas-das/lentil.dat",
  header = TRUE) |> 
  as_tibble() |> janitor::clean_names()

# recode treatment vector
df1$tr = as_factor(df1$tr)

# visualise
stripchart(y ~ tr, data = df1, vertical = TRUE, pch = 1)

# the variance between treatment is by far larger than variance within treatment
# we can already see three groups, control (1), treatment 2,3,4 and 5,6,7

# fit one way ANOVA
model1 = aov(y ~ tr, data = df1)
anova(model1)

# Anove table confirms above conclusion, global F test easily rejects the null 
# of no treatment effect at the 95% level

# check residuals
par(mfrow = c(2,2))
plot(model1)

# some small deviations in TA and QQ plot but should be alright

# comparisons using contrasts
library(multcomp)
mat.contr <- rbind(c(-6, 1, 1, 1, 1, 1, 1),      # control vs. rest
                   c( 0, -1, -1, -1, 1, 1, 1),   # fertiliser vs. no fertiliser
                   c( 0, 2, -1, -1, 2, -1, -1),  # manual weeding vs. herbicidal weeding
                   c( 0, 0, -1, 1, 0, -1, 1),    # spray herbicide before vs. spray herbicide afterwards
                   c( 0, -2, 1, 1, 2, -1, -1),   # interaction fertiliser and weeding
                   c( 0, 0, 1, -1, 0, -1, 1)     # interaction fertiliser and time of herbicide spraying
                   )

# fit model
model.contr = glht(model1, linfct = mcp(tr = mat.contr))

# show estimates for individual comparison (no p-value adjustments)
summary(model.contr, test = adjusted("none")) 

# using bonferroni correction for p values
summary(model.contr, test = adjusted("bonferroni"))

# three first contrasts are still significant at 95% level, even when controlling
# for the FWER



# Exercise 3 --------------------------------------------------------------

# read data
df2 = read.table(
  "http://stat.ethz.ch/Teaching/Datasets/cas-das/automob-engl.dat",
  header = TRUE) |> 
  as_tibble() |> 
  janitor::clean_names()
  
# adjust data type
df2 = df2 |> mutate(
              city = factor(city),
              car = factor(car)
              )

# visualise
# dev.off() # run first in case of error
df2 |> ggplot()+
  geom_point(
    aes(x = car, 
        y = kmp4l,
        colour = city,
        shape = city
    )) +
  labs(
    x = "Type of car",
    y = "Kilometers per 4 litres"
  )+
  guides(
    colour = guide_legend("City"),
    shape  = guide_legend("City")
  )+
  theme_light()+
  theme(legend.position = "bottom")

# fit two way ANOVA
model2 = aov(kmp4l ~ city * car, data = df2)
anova(model2)

# both main effects and the interaction are highly significant

# residual assumptions
par(mfrow = c(1, 2))
plot(model2, which = 1:2)

# neither the QQ nor the TA plot indicate violation of the model assumptions

# interaction plot
dev.off()
with(df2, interaction.plot(x.factor = car, trace.factor = city, response = kmp4l))

# the interaction is, apparently, mostly due to San Francisco
# removing it from the observations yields...
model3 = aov(kmp4l ~ city * car, data = subset(df2, city != "San Francisco"))
anova(model3)

# a model where not only the interactio is not significant anymore but also city
# has now no significant impact!
# already in the interaction plot we saw that the lines for Los Angeles and Portland
# are almost parallel (no interaction) and largely overlapping (no effect of city)
