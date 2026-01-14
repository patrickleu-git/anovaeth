
### series 1 


# Exercise 4 --------------------------------------------------------------


library(tidyverse)

# get data
url = "https://stat.ethz.ch/Teaching/Datasets/cas-das/stream.dat"
df = read_delim(url, delim = " ") |> janitor::clean_names()

# inspect data type
str(df)

# convert zinc and stream to factors
df = df |> 
  mutate(
    zinc = as_factor(zinc),
    stream = as_factor(stream)
  )

# plot biodiversity vs zinc level
stripchart(diversity ~ zinc, data = df, vertical = TRUE, pch = 1)
boxplot(diversity ~ zinc, data = df)

# there seem to be outliers for levels "backgroud" and "medium"

# fitting a one way ANOVA (only zinc)
model1 = aov(diversity ~ zinc, data = df)
anova(model1)

# there seems to be a significant effect of zinc level on diversity (p-value of 0.018)

# check assumptions on residuals
plot(model1)

# QQ plot okay, Tukey Anscombe shows some deviance from zero mean assumtion...

# coefficient estimates
dummy.coef(model1)



# Exercise 5 --------------------------------------------------------------

# read data
data = tibble(blood = c(62, 60, 63, 59,
                        63, 67, 71, 64, 65, 66,
                        68, 66, 71, 67, 68, 68,
                        56, 62, 60, 61, 63, 64, 63, 59),
              treat = factor(c(rep("A", 4), 
                               rep("B", 6), 
                               rep("C", 6),
                               rep("D", 8))
                             )
              )


# visualise
par(mfrow = c(1, 2))
boxplot(blood ~ treat, xlab = "Treatment", ylab = "", data = data)
stripchart(blood ~ treat, vertical = TRUE, pch = 1, method = "stack",
           xlab = "Treatment", ylab = "", data = data)
mtext("Blood clotting times for different treatments", side = 3, line = -2, outer = TRUE, font = 2)

# grand mean
mean(data$blood)

# group mean & variance
summary = data |> summarise(
                    mean = mean(blood),
                    var = var(blood),
                    .by = treat)

# compute SSE and MSE
n = table(data$treat)
g = length(unique(data$treat))
N = nrow(data)

SSE = summary$var %*% (n-1)
MSE = SSE / (N - g)

# compute SSTrt
SSTrt = ( summary$mean - mean(data$blood) )^2 %*% n
MSTrt = SSTrt / (g-1)
  
# now with aov
model2 = aov(blood ~ treat, data = data)
anova(model2)  

# from the ANOVA table above we see that the treatment is highly significant, 
# i.e., we clearly reject the null of no effect of diet on blood clotting time

# model assumptions
par(mfrow = c(2,2))
plot(model2)

# TA okay, QQ plot slight violation due to replicates but should be alright