rm(list = ls())

# R code for STA3001 Homework 2
a2 = read.table("/Users/ziyiou/study database/大三上/STA3001/assignment/homework2/fev.dat.txt",sep=" ",header=T) # Load the data set
fev <- a2$fev; age <- a2$age

# Q1 (a): plot FEV vs age and the scale-location plot
mod1 = lm(fev~age)
par(mfrow=c(1,2))
plot(age,fev,type="p",col="blue",pch=21, main="FEV vs age")
plot(mod1,which=3) # 'which' selects from among the 4 lm plots


# Q1 (b): plot transformation of the data

# Square root transformation
fev_sqrt = sqrt(fev)
age_sqrt = sqrt(age)
mod2 = lm(fev_sqrt~age_sqrt)
par(mfrow = c(1,2))
plot(age_sqrt,fev_sqrt,type="p",col="darkgreen",pch=21, main="sqrt(FEV) vs sqrt(age)")
plot(mod2,which=3)

# Logarithmic transformation
fev_log = log(fev)
age_log = log(age)
mod3 = lm(fev_log~age_log)
par(mfrow = c(1,2))
plot(age_log,fev_log,type="p",col="darkorange",pch=21, main="log(FEV) vs log(age)")
plot(mod3,which=3)

# Reciprocal transformation
reciprocal <- function(x) {
  return(1 / x)
}

fev_reci = reciprocal(fev)
age_reci = reciprocal(age)
mod4 = lm(fev_reci~age_reci)
par(mfrow = c(1,2))
plot(age_reci,fev_reci,type="p",col="purple",pch=21, main="(FEV)^(-1) vs (age)^(-1)")
plot(mod4,which=3)

# Q2 (a): written by R markdown

# Q2 (b)
par(mfrow = c(1,2))
plot(mod1, which=3, main="Untransformed regression")
plot(mod3, which=3, main="log-transformed regression")

par(mfrow = c(2,2))
plot(mod3)

# Q2 (c)
coefficients <- coef(mod3)
slope <- coefficients["age_log"]
cat("The slope is:", slope)

# Q2 (d)

# Confidence Interval for the Mean Response
age_values <- c(8, 17, 21)
mean_response <- predict(mod1, newdata = data.frame(age = age_values), 
                         interval = "confidence", level = 0.95)
print(mean_response)

# Prediction Intervals
prediction_intervals <- predict(mod1, newdata = data.frame(age = age_values), 
                                interval = "prediction", level = 0.95)
print(prediction_intervals)

# Q2 (e)

DFFIT <- dffits(mod3)
DFBETA <- dfbetas(mod3)[, "age_log"] # don't bother with the intercept
CookDistance <- cooks.distance(mod3)

# Identify influential points based on thresholds (you can adjust these)
threshold_DFFIT <- 2 * sqrt(2 / length(age))
threshold_DFBETA <- 2 / sqrt(length(age))
threshold_Cook <- 4 / (length(age))

# Identify influential points
influential_pts_index <- which(abs(DFFIT) > threshold_DFFIT 
                               | abs(DFBETA) > threshold_DFBETA 
                               | CookDistance > threshold_Cook)


data_frame = data.frame("log(Age)" = age_log, "log(FEV)" = fev_log)
influential_pts <- data_frame[influential_pts_index,]

# Print the influential points
print(length(DFBETA))
print("Influential Points:")
print(influential_pts)
cat("The number of influential points:",length(influential_pts_index))

# Plot the influential points with regression line
par(mfrow = c(1,1))
plot(age_log, fev_log, col = "blue", pch = 21, main = "Scatterplot with Influential Points")
abline(mod3, col = "red")  # Add the regression line
points(influential_pts, col = "green", pch = 19)
legend("bottomright", legend = "Influential Points", col = "green", pch = 19)
