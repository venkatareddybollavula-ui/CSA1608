# 1.Covariance and correlation 
# Children of three ages are asked to indicate their preference for three
# photographs of adults. Do the data suggest that there is a significant
# relationship between age and photograph preference? What is wrong
# with this study?
# Age of child   Photograph:
# 
#                   A      B     C 
  # 5-6 years:        18    22     20 
  # 7-8 years:         2    28     40 
  # 9-10 years:       20    10    401. 
  # Use cov() to calculate the sample covariance between B and C.
# 2. Use another call to cov() to calculate the sample covariance matrix for
# the preferences.
# 3. Use cor() to calculate the sample correlation between B and C.
# 4. Use another call to cor() to calculate the sample correlation matrix for
# the preferences.
# Data from the question

A <- c(18, 2, 20)
B <- c(22, 28, 10)
C <- c(20, 40, 40)

# 1. Covariance between B and C
cov(B, C)

# 2. Covariance matrix
data <- data.frame(A, B, C)
cov(data)

# 3. Correlation between B and C
cor(B, C)

# 4. Correlation matrix
cor(data)

# output:
# > A <- c(18, 2, 20)
# > B <- c(22, 28, 10)
# > C <- c(20, 40, 40)
# > 
#   > # 1. Covariance between B and C
#   > cov(B, C)
# [1] -20
# > 
#   > # 2. Covariance matrix
#   > data <- data.frame(A, B, C)
# > cov(data)
# A   B         C
# A  97.33333 -74 -46.66667
# B -74.00000  84 -20.00000
# C -46.66667 -20 133.33333
# > 
#   > # 3. Correlation between B and C
#   > cor(B, C)
# [1] -0.1889822
# > 
#   > # 4. Correlation matrix
#   > cor(data)
# A          B          C
# A  1.0000000 -0.8183918 -0.4096440
# B -0.8183918  1.0000000 -0.1889822
# C -0.4096440 -0.1889822  1.0000000

# 2.Equal-Frequency Partitioning and Data Smoothing
# Question:The following data represent prices of commonly sold items in the All Electronics data warehouse:
#   
#   1, 1, 5, 5, 8, 8, 8, 10, 10, 10, 10, 12, 14, 14, 14, 15, 15, 15, 15, 15, 15, 18, 18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 25, 25, 25, 25, 25, 28, 28, 30
# 
# Perform the following tasks:
# 1. Partition the dataset using equal-frequency partitioning with 3 bins.
# 2. Apply data smoothing using bin means and bin boundaries.
# 3. Plot a histogram for the data.

prices <- c(
  1, 1, 5, 5, 8, 8, 8, 10, 10, 10,
  10, 12, 14, 14, 14, 15, 15, 15, 15, 15,
  15, 18, 18, 18, 18, 18, 18, 20, 20, 20,
  20, 20, 20, 20, 21, 21, 21, 21, 25, 25,
  25, 25, 25, 28, 28, 30
)

# Divide into 3 equal-frequency bins
bins <- split(
  prices,
  cut(seq_along(prices), breaks = 3, labels = FALSE)
)

print(bins)

# Smoothing using bin means
smooth_mean <- lapply(bins, function(x) {
  round(rep(mean(x), length(x)), 2)
})

print(smooth_mean)

# Smoothing using bin boundaries
smooth_boundary <- lapply(bins, function(x) {
  low <- min(x)
  high <- max(x)
  
  sapply(x, function(value) {
    if (abs(value - low) <= abs(value - high)) low else high
  })
})

print(smooth_boundary)

# Histogram
hist(
  prices,
  main = "Electronics Prices",
  xlab = "Price",
  ylab = "Frequency",
  col = "lightblue"
)


# output:
# > prices <- c(
#   +   1, 1, 5, 5, 8, 8, 8, 10, 10, 10,
#   +   10, 12, 14, 14, 14, 15, 15, 15, 15, 15,
#   +   15, 18, 18, 18, 18, 18, 18, 20, 20, 20,
#   +   20, 20, 20, 20, 21, 21, 21, 21, 25, 25,
#   +   25, 25, 25, 28, 28, 30
#   + )
# > 
#   > # Divide into 3 equal-frequency bins
#   > bins <- split(
#     +   prices,
#     +   cut(seq_along(prices), breaks = 3, labels = FALSE)
#     + )
# > 
#   > print(bins)
# $`1`
# [1]  1  1  5  5  8  8  8 10 10 10 10 12 14 14 14 15
# 
# $`2`
# [1] 15 15 15 15 15 18 18 18 18 18 18 20 20 20 20
# 
# $`3`
# [1] 20 20 20 21 21 21 21 25 25 25 25 25 28 28 30
# 
# > 
#   > # Smoothing using bin means
#   > smooth_mean <- lapply(bins, function(x) {
#     +   round(rep(mean(x), length(x)), 2)
#     + })
# > 
#   > print(smooth_mean)
# $`1`
# [1] 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06 9.06
# 
# $`2`
# [1] 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53 17.53
# 
# $`3`
# [1] 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67 23.67
# 
# > 
#   > # Smoothing using bin boundaries
#   > smooth_boundary <- lapply(bins, function(x) {
#     +   low <- min(x)
#     +   high <- max(x)
#     +   
#       +   sapply(x, function(value) {
#         +     if (abs(value - low) <= abs(value - high)) low else high
#         +   })
#     + })
# > 
#   > print(smooth_boundary)
# $`1`
# [1]  1  1  1  1  1  1  1 15 15 15 15 15 15 15 15 15
# 
# $`2`
# [1] 15 15 15 15 15 20 20 20 20 20 20 20 20 20 20
# 
# $`3`
# [1] 20 20 20 20 20 20 20 20 20 20 20 20 30 30 30
# 
# > 
#   > # Histogram
#   > hist(
#     +   prices,
#     +   main = "Electronics Prices",
#     +   xlab = "Price",
#     +   ylab = "Frequency",
#     +   col = "lightblue"
#     + )
http://127.0.0.1:34005/graphics/dc202f00-0214-4b4d-b48b-dbea1b93637e.png


# 3.Comparison of Class A and Class B
# Question: Two Maths teachers are comparing the performance of their Year 9 classes in the end-of-year examination.
# 
# Class A: 76, 35, 47, 64, 95, 66, 89, 36, 84
# 
# Class B: 51, 56, 84, 60, 59, 70, 63, 66, 50
# 
# Perform the following tasks:
# 1. Find which class scored a higher mean, median, and range.
# 2. Plot the data using a boxplot and give your inferences.


#input:

A <- c(76, 35, 47, 64, 95, 66, 89, 36, 84)
B <- c(51, 56, 84, 60, 59, 70, 63, 66, 50)

cat("Class A Mean:", mean(A), "\n")
cat("Class B Mean:", mean(B), "\n")

cat("Class A Median:", median(A), "\n")
cat("Class B Median:", median(B), "\n")

cat("Class A Range:", diff(range(A)), "\n")
cat("Class B Range:", diff(range(B)), "\n")

boxplot(
  A, B,
  names = c("Class A", "Class B"),
  main = "Class Marks Comparison",
  xlab = "Class",
  ylab = "Marks",
  col = c("lightblue", "lightgreen")
)

# output:
 
# > A <- c(76, 35, 47, 64, 95, 66, 89, 36, 84)
# > B <- c(51, 56, 84, 60, 59, 70, 63, 66, 50)
# > 
#   > cat("Class A Mean:", mean(A), "\n")
# Class A Mean: 65.77778 
# > cat("Class B Mean:", mean(B), "\n")
# Class B Mean: 62.11111 
# > 
#   > cat("Class A Median:", median(A), "\n")
# Class A Median: 66 
# > cat("Class B Median:", median(B), "\n")
# Class B Median: 60 
# > 
#   > cat("Class A Range:", diff(range(A)), "\n")
# Class A Range: 60 
# > cat("Class B Range:", diff(range(B)), "\n")
# Class B Range: 34 
# > 
#   > boxplot(
#     +   A, B,
#     +   names = c("Class A", "Class B"),
#     +   main = "Class Marks Comparison",
#     +   xlab = "Class",
#     +   ylab = "Marks",
#     +   col = c("lightblue", "lightgreen")
#     + )
http://127.0.0.1:34005/graphics/27f5a24a-9547-45b9-b8c2-083462a10b17.png


# 4.Min-Max and Z-Score Normalization
# Question: Normalize the following data using the two methods given:
#   
#   200, 300, 400, 600, 1000
# 
# (a) Min-max normalization by setting the minimum to 0 and maximum to 1.
# 
# (b) Z-score normalization.

#input:
x <- c(200, 300, 400, 600, 1000)

# Min-max normalization
min_max <- (x - min(x)) / (max(x) - min(x))

cat("Min-Max Normalization:\n")
print(round(min_max, 4))

# Z-score normalization
z_score <- (x - mean(x)) / sd(x)

cat("Z-Score Normalization:\n")
print(round(z_score, 4))

# output:
 
# > x <- c(200, 300, 400, 600, 1000)
# > 
#   > # Min-max normalization
#   > min_max <- (x - min(x)) / (max(x) - min(x))
# > 
#   > cat("Min-Max Normalization:\n")
# Min-Max Normalization:
#   > print(round(min_max, 4))
# [1] 0.000 0.125 0.250 0.500 1.000
# > 
#   > # Z-score normalization
#   > z_score <- (x - mean(x)) / sd(x)
# > 
#   > cat("Z-Score Normalization:\n")
# Z-Score Normalization:
#   > print(round(z_score, 4))
# [1] -0.9487 -0.6325 -0.3162  0.3162  1.5811


# 5.Histogram of the AirPassengers Dataset
# Question: Make a histogram for the AirPassengers dataset, start at 100 on the x-axis, and make the bins 150 units wide from 200 to 700.


#input:

data("AirPassengers")

hist(
  AirPassengers,
  breaks = seq(100, 700, by = 150),
  xlim = c(100, 700),
  main = "Histogram of AirPassengers",
  xlab = "Number of Passengers",
  ylab = "Frequency",
  col = "lightblue",
  right = FALSE
)

# #output:

# > data("AirPassengers")
# > 
#   > hist(
#     +   AirPassengers,
#     +   breaks = seq(100, 700, by = 150),
#     +   xlim = c(100, 700),
#     +   main = "Histogram of AirPassengers",
#     +   xlab = "Number of Passengers",
#     +   ylab = "Frequency",
#     +   col = "lightblue",
#     +   right = FALSE
#     + )
#http://127.0.0.1:34005/graphics/a84cb8c7-4f42-43c7-9b9b-3a48583f2465.png



# 6.Multiple Lines in a Single Plot Using mtcars
# Question: Obtain multiple lines in a line chart using a single plotting function in R. Use the mpg and qsec attributes of the mtcars dataset.

#input:
  data(mtcars)

values <- mtcars[, c("mpg", "qsec")]

matplot(
  values,
  type = "l",
  lty = 1,
  col = c("blue", "red"),
  main = "MPG and QSEC Line Chart",
  xlab = "Car Index",
  ylab = "Values"
)

legend(
  "topright",
  legend = c("MPG", "QSEC"),
  col = c("blue", "red"),
  lty = 1
)

#output:

# data(mtcars)
# > 
#   > values <- mtcars[, c("mpg", "qsec")]
# > 
#   > matplot(
#     +   values,
#     +   type = "l",
#     +   lty = 1,
#     +   col = c("blue", "red"),
#     +   main = "MPG and QSEC Line Chart",
#     +   xlab = "Car Index",
#     +   ylab = "Values"
#     + )
# > 
#   > legend(
#     +   "topright",
#     +   legend = c("MPG", "QSEC"),
#     +   col = c("blue", "red"),
#     +   lty = 1
#     + )
# > http://127.0.0.1:34005/graphics/fdb35c91-6f62-4c41-8427-03703b30faed.png


# 7.Linear Regression Using the Water Dataset
# Question: Download the water dataset from the R dataset library. Determine whether there is a linear relationship between mortality and hardness using the plot() function. Fit a linear regression model and predict mortality when hardness is 88.

#input:

# Install package (run only once)
install.packages("HSAUR")

# Load package and dataset
library(HSAUR)
data("water")

# Display dataset
print(water)

# Scatterplot
plot(
  water$hardness,
  water$mortality,
  main = "Mortality vs Water Hardness",
  xlab = "Water Hardness",
  ylab = "Mortality",
  pch = 19,
  col = "blue"
)

# Fit linear regression model
model <- lm(mortality ~ hardness, data = water)

# Add regression line
abline(model, col = "red", lwd = 2)

# Display regression results
print(summary(model))

# Predict mortality when hardness = 88
newdata <- data.frame(hardness = 88)
prediction <- predict(model, newdata = newdata)

cat("Predicted mortality =", prediction, "\n")

# output:
#   
#   > data("water")
# > 
#   > # Display dataset
#   > print(water)
# location          town mortality hardness
# 1     South          Bath      1247      105
# 2     North    Birkenhead      1668       17
# 3     South    Birmingham      1466        5
# 4     North     Blackburn      1800       14
# 5     North     Blackpool      1609       18
# 6     North        Bolton      1558       10
# 7     North        Bootle      1807       15
# 8     South   Bournemouth      1299       78
# 9     North      Bradford      1637       10
# 10    South      Brighton      1359       84
# 11    South       Bristol      1392       73
# 12    North       Burnley      1755       12
# 13    South       Cardiff      1519       21
# 14    South      Coventry      1307       78
# 15    South       Croydon      1254       96
# 16    North    Darlington      1491       20
# 17    North         Derby      1555       39
# 18    North     Doncaster      1428       39
# 19    South      East Ham      1318      122
# 20    South        Exeter      1260       21
# 21    North     Gateshead      1723       44
# 22    North       Grimsby      1379       94
# 23    North       Halifax      1742        8
# 24    North  Huddersfield      1574        9
# 25    North          Hull      1569       91
# 26    South       Ipswich      1096      138
# 27    North         Leeds      1591       16
# 28    South     Leicester      1402       37
# 29    North     Liverpool      1772       15
# 30    North    Manchester      1828        8
# 31    North Middlesbrough      1704       26
# 32    North     Newcastle      1702       44
# 33    South       Newport      1581       14
# 34    South   Northampton      1309       59
# 35    South       Norwich      1259      133
# 36    North    Nottingham      1427       27
# 37    North        Oldham      1724        6
# 38    South        Oxford      1175      107
# 39    South      Plymouth      1486        5
# 40    South    Portsmouth      1456       90
# 41    North       Preston      1696        6
# 42    South       Reading      1236      101
# 43    North      Rochdale      1711       13
# 44    North     Rotherham      1444       14
# 45    North     St Helens      1591       49
# 46    North       Salford      1987        8
# 47    North     Sheffield      1495       14
# 48    South   Southampton      1369       68
# 49    South      Southend      1257       50
# 50    North     Southport      1587       75
# 51    North South Shields      1713       71
# 52    North     Stockport      1557       13
# 53    North         Stoke      1640       57
# 54    North    Sunderland      1709       71
# 55    South       Swansea      1625       13
# 56    North      Wallasey      1625       20
# 57    South       Walsall      1527       60
# 58    South West Bromwich      1627       53
# 59    South      West Ham      1486      122
# 60    South Wolverhampton      1485       81
# 61    North          York      1378       71
# > 
#   > # Scatterplot
#   > plot(
#     +   water$hardness,
#     +   water$mortality,
#     +   main = "Mortality vs Water Hardness",
#     +   xlab = "Water Hardness",
#     +   ylab = "Mortality",
#     +   pch = 19,
#     +   col = "blue"
#     + )
# > 
#   > # Fit linear regression model
#   > model <- lm(mortality ~ hardness, data = water)
# > 
#   > # Add regression line
#   > abline(model, col = "red", lwd = 2)
# > 
#   > # Display regression results
#   > print(summary(model))
# 
# Call:
#   lm(formula = mortality ~ hardness, data = water)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -348.61 -114.52   -7.09  111.52  336.45 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1676.3556    29.2981  57.217  < 2e-16 ***
#   hardness      -3.2261     0.4847  -6.656 1.03e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 143 on 59 degrees of freedom
# Multiple R-squared:  0.4288,	Adjusted R-squared:  0.4191 
# F-statistic:  44.3 on 1 and 59 DF,  p-value: 1.033e-08
# 
# > 
#   > # Predict mortality when hardness = 88
#   > newdata <- data.frame(hardness = 88)
# > prediction <- predict(model, newdata = newdata)
# > 
#   > cat("Predicted mortality =", prediction, "\n")
# Predicted mortality = 1392.46 
#   http://127.0.0.1:34005/graphics/8ea228ed-8e72-4640-9a9f-b631cea0a4b6.png

# 8.Boxplot of MPG and Cylinders
# Question: Create a boxplot showing the relationship between mpg (miles per gallon) and cyl (number of cylinders) in the mtcars dataset available in R.
# 
# input:

data(mtcars)

mtcars$cyl <- as.factor(mtcars$cyl)

boxplot(
  mpg ~ cyl,
  data = mtcars,
  main = "MPG vs Number of Cylinders",
  xlab = "Number of Cylinders",
  ylab = "Miles Per Gallon",
  col = c("lightblue", "lightgreen", "lightpink")
)

# output:
#   
#   http://127.0.0.1:34005/graphics/1575185a-7f9d-47e6-a8a4-bf6ef0ed166d.png
# 
#   data(mtcars)
# > 
#   > mtcars$cyl <- as.factor(mtcars$cyl)
# > 
#   > boxplot(
#     +   mpg ~ cyl,
#     +   data = mtcars,
#     +   main = "MPG vs Number of Cylinders",
#     +   xlab = "Number of Cylinders",
#     +   ylab = "Miles Per Gallon",
#     +   col = c("lightblue", "lightgreen", "lightpink")
#     + )


# 9.Identify Outliers Using a Boxplot
# Question: A tennis coach wants to determine whether any team players have unusually high or low scores. Explain how to develop a boxplot to visualize the distribution of points scored by players. Give a suitable example.
# 
# input:

points <- c(10, 12, 15, 18, 20, 22, 24, 25, 28, 30, 85)

# Boxplot
boxplot(
  points,
  main = "Tennis Players' Scores",
  ylab = "Points Scored",
  col = "lightblue"
)


# output:
#   
#   http://127.0.0.1:34005/graphics/d5cff9f2-ef6a-4f62-9c70-ab91118b4cfb.png
#   
#   > points <- c(10, 12, 15, 18, 20, 22, 24, 25, 28, 30, 85)
# > 
#   > # Boxplot
#   > boxplot(
#     +   points,
#     +   main = "Tennis Players' Scores",
#     +   ylab = "Points Scored",
#     +   col = "lightblue"
#     + )
# > 
#   > # Quartiles
#   > Q1 <- quantile(points, 0.25)
# > Q3 <- quantile(points, 0.75)
# > 
#   > # Interquartile range
#   > IQR_value <- IQR(points)
# > 
#   > # Outlier limits
#   > lower_limit <- Q1 - 1.5 * IQR_value
# > upper_limit <- Q3 + 1.5 * IQR_value
# > 
#   > # Find outliers
#   > outliers <- points[
#     +   points < lower_limit | points > upper_limit
#     + ]
# > 
#   > cat("Q1:", Q1, "\n")
# Q1: 16.5 
# > cat("Q3:", Q3, "\n")
# Q3: 26.5 
# > cat("IQR:", IQR_value, "\n")
# IQR: 10 


