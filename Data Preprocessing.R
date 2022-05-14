##Assignment 1: Data Preprocessing##
##Student ID: 20047638
##User ID: ZM21AAS

# Dataset IRIS is chosen
data("iris")
iris
#Summarising the data
summary(iris)
library(Hmisc)
describe(iris)

library(pastecs)
stat.desc(iris)

library(psych)
pairs.panels(iris)

table(iris$Sepal.Length, iris$Sepal.Width)
by(iris, iris$Sepal.Length, summary)

# box plot with outliers 
library(ggstatsplot)
boxplot(iris, main = 'Box Plot with Outliers')$out

#Finding the IQR for each column
iqr_sl <- IQR(iris$Sepal.Length)
iqr_sw <- IQR(iris$Sepal.Width)
iqr_pl <- IQR(iris$Petal.Length)
iqr_pw <- IQR(iris$Petal.Width)

library(dplyr) 
# Checking outliers for Sepal Length and converting them to NA
Q1_sl<- quantile(iris$Sepal.Length, 0.25)
Q3_sl<- quantile(iris$Sepal.Length, 0.75)
x_sl <- as.numeric(c(Q1_sl-1.5*iqr_sl, Q3_sl+1.5*iqr_sl))
summary(iris$Sepal.Length)
iris$Sepal.Length[iris$Sepal.Length < x_sl[1] | iris$Sepal.Length > x_sl[2]] = NA
iris$Sepal.Length

# Checking outliers for Sepal Width and converting them to NA
Q1_sw <- quantile(iris$Sepal.Width, 0.25)
Q3_sw <- quantile(iris$Sepal.Width, 0.75)
x_sw <- as.numeric(c(Q1_sw-1.5*iqr_sw, Q3_sw+1.5*iqr_sw))
summary(iris$Sepal.Width)
iris$Sepal.Width[iris$Sepal.Width < x_sw[1] | iris$Sepal.Width > x_sw[2]] = NA
iris$Sepal.Width

# Checking outliers for Petal Length and converting them to NA
Q1_pl <- quantile(iris$Petal.Length, 0.25)
Q3_pl <- quantile(iris$Petal.Length, 0.75)
x_pl <- as.numeric(c(Q1_pl-1.5*iqr_pl, Q3_pl+1.5*iqr_pl))
summary(iris$Petal.Length)
iris$Petal.Length[iris$Petal.Length < x_pl[1] | iris$Petal.Length > x_pl[2]] = NA
iris$Petal.Length

# Checking outliers for Petal Width and converting them to NA
Q1_pw <- quantile(iris$Petal.Width, 0.25)
Q3_pw <- quantile(iris$Petal.Width, 0.75)
x_pw <- as.numeric(c(Q1_pw-1.5*iqr_pw, Q3_pw+1.5*iqr_pw))
summary(iris$Petal.Width)
iris$Petal.Width[iris$Petal.Width < x_pw[1] | iris$Petal.Width > x_pw[2]] = NA
iris$Petal.Width

# Z-score approach to controlling outliers
# Standardizing the columns 
library(robustHD)
# Standardizing the sepal length
Z_sl <- (iris$Sepal.Length - mean(iris$Sepal.Length, na.rm = TRUE)) /
            sd(iris$Sepal.Length, na.rm = TRUE) 
#Summary of Standardized Sepal Length
summary(Z_sl)
var(Z_sl)
#Summary of Original Sepal Length
summary(iris$Sepal.Length)
var(iris$Sepal.Length)
# Standardizing the sepal Width
Z_sw <- (iris$Sepal.Width - mean(iris$Sepal.Width, na.rm = TRUE)) /
  sd(iris$Sepal.Width, na.rm = TRUE)
# Summary of Standardized Sepal Width
summary(Z_sw)
var(Z_sw, na.rm = TRUE)
# Summary of original sepal width
summary(iris$Sepal.Width)
var(iris$Sepal.Width, na.rm = TRUE)
# Standardizing the petal length
Z_pl <- (iris$Petal.Length - mean(iris$Petal.Length, na.rm = TRUE)) /
  sd(iris$Petal.Length, na.rm = TRUE) 
# Summary of Standardized Petal Length
summary(Z_pl)
var(Z_pl)
# Summary of original Petal Length
summary(iris$Petal.Length)
var(iris$Petal.Length)
# Standardizing the petal Width
Z_pw <- (iris$Petal.Width - mean(iris$Petal.Width, na.rm = TRUE)) /
  sd(iris$Petal.Width, na.rm = TRUE) 
# Summary of Standardized Petal Width
summary(Z_pw)
var(Z_pw)
# Summary of original Petal width
summary(iris$Petal.Width)
var(iris$Petal.Width)

#Box Plot for clean dataset
boxplot(iris, main = 'Box Plot without Outliers')$out

#performing the Shapiro-Wilk test on original continuous columns
shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.Width)
shapiro.test(iris$Petal.Length)
shapiro.test(iris$Petal.Width)

#performing the Box-Cox transformation for one column (for Sepal Length)
library(MASS)
Box_Cox <- boxcox(iris$Sepal.Length ~ iris$Petal.Length)
lambda <- Box_Cox$x[which.max(Box_Cox$y)]
Trans_sl <- (iris$Sepal.Length^lambda-1)/lambda

#Plotting the results
par(mfrow = c(1, 2))
#Q-Q plot for original model
qqnorm(iris$Sepal.Length, main = 'Original Model')
qqline(iris$Sepal.Length)
#Q-Q plot for Box-Cox transformed model
qqnorm(Trans_sl, main = 'Transformed Model')
qqline(Trans_sl)

#performing Shapiro-Wilk test for the transformed Sepal Length Column
shapiro.test(Trans_sl)
#comapring with Shapiro-Wilk test of original column
shapiro.test(iris$Sepal.Length)

