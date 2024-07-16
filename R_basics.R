#------------------------------------------------#
# A brief introduction to R Programming Language #
#            Ashok Krishnamurthy, PhD            #
#------------------------------------------------#

x <- available.packages() 
length(unique(rownames(x)))

sessionInfo()   # Print version information about R, the OS and attached or loaded packages.

search()        # Simplified list of the attached packages in a session

(.packages())   # Packages loaded (by default)

# installed.packages()

# Simple Arithmetic Operations: R works like a normal calculator #

2+2

4^3

2^3 - 3^2

3^-5

?log

args(log)

log(100)

log(100, base = 10)

log(100, 10)

log10(100)

# Descriptive Statistics

v <- c(12, 9, 10, 11, 8, 4)

v

sort(v)

length(v)

mean(v)

median(v)

sd(v)

var(v)

IQR(v)

range(v)

quantile(v,  probs = c(10, 50, 80)/100)

# use tab completion for known functions. Double tab gives all possibilities

quantile(v)

fivenum(v)

# fivenum function returns Tukey's 5-number (min, lower-hinge, median, upper-hinge, max) summary.
# The lower hinge is the median of the lower half of the data up to and including the median.
# The upper hinge is the median of the upper half of the data up to and including the median.

summary(v)

# sample.mode <- names(sort(-table(v)))[1]

x <- c(1,2,NA,3)
mean(x) # returns NA
mean(x, na.rm=TRUE) # returns 2
b <- seq(1:10)

plot(b, b^2)

plot(b, b^2, type = "l")

plot(b, b^2, type = "l", col = "blue")

#########

thickness <-    c(3.468, 3.428, 3.509, 3.516, 3.461, 3.492, 3.478, 3.556, 3.482, 3.512,
			3.490, 3.467, 3.498, 3.519, 3.504, 3.469, 3.497, 3.495, 3.518, 3.523,
			3.458, 3.478, 3.443, 3.500, 3.449, 3.525, 3.461, 3.489, 3.514, 3.470,
			3.561, 3.506, 3.444, 3.479, 3.524, 3.531, 3.501, 3.495, 3.443, 3.458,
			3.481, 3.497, 3.461, 3.513, 3.528, 3.496, 3.533, 3.450, 3.516, 3.476,
			3.512, 3.550, 3.441, 3.541, 3.569, 3.531, 3.468, 3.564, 3.522, 3.520,
			3.505, 3.523, 3.475, 3.470, 3.457, 3.536, 3.528, 3.477, 3.536, 3.491,
			3.510, 3.461, 3.431, 3.502, 3.491, 3.506, 3.439, 3.513, 3.496, 3.539,
			3.469, 3.481, 3.515, 3.535, 3.460, 3.575, 3.488, 3.515, 3.484, 3.482,
			3.517, 3.483, 3.467, 3.467, 3.502, 3.471, 3.516, 3.474, 3.500, 3.466)

hist(thickness, col = "green",  main = "Analysis of Plating Thickness", xlim = c(3.4,3.6), xlab = "Plating Thickness (mil)", ylab = "Frequency")

?hist

vignette("dplyr") # more in-depth with a walk through

browseVignettes("dplyr") # opens in your default browser

getwd()

# setwd() # To set your working directory

dir()

# install.packages("Rcmdr", dependencies = T)

library(Rcmdr)

citation("Rcmdr")
print(citation("Rcmdr"), bibtex=TRUE)

# citation("shiny")
# update.packages() # recommended every once in a while
#
# ################## MISC ##################
# library(car)
# attach(mtcars)
# 
# scatter3d(wt, disp, mpg) 
# 
# ##################
# 
# #http://stat.ethz.ch/R-manual/R-patched/library/stats/html/00Index.html 
# 
# Group.A <- c(1.31, 1.45, 1.12, 1.16, 1.3, 1.5, 1.2, 1.22, 1.42, 1.14, 1.23, 1.59, 1.11, 1.1, 1.53, 1.52, 1.17, 1.49, 1.62, 1.29)
# Group.B <- c(1.13, 1.71, 1.39, 1.15, 1.33, 1, 1.03, 1.68, 1.76, 1.55, 1.34, 1.47, 1.74, 1.74, 1.19, 1.15, 1.2, 1.59, 1.47)
# 
# length(Group.A)
# length(Group.B)
# 
# boxplot(Group.A)
# boxplot(Group.B)
# 
# group <- c(rep("A",20), rep("B",19))
# dat <-   c(Group.A, Group.B)
# 
# boxplot(dat ~ group, xlab = "Group",  ylab = "Zinc Concentration in mg/ml", main = "Side-by-side Boxplots")
# 
# #########
# 
# #install.packages("scatterplot3d")
# 
# library(scatterplot3d)
# attach(mtcars)
# 
# mtcars
# 
# mtcars$mpg
# 
# mpg
# 
# ?mtcars
# 
# s3d <-scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot")
# fit <- lm(mpg ~ wt+disp)
# #s3d$plane3d(fit)
#  
# #########
# 
# curve(x^3, 0, 10, xlab = "x", ylab = "y", main = "Plot of y = x^3")
# 
# #########
# 
# y <- expression(x^3)
# 
# D(y, "x")
# 
# #########
# 
# yy <- expression(4*x^3 -12*x +5)
# 
# D(yy, "x")
# 
# ##########
# 
# trig.exp <- expression(sin(cos(x + y^2)))
# D.sc <- D(trig.exp, "x")
# 
# D.sc
# 
# ?D
# 
# ?deriv
# 
# #########
# 
# x = 2*pi
# 
# y = sin(x)
# 
# y
# #########
# help.search("linear programming")
# 
# #install.packages("lpSolve")
# #install.packages("boot")
# library(boot)
# ?simplex
# 
# enj <- c(200, 6000, 3000, -200)
# fat <- c(800, 6000, 1000, 400)
# vitx <- c(50, 3, 150, 100)
# vity <- c(10, 10, 75, 100)
# vitz <- c(150, 35, 75, 5)
# simplex(a = enj, A1 = fat, b1 = 13800, A2 = rbind(vitx, vity, vitz), b2 = c(600, 300, 550), maxi = TRUE)
