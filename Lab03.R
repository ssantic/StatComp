## The data is contained in a data frame called cats, in an R library
## (or package) called MASS. (This package is part of the standard R installation.)
## This records the sex of each cat, its weight in kilograms, and the
## weight of its heart in grams. Load the data and execute the summary() function.

library(MASS)
data(cats)
summary(cats)



## Calculate the mean, standard deviation, and variance of the heart
## weights.

mean(cats$Hwt)
sd(cats$Hwt)
var(cats$Hwt)



## Plug the mean and variance of the cats' hearts into your formulas and
## get estimates of a and s. What are they? Do not report them to more
## signiffcant digits than is reasonable.

a <- round(mean(cats$Hwt)/(sd(cats$Hwt)^2), 5)
s <- round((sd(cats$Hwt)^2)/mean(cats$Hwt), 5)



## Write a function, gamma.est(), which takes as input a vector of
## numbers, calculates the mean and variance, and returns the estimate of a
## and s.

gamma.est <- function(x) {
	a <- round(mean(x)^2/var(x), 5)
	s <- round(var(x)/mean(x), 5)
	return(c(a,s))
}



## Plot a histogram of these weights. Using curve and dgamma, add the
## gamma pdf with the estimated shape and scale parameters.

hist(cats$Hwt)
curve(dgamma(x, shape = a, scale = s), from = 0, to = 25)



## Estimate the a and s separately for all the male cats and all the female
## cats, using gamma.est.

Fcats <- subset(cats, cats$Sex == "F")
Mcats <- subset(cats, cats$Sex == "M")
a_Fcats <- gamma.est(Fcats$Hwt)[1]
s_Fcats <- gamma.est(Fcats$Hwt)[2]
a_Mcats <- gamma.est(Mcats$Hwt)[1]
s_Mcats <- gamma.est(Mcats$Hwt)[2]



## Plot the two estimated density functions for the two sexes on the same
## plot, using curve and dgamma.



## Randomly divide the cats into two groups, of the same size as the
## male and female cats. Estimate the gamma parameters separately for
## your random groups. Do this multiple times. Report the typical range of
## differences in parameters between the two artiffcial groups (as well as the
## commands you used). Hint: Recipe 8.5, and/or 8.7.

cats1 <- sample(cats$Hwt, nrow(Fcats))
cats2 <- sample(cats$Hwt, nrow(Mcats))
a_cats1 <- gamma.est(cats1)[1]
s_cats1 <- gamma.est(cats1)[2]
a_cats2 <- gamma.est(cats2)[1]
s_cats2 <- gamma.est(cats2)[2]