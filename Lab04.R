## This is the accompanying gamma.est
## function for this lab.

gamma.est <- function(data) {
  m <- mean(data)
  v <- var(data)
  s <- v/m
  a <- m/s
  return(c(a=a,s=s))
}



## Take the first three cats from the cats from the data frame. Using
## gamma.est, estimate a and s for each of the three pairs of cats. Calculate
## the jackknife standard errors for a and s this would imply. Do not write
## a function for this; instead, repeatedly call gamma.est, store the results,
## and calculate variances from the estimates.

library(MASS)
data(cats)

hearts <- cats[1:3,]$Hwt

subsample.1 <- gamma.est(hearts[-1])
subsample.2 <- gamma.est(hearts[-2])
subsample.3 <- gamma.est(hearts[-3])

a.estimates <- c(subsample.1[1], subsample.2[1], subsample.3[1])
s.estimates <- c(subsample.1[2], subsample.2[2], subsample.3[2])

sqrt(4/3*var(a.estimates))
sqrt(4/3*var(s.estimates))



## Write a function, gamma.jackknife(), which takes a data vector and
## returns the jackknife standard errors in estimates of a and s. (It does not
## have to return the estimate itself.) It should call gamma.est. You may
## use a for loop.

gamma.jackknife <- function(data) {
	a.estimates <- c()
	s.estimates <- c()
	n <- length(data)
	for (i in 1:n) {
		a.estimates <- c(a.estimates, gamma.est(data[-i])[1])
		s.estimates <- c(s.estimates, gamma.est(data[-i])[2])
	}
	result.a <- sqrt(((n-1)^2/n)*var(a.estimates))
	result.s <- sqrt(((n-1)^2/n)*var(s.estimates))
	return(c(result.a, result.s))	
}



## When run on the first three cats, does the output of gamma.jackknife
## agree with your calculation in problem 1?

gamma.jackknife(hearts)
#[1] 413.99123   0.21679



## Use gamma.jackknife to find standard errors for a and s for the whole
## data.

gamma.jackknife(cats$Hwt)



## Estimate the a and s parameters separately for male and female cats.
## Find jackknife standard errors for both parameters for both estimates
## four standard errors in all.

est.males <- gamma.est(subset(cats, cats$Sex == "M")$Hwt)
est.females <- gamma.est(subset(cats, cats$Sex == "F")$Hwt)

a.sd.males <- gamma.jackknife(subset(cats, cats$Sex =="M")$Hwt)[1]
s.sd.males <- gamma.jackknife(subset(cats, cats$Sex =="M")$Hwt)[2]
a.sd.females <- gamma.jackknife(subset(cats, cats$Sex =="F")$Hwt)[1]
s.sd.females <- gamma.jackknife(subset(cats, cats$Sex =="F")$Hwt)[2]



## When two independent estimated quantities d1 and d2 have standard
## errors s1 and s2, the standard error of their difference, d1-d2, is
## sqrt(s1^2 + s2^2). Calculate the difference in both estimated parameters 
## between the female and male cats, and the standard error of the differences.

diff.est <- est.males - est.females
sd.diff.s <- sqrt(a.sd.males^2 + a.sd.females^2)
sd.diff.a <- sqrt(s.sd.males^2 + s.sd.females^2)