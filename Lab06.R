## In this lab, we begin working with likelihood functions, continuing to use
## the data on the heart weight of cats from previous labs.

library(MASS)
data(cats)



## Fit the gamma distribution to the cats' hearts', using the "method of
## moments" from Lab 3.

gamma.est <- function(data) {
	a <- round(mean(data)^2/var(data), 5)
	s <- round(var(data)/mean(data), 5)
	return(c(a = a, s = s))
}

cat.hearts.est <- gamma.est(cats$Hwt)



## Calculate the log-likelihood of the shape and scale parameters you
## just estimated. The answer, rounded to the nearest integer, should be
## -326. Hint: ?dgamma.

sum(dgamma(cats$Hwt, shape = cat.hearts.est["a"], scale = cat.hearts.est["s"], log = TRUE))



## Write a function, gamma.loglike, which takes in a vector, contain-
## ing a shape and a scale parameter, and returns the log-likelihood of the
## cats' hearts' masses. Check that when you run gamma.loglike with the
## estimate from question 1, you get the log-likelihood from question 2.

gamma.loglike <- function(params) {
	return(sum(dgamma(cats$Hwt, shape = params[1], scale = params[2], log = TRUE)))
}

gamma.loglike(cat.hearts.est)



## Make a contour plot of the log-likelihood, with the shape parameter
## on the horizontal axis (range 1 to 40) and the scale parameter on the
## vertical (range 0.01 to 1). Add a point indicating the location of your
## moment-based estimate from question 1. Hints: Consider surface.0 from
## lecture 10. Also, you will probably want to increase the number of
## levels on the contour plot above the default of 10.

surface.0 <- function(f, from.x = 0, to.x = 1, from.y = 0, to.y = 1, n.x = 101,
  n.y = 101, ...) {
  x.seq <- seq(from = from.x, to = to.x, length.out = n.x)
  y.seq <- seq(from = from.y, to = to.y, length.out = n.y)
  plot.grid <- expand.grid(x = x.seq, y = y.seq)
  z.values <- apply(plot.grid, 1, f)
  z.matrix <- matrix(z.values, nrow = n.x)
  contour(x = x.seq, y = y.seq, z = z.matrix, ...)
  invisible(list(x = x.seq, y = y.seq, z = z.matrix))
}

surface.0(gamma.loglike, from.x = 1, to.x = 40, from.y = 0.01, to.y = 1, nlevels = 100,
xlab = "shape", ylab = "scale")

points(x = cat.hearts.est["a"], y = cat.hearts.est["s"], col = "red")



## Use the plot from the previous question to locate the region where
## the likelihood seems to be largest. Make a new plot which zooms in on
## this region.

surface.0(gamma.loglike, from.x = 30, to.x = 40, from.y = 0.6, to.y = 1, nlevels = 100,
xlab = "shape", ylab = "scale")