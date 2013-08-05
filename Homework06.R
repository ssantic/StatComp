## Using the version of gradient descent from lecture 10 and the log-
## likelihood function from lab, maximize the likelihood of the gamma dis-
## tribution on the cats' hearts. Start the optimization at the estimate you
## get from the method of moments.

gamma.loglike <- function(params) {
	return(sum(dgamma(cats$Hwt, shape = params[1], scale = params[2], log = TRUE)))
}


gradient <- function(f, x, deriv.steps, ...) {
  p <- length(x)
  stopifnot(length(deriv.steps) == p)
  f.old <- f(x, ...)
  x.new <- matrix(rep(x, times = p), nrow = p) + diag(deriv.steps, nrow = p)
  f.new <- apply(x.new, 2, f, ...)
  gradient <- (f.new - f.old)/deriv.steps 
  return(gradient)
}


gradient.descent <- function(f, x, max.iterations, step.scale,
  stopping.deriv, ...) {
  for (iteration in 1:max.iterations) {
    grad <- gradient(f, x, ...)
    if(all(abs(grad) < stopping.deriv)) { break() }
    x <- x - step.scale*grad
  }
  fit <- list(argmin = x, final.gradient = grad, final.value = f(x),
    iterations = iteration)
  return(fit)
}



## What command do you use to maximize the log-likelihood?
## What is the estimate? What is the log-likelihood there? The gradient?

fit.mle <- gradient.descent(f = gamma.loglike, x = cat.hearts.est, max.iterations=1e4,
step.scale = c(-1e-2, -1e-4), stopping.deriv = 1e-4, deriv.steps = c(1e-6,1e-6))

fit.mle$argmin
fit.mle$final.value
fit.mle$final.gradient




## We need standard errors for the estimated parameters. If the model
## is right, we can get standard errors for maximum likelihood estimates from
## the second derivatives of the log-likelihood. (The second derivatives tell
## us how sharp the maximum of the likelihood is.) Speciffcally:
## Var(Theta) = -H^(-1)(Theta), where H is the Hessian of the log-likelihood, 
## its matrix of second par tial derivatives. (This is sometimes called the observed
## information matrix.)

## Install the package numDeriv. Its function hessian will calculate
## a numerical approximation to the Hessian of a given function at a
## given point. What is the Hessian matrix of the log-likelihood at the
## MLE?

install.packages("numDeriv")
library(numDeriv)

H <- hessian(gamma.loglike, fit.mle$argmin)



## What is the inverse of the Hessian matrix?

inverse.H <- solve(H)



## What standard errors does this imply for the shape and scale
## estimate? Hint: Remember how the standard error of an estimate
## relates to the variance of the estimator.

var.estimates <- -inverse.H
se.estimates <- sqrt(var.estimates)




## An alternative to using the Hessian is to use the jack-knife. This does
## not assume that the model is correct, but does mean we need to be able
## to re-compute the MLE after deleting points from the data set.

## Write a function, make.gamma.loglike, which takes in a data
## vector x and returns a log-likelihood function. Check that if x is
## cats$Hwt, then make.gamma.loglike returns a function which matches
## your old gamma.loglike at multiple parameter values.

make.gamma.loglike <- function(data) {
	gamma.loglike <- function(params) {
		return(sum(dgamma(data, shape = params[1], scale = params[2], log = TRUE)))
	}
	return(gamma.loglike)	
}



## Write a function to calculate jack-knife standard errors for the
## MLE of the gamma distribution on an arbitrary data vector. The
## only argument should be the data vector. It should return a vec-
## tor of length two, giving the standard errors for the shape and the
## scale parameters. Your code should use make.gamma.loglike and
## gradient.descent, and may use a for loop, or sapply. Hint: look
## at the solutions to lab 4.

gamma.MLE.jackknife <- function(data) {
	n <- length(data)
	jack.ests <- matrix(0, n, ncol=2)
	for (i in 1:n) {
		moments <- gamma.est(data[-i])
		loglike <- make.gamma.loglike(data[-i]) 
		jack.ests[i,] <- gradient.descent(f = loglike , x = moments, max.iterations=1e4,
		step.scale = c(-1e-2, -1e-4), stopping.deriv = 1e-4, deriv.steps = c(1e-6,1e-6))$argmin
	}
	jack.var <- ((n-1)^2/n)*apply(jack.ests, 2, var)
	jack.se <- sqrt(jack.var)
	return(jack.se)
}



## Are the maximum-likelihood and the method-of-moments estimates
## compatible with each other?

signif(fit.mle$argmin - cat.hearts.est, 3)



## What is the likelihood (not the log-likelihood) at the maximum?

signif(exp(fit.mle$final.value), 3)