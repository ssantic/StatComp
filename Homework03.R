## Write a function, called mse(), which calculates the mean squared
## error of the model on a given data set. mse() should take three
## arguments: a numeric vector of length two, the first component standing
## for y0 and the second for a; a numerical vector containing the values of N;
## and a numerical vector containing the values of Y. The function should
## return a single numerical value. The latter two arguments should have as
## the default values the columns pop and pcgmp (respectively) from the gmp
## data frame in Lab 2. Your function may not use for() or any other loop.

mse <- function(x, N = gmp$pop, Y = gmp$pcgmp) {
	if (length(x) != 2) {
		print("The input vector x must have a length 2.")
		break()
	}
	return(mean((Y - x[1]*N^x[2])^2))
}



## Check that, with the default data, you get the following values.

mse(c(6611,0.15))
# [1] 207057513

mse(c(5000,0.10))
# [1] 298459915



## Write a function, mse.grad(), which approximates the gradient of
## the mean squared error. It should take five arguments: a vector of length
## 2 giving the point at which we want the gradient; the increment for y0;
## the increment for a; and the vectors containing the values of N and Y.
## Provide default values for everything except the first vector. mse.grad()
## should return a length two vector containing the gradient. This function
## must call your mse().

mse.grad <- function(coordinates, increments = c(1, 0.005), N = gmp$pop, Y = gmp$pcgmp) {
	gradient <- c()
	mse.0 <- mse(coordinates)
	for (i in 1:2) {
		new.coordinates <- coordinates
		new.coordinates[i] <- coordinates[i] + increments[i]
		new.mse <- mse(new.coordinates)
		gradient[i] <- (new.mse - mse.0)/increments[i]
		}
	return(gradient)
}



## Check that you get the following values.

mse.grad(c(6611,0.15), c(10,1e-5))
# [1] 1.650303e+05 1.429197e+10

mse.grad(c(5000,0.10), c(7,-1e-5))
# [1] -109811.2 -7129496031.7



## Write a function, plm(), which estimates the parameters y0 and a
## of the model by minimizing the mean squared error, and minimizes
## the MSE by gradient descent. It should take the following arguments:
## an initial guess for y0; an initial guess for a; a vector containing the N
## values; a vector containing the Y values; the increments for calculating
## the gradient; the scaling factor r; the threshold below which the gradient
## is considered effectively zero; and the maximum number of iterations. All
## arguments except the initial guesses should have suitable default values.
## It should return a list with the following components: the final guess for
## y0; the final guess for a; the final value of the MSE; the final gradient; the
## number of iterations taken; a flag for whether the function stopped before
## running out of iterations. Your function must call those you wrote in earlier questions, 
## and the appropriate arguments to plm() should be passed on to them.
## Hint: See the slides for lecture 4.

plm <- function(params, N = gmp$pop, Y = gmp$pcgmp, increments = c(10, 1e-5), r = c(1e-2, 1e-12), min.threshold = 1e-2, max.iterations = 1e5) {
	for (iterations in 1:max.iterations) {
		gradient <- mse.grad(params)
		params <- params - r*gradient
		if (all(abs(gradient)) < min.threshold) {break()}		
	}
	result <- list(y0 = params[1], a = params[2], gradient = gradient, iterations = iterations, converged = iterations < max.iterations)
	return(result)
}