## Using the read.table command, load the gmp.dat file into a data frame
## called "gmp".

gmp <- read.table("http://www.stat.cmu.edu/~cshalizi/statcomp/labs/02/gmp.dat")



## The last two columns give, for each city, the total gross metropolitan
## product (Y N) and the per-capita metropolitan product (Y). Use this to
## make a vector which has the population (N) for each city.

pop <- gmp$gmp/gmp$pcgmp



## Add the vector of populations to the gmp data frame as a new column,
## named pop.

gmp <- data.frame(gmp, pop)



## Calculate the MSE of the power-law model, when y0 = 6611 (in dollars)
## and a = 0.15.

mean((gmp$pcgmp - 6611*gmp$pop^0.15)^2)



## Use seq() to make a sequence of values of a, from 0.10 to 0.15, in steps
## of 0.005.

a <- seq(from = 0.10, to = 0.15, by = 0.005)



## Calculate the MSE for each value of a in that sequence, and store all the
## MSEs in a vector. (Hint: There are many ways to do this, including a for
## loop.)

MSEs <- c()
for (i in 1:length(a)) {
	mse <- mean((gmp$pcgmp - 6611*gmp$pop^a[i])^2)	
	} 
MSEs <- c(MSEs, mse)
}



## Plot the square roots of the MSEs vs. a. What are the units of the root
## mean squared error? What value of a, among those you looked at, leads
## to the least squared error?

plot(a, sqrt(MSEs))



## Explain what the following code does.

maximum.iterations <- 100
deriv.step <- 1/1000
step.scale <- 1e-12
stopping.deriv <- 1/100
iteration <- 0
deriv <- Inf
a <- 0.15
while ((iteration < maximum.iterations) && (deriv > stopping.deriv)) {
iteration <- iteration + 1
mse.1 <- mean((gmp$pcgmp - 6611*gmp$pop^a)^2)
mse.2 <- mean((gmp$pcgmp - 6611*gmp$pop^(a+deriv.step))^2)
deriv <- (mse.2 - mse.1)/deriv.step
a <- a - step.scale*deriv
}
list(a=a,iterations=iteration,converged=(iteration < maximum.iterations))



## What value of a does the code estimate? What is the root mean squared
## error at this value of a? How well does this agree with what you expect
## from the plot you made for question 7?



## Re-run this code, but change the initial value of a from 0.15, to your
## estimate from question 9. What happens? Why?