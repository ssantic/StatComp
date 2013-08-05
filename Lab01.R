## Generate 100 random values from the standard exponential
## distribution and store them in a vector x1. Find the mean and standard
## deviation of x1.

x1 <- rexp(n = 100)
mean(x1)
sd(x1)



## Repeat, but changing the rate to 0.1, 0.5, 5 and 10, storing the
## results in different vectors.

x2 <- rexp(n = 100, rate = 0.1)
x3 <- rexp(n = 100, rate = 0.5)
x4 <- rexp(n = 100, rate = 5)
x5 <- rexp(n = 100, rate = 10)



## Make scatterplots of
## (a) the five means versus the five rates;
## (b) the standard deviations versus the rates;
## (c) and the means versus the standard deviations.
## Explain, in words, what's going on in these plots.

rates <- c(1, 0.1, 0.5, 5, 10)
means <- c(mean(x1), mean(x2), mean(x3), mean(x4), mean(x5))
plot(rates, means)

sdevs <- c(sd(x1), sd(x2), sd(x3), sd(x4), sd(x5))
plot(rates, sdevs)

plot(sdevs, means)



## Generate one million numbers from the standard exponential
## istribution and store them in a vector called y. Find the mean and
## standard deviation.

y <- rexp(n = 1000000)
mean(y)
sd(y)



## Plot a histogram of y. Does it match the function 1 - e^(-x)?
## Should it?

hist(y)



## Create a matrix, y.mat, with the values in y, 1000 rows, and
## 1000 columns.

y.mat <- matrix(y, nrow = 1000)



##Explain what happens when you create a histogram of y.mat.

hist(y.mat)



## Find the mean of the 371st column of y.mat.

mean(y.mat[,371])



## Find the means of all 1000 columns of y.mat. Plot the histogram
## of column means. Explain why its shape does not match the histogram in
## problem 5.

colMeans(y.mat)
hist(colMeans(y.mat))



## Find the mean of all of the entries in y which are strictly
## greater than 1.

mean(y[y > 1])



##Explain what these two commands do:

sum(y > 1)
mean(y[y.mat > 1])



## Take the square of each number in y, and find their mean.
## Explain this in terms of the mean and standard deviation of y. Hint: think
## carefully about the formula R uses to calculate the standard deviation.

mean(y^2)