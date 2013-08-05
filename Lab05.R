## Identifying outliers in data is an important part of statistical analyses. One
## simple rule of thumb (due to John Tukey) for finding outliers is based on the
## quartiles of the data: the first quartile Q1 is the value >= 1/4 of the data, the
## second quartile Q2 or the median is the value >= 1/2 of the data, and the third
## quartile Q3 is the value >= 3/4 of the data. The interquartile range, IQR, is
## Q3-Q1. Tukey's rule says that the outliers are values more than 1.5 times the
## interquartile range from the quartiles - either below Q1-1.5*IQR, or above
## Q3+1.5*IQR. Consider the data values:

x <- c(2.2, 7.8, -4.4, 0.0, -1.2, 3.9, 4.9, -5.7, -7.9, -4.9, 28.7, 4.9)

## We will use these as part of writing a function to identify outliers according to
## Tukey's rule. Our function will be called tukey.outlier, and will take in a
## data vector, and return a Boolean vector, TRUE for the outlier observations and
## FALSE elsewhere.

## Calculate the first quartile, the third quartile, and the inter-quartile
## range of x. Some built-in R functions calculate these; you cannot use
## them, but you could use other functions, like sort and quantile.

Q1 <- fivenum(x)[2]
Q3 <- fivenum(x)[4]
IQ.range <- Q3 - Q1



## Write a function, quartiles, which takes a data vector and returns
## a vector of three components, the first quartile, the third quartile, and the
## inter-quartile range. Show that it gives the right answers on x. (You do
## not have to write a formal test for quartiles.)

quartiles <- function (data) {
	Q1 <- fivenum(data)[2]
	Q3 <- fivenum(data)[4]
	IQ.range <- Q3 - Q1
	return(c(Q1 = Q1, Q3 = Q3, IQR = IQ.range))
}



## Which points in x are outliers, according to Tukey's rule, if any?

for (i in 1:length(x)) {
	if (x[i] < Q1 - 1.5*IQ.range) { 
		print(x[i]) 
	}
	else if (x[i] > Q3 + 1.5*IQ.range) {
		print(x[i])
	}
}



## Write a function, test.tukey.outlier, which tests the function
## tukey.outlier against your answer in the previous question. This func-
## tion should return TRUE if tukey.outlier works properly; otherwise, it
## can either return FALSE, or an error message, as you prefer.

test.tukey.outlier <- function(foo) {
	return(foo == 28.7)
}



## Write tukey.outlier, using your quartiles function. Show that it
## passes test.tukey.outlier.

tukey.outlier <- function(data) {
	Q <- quartiles(data)
	outliers <- c()
	for (i in 1:length(data)) {
		if (data[i] < Q[1] - 1.5*Q[3]) { 
			outliers <- c(outliers, data[i]) 
		}
		else if (data[i] > Q[2] + 1.5*Q[3]) {
			outliers <- c(outliers, data[i])
		}
	}
	return(outliers)
}


test.tukey.outlier(tukey.outlier(data))



## Which data values should be outliers in -x?

tukey.outlier(-x)



## Which data values should be outliers in 100*x?

tukey.outlier(100*x)



## Modify test.tukey.outlier to include tests for these cases.

test.tukey.outlier <- function(foo) {
		if (28.7 == foo) {return(28.7 == foo)}
		if (-28.7 == foo) {return(-28.7 == foo)}
		if (220 == as.integer(foo)) {return(220 == as.integer(foo))}
		return(FALSE)
}



## According to Tukey's rule, which points in the next vector are out-
## liers?

y <- c(11.0, 14.0, 3.5, 52.5, 21.5, 12.7, 16.7, 11.7, 10.8, -9.2, 12.3, 13.8)
tukey.outlier(y)