## HANDLING MISSING VALUES. Load the rainfall data set from homework 1.

rain.df <- read.table("http://www.stats.uwo.ca/faculty/braun/data/rnf6080.dat")



## The entries of -999 represent missing observations, not hours of
## negative rainfall. Replace the negative numbers with NA.

rain.df[rain.df == -999] <- NA



## Run the 6th column of the cleaned data through your tukey.outlier
## function. What error message do you get? Where is the error hap-
## pening? Why is it happening?

tukey.outlier(rain.df[,6])



## Write a test case, based on the x vector from lab, which shows
## how you would like your outlier-detector to handle NA values. Add it
## to your testing function.

x <- c(2.2, 7.8, NA, 0.0, -1.2, 3.9, NA, -5.7, -7.9, -4.9, 28.7, 4.9)

test.tukey.outlier <- function(foo) {
	if (foo == NA) {return(FALSE)}
		else {return(foo == 28.7)}
}



## Modify your code for tukey.outlier until it passes all your test
## cases, including the new one with NA.

quartiles <- function (data) {
	Q1 <- fivenum(data, na.rm = TRUE)[2]
	Q3 <- fivenum(data, na.rm = TRUE)[4]
	IQ.range <- Q3 - Q1
	return(c(Q1 = Q1, Q3 = Q3, IQR = IQ.range))
}



tukey.outlier <- function(data) {
	Q <- quartiles(data)
	data.na.removed <- na.omit(data)
	outliers <- c()
	for (i in 1:length(data.na.removed)) {
		if (data.na.removed[i] < Q[1] - 1.5*Q[3]) { 
			outliers <- c(outliers, (data.na.removed[i])) 
		}
		else if (data.na.removed[i] > Q[2] + 1.5*Q[3]) {
			outliers <- c(outliers, (data.na.removed[i])) 
		}
	}
	return(outliers)
}



## How many observations in the 6th column of the rainfall data are
## anomalies according to your improved tukey.outlier? How many
## are anomalies in the whole data set?

length(tukey.outlier(rain.df[,6]))
for (i in 4:27) {print(length(tukey.outlier(rain.df[,i])))}




## OUTLIER WORDS. The R workspace http://www.stat.cmu.edu./~cshalizi/
## statcomp/hw/05/hw-05.RData contains an object, hd table, which gives
## the counts for the number of times different words were used in a famous
## novel. (Look at recipes 9.3 and 10.20 in The R Cookbook for basics of ta-
## bles, and section 6.3 in Matloff for more.) The names for each component
## are the words, and the values are the number of times that word was used.

load(url("http://www.stat.cmu.edu./~cshalizi/statcomp/hw/05/hw-05.RData"))



## How many times did the novel use the word "river"? How many
## times did it use the word "rivers"? How many times did it use the
## word "puddle"?

hd_table["river"]
hd_table["rivers"]
hd_table["puddle"]



## What was the length of the novel, in words? How many distinct
## words did the novel use?

sum(hd_table)
dim(hd_table)



## Create a vector which indicates, for each word in hd table,
## whether or not it is an outlier, by Tukey's rule.

tukey.outlier <- function(x) {
	quartiles <- quartiles(x)
	lower.limit <- quartiles[1]-1.5*quartiles[3]
	upper.limit <- quartiles[2]+1.5*quartiles[3]
	outliers <- ((x < lower.limit) | (x > upper.limit))
	outliers[is.na(outliers)] <- FALSE
	return(outliers)
}

is.hd.outlier <- tukey.outlier(hd_table)



## Create a vector which gives the outlier words and their counts.

outliers <- hd_table[is.hd.outlier]



## What are the 20 most common words which the rule says are
## outliers? How often do they occur?

sort(outliers, decreasing = TRUE)[1:20]



## In large samples of text, the most common words in English are
## "the", "of", "a", "and", "to", typically each being a few percent of
## the total number of words. Is the novel anomalous in how often it
## uses these words?

signif(100*sort(outliers, decreasing=TRUE)[1:20]/sum(hd_table), 2)




## MULTIPLE DIMENSIONS. Consider the following data:

x <- c(2.2, 7.8, -4.4, 0.0, -1.2, 3.9, 4.9, -5.7, -7.9, -4.9, 28.7, 4.9)
y <- c(11.0, 14.0, 3.5, 52.5, 21.5, 12.7, 16.7, 11.7, 10.8, -9.2, 12.3, 13.8)
z <- cbind(x,y)



## Which rows of z ought to be considered outliers, according to
## Tukey's rule? Why?

tukey.outlier(z[,1])
tukey.outlier(z[,2])



## Here is some code which tries to implement Tukey's rule in multi-
## ple dimensions. It has a bug. What happens when you run this on z?

tukey_multiple <- function(x) {
	outliers <- array(TRUE,dim=dim(x))
	for (j in 1:ncol(x)) {
		outliers[,j] <- outliers[,j] && tukey.outlier(x[,j])
	}
	outlier.vec <- vector(length=nrow(x))
	for (i in 1:nrow(x)) {
		outlier.vec[i] <- all(outliers[i,])
	}
	return(outlier.vec)
}



## Fix the bug. Verify that the corrected code works properly on z.

tukey_multiple <- function(x) {
	outliers <- array(TRUE,dim=dim(x))
	for (j in 1:ncol(x)) {
		outliers[,j] <- tukey.outlier(x[,j])
	}
	outlier.vec <- vector(length=nrow(x))
	for (i in 1:nrow(x)) {
		outlier.vec[i] <- any(outliers[i,])
	}
	return(outlier.vec)
}



## Modify this code to get rid of the loops. Make sure it still works
## on z. Hint: apply.

tukey_multiple <- function(x) {
	outliers <- apply(x, 2, tukey.outlier)
	outlier.vec <- apply(outliers, 1, any)
	return(outlier.vec)
}



## Modify your tukey.outlier function so that it still works on
## vectors, but if it is given an array, it returns a Boolean vector in-
## dicating which rows are outliers. Modify your test.tukey.outlier
## function so that it tests both all the old vector cases, and z as an
## array case. Make sure your new tukey.outlier works in all your
## test cases.

tukey.outlier <- function(x) {
	if (is.vector(x)) {
		quartiles <- quartiles(x)
		lower.limit <- quartiles[1]-1.5*quartiles[3]
		upper.limit <- quartiles[2]+1.5*quartiles[3]
		outliers <- ((x < lower.limit) | (x > upper.limit))
		outliers[is.na(outliers)] <- FALSE
		return(outliers)
	} else {
		return(tukey_multiple(x))
	}
}



## How many days in the rainfall data set are outliers?

sum(tukey.outlier(rain.df[,-(1:3)]))