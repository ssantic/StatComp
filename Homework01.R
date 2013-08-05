## WORKING WITH DATA. The data set at http://www.stats.
## uwo.ca/faculty/braun/data/rnf6080.dat records hourly rainfall at a
## certain location in Canada, every day from 1960 to 1980.

## Load the data set into R and make it a data frame called rain.df.
## What command did you use?

rain.df <- read.table("http://www.stats.uwo.ca/faculty/braun/data/rnf6080.dat")



## How many rows and columns does rain.df have? How do you
## know?

dim(rain.df)



## What command would you use to get the names of the columns
## of rain.df? What are those names?

names(rain.df)



## What command would you use to get the value at row 2, column 4? 
## What is the value?

rain.df[2,4]



## What command would you use to display the whole second row?
## What is the content of that row?

rain.df[2,]



## What does the following command do?

names(rain.df) <- c("year","month","day",seq(0,23))



## Create a new column called "daily", which is the sum of the 24
## hourly columns.

daily <- c(rowSums(rain.df[,4:27]))
rain.df <- data.frame(rain.df, daily)
names(rain.df) <- c("year","month","day",seq(0,23), "daily")



## Give the command you would use to create a histogram of the
## daily rainfall amounts. Save this histogram as a separate PDF file.
## Explain why that histogram cannot possibly be right.

pdf("C:/hist1.pdf")
hist(rain.df$daily)
dev.off()



## Give the command you would use to fix the data frame.

rain.df <- read.table("http://www.stats.uwo.ca/faculty/braun/data/rnf6080.dat")

rain.df[rain.df < 0] <- 0

daily <- c(rowSums(rain.df[,4:27]))
rain.df <- data.frame(rain.df, daily)
names(rain.df) <- c("year","month","day",seq(0,23), "daily")



## Create a corrected histogram and save a PDF file of it. Explain why it is more
## reasonable than the previous histogram.

pdf("C:/hist2.pdf")
hist(rain.df$daily)
dev.off()




## DATA TYPES. Make sure your answers to different parts of this
## problem are compatible with each other.

##For each of the following commands, either explain why they
##should be errors, or explain the non-erroneous result.

x <- c("5","12","7")
max(x)
sort(x)
sum(x)



## For the next two commands, either explain their results, or why
## they should produce errors.

y <- c("5",7,12)
y[2] + y[3]



## For the next two commands, either explain their results, or why
## they should produce errors.

z <- data.frame(z1="5",z2=7,z3=12)
z[1,2] + z[1,3]




## DRAWING OUTSIDE THE LINES. George is bored in class, so he fills his note-
## pad with random lines. The paper is horizontally ruled, with rules exactly
## two centimeters apart. George's lines have centers uniformly distributed
## over the page, and uniformly distributed angles. Each of his lines is exactly
## a centimeter long. (George is obsessive as well as bored.)

## Explain why the vertical distance between the center of one of
## George's lines and the nearest rule on the paper should be uniformly
## distributed between 0 and 1 cm.



## The R command runif(n = 137, min = -1, max = 0.9) returns 137
## numbers uniformly distributed between -1 and 0.9. What commands
## would you use to generate the random centers of 1000 of George's
## lines, and store them in a vector named "centers"?

centers <- runif(n = 1000, min = 0, max = 1)



## R measures angles in radians. What commands would you use to
## generate the angle from vertical of George's lines, and store them in
## a vector called orientations?



## If y is the location of the center of a line and a is its angle from the
## vertical, the line crosses one of the horizontal rules when y <= f(a).
## What is the function f? (Hint: trigonometry.)



## What commands would you use to calculate the fraction of George's
## 1000 lines which cross horizontal rules? What is the fraction?



## This is a version of a famous math problem. Name
## the problem, and what the fraction in the last part should be.