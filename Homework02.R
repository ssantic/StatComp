## The file hw-02.R on the class website contains the code for the re-
## source allocation example from lecture 3. Download it.

factory <- matrix(c(40,1,60,3),nrow=2,
  dimnames=list(c("labor","steel"),c("cars","trucks")))
available <- c(1600,70); names(available) <- rownames(factory)
slack <- c(8,1); names(slack) <- rownames(factory)
output <-c(30,20); names(output) <- colnames(factory)

passes <- 0 # How many times have we  been around the loop?
repeat {
   passes <- passes + 1
   needed <- factory %*% output # What do we need for that output level?
   # If we're not using too much, and are within the slack, we're done
   if (all(needed <= available) &&
       all((available - needed) <= slack)) {
     break()
   }
   # If we're using too much of everything, cut back by 10%
   if (all(needed > available)) {
     output <- output * 0.9
     next()
   }
   # If we're using too little of everything, increase by 10%
   if (all(needed < available)) {
     output <- output * 1.1
     next()
   }
   # If we're using too much of some resources but not others, randomly
   # tweak the plan by up to 10%
   output <- output * (1+runif(length(output),min=-0.1,max=0.1))
}



## Set the initial output levels to 30 cars and 20 trucks (as in the
## slides) and run the code. Repeat this five times (resetting the initial
## output levels each time). Why are the results different each time?
## What are the means and the standard deviations of the final output
## levels? (You should report two means and two standard deviations.)

mean(output[1])
mean (output[2])
sd(output)

The results are different because there is no starting point (seed)
set for the sumulation.



## Repeat another five times. What are the means and standard
## deviations of the resources needed? Why are the standard deviations
## less than the slack levels?

mean(output[1])
mean (output[2])
sd(output)

The standard deviations are less than the slack levels, as slack
represents the MAXIMUM amount of unused materials, and thus the
SDs cannot be larger than that.



## Re-run the code twice more, starting from an initial output level
## of 5 cars and 5 trucks, and from 12 cars and 19 trucks. Do you get
## the same results as before? Should you?

No, the result are not the same, as they should not be.




## Negative trucks? No trucks?

## Suppose that by accident we set the initial output levels to
## c(-20,-30). What would happen with the code as given? Hint:
## think this through, rather just running it.

The solution will not converge, as the output levels would
be negative infinite.



## Suppose that by accident we set the initial output levels to
## c(0,0). What would happen with the code as given? Hint: See
## previous hint.

The solution will not converge, as the output levels will remain
equal to 0.



## Fix the code to check whether any of the output levels are <= 0,
## and replace those output levels with a small positive number.
## Re-run the code starting from an output level of 30 cars and 20
## trucks, and verify that it still works.
## Does your code work starting from 0 cars and 0 trucks?
## Does your code work starting from -20 cars and -30 trucks?

if (output[1] <= 0) {output[1] <- 0.01}
if (output[2] <= 0) {output[2] <- 0.01}




## Limiting run time. The slack variables stop the iteration when the
## plan comes close to fully using the available resources. It can also be a
## good idea to stop when we've made a certain number of passes, to keep
## from spending forever tinkering.

## Add a variable called max.passes, and a test inside the repeat
## body which halts the loop when the number of passes exceeds this
## number.

max.passes <- 100 # Added before the main loop.
if (passes == max.passes) {break()} # Added at the beginning of the main loop.



## Check that the code works when the initial output is 30 cars and
## 20 trucks, and max.passes is set high enough. How high is high
## enough?

Max. passes must be >= 0.



## Replace the repeat loop with a for loop. Ensure that the code
## doesn't go through the loop more than max.passes times, but that
## once the plan is within acceptable limits, no time is wasted on extra
## loops. Check that the code still works on the 30 car/20 trucks test
## case.

for (i in 1:max.passes) # This replaces the repeat loop.
if (round(output[1],1) == 10 && round(output[2],1) == 20) {break()} # This condition is added at the end of the loop.




## De-randomizing. The random part of the change to the plan makes it
## hard to reproduce results. Let's get rid of that.

## Modify the code to replace the random change to the plan when
## only one variable is in excess with one or more deterministic changes.
##Verify that it still works on the 30 cars/20 trucks test case.

available <- c(1000,70)




## Suppose we want to consider producing cars, trucks, motorcycles, 
## and vacuum cleaners, using labor, steel, rubber, plastic, electric
## wire and glass. What you would have to change in the original code?

We would have to add more rows and columns to the "factory" matrix, representing
the added products, as well as required inputs. We would also need to add additional
data for available quantities of inputs, as well as slack and beginning output levels.