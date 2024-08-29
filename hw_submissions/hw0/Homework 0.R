# STAT 1361/2360 Homework 0
# Prof. Lucas Mentch

# Students in this course will need a basic familiarity with the R programming language.  This first
# preliminary homework is designed for you to check and practice your R skills using a lot of
# the tools, functions, and techniques that will be used more heavily later in the course.  Ideally
# you should be able to breeze through this without much issue but for many of you, there may be
# some tasks that you never learned or just don't remember how to do.  That's fine.  If you find
# yourself in that circumstance, feel free to look up whatever you need to (just don't copy and
# paste code from elsewhere) -- learning to identify the task you need to accomplish and looking 
# up how to achieve that in the programming language you're using can be seen as a skill in and
# of itself.  That being said, if the tasks below feel extremely difficult and foreign and you
# find yourself spending an inordinate amount of time on this, you may want to consider whether 
# you are properly prepared for the course; please feel free to talk to me about this early on
# if you have concerns.

# Each of the numbered comments below contains a task.  Most tasks (though not all) require only
# one or two lines of code to complete.  Write your code beneath each individual task.

# 1.  Let's begin by defining two variables -- call them X1 and X2 that contain 50 samples from
# a (continuous) uniform distribution on (0,1):
X1 <- runif(50,0,1)
X2 <- runif(50,0,1)

# 2.  Now define another variable called 'epsilon' that contains 50 random samples from a normal 
# distribution with mean 0 and variance 0.1:
epsilon <- rnorm(50, 0, sd = sqrt(0.1))

# 3.  Change the name of 'epsilon' to 'eps'
eps <- epsilon
rm(epsilon)

# 4.  Now define a variable called 'Y' that is the sum of the 3 variables previously defined:
Y <- X1 + X2 + eps

# 5.  Make a dataframe called 'df' that consists of the variables Y, X1, and X2:
df <- as.data.frame(cbind(Y, X1, X2))

# 6.  Check the dimension of this dataframe:
dim(df)

# 7.  Print the dataframe.  Note that it's likely too much to fit on your screen so just look at 
# the beginning and end of it using the 'head' and 'tail' functions:
head(df)
tail(df)

# 8.  Get the names of this dataframe:
names(df)

# 9.  Change the names of the variables to thier lowercase versions (y, x1, and x2)
names(df)<- c("y", "x1", "x2")

# 10.  Built a linear model called 'lm.orig' where y is regressed on all other variables.  Your
# code should work regardless of how many variables are in the dataframe:
lm.orig <- lm(y~., data = df)

# 11. Print the summary of this model:
summary(lm.orig)

# 12.  Check what kind of object lm.orig is using the 'class' function:
class(lm.orig)

# 13.  Note that the 'summary' function returns a list of various objects.  Use one line of code to
# show both the names of the items in the list as well as their values:
summary(lm.orig)$coef

# 14.  Using one line of code and the '$', make a new variable called 'coef' that stores the 
# 'coefficients' that get returned from the summary of the linear model:
coef <- lm.orig$coefficients 

# 15.  What class of object is 'coef'?
class(coef) #numeric

# 16.  Get the row names of coef:
rownames(coef)

# 17.  Save the coefficient estimate for x2 as 'est1'
est1<- coef["x2"]

# 18.  Now create a new variable called 'x2.perm' that is simply a shuffled version (i.e. a 
# permutation) of x2:
x2.perm <- sample(df$x2)

# 19.  Now create a new dataframe called 'df.perm' where x2 is replaced by x2.perm:
df.perm <- df
df.perm$x2.perm <- df.perm$x2
df.perm$x2 <- NULL

# 20.  Now rebuild the linear model from step 10 using this new dataset and call it lm.perm:
lm.perm <- lm(y~., df.perm)
summary(lm.perm)

# Note that the coefficient estimate for x2 in the new linear model is (or at least should be) much
# less than the original estimate saved as est1.  Think about why that is (you don't need to write
# anything here).

# 21.  Now let's repeat the above process many times.  Make a 'for' loop that repeats 'nsim' times
# where nsim is a variable whose value is 100.  Each time through the loop, you should create a 
# permutation of x2, put it in a dataframe, built a linear model, and save the coefficient 
# estimate for x2.  Your loop should output a vector with the 100 coefficient estimates and that
# vector should be prespecified (pre-allocated) before the loop:
nsim <- 100
x2.coef <- vector()
for(i in 1:nsim){
  x2.permu <- sample(X2)
  df.permu <- df
  df.permu$x2.permu <- x2.permu
  df.permu$x2 <- NULL
  
  lm.permu <- lm(y~., df.permu)
  est.loop <- lm.permu$coef['x2.permu']
  x2.coef <- append(x2.coef, est.loop)
}

# 22.  Make a histogram of the coefficient estimates calculated in the previous loop:
hist(x2.coef)

# 23.  Now change the histogram so that the x-axis goes from -1.5 to 1.5:
hist(x2.coef, xlim = c(-1.5, 1.5))

# 24.  Now add a vertical line in red corresponding to location of your original coefficient 
# estimate:
hist(x2.coef, xlim = c(-1.5, 1.5))
abline(v = est1, col = "red")

# It's quite likely that all of the new coefficient estimates are less than your original estimate.
# Again, make sure it's clear to you why that is.  Your histogram should be centered around 0.  
# Re-run all of the above steps (starting with the loop) using 1000 simulations instead of 100
# to make sure you see the same kind of output.  You should be able to do this by only changing
# the value of nsim:
