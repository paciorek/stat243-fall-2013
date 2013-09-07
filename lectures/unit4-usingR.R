##################################################
### Demo code for Unit 4 of Stat243, "Using R"
### Chris Paciorek, September 2013
##################################################

#####################################################
# 1: Interacting with the operating system from R
#####################################################



system("ls -al") # knitr/Sweave doesn't seem to show the output of system()

files <- system("ls", intern = TRUE)

files[1:5]


## 
## file.exists("file.txt")
## 
## list.files("~/research")
## 

Sys.info()

# options()  # this would print out a long list of options

options()[1:5]

options()[c('width', 'digits')]

# options(width = 120) # often nice to have more characters on screen

options(width = 55)  # for purpose of making the pdf of this document

options(max.print = 5000)

options(digits = 3)

a <- 0.123456; b <- 0.1234561

a; b; a == b

sessionInfo()


## 
## args <- commandArgs(TRUE)
## # now args is a character vector containing the arguments.
## # Suppose the first argument should be interpreted as a number
## 
## # and the second as a character string and the third as a boolean:
## numericArg <- as.numeric(args[1])
## charArg <- args[2]
## logicalArg <- as.logical(args[3]

#####################################################
# 2: Packages
#####################################################


library(fields)
library(help = fields)
# library()  # I don't want to run this on SCF because so many are installed

.libPaths()
searchpaths()

## install.packages('fields', lib = '~/Rlibs') # ~/Rlibs needs to exist!

search()
# ls(pos = 7) # for the stats package
ls(pos = 7)[1:5] # just show the first few
ls("package:stats")[1:5] # equivalent

#####################################################
# 3: Objects in R
#####################################################

### 3.1 Assignment and coercion

out = mean(rnorm(7)) # OK
system.time(out = rnorm(10000)) 
# NOT OK, system.time() expects its argument to be a complete R expression
system.time(out <- rnorm(10000))



mean
x <- 0; y <- 0
out <- mean(x = c(3,7)) # the usual way to pass an argument to a function
out <- mean(x <- c(3,7)) # this is allowable, though perhaps not useful; what does it do?
out <- mean(y = c(3,7))
out <- mean(y <- c(3,7))



mat <- matrix(c(1, NA, 2, 3), nrow = 2, ncol = 2)
apply(mat, 1, sum.isna <- function(vec) {return(sum(is.na(vec)))})
# What is the side effect of what I have done here?
apply(mat, 1, sum.isna = function(vec) {return(sum(is.na(vec)))}) # NOPE



vals <- c(1, 2, 3)
class(vals)
vals <- 1:3
class(vals)
vals <- c(1L, 2L, 3L)
vals
class(vals)



as.character(c(1,2,3))
as.numeric(c("1", "2.73"))
as.factor(c("a", "b", "c"))



x <- rnorm(5)
x[3] <- 'hat' # What do you think is going to happen?
indices = c(1, 2.73)
myVec = 1:10
myVec[indices]

### 3.2 Type vs. class

n <- 5
df <- data.frame(rep('a', n), rnorm(n), rnorm(n))
apply(df, 1, function(x) x[2] + x[3])
# why does that not work?
apply(df[ , 2:3], 1, function(x) x[1] + x[2])
df2 <- data.frame(cbind(rep('a', n), rnorm(n), rnorm(5)))
apply(df2[ , 2:3], 1, function(x) x[1] + x[2])



a <- data.frame(x = 1:2)
class(a)
typeof(a)
m <- matrix(1:4, nrow = 2) 
class(m) 
typeof(m)



me <- list(firstname = 'Chris', surname = 'Paciorek')
class(me) <- 'personClass'  # it turns out R already has a 'person' class
class(me)
is.list(me)
typeof(me)
typeof(me$firstname)

### 3.3 Information about objects

is(me, 'personClass')
str(me)
attributes(me)
mat <- matrix(1:4, 2)
class(mat)
typeof(mat)
length(mat) 
# recall that a matrix can be thought of as a vector with dimensions
attributes(mat)
dim(mat)



x <- rnorm(10 * 365)
qs <- quantile(x, c(.025, .975))
qs
qs[1] + 3



names(qs) <- NULL
qs



row.names(mtcars)[1:6]
names(mtcars)
mat <- data.frame(x = 1:2, y = 3:4)
row.names(mat) <- c("first", "second")
mat
vec <- c(first = 7, second = 1, third = 5)
vec['first']

### 3.4 The workspace

# objects() # what objects are in my workspace
identical(ls(), objects()) # synonymous
dat <- 7; dat2 <- 9; subdat <- 3; obj <- 5; obj2 <- 7
objects(pattern = "^dat") 
rm(dat2, subdat)
rm(list = c("obj", "obj2")) 
# a bit confusing - the 'list' argument should be a character vector
rm(list = ls(pattern = "^dat")) 
exists('dat') # can be helpful when programming
dat <- rnorm(500000)
object.size(dat)
print(object.size(dat), units = "Mb") # this seems pretty clunky!
# but we'll understand why when we see S3 classes in detail
rm(list = ls()) # what does this do?

### 3.5 Some other details

rnorm(10)
# .Last.value  # this should return the 10 random normals but knitr is messing things up, commented out here


x <- 1e5
log10(x)
y <- 100000
x <- 1e-8


