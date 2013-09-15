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

?lm
help.search('lm')
apropos('lm')

help('[[')


ch1 <- "Chris's\n"
ch2 <- 'He said, "hello."\n'
ch3 <- "He said, \"hello.\"\n"

#####################################################
# 4: Working with data structures
#####################################################

### 4.1 Lists and dataframes

x <- list(a = 1:2, b = 3:4, sam = rnorm(4))
x[[2]] # extracts the indicated component, which can be anything, 
# in this case just an integer vector
x[c(1, 3)] # extracts subvectors, which since it is a list, 
# will also be a list


lapply(x, length)
sapply(x, length) # returns things in a user-friendly way

apply(CO2, 2, class) # hmmm
sapply(CO2, class)

params <- list( a = list(mn = 7, sd = 3), b = list(mn = 6, sd =1), c = list(mn = 2, sd = 1))
sapply(params, '[[', 1)

unlist(x)

tapply(mtcars$mpg, mtcars$cyl, mean) 
tapply(mtcars$mpg, list(mtcars$cyl, mtcars$gear), mean)

aggregate(mtcars, by = list(cyl = mtcars$cyl), mean) 
# this uses the function on each column of mtcars, split by the 'by' argument
by(warpbreaks, warpbreaks$tension, function(x) {lm(breaks ~ wool, data = x)})

splt <- split(mtcars, mtcars$cyl)


x <- rnorm(100)
f <-  cut(x, breaks = c(-Inf, -1, 1, Inf), labels = c('low', 'medium', 'high'))
levels(f) # note that f is not explicitly ordered
f <- relevel(f, 'high') # puts high as first level
f <-  cut(x, breaks = c(-Inf, -1, 1, Inf), labels = c('low', 'medium', 'high'), ordered_result = TRUE)

myList <- list(a = 1:3, b = 11:13, c = 21:23)
args(rbind)
rbind(myList$a, myList$b, myList$c)
rbind(myList)
do.call(rbind, myList)

### 4.2 Vectors and matrices

mat <- matrix(rnorm(500), nr = 50) 
identical(mat[1:50], mat[ , 1])
identical(mat[1:10], mat[1, ])
vec <- c(mat) 
mat2 <- matrix(vec, nr = 50) 
identical(mat,mat2) 



matrix(1:4, 2, byrow = TRUE)



x <- c(1, 10, 2, 9, 3, 8)
which(x < 3)
x <- matrix(1:6, nrow = 2)
which(x < 3, arr.ind = TRUE)



set <- c("Mazda RX4", "Merc 240D", "Fiat 128")
row.names(mtcars) %in% set
which(row.names(mtcars) %in% set)
match(row.names(mtcars), set)

# different ways to subset:
set.seed(0)
vec <- rnorm(8); mat <- matrix(rnorm(9), 3)
vec; mat
vec[vec < 0]
vec[vec < 0] <- 0
vec
mat[mat[ , 1] < 0, ] # similarly for data frames
mat[mat[ , 1] < 0, 2:3]  # similarly for data frames
mat[ , mat[1, ] < 0]
mat[mat[ , 1] < 0, 2:3] <- 0
set.seed(0) # so we get the same vec as we had before
vec <- rnorm(8)
wh <- which(vec < 0)
logicals <- vec < 0
logicals; wh
identical(vec[wh], vec[logicals])
vec <- c(1L, 2L, 1L)
is.integer(vec)
vec[vec == 1L] # in general, not safe with numeric vectors
vec[vec != 3L] # nor this


# subsetting with a two-column matrix of {row,col} indices
mat <- matrix(rnorm(25), 5)
rowInd <- c(1, 3, 5); colInd <- c(1, 1, 4)
mat[cbind(rowInd, colInd)]



students <- factor(c("basic", "proficient", "advanced", "basic", "advanced", "minimal"))
score = c(minimal = 3, basic = 1, advanced = 13, proficient = 7)
score["advanced"]
score[students[3]]
score[as.character(students[3])]



x <- matrix(1:6, nr = 2)
x
apply(x, 1, min) # by row 
apply(x, 2, min) # by column
x <- array(1:24, c(2, 3, 4))
apply(x, 2, min) # for(j in 1:3) print(min(x[ , j, ]))
apply(x, c(2, 3), min) 
#  equivalent to: 
#  for(j in 1:3) {
#   for(k in 1:4) { 
#     print(min(x[ , j, k]))
#   }}



n <- 500000; nr <- 10000; nCalcs <- n/nr
mat <- matrix(rnorm(n), nrow = nr)
times <- 1:nr
system.time(out1 <- apply(mat, 2, function(vec) {mod = lm(vec ~ times); return(mod$coef[2])})) 
system.time({out2 = rep(NA, nCalcs); for(i in 1:nCalcs){out2[i] = lm(mat[ , i] ~ times)$coef[2]}}) 

### 4.3 Long and wide formats

load('../data/prec.RData')
prec <- prec[1:1000, ] # just to make the example code run faster
precVars <- 5:ncol(prec)
precStacked <- stack(prec, select = precVars)
out <- unstack(precStacked)
## # to use reshape, we need a unique id for each row since reshape considers each row in the wide format as a subject
prec <- cbind(unique = 1:nrow(prec), prec)
precVars <- precVars + 1
precLong <- reshape(prec, varying = names(prec)[precVars], idvar = 'unique', direction = 'long', sep = '')
precLong <- precLong[!is.na(precLong$prec), ]
precWide <- reshape(precLong, v.names = 'prec', idvar = 'unique', direction = 'wide', sep='')

### 4.4 Linear algebra

X <- matrix(rnorm(9), 3)
X
X[col(X) >= row(X)]


diag(X)
diag(X) <- 1
X
d <- diag(c(rep(1, 2), rep(2, 2)))
d


X %*% Y # matrix multiplication
X * Y # direct product
x %o% y # outer product of vectors x, y: x times t(y)
outer(x, y) # same thing
# evaluation of f(x,y) for all pairs of x,y values:
outer(x, y, function(x, y) cos(y)/(1 + x^2))
crossprod(X, Y) # same as but faster than t(X) %*% Y!

solve(X) # inverse of X
solve(X, y) # (inverse of X) %*% y

#####################################################
# 5: Functions, variable scoping, and frames
#####################################################

### 5.1 Inputs

args(lm)

pplot <- function(x, y, pch = 16, cex = 0.4, ...) {
	plot(x, y, pch = pch, cex = cex, ...)
}

myFun <- function(...){
print(..2) 
args <- list(...)
print(args[[2]])
}
myFun(1,3,5,7)


args(dgamma)


tmpf = function(x, shape, rate = 1, scale = 1/rate, log = FALSE){
  print(shape)
  print(rate)
  print(scale)
}
tmpf(1, 2, rate = 3)
tmpf(1, 2, scale = 5)
dgamma(1, 2, scale = 5)
# does it matter that the rate and scale are inconsistent?
dgamma
# why can't we use "rate = 1/scale, scale = 1/rate"? or can we?


mat <- matrix(1:9, 3)
apply(mat, 2, function(vec) vec - vec[1])
apply(mat, 1, function(vec) vec - vec[1]) 
# explain why the result of the last expression is transposed


f <- function(x, y = 2, z = 3 / y) { x + y + z }
args(f)
formals(f)
class(formals(f))


match.call(mean, quote(mean(y, na.rm = TRUE))) 
# what do you think quote does? Why is it needed?



## f <- function(){
## 	oldpar <- par()
## 	par(cex = 2)
## 	# body of code
## 	par() <- oldpar
## }

### 5.2 Outputs

f <- function(x) { res <- x^2}
f(3)
a <- f(3)
a



f <- function(x){ invisible(x^2) }
f(3)
a <- f(3)
a



mod <- lm(mpg ~ cyl, data = mtcars)
class(mod)
is.list(mod)

### 5.3 Variable scope

x <- 3
f <- function() {x <- x^2; print(x)}
f(x)
x # what do you expect?
f <- function() {assign('x', x^2, env = .GlobalEnv)}
# careful, this could be dangerous as a variable is changed as a side effect

x <- 3
f <- function() {
f2 <- function() { print(x) }
f2()
}
f() # what will happen?

f <- function() {
f2 <- function() { print(x) }
x <- 7
f2()
}
f() # what will happen?

f2 <- function() print(x)
f <- function() {
x <- 7
f2()
}
f() # what will happen?



y <- 100
f <- function(){
	y <- 10
	g <- function(x) x + y
	return(g)
}
# you can think of f() as a function constructor
h <- f()
h(3)



z <- 3
x <- 100
f <- function(x, y = x*3) {x+y}
f(z*5)

### 5.4 Environments and the search path

search()
searchpaths()



x <- .GlobalEnv
parent.env(x)
while (environmentName(x) != environmentName(emptyenv())) {
	print(environmentName(parent.env(x)))
	x <- parent.env(x)
}



ls(pos = 7)[1:5] # what does this do?
ls("package:stats")[1:5]
environment(lm)



x <- environment(lm)
parent.env(x)
while (environmentName(x) != environmentName(emptyenv())) {
	print(environmentName(parent.env(x)))
	x <- parent.env(x)
}



lm <- function() {return(NULL)} # this seems dangerous but isn't
x <- 1:3; y <- rnorm(3); mod <- lm(y ~ x)
mod <- get('lm', pos = 'package:stats')(y ~ x)
mod <- stats::lm(y ~ x) # an alternative

### 5.5 Frames and the call stack

sys.nframe()
f <- function() {
  cat('f: Frame number is ', sys.nframe(), '; parent frame number is ', sys.parent(), '.\n', sep = '')
  cat('f: Frame (i.e., environment) is: ')
  print(sys.frame(sys.nframe()))
  cat('f: Parent is ')
  print(parent.frame())
  cat('f: Two frames up is ')
  print(sys.frame(-2))
}
f()
f2 <- function() {
  cat('f2: Frame (i.e., environment) is: ')
  print(sys.frame(sys.nframe()))
  cat('f2: Parent is ')
  print(parent.frame())	
  f()
}
f2()



# exploring functions that give us information the frames in the stack
g <- function(y) {
  gg <- function() {
    # this gives us the information from sys.calls(), sys.parents() and sys.frames() as one object
    #print(sys.status())
    tmp <- sys.status()
    print(tmp)
  }
  if(y > 0) g(y-1) else gg()
}
g(3)

### 5.6 with() and within()

with(mtcars, cyl*mpg)
new.mtcars <- within(mtcars, crazy <- cyl*mpg)
names(new.mtcars)

#####################################################
# 6: Flow control and logical operations
#####################################################

### 6.1 Logical operators

x <- rnorm(10)
x[x > 1 | x < -1]
x <- 1:10; y <- c(rep(10, 9), NA)
x > 5 | y > 5 # note that TRUE | NA evaluates to TRUE



a <- 7; b <- NULL
a < 8 | b > 3
a < 8 || b > 3
a <- c(0, 3); b <- c(4, 2)
if(a < 7 & b < 7) print('this is buggy code')
if(a < 7 && b < 7) 
   print('this is buggy code too, but runs w/o warnings')
if(a[1] < 7 && b[1] < 7) 
   print('this code is correct and the condition is TRUE')



a <- 7; b <- 5
!(a < 8 & b < 6)

### 6.2 If statements

x <- 5
if(x > 7){
	x <- x + 3
} else{
	x <- x - 3
}



if(x > 7) 
x <- x + 3 else x <- x - 3



x <- -3
if (x > 7){
	x <- x + 3
	print(x)
} else if (x > 4){
	x <- x + 1
	print(x)
} else if (x > 0){
	x <- x - 3
	print(x)
} else{
	x <- x - 7
	print(x)
}



x <- rnorm(6)
truncx <- ifelse(x > 0, x, 0)
truncx




vals <- c(1, 2, NA); eps <- 1e-9 

# now pretend vals comes from some other chunk of code that we don't control

if(min(vals) > eps) # not good practice

   { print(vals) } 

# better practice:

minval <- min(vals) 

if(!is.na(minval) && minval > eps) { print(vals) } 

### 6.3 switch()

x <- 2; y <- 10
switch(x, log(y), sqrt(y), y) 
center <- function(x, type){
	switch(type, 
	mean = mean(x), # make sure to use = and not <-
	median = median(x),
	trimmed = mean(x, trim = .1))
}
x <- rgamma(100, 1)
center(x, 'median')
center(x, 'mean')

### 6.4 Loops

nIts <- 500; means <- rep(NA, nIts)
for(it in 1:nIts){ 
	means[it] <- mean(rnorm(100))
	if(identical(it %% 100, 0)) cat('Iteration', it, date(), '\n')
}



for(state in c('Ohio', 'Iowa','Georgia')) {
	sub <- row.names(state.x77) == state
	print(state.x77[sub, "Income"])
}



for(i in 1:10) i



for(i in 1:10){
	if(i == 5) break
	print(i)
}
for(i in 1:5){
	if(i == 2) next
	print(i)
}

# while loop example - MLE for zero-truncated Poisson, from p. 59 of Venables and Ripley, 4th edition

yp = rpois(50, lambda = 1)
y = yp[yp > 0]

ybar = mean(y)
lam = ybar
it = 0
del = 1
tol = 0.0001
while(abs(del) > tol && (it <- it + 1) < 10){  # let's parse this
  del = (lam - ybar * (1- exp(-lam)))/(1 - ybar * exp(-lam))  # adjustment to parameter estimate
  lam = lam - del  # new parameter value
  cat(it, lam, "\n")
}


mat <- matrix(1:4, 2)
submat <- mat[mat[1, ] > 5]
for(i in 1:nrow(submat)) print(i)

#####################################################
# 7: Formulas
#####################################################


resp <- 'y ~'
covTerms <- 'x1'
for(i in 2:5){ covTerms <- paste(covTerms, '+ x', i, sep='')}
form <- as.formula(paste(resp, covTerms, sep = ''))
# lm(form, data = dat)
form
class(form)



resp <- 'y ~'
covTerms <- paste('x', 1:5, sep = '', collapse = ' + ')
form <- as.formula(paste(resp, covTerms))
form
# lm(form, data = dat)


#####################################################
# 8: Text manipulations and regular expressions
#####################################################

### 8.1 Basic text manipulation

out <- paste("My", "name", "is", "Chris", ".", sep = " ")
paste(c("My", "name", "is", "Chris", "."), collapse = " ") # equivalent
strsplit(out, split = ' ')



vars <- c("P", "HCA24", "SOH02")
substring(vars, 2, 3)
vars <- c("date98", "size98", "x98weights98", "sdfsd")
grep("98", vars)
gregexpr("98", vars)
gsub("98", "04", vars)

### 8.2 Regular expressions (regexp/regex)

# Character sets and character classes

addresses <- c("john@att.com", "stat243@bspace.berkeley.edu", "john_smith@att.com")
grep("[[:digit:]_]", addresses, perl = TRUE)

# Location-specific matches

text <- c("john","jennifer pierce","Juan carlos rey")
grep("^[[:upper:]]", text) # finds text starting with upper case letter
grep("[[:digit:]]$", text) # finds text with a number at the end



text <- c("john","jennifer pierce","Juan carlos rey")
grep("[ \t]", text)
gregexpr("[ \t]", text)
gsub("^j", "J", text)



text <- c("john","jennifer pierce","Juan carlos rey")
matches <- gregexpr("^[[:upper:]][[:lower:]]+ ", text)
regmatches(text, matches)

# Repetitions

text <- c("hi John", "V1@gra", "here's the problem set")
grep("[[:alpha:]]+[[:digit:][:punct:]]+[[:alpha:]]*", text) 
# ok, so that doesn't quite work...

# Grouping and references

## grep("([[:digit:]]{1,3}\\.){3}[[:digit:]]{1,3}", text)



text <- ('"H4NY07011","ACKERMAN, GARY L.","H","$13,242",,,')
gsub("([^\",]),", "\\1", text)


gregexpr("(http|ftp):\\/\\/", c("at the site http://www.ibm.com", "other text", "ftp://ibm.com"))

# Greedy matching

text <- "Students may participate in an internship <b> in place
   </b> of <b> one </b> of their courses."
gsub("<.*>", "", text)

gsub("<.*?>", "", text)

# Regular expressions in other contexts

line <- "a dog\tjumped\nover \tthe moon."
cat(line)
strsplit(line, split = "[[:space:]]+")
strsplit(line, split = "[[:blank:]]+")


