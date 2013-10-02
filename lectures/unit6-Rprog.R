##################################################
### Demo code for Unit 6 of Stat243, "R programming"
### Chris Paciorek, September 2013
##################################################

#############################
# 1: Efficiency
#############################

### 1.1 Tools for assessing efficiency

### 1.1.1 Benchmarking


n <- 1000
x <- matrix(rnorm(n^2), n)
system.time({mns <- rep(NA, n); for(i in 1:n) mns[i] <- mean(x[i , ])})
system.time(rowMeans(x))



library(rbenchmark)
# speed of one calculation
n <- 1000
x <- matrix(rnorm(n^2), n)
benchmark(crossprod(x), replications = 10, columns=c('test', 'elapsed', 'replications'))
# comparing different approaches to a task
benchmark(
{mns <- rep(NA, n); for(i in 1:n) mns[i] <- mean(x[i , ])},
rowMeans(x), 
replications = 10, columns=c('test', 'elapsed', 'replications'))

### 1.1.2 Profiling

makeTS <- function(param, len){
	times <- seq(0, 1, length = len)
	dd <- rdist(times)
	C <- exp(-dd/param)
	U <- chol(C)
	white <- rnorm(len)
	return(crossprod(U, white))
}

library(fields)
Rprof("makeTS.prof")
out <- makeTS(0.1, 1000)
Rprof(NULL)
summaryRprof("makeTS.prof")

### 1.2 Strategies for improving efficiency

### 1.2.1 Fast initialization

n <- 1000
x <- 1; 
benchmark( for(i in 2:n) x <- c(x, i), 
{x <- rep(as.numeric(NA), n); for(i in 1:n) x[i] <- i},
replications = 10, columns=c('test', 'elapsed', 'replications'))



n <- 1000000
benchmark(x <- rep(as.numeric(NA), n),
{x <- as.numeric(NA); length(x) <- n},
replications = 10, columns=c('test', 'elapsed', 'replications'))



nr <- nc <- 2000
benchmark(x <- matrix(as.numeric(NA), nr, nc),
{x <- as.numeric(NA); length(x) <- nr * nc; dim(x) <- c(nr, nc)},
replications = 10, columns=c('test', 'elapsed', 'replications'))



myList <- vector("list", length = n)

### 1.2.2 Vectorized calculations

n <- 1e6
x <- rnorm(n)
system.time(x2 <- x^2)
x2 <- as.numeric(NA)
system.time({length(x2) <- n; for(i in 1:n){ x2[i] <- x[i]^2}}) # how many orders of magnitude slower?



line <- c("Four score and 7 years ago, this nation")
startIndices = seq(1, by = 3, length = nchar(line)/3)
substring(line, startIndices, startIndices + 1)



x <- rnorm(1000000)
benchmark(truncx <- ifelse(x > 0, x, 0),
{truncx <- x; truncx[x < 0] <- 0},
truncx <- x * (x > 0),
replications = 10, columns=c('test', 'elapsed', 'replications'))

### 1.2.3 Using apply() and specialized functions

n <- 3000; x <- matrix(rnorm(n * n), nr = n)
system.time(out <- apply(x, 1, mean))
system.time(out <- rowMeans(x))



system.time(out <- sweep(x, 2, STATS = colMeans(x), FUN = "-"))


system.time(out2 <- t(t(x) - colMeans(x)))
identical(out, out2)

# surprising case where vectorization with a for loop beats apply()
rmRows1 <- function(x){
    omit <- FALSE
    n <- ncol(x)
    for(i in 1:n)
       omit <- omit | is.na(x[ , i])
    x[!omit,]
}

rmRows2 <- function(x){
  sum.isna <- function(row){
    return(sum(is.na(row)))
  }
  n.isna <- apply(x, 1, sum.isna)
  return(x[!n.isna, ])
}

# this seems clever; does it improve things?
rmRows3 <- function(x){
  return(x[!is.na(rowMeans(x)), ])
}


### 1.2.4 Matrix algebra efficiency

mat <- matrix(rnorm(500*500), 500)
benchmark(apply(mat, 1, sum),
	mat %*% rep(1, ncol(mat)),
	rowSums(mat),
	replications = 10, columns=c('test', 'elapsed', 'replications'))

# how do I take successive differences of columns of a matrix quickly?
nc = 500
nr = 500
mat = matrix(rnorm(nr * nc), nr, nc)

# try #1 via matrix algebra
system.time({A <- matrix(0, nr = nc, nc = nc - 1);
             A[row(A) == col(A)] <- 1;
             A[row(A) == col(A) + 1] <- -1;
             res <- mat %*% A} )

# try #2 via a for loop
system.time({res <- matrix(NA, nr, nc -1);
             for(j in 2:nc) res[ , j-1] = mat[ , j-1] - mat[ , j]})

# try #3 via direct matrix subtraction with clever subsetting
system.time(res <- mat[ , -nc] - mat[ , -1])


n <- 5000
A <- matrix(rnorm(5000 * 5000), 5000)
B <- matrix(rnorm(5000 * 5000), 5000)
x <- rnorm(5000)
res <- A %*% B %*% x



n <- 1000
X <- matrix(rnorm(n^2), n) 
diagvals <- rnorm(n)
D = diag(diagvals)
diag(X) <- diag(X) + diagvals
tmp <- diagvals * X # instead of D %*% X
tmp2 <- t(t(X) * diagvals) # instead of X %*% D

### 1.2.5 Fast mapping/lookup tables

vals <- rnorm(10)
names(vals) <- letters[1:10]
select <- c("h", "h", "a", "c")
vals[select]



n=1000000
x <- 1:n
xL <- as.list(x)
nms <- as.character(x)
names(x) <- nms
names(xL) <- nms
benchmark(x[500000], x["500000"], xL[[500000]], xL[["500000"]], replications = 10)



xEnv <- as.environment(xL)  # convert from a named list
xEnv$"500000"  
# I need quotes above because numeric; otherwise xEnv$nameOfObject is fine
xEnv[["500000"]]
benchmark(x[500000], xL[[500000]], xEnv[["500000"]], xEnv$"500000", replications = 10000) 

### 1.2.7 Byte compiling

library(compiler); library(rbenchmark)
f <- function(x){
	for(i in 1:length(x)) x[i] <- x[i] + 1
	return(x)
}
fc <- cmpfun(f)
fc # notice the indication that the function is byte compiled.
x <- rnorm(100000)
benchmark(f(x), fc(x), x <- x + 1, replications = 5)


#################################################
# 2: Advanced topics in working with functions
#################################################

### 2.2 Alternatives to pass by value in R

# simple closure example
x <- rnorm(10)
f <- function(input){
	data <- input
	g <- function(param) return(param * data) 
	return(g)
}
myFun <- f(x)
rm(x) # to demonstrate we no longer need x
myFun(3)
x <- rnorm(1e7)
myFun <- f(x)
object.size(myFun) # hmmm
object.size(environment(myFun)$data)


# closure example
make_container <- function(n) {
	x <- numeric(n)
	i <- 1
	
	function(value = NULL) {
		if (is.null(value)) {
			return(x)
		} else {
			x[i] <<- value
			i <<- i + 1
		}	 
	}
}

nboot <- 100
bootmeans <- make_container(nboot)
data <- faithful[ , 1] # length of Old Faithful geyser eruption times
for (i in 1:nboot)
	bootmeans(mean(sample(data, length(data),
      replace=TRUE)))
# this will place results in x in the function env't and you can grab it out as
bootmeans()


# using with()
x <- rnorm(10)
myFun2 <- with(list(data = x), function(param) return(param * data))
rm(x)
myFun2(3)
x <- rnorm(1e7)
myFun2 <- with(list(data = x), function(param) return(param * data))
object.size(myFun2)


### 2.3 Operators

a <- 7; b <- 3
# let's think about the following as a mathematical function
#  -- what's the function call?
a + b 
`+`(a, b)



myList = list(list(a = 1:5, b = "sdf"), list(a = 6:10, b = "wer"))
myMat = sapply(myList, `[[`, 1) 
# note that the index "1" is the additional argument to the [[ function
x <- 1:3; y <- c(100,200,300)
outer(x, y, `+`)


# creating your own binary operator
`%2%` <- function(a, b) {2 * (a + b)}
3 %2% 7


# additional arguments to operators
mat <- matrix(1:4, 2, 2)
mat[ , 1] 
mat[ , 1, drop = FALSE] # what's the difference?



## if(x > 27){
##	print(x)	
## } else{
## 	print("too small")
## }


# examples of replacement expressions
diag(mat) <- c(3, 2)
is.na(vec) <- 3
names(df) <- c('var1', 'var2')



mat <- matrix(rnorm(4), 2, 2)
diag(mat) <- c(3, 2)
mat <- `diag<-`(mat, c(10, 21))
base::`diag<-`


# defining a replacement function
me <- list(name = 'Chris', age = 25)
`name<-` <- function(obj, value){
obj$name <- value
return(obj)
}
name(me) <- 'Christopher'

### 2.5 Functions as objects

x <- 3
x(2)
x <- function(z) z^2
x(2)



myFun <- 'mean'; x <- rnorm(10)
eval(as.name(myFun))(x)



f <- function(fxn, x){
	match.fun(fxn)(x) 
}
f("mean", x)



f <- function(x) x
f2 <- function(x) y <- x^2
f3 <- function(x) {y <- x^2; z <- x^3; return(list(y, z))}
class(f)
typeof(body(f)); class(body(f))
typeof(body(f2)); class(body(f2))
typeof(body(f3)); class(body(f3))



f4 <- function(x, y = 2, z = 1/y) {x + y + z}
args <- formals(f4)
args
class(args)

### 2.6 Promises and lazy evaluation

f <- function(a, b = c) {
	c <- log(a);
	return(a*b)
}
f(7)



f <- function(x) print("hi")
system.time(mean(rnorm(1000000)))
system.time(f(3))
system.time(f(mean(rnorm(1000000)))) 

#################################################
# 3: Evaluating memory use
#################################################

### 3.2 Monitoring overall memory use

# restart R at this point
gc()
x <- rnorm(1e8) # should use about 800 Mb
object.size(x)
gc()
rm(x)
gc() # note the "max used" column


# a handy function I have in my .Rprofile file
ls.sizes <- function(howMany = 10, minSize = 1){
	pf <- parent.frame()
	obj <- ls(pf) # or ls(sys.frame(-1)) 
	objSizes <- sapply(obj, function(x) {
	object.size(get(x, pf))})
	# or sys.frame(-4) to get out of FUN, lapply(), sapply() and sizes()
	objNames <- names(objSizes)
	howmany <- min(howMany, length(objSizes))
	ord <- order(objSizes, decreasing = TRUE)
	objSizes <- objSizes[ord][1:howMany]
	objSizes <- objSizes[objSizes > minSize]
	objSizes <- matrix(objSizes, ncol = 1)
	rownames(objSizes) <- objNames[ord][1:length(objSizes)]
	colnames(objSizes) <- "bytes"
	cat('object')
	print(format(objSizes, justify = "right", width = 11), quote = FALSE)
}


# look at the internals of an R object,
#  including its memory address
a <- rnorm(5)
.Internal(inspect(a))

### 3.3 Hidden uses of memory

# restart R at this point
x <- rnorm(1e7)
gc()
dim(x) <- c(1e4, 1e3)
diag(x) <- 1
gc()



x <- rnorm(1e7)
.Internal(inspect(x))
x[5] <- 7
.Internal(inspect(x))
gc()


# restart R
x <- rnorm(1e7)
gc()
y <- x[1:(length(x) - 1)]
gc()

### 3.4 Passing objects to compiled code

f <- function(x){
	print(.Internal(inspect(x)))
	return(mean(x))
}
x <- rnorm(1e7)
class(x)
debug(f)
f(x)
f(as.numeric(x))
f(as.integer(x))



library(inline) 
src <- '
	for (int i = 0; i < *n; i++) {
		x[i] = exp(x[i]);
	}
'
sillyExp <- cfunction(signature(n = "integer", x = "numeric"),
	src, convention = ".C")
len <- as.integer(100)  # or 100L
vals <- rnorm(len)
vals[1]
out1 <- sillyExp(n = len, x = vals)
.Internal(inspect(vals))
.Internal(inspect(out1$x))

### 3.5 Delayed copying (copy-on-change)

f <- function(x){
	print(gc())
	z <- x[1]
	.Internal(inspect(x))
	return(x)
}
y <- rnorm(1e7)
gc()
.Internal(inspect(y))
out <- f(y)
.Internal(inspect(out))



y <- rnorm(1e7)
gc()
.Internal(inspect(y))
x <- y
gc()
.Internal(inspect(x))
x[1] <- 5
gc()
.Internal(inspect(x))
rm(x)
x <- y
.Internal(inspect(x))
.Internal(inspect(y))
y[1] <- 5
.Internal(inspect(x))
.Internal(inspect(y))


# examples of fooling the NAMED mechanism for copy-on-change

rm(x, y)
f <- function(x) sum(x^2)
y <- rnorm(10)
f(y)
.Internal(inspect(y))
y[3] <- 2
.Internal(inspect(y))

a <- 1:5
b <- a
.Internal(inspect(a))
.Internal(inspect(b))
a[2] <- 0
b[2] <- 4
.Internal(inspect(a))
.Internal(inspect(b))



a <- 1:10     
tracemem(a)      
## b and a share memory      
b <- a      
b[1] <- 1      
untracemem(a)    
.Internal(inspect(a))
.Internal(inspect(b))


# explain this:
y <- 1:5
.Internal(inspect(y))
x <- y
.Internal(inspect(x))
y[2] <- 4
.Internal(inspect(y))
.Internal(inspect(x))


# How much memory is used here?
x <- rnorm(1e7) 
myfun <- function(y){ 
	z <- y 
	return(mean(z)) 
} 
myfun(x)


# How about here?
x <- rnorm(1e7)
x[1] <- NA
myfun <- function(y){ 
	return(mean(y, na.rm = TRUE))
}
myfun(x)

### 3.7 Example

## fastcount <- function(xvar,yvar) {
## 	naline <- is.na(xvar)
## 	naline[is.na(yvar)] = TRUE
## 	xvar[naline] <- 0
## 	yvar[naline] <- 0
## 	useline <- !naline;
## 	# Table must be initialized for -1's
## 	tablex <- numeric(max(xvar)+1)
## 	tabley <- numeric(max(xvar)+1)
## 	stopifnot(length(xvar) == length(yvar))
## 	res <- .C("fastcount",PACKAGE="GCcorrect",
## 		tablex = as.integer(tablex), tabley = as.integer(tabley),
## 		as.integer(xvar), as.integer(yvar), as.integer(useline),
## 		as.integer(length(xvar)))
## 	xuse <- which(res$tablex>0)
## 	xnames <- xuse - 1
## 	resb <- rbind(res$tablex[xuse], res$tabley[xuse])
## 	colnames(resb) <- xnames
## 	return(resb)
## }



#################################################
# 4: Object-oriented programming (OOP) 
#################################################

### 4.1 S3 approach

library(methods)
yb <- sample(c(0, 1), 10, replace = TRUE)
yc <- rnorm(10)
x <- rnorm(10)
mod1 <- lm(yc ~ x)
mod2 <- glm(yb ~ x, family = binomial)
class(mod1)
class(mod2)
is.list(mod1)
names(mod1)
is(mod2, "lm")
methods(class = "lm")


# creating our own class
me <- list(firstname = 'Chris', surname = 'Paciorek', age = NA)
class(me) <- 'indiv' # there is already a 'person' class in R


# making a constructor
indiv <- function(firstname = NA, surname = NA, age = NA){
	# constructor for 'indiv' class
	obj <- list(firstname = firstname, surname = surname, age = age)
	class(obj) <- 'indiv' 
	return(obj)
}
me <- indiv('Chris','Paciorek')

# S3 is an informal system...
class(me) <- "silly"
class(me) <- "indiv"


# methods

mod <- lm(yc ~ x)
summary(mod)
gmod <- glm(yb ~ x, family = 'binomial')
summary(gmod)



mean
methods(mean)


# define a generic method if it's not already defined
summarize <- function(object, ...) 
	UseMethod("summarize") 

# now define the class-specific method
summarize.indiv <- function(object) 
	return(with(object, cat("Individual of age ", age, 
	" whose name is ", firstname, " ", surname, ".\n",sep = "")))
summarize(me)


out <- summary(mod)
out
print(out)
getS3method(f="print",class="summary.lm")


# defining inheritance
class(me) <- c('BerkeleyIndiv', 'indiv')
summarize(me) 


# class-specific operators
methods(`+`)
`+.indiv` <- function(object, incr) {
	object$age <- object$age + incr
	return(object)
}
old.me <- me + 15

# class-specific replacement functions
`age<-` <- function(x, ...) UseMethod("age<-")
`age<-.indiv` <- function(object, value){ 
	object$age <- value
	return(object)
}
age(old.me) <- 60

### 4.2 S4 approach

# create a class
library(methods)
setClass("indiv",
	representation(
		name = "character",
		age = "numeric",
		birthday = "Date" 
	)
)

# instantiate an object of the class
me <- new("indiv", name = 'Chris', age = 20, 
			birthday = as.Date('91-08-03'))
# next notice the missing age slot
me <- new("indiv", name = 'Chris', 
	birthday = as.Date('91-08-03')) 
# finally, apparently there's not a default object of class Date
me <- new("indiv", name = 'Chris', age = 20)
me
me@age <- 60


# formal checking of slots
setValidity("indiv",
	function(object) {
		if(!(object@age > 0 && object@age < 130)) 
			return("error: age must be between 0 and 130")
		if(length(grep("[0-9]", object@name))) 
			return("error: name contains digits")
		return(TRUE)
	# what other validity check would make sense given the slots?
	}
)
me <- new("indiv", name = "5z%a", age = 20, 
	birthday = as.Date('91-08-03'))
me <- new("indiv", name = "Z%a B''*", age = 20, 
	birthday = as.Date('91-08-03'))
me@age <- 150 # so our validity check is not foolproof



# generic method
setGeneric("isVoter", function(object, ...) { standardGeneric("isVoter") })
# class-specific method
isVoter.indiv <- function(object){
	if(object@age > 17){
		cat(object@name, "is of voting age.\n")
	} else cat(object@name, "is not of voting age.\n")
}
setMethod(isVoter, signature = c("indiv"), definition = isVoter.indiv)
isVoter(me)


# inheritance
setClass("BerkeleyIndiv",
	representation(
		CalID = "character"
	),
	contains = "indiv"
)
me <- new("BerkeleyIndiv", name = "Chris", age = 20, 
   birthday = as.Date('91-08-03'), CalID = "01349542")
isVoter(me)
is(me, "indiv")



showClass("data.frame")

### 4.3 Reference classes

# define a class
tsSimClass <- setRefClass("tsSimClass", 
	fields = list(
		   n = "numeric", 
		   times = "numeric",
		   corMat = "matrix",
		   lagMat = "matrix",
		   corParam = "numeric",
		   U = "matrix",
		   currentU = "logical"),

	methods = list(
		initialize = function(times = 1:10, corParam = 1, ...){
			   # we seem to need default values for the copy() method to function properly
			   require(fields)
			   times <<- times # field assignment requires using <<-
			   n <<- length(times)
			   corParam <<- corParam
			   currentU <<- FALSE
			   calcMats()
			   callSuper(...) # calls initializer of base class (envRefClass)
		},

		calcMats = function(){
			   # Python-style doc string
			   ' calculates correlation matrix and Cholesky factor ' 
			   lagMat <- rdist(times) # local variable
			   corMat <<- exp(-lagMat / corParam) # field assignment
			   U <<- chol(corMat) # field assignment
			   cat("Done updating correlation matrix and Cholesky factor\n")
			   currentU <<- TRUE
		},

		changeTimes = function(newTimes){
		   times <<- newTimes
		   calcMats()
		},

		show = function(){ # 'print' method
		   cat("Object of class 'tsSimClass' with ", n, " time points.\n", sep = '')
		}
	)
)


# add another method
tsSimClass$methods(list(

	simulate = function(){
		  ' simulates random processes from the model ' 
		   if(!currentU)
		  	 calcMats()
		   return(crossprod(U, rnorm(n)))
	})
)


# example usage
master <- tsSimClass$new(1:100, 10)
master
tsSimClass$help('calcMats')
devs <- master$simulate()
plot(master$times, devs, type = 'l')
mycopy <- master
myDeepCopy <- master$copy()
master$changeTimes(seq(0,1, length = 100))
mycopy$times[1:5]
myDeepCopy$times[1:5]


master$field('times')[1:5]
## # the next line is dangerous in this case, since
## # currentU will no longer be accurate
## master$field('times', 1:10)

#################################################
# 5: Creating and working in an environment
#################################################


e <- new.env()
assign('x', 3, envir = e) # same as e$x <- 3
e$x
get('x', envir = e, inherits = FALSE) 
# the FALSE avoids looking for x in the enclosing environments
e$y <- 5
objects(e)
rm('x', envir = e)
parent.env(e)


# using an environment to avoid R's pass-by-value semantics
myWalk <- new.env(); myWalk$pos = 0
nextStep <- function(walk) walk$pos <- walk$pos + sample(c(-1, 1), size = 1)
nextStep(myWalk)



eval(quote(pos <- pos + sample(c(-1, 1), 1)), envir = myWalk)
evalq(pos <- pos + sample(c(-1, 1), 1), envir = myWalk) 


#################################################
# 6: Computing on the language
#################################################

### 6.1 The R interpreter

plot.xy
print(`%*%`)

### 6.2 Parsing code and understanding language objects

obj <- quote(if (x > 1) "orange" else "apple")
as.list(obj)
class(obj)
weirdObj <- quote(`if`(x > 1, 'orange', 'apple'))
identical(obj, weirdObj)



x <- 3; typeof(quote(x))



myExpr <- expression(x <- 3)
eval(myExpr)
typeof(myExpr)



a <- quote(x <- 5)
b <- expression(x <- 5, y <- 3)
class(a)
class(b)
b[[1]]
class(b[[1]])
identical(a, b[[1]])

# here's what I did to gather the information for the table in the notes
obj1 = quote(x)
obj2 = expression(x <- 3)
obj3 = quote(f())
obj4 = quote(if(x < 3) y = 5)
obj5 = quote(x <- 3)
class(obj1); typeof(obj1)
class(obj2); typeof(obj2)
class(obj3); typeof(obj3)
class(obj4); typeof(obj4)
class(obj5); typeof(obj5)
is.call(obj5)
is.language(obj1)


e0 <- quote(3)
e1 <- expression(x <- 3) 
e1m <- expression({x <- 3; y <- 5}) 
e2 <- quote(x <- 3) 
e3 <- quote(rnorm(3))
print(c(class(e0), typeof(e0)))
print(c(class(e1), typeof(e1)))
print(c(class(e1[[1]]), typeof(e1[[1]])))
print(c(class(e1m), typeof(e1m)))
print(c(class(e2), typeof(e2)))
identical(e1[[1]], e2)
print(c(class(e3), typeof(e3)))
e4 <- quote(-7)
print(c(class(e4), typeof(e4))) # huh? what does this imply?
as.list(e4)



rm(x)
eval(e1)
rm(x)
eval(e2)
e1mlist <- as.list(e1m)
e2list <- as.list(e2)
eval(as.call(e2list)) 
# here's how to do it if the language object is actually an
# expression (a set of statements)
eval(as.expression(e1mlist))


# delving into the list-like structure of language objects
e1 = expression(x <- 3) 
# e1 is one-element list with the element an object of class '<-' 
print(c(class(e1), typeof(e1)))
e1[[1]]
as.list(e1[[1]])
lapply(e1[[1]], class)
y = rnorm(5)
e3 = quote(mean(y))
print(c(class(e3), typeof(e3)))
e3[[1]] 
print(c(class(e3[[1]]), typeof(e3[[1]])))
e3[[2]]
print(c(class(e3[[2]]), typeof(e3[[2]])))
# we have recursion
e3 = quote(mean(c(12,13,15)))
as.list(e3)
as.list(e3[[2]])

### 6.3 Manipulating the parse tree

out <- quote(y <- 3)
out[[3]] <- 4
eval(out)
y



e1 <- quote(4 + 5)
e2 <- quote(plot(x, y))
e2[[1]] <- `+`
eval(e2)
e1[[3]] <- e2
e1
class(e1[[3]]) # note the nesting
eval(e1) # what should I get?



codeText <- deparse(out)
parsedCode <- parse(text = codeText) # works like quote except on the code in the form of a string
eval(parsedCode)
deparse(quote(if (x > 1) "orange" else "apple"))



x3 <- 7
i <- 3
as.name(paste('x', i, sep=''))
eval(as.name(paste('x', i, sep='')))

### 6.4 Parsing replacement expressions

animals = c('cat', 'dog', 'rat','mouse')
out1 = quote(animals[4] <- 'rat') 
out2 = quote(`<-`(animals[4], 'rat'))  # same as out1
out3 = quote('[<-'(animals,4,'rat'))  # same as out2 when evaluated (see below),
as.list(out1)
as.list(out2)
identical(out1, out2)
as.list(out3)
identical(out1, out3)
typeof(out1[[2]]) # language
class(out1[[2]]) # call

# but parse tree is different (so parsing is not completely invertible)
as.list(out1)
as.list(out2)
identical(out1, out2)
as.list(out3)
identical(out1, out3)
typeof(out1[[2]])  # language
class(out1[[2]])   # call


eval(out1)
animals
animals[4] = 'dog'
eval(out3) 
animals # both do the same thing


# here's another example of a parsed replacement expression 
x = diag(rep(1, 3))
obj = quote(diag(x) <- 3)
as.list(obj)
class(obj[[2]])

### 6.5 substitute()

identical(quote(z <- x^2), substitute(z <- x^2))

e <- new.env(); e$x <- 3
substitute(z <- x^2, e)

e$z <- 5
substitute(z <- x^2, e)

f <- function(obj){
objName <- deparse(substitute(obj))
print(objName)
}
f(y)

substitute(a + b, list(a = 1, b = quote(x)))

e1 <- quote(x + y)
e2 <- substitute(e1, list(x = 3))

e2 <- substitute(substitute(e, list(x = 3)), list(e = e1))
substitute(substitute(e, list(x = 3)), list(e = e1)) 
# so e1 is substituted as an evaluated object, 
# which then allows for substitution for 'x' 
e2
eval(e2)


