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

### 1.2: Strategies for improving efficiency

### 1.2.1: Fast initialization

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

### 1.2.2: Vectorized calculations

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

### 1.2.3: Using apply() and specialized functions

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


### 1.2.4: Matrix algebra efficiency

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

### 1.2.5: Fast mapping/lookup tables

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

### 1.2.7: Byte compiling

library(compiler); library(rbenchmark)
f <- function(x){
	for(i in 1:length(x)) x[i] <- x[i] + 1
	return(x)
}
fc <- cmpfun(f)
fc # notice the indication that the function is byte compiled.
x <- rnorm(100000)
benchmark(f(x), fc(x), x <- x + 1, replications = 5)



