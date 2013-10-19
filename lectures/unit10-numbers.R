#########################################################
### Demo code for Unit 10 of Stat243, "Computer numbers"
### Chris Paciorek, October 2013
#########################################################

############################
# 1: Basic representations
############################

a <- as.integer(3423333)
a * a


x <- rnorm(100000)
y <- 1:100000
z <- rep('a', 100000)
object.size(x) # 
object.size(y) # so how many bytes per integer in R?
object.size(z) # hmm, what's going on here?

############################
# 2: Floating point basics
############################

### 2.1 Representing real numbers

0.3 - 0.2 == 0.1
0.3
0.2
0.1 # Hmmm...
options(digits=22)
a <- 0.3
b <- 0.2
a - b 
0.1
1/3
# so empirically, it looks like we're accurate up to the 16th decimal place


1e-16 + 1
1e-15 + 1
2e-16 + 1
.Machine$double.eps
.Machine$double.eps + 1

.1
.5
.25
.26
1/32
1/33

### 2.2 Overflow and underflow

log10(2^1024) # whoops ... we've actually just barely overflowed
log10(2^1023)

.Machine$double.xmax
.Machine$double.xmin

### 2.3 Integers or floats

x <- 2^45
z <- 25
class(x)
class(z)
as.integer(x)
as.integer(z)
1e308
as.integer(1e308)
1e309


x <- 3; typeof(x)
x <- as.integer(3); typeof(x)
x <- 3L; typeof(x)

### 2.4 Precision

options(digits = 22)
# large vs. small numbers
.1234123412341234 
1234.1234123412341234 # not accurate to 16 places 
123412341234.123412341234 # only accurate to 4 places 
1234123412341234.123412341234 # no places! 
12341234123412341234 # fewer than no places! 

# How precision affects calculations
1234567812345678 - 1234567812345677
12345678123456788888 - 12345678123456788887
.1234567812345678 - .1234567812345677
.12345678123456788888 - .12345678123456788887
.00001234567812345678 - .00001234567812345677 
# not as close as we'd expect, should be 1e-20
.000012345678123456788888 - .000012345678123456788887

123456781234 - .0000123456781234 # the correct answer is 123456781233.99998765....

1000000.1


2^52
2^52+1
2^53
2^53+1
2^53+2
2^54
2^54+2
2^54+4

### 2.5 Working with higher precision numbers

require(Rmpfr)
piLong <- Const("pi", prec = 260) # pi "computed" to correct 260-bit precision 
piLong # nicely prints 80 digits 
mpfr(".1234567812345678", 40)
mpfr(".1234567812345678", 80)
mpfr(".1234567812345678", 600)



##################################################
# 3: Implications for calculation and comparisons
##################################################

### 3.1 Computer arithmetic is not mathematical arithmetic!

val1 <- 1/10; val2 <- 0.31; val3 <- 0.57
res1 <- val1*val2*val3
res2 <- val3*val2*val1
identical(res1, res2)
res1
res2

### 3.3 Comparisons

a = 12345678123456781000
b = 12345678123456782000

approxEqual = function(a, b){
  if(abs(a - b) < .Machine$double.eps * abs(a + b))
    print("approximately equal") else print ("not equal")
}

approxEqual(a,b)

a = 1234567812345678
b = 1234567812345677

approxEqual(a,b)   

### 3.4 Calculations

# catastrophic cancellation w/ large numbers
123456781234.56 - 123456781234.00
# how many accurate decimal places?

# catastrophic cancellation w/ small numbers
a = .000000000000123412341234
b = .000000000000123412340000

# so we know the right answer is .000000000000000000001234 EXACTLY

a-b  # 1.233999993151397490851e-21, which is accurate only to 8 places + 21 = 29 places, as expected from a machine epsilon calculation, since the "1" is in the 13th position, plus 16 more
# ideally we would have accuracy to 37 places on a computer since the actual result is of order 10^-21 and 37 = 21 + 16, but we've lost 8 digits of accuracy (the '12341234' cancellation) to catastrophic cancellation

x <- c(-1, 0, 1) + 1e8
n <- length(x)
sum(x^2)-n*mean(x)^2 # that's not good!
sum((x - mean(x))^2)

# adding/subtracting numbers very different in magnitude gives precision of the larger number
123456781234 - 0.000001

# part of the predictive density calculation example:
exp(-1000)

xs <- 1:100
dists <- rdist(xs)
corMat <- exp(- (dists/10)^2) # this is a p.d. matrix (mathematically)
eigen(corMat)$values[80:100]  # but not numerically


