##################################################
### Demo code for Unit 3 of Stat243, "Reading and Writing to/from R"
### Chris Paciorek and Tessa Childers-Day, September 2013
##################################################

#####################################################
# 2: Reading data from text files into R
#####################################################

 getwd()  # a common error is not knowing what directory R is looking at
 setwd('../data')
 dat <- read.table('RTADataSub.csv', sep = ',', head = TRUE)
 lapply(dat, class)
 levels(dat[ ,2])
 dat2 <- read.table('RTADataSub.csv', sep = ',', head = TRUE,
    na.strings = c("NA", "x"), stringsAsFactors = FALSE)
 unique(dat2[ ,2])
 # hmmm, what happened to the blank values this time?
 which(dat[ ,2] == "")
 dat2[which(dat[, 2] == "")[1], ] # deconstruct it!
 sequ <- read.table('hivSequ.csv', sep = ',', header = TRUE,
   colClasses = c('integer','integer','character',
     'character','numeric','integer'))
 # let's make sure the coercion worked - sometimes R is obstinant
 lapply(sequ, class)
 # that made use of the fact that a data frame is a list



dat <- readLines('../data/precip.txt')
id <- as.factor(substring(dat, 4, 11) )
year <- substring(dat, 17, 20)
year[1:5]
class(year)
year <- as.integer(substring(dat, 18, 21))
month <- as.integer(substring(dat, 22, 23))
nvalues <- as.integer(substring(dat, 28, 30))



 dat <- readLines(pipe("ls -al"))
 dat <- read.table(pipe("unzip dat.zip"))
 dat <- read.csv(gzfile("dat.csv.gz"))
 dat <- readLines("http://www.stat.berkeley.edu/~paciorek/index.html")



 con <- file("../data/precip.txt", "r") # "r" for 'read' - you can also open files for writing with "w" (or "a" for appending)
 class(con)
 blockSize <- 1000 # obviously this would be large in any real application
 nLines <- 300000
 for(i in 1:ceiling(nLines / blockSize)){
 	lines <- readLines(con, n = blockSize)
 	# manipulate the lines and store the key stuff
 }
 close(con)



dat <- readLines('../data/precip.txt')
con <- textConnection(dat[1], "r")
read.fwf(con, c(3,8,4,2,4,2))


#####################################################
# 3: Output from R
#####################################################

temps <- c(12.5, 37.234324, 1342434324.79997234, 2.3456e-6, 1e10)
sprintf("%9.4f C", temps)



val <- 1.5
cat('My value is ', val, '.\n', sep = '')
print(paste('My value is ', val, '.', sep = ''))



 # input
 x <- 7
 n <- 5
 # display powers
 cat("Powers of", x, "\n")
 cat("exponent   result\n\n")
 result <- 1
 for (i in 1:n) {
 	result <- result * x
 	cat(format(i, width = 8), format(result, width = 10),"\n", sep = "")
 }
 x <- 7
 n <- 5
 # display powers
 cat("Powers of", x, "\n")
 cat("exponent result\n\n")
 result <- 1
 for (i in 1:n) {
 	result <- result * x
 	cat(i, '\t', result, '\n', sep = '')
 }


#####################################################
# 4: File and string encodings
#####################################################

Sys.getlocale()



text <- "_Melhore sua seguran\xe7a_"
iconv(text, from = "latin1", to = "UTF-8")
iconv(text, from = "latin1", to = "ASCII", sub = "???")



x <- "fa\xE7ile" 
Encoding(x) <- "latin1" 
x
# playing around... 
x <- "\xa1 \xa2 \xa3 \xf1 \xf2" 
Encoding(x) <- "latin1" 
x 



load('../data/IPs.RData') # loads in an object named 'text'
tmp <- substring(text, 1, 15)
# the issue occurs with the 6402th element (found by trial and error):
tmp <- substring(text[1:6401],1,15)
tmp <- substring(text[1:6402],1,15)
text[6402] # note the Latin-1 character
text <- iconv(text, from = "latin1", to = "UTF-8")
text[6402]
tmp <- substring(text, 1, 15)
tmp[6402]
# Interesting:
table(Encoding(text))


