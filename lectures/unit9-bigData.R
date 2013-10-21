############################################################
### Demo code for Unit 9 of Stat243, "Databases and Big Data"
### Chris Paciorek, October 2013
############################################################

#####################################################
# 2: Databases
#####################################################

### 2.2 Accessing databases in R

## @knitr chunk1
library(RSQLite)

fileName <- "/mirror/data/pub/html/scf/cis.db"
drv <- dbDriver("SQLite")
db <- dbConnect(drv, dbname = fileName) # using a connection once again!
# con <- dbConnect(SQLite(), dbname = fileName) # alternative

# get information on the database schema
dbListTables(db)
dbListFields(db, "articles")
dbListFields(db, "authors")
dbListFields(db, "authorships")


## @knitr chunk2

auth <- dbSendQuery(db, "select * from authorships")
fetch(auth, 5)
dbClearResult(auth)

query <- "select id from authors where name like 'Breiman%'"
a_ids <- dbGetQuery(db, query)


a_ids <- as.list(unlist(a_ids))
query <- paste("select id_title from authorships where author_id in (",
               paste(rep("?", length(a_ids)), collapse = ","), ")")
query
a_ids
t_ids <- dbGetQuery(db, query, a_ids)
t_ids$id_title[1:5]

t_ids <- as.list(unlist(t_ids))
query <- paste("select * from articles where id_title in (",
               paste(rep("?", length(t_ids)), collapse = ","), ")")
titles <- dbGetQuery(db, query, t_ids)
head(titles)
# do a google scholar check to see that things seem to be ok

## @knitr chunk3

# alternatively, we can do a query that involves multiple tables
info <- dbGetQuery(db, "select * from articles, authors, authorships where authors.name like 'Breiman%' and authors.id = authorships.author_id and authorships.id_title = articles.id_title")
# "select * from articles, authors, authorships where authors.name
#  like 'Breiman%' and authors.id = authorships.author_id and
#  authorships.id_title = articles.id_title"
head(info)

## @knitr chunk4
# that db is read-only; to create a view we need to be able to modify it
system(paste0('cp ', fileName, ' /tmp/.'))
dbDisconnect(db)
db <- dbConnect(drv, dbname = '/tmp/cis.db') 

# finally, we can create a view that amounts to joining the tables
fullAuthorInfo <- dbSendQuery(db, 'create view fullAuthorInfo as select * from authors join authorships on authorships.author_id = authors.id')
# 'create view fullAuthorInfo as select * from authors join
#  authorships on authorships.author_id = authors.id'

partialArticleInfo <- dbSendQuery(db, 'create view partialArticleInfo as select * from articles join fullAuthorInfo on articles.id_title=fullAuthorInfo.id_title')
# 'create view partialArticleInfo as select * from articles join
#  fullAuthorInfo on articles.id_title=fullAuthorInfo.id_title'

fullInfo <- dbSendQuery(db, 'select * from journals join partialArticleInfo
   on journals.id = partialArticleInfo.journal_id')
# 'select * from journals join partialArticleInfo on
#  journals.id = partialArticleInfo.journal_id')
subData <- fetch(fullInfo, 3)
subData
dbClearResult(fullInfo)

## @knitr extra

# demo that cross and inner joins can be the same
tmp <- dbSendQuery(db, 'select * from journals join partialArticleInfo on journals.id = partialArticleInfo.journal_id order by id_entity')
sub1 <- fetch(tmp, 100)
dbClearResult(tmp)
tmp <- dbSendQuery(db, 'select * from journals cross join partialArticleInfo where journals.id = partialArticleInfo.journal_id order by id_entity')
sub2 <- fetch(tmp, 100)
dbClearResult(tmp)
identical(sub1, sub2)

dbDisconnect(db)

## @knitr extra-end

#####################################################
# 3: R and big data
#####################################################

### 3.1 Working with big datasets on disk

## @knitr bigmemory-prep, engine='bash'

for yr in {1987..2008}; do
 curl http://stat-computing.org/dataexpo/2009/${yr}.csv.bz2 -o /scratch/users/paciorek/${yr}.csv.bz2
done

cd /scratch/users/paciorek
cp 1987.csv.bz2 AirlineDataAll.csv.bz2
bunzip2 AirlineDataAll.csv.bz2
for yr in {1988..2008}; do
  bunzip2 ${yr}.csv.bz2 -c | tail -n +2 >> AirlineDataAll.csv
done

python AirlineFormatter.py


# try to determine types and values of fields...
cut -d',' -f11 AirlineDataAll.csv | sort | uniq | less
cut -d',' -f29 AirlineDataAll.csv | sort | uniq | less

cp /scratch/users/paciorek/AirlineDataAll.csv /tmp/.

# create a small test file for testing our code
head -n 10000 AirlineDataAll.csv > test.csv


## @knitr ff

require(ff)
require(ffbase)

# I put the data file on local disk on the machine I am using
# (/tmp on arwen)
fileName <- 'test.csv'
dat <- read.csv.ffdf(file = fileName, header = TRUE,
     colClasses = c('integer', rep('factor', 3),
     rep('integer', 4), 'factor', 'integer', 'factor',
     rep('integer', 5), 'factor','factor', rep('integer', 4),
     'factor', rep('integer', 6)))


fileName <- 'AirlineDataAll.csv'
system.time(  dat <- read.csv.ffdf(file = fileName, header = TRUE,
    colClasses = c('integer', rep('factor', 3), rep('integer', 4),
    'factor', 'integer', 'factor', rep('integer', 5), 'factor',
    'factor', rep('integer', 4), 'factor', rep('integer', 6))) )
# takes about 40 minutes

system.time(ffsave(dat, file = '/tmp/AirlineDataAll'))
## file is saved as AirlineDataAll.ffData

## with metadata in AirlineDataAll.RData

## takes a while - I forgot to record how long

system.time(ffload('/tmp/AirlineDataAll'))
# this is much quicker:
# 78.156  15.836 169.974 

## @knitr tableInfo

# load again as previous chunk not run w/in pdf compilation

# note that ideally we'd want this on local disk

ffload('/tmp/AirlineDataAll')
# [1] "tmp/RtmpU5Uw6z/ffdf4e684aecd7c4.ff" "tmp/RtmpU5Uw6z/ffdf4e687fb73a88.ff"
# [3] "tmp/RtmpU5Uw6z/ffdf4e6862b1033f.ff" "tmp/RtmpU5Uw6z/ffdf4e6820053932.ff"
# [5] "tmp/RtmpU5Uw6z/ffdf4e681e7d2235.ff" "tmp/RtmpU5Uw6z/ffdf4e686aa01c8.ff"
# ...

dat$Dest
# ff (closed) integer length=123534969 (123534969) levels: BUR LAS LAX OAK PDX RNO SAN SFO SJC SNA
# ABE ABQ ACV ALB ALO AMA ANC ATL AUS AVP AZO BDL BFL BGR BHM BIL BLI BNA BOI BOS BTV BUF BWI CAE
# CAK CCR CHS CID CLE CLT CMH CMI COS CPR CRP CRW CVG DAB DAL DAY DCA DEN DFW DLH DRO DSM DTW ELP
# EUG EVV EWR FAI FAR FAT FLG FLL FOE FSD GCN GEG GJT GRR GSO GSP GTF HNL HOU HPN HRL HSV IAD IAH
# ICT ILG ILM IND ISP JAN JAX JFK KOA LBB LEX LGA LGB LIH LIT LMT LNK MAF MBS MCI MCO MDT MDW MEM
# MFR MHT MIA MKE MLB MLI MOB MRY MSN MSP MSY OGG OKC OMA ONT ORD ORF PBI PHL PHX PIA PIT PNS PSC
# ...

DestTable <- sort(table.ff(dat$Dest), decreasing = TRUE)
# table is not a generic...

# takes a while

#    ORD     ATL     DFW     LAX     PHX     DEN     DTW     IAH     MSP     SFO

# 6638035 6094186 5745593 4086930 3497764 3335222 2997138 2889971 2765191 2725676

#    STL     EWR     LAS     CLT     LGA     BOS     PHL     PIT     SLC     SEA

#  2720250 2708414 2629198 2553157 2292800 2287186 2162968 2079567 2004414 1983464 

# looks right - the busiest airports are ORD (O'Hare in Chicago) and ATL (Atlanta)

dat$DepDelay[1:50]
#opening ff /tmp/RtmpU5Uw6z/ffdf4e682d8cd893.ff
#  [1] 11 -1 11 -1 19 -2 -2  1 14 -1  5 16 17  1 21  3 13 -1 87 19 31 17 32  0  1
# [26] 29 26 15  5 54  0 25 -2  0 12 14 -1  2  1 16 15 44 20 15  3 21 -1  0  7 23

min.ff(dat$DepDelay, na.rm = TRUE)
# [1] -1410
max.ff(dat$DepDelay, na.rm = TRUE)
# [1] 2601

# tmp <- clone(dat$DepDelay) # make a deep copy

## @knitr airline-model

require(ffbase)
require(biglm)

datUse <- subset(dat, dat$DepDelay < 60*12 & dat$DepDelay > (-30) &
                 !is.na(dat$DepDelay))

# any concern about my models?
system.time(mod <- bigglm(DepDelay ~ Year, data = datUse))
# 834.008   1.044 838.902
summary(mod)

system.time(mod <- bigglm(DepDelay ~ Year + Month + DayOfWeek, data = dat))
# 1990.676   4.837   1995.974
summary(mod)

## @knitr significance-prep

n <- 150000000  # n*4*8/1e6 Mb of RAM (~5 Gb)
# but turns out to be 11 Gb as a text file
nChunks <- 100
chunkSize <- n/nChunks

set.seed(0)

for(p in 1:nChunks) {
  x1 <- runif(chunkSize)
  x2 <- runif(chunkSize)
  x3 <- runif(chunkSize)
  y <- rnorm(chunkSize, .001*x1, 1)
  write.table(cbind(y,x1,x2,x3), file = '/tmp/signif.csv',
     sep = ',', col.names = FALSE,  row.names = FALSE,
     append = TRUE, quote = FALSE)
}


fileName <- '/tmp/signif.csv'
system.time(  dat <- read.csv.ffdf(file = fileName,
   header = FALSE, colClasses = rep('numeric', 4)))
# 922.213  18.265 951.204

names(dat) <- c('y', 'x1','x2', 'x3')
ffsave(dat, file = '/tmp/signif')

## @knitr significance-model
system.time(ffload('/tmp/signif'))
# 52.323   7.856  60.802 

system.time(mod <- bigglm(y ~ x1 + x2 + x3, data = dat))
#  1957.358    8.900 1966.644 

options(digits = 12)
summary(mod)


# R^2 on a subset (why can it be negative?)
coefs <- summary(mod)$mat[,1]
wh <- 1:1000000
1 - sum((dat$y[wh] - coefs[1] + coefs[2]*dat$x1[wh] +
  coefs[3]*dat$x2[wh] + coefs[4]*dat$x3[wh])^2) /
  sum((dat$y[wh] - mean(dat$y[wh]))^2)

## @knitr endchunk

#####################################################
# 4: Sparsity
#####################################################


## @knitr spam
require(spam)
mat = matrix(rnorm(1e8), 1e4)
mat[mat > (-2)] <- 0
sMat <- as.spam(mat)
print(object.size(mat), units = 'Mb')
print(object.size(sMat), units = 'Mb')

vec <- rnorm(1e4)
system.time(mat %*% vec)
system.time(sMat %*% vec)
