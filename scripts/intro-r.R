
## You can recreate the figure exactly, even if you don't know what you're doing!
## Just need to make sure that the data file "sade1.txt" is in a subdirectory (i.e., folder) called "data"
## and that this subdirectory resides in the same directory that the subdirectory containing this script lives
## fortunately, that is the way these marterials are organized!
require(igraph)
rhesus <- read.table("../data/sade1.txt", skip=1, header=FALSE)
rhesus <- as.matrix(rhesus)
nms <- c("066", "R006", "CN", "ER", "CY", "EC", "EZ", "004", "065", "022", "076", 
         "AC", "EK", "DL", "KD", "KE")
sex <- c(rep("M",7), rep("F",9))
dimnames(rhesus)[[1]] <- nms
dimnames(rhesus)[[2]] <- nms
grhesus <- graph_from_adjacency_matrix(rhesus, weighted=TRUE)
V(grhesus)$sex <- sex

rhesus.layout <- layout.kamada.kawai(grhesus)
plot(grhesus, 
     edge.width=log10(E(grhesus)$weight)+1, 
     edge.arrow.width=0.5,
     vertex.label=V(grhesus)$name,
     vertex.label.family="Helvetica",
     vertex.color=as.numeric(V(grhesus)$sex=="F")+5, 
     layout=rhesus.layout)


## comments, enclosing commands in parentheses 
# a comment
x <- c(1,2,3)
(y <- c(1,2,3))


## R as a calculator
# addition
2+2
# multiplication
2*3
a <- 2
b <- 3
a*b
# division
2/3
b/a
1/b/a
# note order of operations!
1/(b/a)
# parentheses can override order of operations
# an exponential
exp(-2)
# why we age
r <- 0.02
exp(-r*45)
# something more tricky
exp(log(2))
# generate 20 normally distributed random numbers
rnorm(20)


## everything is a float!
# it looks like an integer, but don't be fooled!
a <- 2
is.numeric(a)
is.integer(a)
is.double(a)


## Unless you tag on an "L" 
a <- 2L
is.integer(a)


## Characters
(countries <- c("Uganda", "Tanzania", "Kenya", "Rwanda"))
as.character(1:5)


## Factors (for use in statistical models)
countries <- factor(c("Uganda", "Tanzania", "Kenya", "Rwanda"))
countries
# is that the order you expected? watch out for that!
# a trick to get some insight into how factors are handled by R
unclass(countries)
countries1 <- c("Uganda", "Tanzania", "Kenya", "Rwanda")
countries1 == unclass(countries1)
countries == unclass(countries)


## Logicals
t.or.f <- c(T,F,F,T,T)
is.logical(t.or.f)
aaa <- c(1,2,3,4,5)
# subset
aaa[t.or.f]


## Lists, the most versatile data type
## the output of most functions comes as a list -- this can trip up the unaware
child1 <- list(name="mary", child.age=6,
status="foster",mother.alive=F, father.alive=T, parents.ages=c(24,35))
str(child1)


## Coercion
countries <- factor(c("Uganda", "Tanzania", "Kenya", "Rwanda"))
as.character(countries)
as.numeric(countries)
# werk it backwards
countries1 <- c("Uganda", "Tanzania", "Kenya", "Rwanda")
as.factor(countries1)
# sometimes you want your numbers to actually be strings (e.g., when you make labels or column names)
as.character(1:5)
# there actually is an integer class; it just doesn't get used much at all
a <- 2
is.integer(a)
is.integer(as.integer(a))


## Everything is a vector
( manual <- c(1,3,5,7,9))
( count <- 1:20 )
( ages <- seq(0,85,by=5) )
( ones <- rep(1,10) )
( fourages <- rep(c(1,2),c(5,10)) )
( equalspace <- seq(1,5, length=20) )


rep(2,10)
## rep() is your friend for creating vectors of repeated values
rep(c(1,2),10)
rep(c(1,2), c(5,10))
rep("R roolz!", 3)


## binding vectors together
# age distribution of Gombe chimps in 1980 and 1986
cx1980 <- c(7, 13, 8, 13, 5, 35, 9)
cx1988 <- c(9, 11, 15, 8, 9, 38, 0)
( C <- cbind(cx1980, cx1988) )
# another way
C <- c(cx1980, cx1988)
(  C <- matrix(C, nrow=7, ncol=2) )


## what happens when vectors aren't the same length?
# age distribution at Tai; Boesch uses fewer age classes
cxboesch <- c(18,10,15,30)
( C <- cbind(C,cxboesch) )


## it's actually a feature, not a bug
( X <- matrix(1,nr=3,nc=3) )


## cross-classified data on hair/eye color 
freq <- c(32,11,10,3,  38,50,25,15,  10,10,7,7,  3,30,5,8)
hair <- c("Black", "Brown", "Red", "Blond")
eyes <- c("Brown", "Blue", "Hazel", "Green")
freqmat <- matrix(freq, nr=4, nc=4, byrow=TRUE)
dimnames(freqmat)[[1]] <- hair
dimnames(freqmat)[[2]] <- eyes
freqmat
# might as well do something with it
mosaicplot(freqmat)


## Data frames
# five columns of data
satu <- c(1,2,3,4,5)
dua <- c("a","b","c","d","e")
tiga <- sample(c(TRUE,FALSE),5,replace=TRUE)
empat <- LETTERS[7:11]
lima <- rnorm(5)
# construct a data frame
(collection <- data.frame(satu,dua,tiga,empat,lima))
# extract the third variable
collection$tiga


## Working directories
getwd()
setwd("/Users/jhj1/Projects/git/AABA2023_Workshop/Markdown")


## More often than not, you read these into R
## tables read in using read.table() become data frames
## note that some functions require data to be of the matrix type, not data.frame (need to coerce)
# read a space-delimitted file (a sociomatrix of kids 17 kids aggressive acts toward each other)
(kids <- read.table("../data/strayer_strayer1976-fig2.txt", header=FALSE))


## Skipping lines
quercus <- read.delim("../data/quercus.txt", skip=24, sep="\t", header=TRUE)
head(quercus)


## The Workspace
objects()
rm(aaa)
rm(list=ls())
objects()


## Scope
## load it again because we cleared all objects!
quercus <- read.delim("../data/quercus.txt", skip=24, sep="\t", header=TRUE)
plot(quercus$tree.height, quercus$acorn.size, pch=16, col="red", xlab="Tree Height (m)", ylab="Acorn Size (cm3)")


## Is this easier?
with(quercus, plot(tree.height, acorn.size, pch=16, col="blue", xlab="Tree Height (m)", ylab="Acorn Size (cm3)"))


## Indexing and subsetting
myvec <- c(1,2,3,4,5,6,66,77,7,8,9,10)
myvec[1]
myvec[1:5]
myvec[-1]
myvec[-(1:5)]
# try without the parentheses
#myvec[-1:5]
myvec[c(2,5,1,11)]


## Access the elements of a data frame using the dollar sign
dim(quercus)
size <- quercus$acorn.size
size[1:3] #first 3 elements
size[17]  #only element 17
size[-39] #all but the last element
size[c(3,6,9)] # elements 3,6,9
size[quercus$Region=="California"] # use a logical test to subset
quercus[3,4] # access an element of an array or data frame by X[row,col]
quercus[,"tree.height"]


## More subsetting
myvec <- c(1,2,3,4,5,6,66,77,7,8,9,10)
myvec <- myvec[myvec<=10]
myvec
x <- 1:7
# elements that are greater than 2 but less than 6
(x>2) & (x<6)
# this is not intuitive
(x>2) && (x<6)
# but dis doe...
(x<2) && (x<6)
is.even <- rep(c(FALSE, TRUE),5)
(evens <- myvec[is.even])


## Missing values (NA)
aaa <- c(1,2,3,NA,4,5,6,NA,NA,7,8,9,NA,10)
aaa <- aaa[!is.na(aaa)]
aaa


## applying logical tests to NAs
aaa <- c(1,2,3,NA,4,5,6,NA,NA,7,8,9,NA,10)
is.na(aaa)
!is.na(aaa)


## table() for quick-n-dirty summaries of vectors
aaa <- rpois(100,5)
table(aaa)


## table() works for dimensions >1 too
donner <- read.table("../data/donner.dat", header=TRUE, skip=2)
# survival=0 == died; male=0 == female
with(donner, table(male,survival))
# table along 3 dimensions
with(donner, table(male,survival,age))
cage <- rep(0,length(donner$age))
# simplify by defining 2 age classes: over/under 25
cage[donner$age<=25] <- 1
cage[donner$age>25] <- 2
donner <- data.frame(donner,cage=cage)
with(donner, table(male,survival,cage))


## Sorting vectors
aaa <- rpois(100,5)
sort(aaa)
# decreasing order
sort(aaa,decreasing=TRUE)


## Sorting data frames (this is why we use dplyr)
# five columns of data again
satu <- c(1,2,3,4,5)
dua <- c("a","b","c","d","e")
tiga <- sample(c(TRUE,FALSE),5,replace=TRUE)
empat <- LETTERS[7:11]
lima <- rnorm(5)
# construct a data frame
(collection <- data.frame(satu,dua,tiga,empat,lima))
o <- order(collection$lima)
collection[o,]


## Naming data
## load it again because we cleared all objects!
kids <- read.table("../data/strayer_strayer1976-fig2.txt", header=FALSE)
kid.names <- c("Ro","Ss","Br","If","Td","Sd","Pe","Ir","Cs","Ka",
                "Ch","Ty","Gl","Sa", "Me","Ju","Sh")
colnames(kids) <- kid.names
rownames(kids) <- kid.names
kids


## apply(), lapply(), and sapply()
## utility mostly supplanted by dplyr and related tidyverse tools
aaa <- list(alpha = 1:10, beta = rnorm(100), x = sample(1:100, 100, replace=TRUE))
lapply(aaa,mean)
# more compact as a vector
sapply(aaa,mean)
# cross-tabulation of sex partners from NHSLS
sextable <- read.csv("../data/nhsls_sextable.txt", header=FALSE)
dimnames(sextable)[[1]] <- c("white","black","hispanic","asian","other")
dimnames(sextable)[[2]] <- c("white","black","hispanic","asian","other")
# calculate marginals
(row.sums <- apply(sextable,1,sum))
(col.sums <- apply(sextable,2,sum))
# using sapply() gives similar output
sapply(sextable,sum)
# compare the output of sapply() to lapply()
lapply(sextable,sum)


## if, ifelse
(coin <- sample(c("heads","tails"),1))
if(coin=="tails") b <- 1 else b <- 0
b

x <- 4:-2
# sqrt(x) produces warnings, but using ifelse to check works without producing warings
sqrt(ifelse(x >= 0, x, NA))


## for loops
x <- 1:5
for(i in 1:5) print(x[i])


## loading packages
library(igraph)
# might as well do something with it
# a small graph
g <- graph( c(1,2, 1,3, 2,3, 3,5), n=5 )
plot(g)

