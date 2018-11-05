# OuterJoinTester.R
# Copyright ? 2017 by Ernst Henle.  All rights reserved

rm(list=ls()) # Clear Workspace
cat("\014") # Clear Console

# Replace the following string with the path to your file and click source
fileToTest <- "/Users/duygusonmez/Desktop/FullJoins.R" 

print(paste("OuterJoinTest.R started", Sys.time()))
stopifnot(file.exists(fileToTest))
# install package with if not already installed
if (!require("compare")) {install.packages("compare", dep=TRUE, repos="http://cran.rstudio.com/")} else {" compare is already installed "}
# Now that the package is installed, we want to load the package so that we can use its functions
library(compare)

v1 <- c(1,2,3,1,2,3,1)
v2 <- c("a1","b1","c1","d1","e1","f1","g1")
tA <- data.frame(A1=v1,A2=v2,stringsAsFactors=FALSE)
v3 <- c(2,4,6,2,4,6,2)
v4 <- c("a2","b2","c2","d2","e2","f2","g2")
tB <- data.frame(B1=v3,B2=v4,stringsAsFactors=FALSE)
v5 <- c("a1","b1","b1","b1","c1","d1","e1","e1","e1","f1","g1",NA,NA,NA,NA)
v6 <- c(NA,"a2","d2","g2",NA,NA,"a2","d2","g2",NA,NA,"b2","c2","e2","f2")
tC <- data.frame(A2=v5,B2=v6,stringsAsFactors=FALSE)
tC <- tC[order(tC$A2, tC$B2), c("A2", "B2")]
OuterJoinStd <- function(tableA=tA, tableB=tB, keyA="A1", keyB="B1")
{
  merge(x=tableA, y=tableB, by.x=keyA, by.y=keyB, all=TRUE)
} # OuterJoin

OuterJoinTester <- function(FUN=NULL, FunStd=OuterJoinStd, tableA=tA, tableB=tB, keyA="A1", keyB="B1", Standard=tC)
{
  tryCatch(
    {
      ret <- FALSE
      if (!is.null(FunStd))
      {
        Standard <- FunStd(tableA, tableB, keyA, keyB)
        Standard <- Standard[!names(Standard) %in% c(keyA, keyB)]
        Standard <- Standard[order(Standard[1], Standard[2]), ]
        row.names(Standard) <- NULL
      } # if
      OjTestResult <- NULL
      if (!is.null(FUN))
      {
        OjTestResult <- FUN(tableA, tableB, keyA, keyB)
        OjTestResult <- OjTestResult[!names(OjTestResult) %in% c(keyA, keyB)]
        OjTestResult <- OjTestResult[order(OjTestResult[1], OjTestResult[2]), ]
        row.names(OjTestResult) <- NULL
      } # if
      ret <- compare(Standard, OjTestResult)$result
    }, error=function(e){
      cat("Failure! Function throws error \n")
      ret <- FALSE
    } # error
  ) # tryCatch
  return(ret)
} # OuterJoinTester

stopifnot(!OuterJoinTester())
stopifnot(OuterJoinTester(FUN=OuterJoinStd))

# Does the file source?
tryCatch(
  {
    sourced <- TRUE
    source(fileToTest)
    cat("Success!  Sourced", fileToTest," \n")
  }, error=function(e){
    cat("Error!  Cannot source", fileToTest," \n")
    sourced <- FALSE
  } # error
) # tryCatch

if (!exists("sourced"))
{
  cat("Sourced file removes objects!  Do not use rm(list=ls()) !\n")
} # if

TestBasics <- function(FUN)
{
  Success <- FALSE
  tryCatch(
    {
      zzz <- FUN(tA, tB, keyA="A1", keyB="B1")
      cat("Success!  Function runs without throwing error \n")
      if (class(zzz)[1] == "data.frame")
      {
        cat("Success!  Function returns data frame \n")
        Success <- TRUE
      } else
      {
        cat("Error!  Function does not return data frame \n")
      } # if else
    }, error=function(e){
      cat(fileToTest, "Error!  Function fails \n")
      print(paste('e:',e))
    } # error
  ) # tryCatch
  Success
} # TestBasics

CombinedTests <- function(FUN)
{
  if (TestBasics(FUN=FUN)) 
  {
    if (OuterJoinTester(FUN=FUN))
    {
      cat("Success!  Function returns expected result \n")
    } else
    {
      cat("Failure!  Function returns unexpected result \n")
    } # if else
  } # if
} # CombinedTests

cat("############################# \n")
cat("OuterJoin1 \n")
CombinedTests(OuterJoin1)
cat("############################# \n")
cat("OuterJoin2 \n")
CombinedTests(OuterJoin2)
cat("############################# \n")
cat("OuterJoin3 \n")
CombinedTests(OuterJoin3)
cat("############################# \n")
print(paste("OuterJoinTest.R completed", Sys.time()))
