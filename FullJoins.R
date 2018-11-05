# Assignment 5

# Once you install the package, load it so that you can use its functions
library(sqldf) 

OuterJoin1 <- function(tableA, tableB, keyA, keyB) 
{
  results1 <- merge(tableA, tableB, by.x=keyA, by.y=keyB, all=TRUE) 
  return(results1)
}

OuterJoin2 <- function(tableA, tableB, keyA, keyB)
{ 
  # Since keyA and keyB are the type of character, you use paste to get the values of keyA and keyB. 
   
   sqlstr <- paste("select TA1.*, TB1.* from tableA TA1 left join tableB TB1 on TA1.",keyA, "=TB1.",keyB, 
                    " UNION ",
                    "select TA2.*, TB2.* from tableB TB2 left join tableA TA2 on TA2.",keyA, "=TB2.",keyB)
   results2 <- sqldf(sqlstr) 
   return(results2)
} 
  
OuterJoin3 <- function(tableA, tableB, keyA, keyB)
{
 sqlstr1 <- paste("select tbA.*, tbB.* from tableA tbA left join tableB tbB on tbA.", keyA, " =tbB.", keyB) 
 sqlstr2 <- paste("select tbA.*, tbB.* from tableB tbB left join tableA tbA on tbA.", keyA, " =tbB.", keyB)
  
 A1 <- sqldf(sqlstr1)
 A2 <- sqldf(sqlstr2) 
 
 results3 <- unique(rbind(A1, A2))
 return(results3)
}
