#AUTHOR: SANBORN, KATE
#ST758 HW5
#28SEP2021
################################################################################

#Q1 EIGENDECOMP OF SYMM VERSUS NONSYMM
#we want to study the difference for eigencomp for symm versus nonsymm 

library(RSpectra)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)

# Registering cores for parallel process
library(doSNOW)
cl <- makeCluster(4, type="SOCK") # 4 - number of cores
registerDoSNOW(cl) # Register back end Cores for Parallel Computing

#creating list of m values 
#talked to dr. sengupta about scaling down m values because estimated time to run this sim is 9 days!!!!
mlist <- list(seq(from = 100, to = 1000, by = 100))
grid <- tibble(do.call(expand.grid, mlist)) %>% mutate(timeA=0,timeB=0)
grid <- rownames_to_column(grid, var="seed") # set seed for each setting of m in mlist
grid$seed <- as.numeric(grid$seed)
# repeat each setting 100 times for each m in mlist
grid <- grid %>% mutate(freq=100) %>% uncount(freq)

symmnonsymm<-function(m, seed){
  set.seed(seed)

    A<-matrix(rnorm(m*m, mean=0,sd=1), m, m) #arbitrarily selecting to use std normal
    
    B<-A+t(A) #copy matrix A call it B
    #B[lower.tri(B)] = t(B)[lower.tri(B)] #making symmmetric matrix B from random A
    
    startA<-proc.time()[[3]] #start clock for A
    aeig<-eigen(A,symmetric = FALSE, only.values = TRUE)
    elaa<-proc.time()[[3]] -startA
    
    startB<-proc.time()[[3]] #start clock for B
    beig<-eigen(B,symmetric = TRUE, only.values = TRUE)
    elab<-proc.time()[[3]]-startB
  #return elapsed times for eigencomp of A and B
  mylist<-c(elaa, elab)
  return(mylist)
  
}

#testing
symmnonsymm(10,100)

cl2<-parallel::makeCluster(detectCores())
doParallel::registerDoParallel(cl2)

#dump results here
results <- foreach(m=grid$Var1, seed=grid$seed) %dopar% { out<-symmnonsymm(m,seed) 
return(out)}

#throwing output into grid
for(i in 1:1000){
    grid[i,3]<-results[[i]][[1]]
    grid[i,4]<-results[[i]][[2]]
  
}

grid2<-grid[,c(2:4)] #get rid of seed

#creating mean and standard dev tables
library(dplyr)

grid2$timeA<-as.numeric(grid2$timeA)
grid2$timeB<-as.numeric(grid2$timeB)


outt<-grid2 %>% group_by(as.factor(Var1)) %>% summarise_each(funs(mean, sd))
outt<-outt[,c(1,3,4,6,7)]
outt$SizeofMatrix_M<-outt$`as.factor(Var1)`
outt<-outt[,c(6,2,3,4,5)]

