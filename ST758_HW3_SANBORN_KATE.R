#ST758 HOMEWORK 3 - DR. SENGUPTA
#AUTHOR: SANBORN, KATE
#DATE: 12SEP2021
####################################
####################################

#Q1: QUICKSORT
##############
#first write a function for implementing quicksort. it should take a vector as 
#input and return an ordered vector small to large



#worst case selects first element as pivot
quicksortworstcase <- function(vect){
  start<- proc.time()
  #if vector has one element then no need to run. return vector
  if(length(vect)<=1){ 
    return(vect) 
  } else {
    pivot <- vect[1] #take first element of vector
    chunk <- vect[-1] #take the rest of the vector minus first element
    pivstay <- chunk[chunk>pivot] #return a vector of elements in chunk that are larger than pivot
    pivmove <- chunk[chunk<=pivot] #return a vector of elements in chunk that are smaller than pivot
    #now we have split out vector into elements larger and smaller than the pivot.
    #we want to apply quicksort to both vectors
    pivstay <- quicksort(pivstay) 
    pivmove <- quicksort(pivmove)
    sorted<-c(pivmove,pivot,pivstay) 
    #this places the pivot in correct spot in relation to pivmove and pivstay
    #return(sorted)
    #removed the return statement because for the purpose of this question we just want to time this code
  }
  elapsed<-proc.time() - start
  return(elapsed)
}


#testing my function
start<-proc.time()
b<-as.vector(c(3,2,7,9,11,15,20))
quicksort(b)
proc.time() - start
#elapsed time is 0.02

#USE THIS QUICKSORT FUNCTION!
#this one is more efficient by picking a random element. mostly sorted vectors take a long time with function 1
#trying a different method - random select pivot and then place left and right values and quicksort those
quicksort2 <- function(vec) {
  # Pick a number at random and its index.
  pivotselect <- sample(seq_along(vec),1); 
  pivot <- vec[pivotselect]
  vec <- vec[-pivotselect]
  # Place-holders for left and right values.
  left <- c()
  right <- c()
  # Move all the smaller and equal values to the left, bigger values to the right.
  left<-vec[which(vec<=pivot)]
  right<-vec[which(vec>pivot)]
  if (length(left) > 1) {
    left <- quicksort2(left) #now quicksort the smaller
  }
  if (length(right) > 1) {
    right <- quicksort2(right) #quicksort the larger
  }
  #Finally, return the sorted values.
  return(c(left, pivot, right))
}


#testing my function
start<-proc.time()
b<-as.vector(c(3,2,7,9,11,15,20))
quicksort2(b)
proc.time() - start


# this is me figuring out how to create the function above by testing out the pieces
#vector<-as.vector(c(2,3,1,4,6,5))
#ch<-vector[-1]
#f<-vector[1]
#lar<-ch[ch>f]
#sm<-ch[ch<=f]

#next part of this question asks us to investigate run-time of my function I created
#simulation study


#LET'S WORK ON SOLVING THIS QUESTION IN AN ITERATIVE FASHION. I ATTEMPTED TO WRITE A LARGE CHUNK OF CODE  AND IT ENDED IN 
#NODE STACK OVERFLOW. 

###################
#ITERATIVE SOLUTION

#generate x and y vectors. These remain fixed for all 20 simulations

N= 20

#these are null vectors with 20 list elements which my nested for loops below will dump each generated x,y, z vectors into
#each time the for loop reiterates  to the next 'i' it will reference [[i]] element in these vectors...
#this is relevant for the ranodmly generated z vectors
lz<-vector(mode="list", length=20) 
lx<-vector(mode="list", length=20)
ly<-vector(mode="list", length=20)

for (i in 1:N){
  nlist = c(1000, 5000, 10000, 20000) #list of different sample sizes
  x<-vector(mode="list", length=4) #empty x vector list
  y<-vector(mode="list", length=4) #empty y vector list
  z<-vector(mode="list", length=4) #empty z vector list
  index1=1 #intialization
  for (j in 1:4){
    x[[index1]]<-as.vector(c(1:nlist[j])) #generate sorted x for each n
    y[[index1]]<-as.vector(c(nlist[j]:1)) #generte backwards sorted y for each n
    index1=index1+1
  }
  lx[[i]]<-x #dump the vector list into lx for each simulation run (x won't change)
  ly[[i]]<-y #dump the vector list into ly for each simulation run (y won't change)
  index2=1 #intialization
  for(k in 1:4){
    z[[index2]]<-as.vector(sample(x[[k]], replace = FALSE) )#random sample from x (z changes each simulation run)
    index2=index2+1
  }
  lz[[i]]<-z #dump the randomly generated z into lz for each simulation run 
}

#ok good. We have 20 simulated values for EACH 'n' for x and y AND 20 random simulated values for each level of n from the x vectors created. 
#lx , ly , lz are the lists containing these rnaomdly generated vectors

#now we want to quicksort these and get the computation time for each of the 80 vectors for x , y , z in lx, ly , lz

#creating empty vectors for capturing the times in the nested for loop of quicksort
elapsedtime1<-vector(mode='list', length=20)
elapsedtime2<-vector(mode='list', length=20)
elapsedtime3<-vector(mode='list', length=20)
for (i in 1:N){
  for (j in 1:4){
    print(i) #double checking that it runs
    print(j)
    start1<-proc.time()
    quicksort2(lx[[i]][[j]])
    elapsedtime1[[i]][[j]]<-proc.time() -start1
    start2<-proc.time()
    quicksort2(ly[[i]][[j]])
    elapsedtime2[[i]][[j]]<-proc.time() -start2
    start3<-proc.time()
    quicksort2(lz[[i]][[j]])
    elapsedtime3[[i]][[j]]<-proc.time() -start3
    
  }
}
#each elapsedtime[[i]][[j]] corresponds to each [[i]][[j]] in lx ly lz 

#now the goal of the question is to get an average runtime for each n and x,y,z combination
avgx<-vector(mode='list', length=4)
avgy<-vector(mode='list', length=4)
avgz<-vector(mode='list', length=4)


for ( i in 1:20){
  avgx[[1]]<- sum(elapsedtime1[[i]][[1]][["elapsed"]]) / N
  avgx[[2]]<- sum(elapsedtime1[[i]][[2]][["elapsed"]]) / N
  avgx[[3]]<- sum(elapsedtime1[[i]][[3]][["elapsed"]]) / N
  avgx[[4]]<- sum(elapsedtime1[[i]][[4]][["elapsed"]]) / N
  avgy[[1]]<- sum(elapsedtime2[[i]][[1]][["elapsed"]]) / N
  avgy[[2]]<- sum(elapsedtime2[[i]][[2]][["elapsed"]]) / N
  avgy[[3]]<- sum(elapsedtime2[[i]][[3]][["elapsed"]]) / N
  avgy[[4]]<- sum(elapsedtime2[[i]][[4]][["elapsed"]]) / N
  avgz[[1]]<- sum(elapsedtime3[[i]][[1]][["elapsed"]]) / N
  avgz[[2]]<- sum(elapsedtime3[[i]][[2]][["elapsed"]]) / N
  avgz[[3]]<- sum(elapsedtime3[[i]][[3]][["elapsed"]]) / N
  avgz[[4]]<- sum(elapsedtime3[[i]][[4]][["elapsed"]]) / N
}

#results in avgx, avgy, and avgz will be reported in table in HW submission
avgx
# 0 , 0.002, 0.003, 0.0055
avgy
#0, 0.002,  0.0025,  0.0055
avgz
#5e-04,  0.001, 0.003, 0.006

######################################################
#Q6 
#Aim of this question is to replicate the Monte Carlo simulation study from the Salmeron paper study of VIF and CN
#replicate 3.1 simulation study

#now to set up the Monte Carlo simulation
plist = c(3,4,5)
gamma<- seq(0,0.95, by = 0.05)
nlist2 <- seq(15,200, by=5)


#attempt at getting VIF USING THE FORMULA IN PAPER with eigenvector and eigenvalues
#the resulting graphs were not consistent with results in paper which is why I will not use this function
set.seed(1001)
VIFmontecarlo1<-function(n,p,gamma){
  W<-matrix( rnorm(n*(p-1),mean=10,sd=100), n, (p-1))
  X<-matrix( , nrow = n, ncol = (p-1))
  
  for (i in 1:(p-1)){
    X[,i]<- (sqrt(1 - gamma^2) * W[,i]) + (gamma * W[, (p-1)])
  }
  xx<-crossprod(X)
  eigvec<-eigen(xx)$vectors
  eigval<-eigen(xx)$values
  sqr<-matrix(,nrow=(p-1), ncol=(p-1)) #empty matrix to place square eigenvector elements into
  for (i in 1:(p-1)){
    for (j in 1:(p-1)){
      sqr[i,j]<-(eigvec[i,j]^2) #square the elements in eigenvectors
    }
  }
  vi<-vector(length = (p-1))
  for (i in 1:(p-1)){
    vi[i]<-sum(sqr[,i]) / eigval[i]
    #take each element in eigenvector and square it and divide by corresponding eigenvalue
  }
  
  vifout<-max(vi) #sum the squared eigenvec elements/ eigenvalue to get VIF!
  minieig<-min(eigval) #minimum eigen of X'X
  listed<-c(minieig, vifs)
  return(listed)
}

#####USE THIS FUNCTION ... SEE VIF CALCULATION AT BOTTOM OF FUNCTION #####
set.seed(1001)
VIFmontecarlo<-function(n,p,gamma){
  W<-matrix( rnorm(n*(p-1),mean=10,sd=100), n, (p-1))
  X<-matrix( , nrow = n, ncol = (p-1))
  
  for (i in 1:(p-1)){
    X[,i]<- (sqrt(1 - gamma^2) * W[,i]) + (gamma * W[, (p-1)])
  }
  xx<-crossprod(X) #getting x'x
  eigvec<-eigen(xx)$vectors 
  eigval<-eigen(xx)$values #will use this to get minimum eigenvalue
  vifs = diag(solve(cor(X))) #getting correlation matrix of X and then inverting it, then extracting the diagonal components
  vifout<-max(vifs) #maximum vif from above calc
  minieig<-min(eigval) #minimum eigen of X'X
  listed<-c(minieig, vifout)
  return(listed)
}
#testing it out
VIFmontecarlo(200,5,.1)
#good to go


N = 1000 #number of runs
out3<-vector(mode="list",length = N*length(nlist2)*length(gamma))

#i will run separate nested for loops for each value of p 
#I could add another nested for loop for 'p', but there are only three levels of p
#and we want to plot the VIF results for each 'p'' so it is simpler in my head
#to run it this way

### p = 3 ###
index = 1 #initialize
for ( run in 1:N){
  for (n in nlist2){
    for (g in gamma){
      print(n)
      print(g)
      print(run)
      out3[[index]]<-VIFmontecarlo(n, 3 ,g)
      index<-index + 1
    }
  }
}

#extracting the minieigs from out3
minie3<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  minie3[[index]]<-out3[[i]][1]
  index<-index + 1
}
#good


#extract just the VIF
vif3<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  vif3[[index]]<-out3[[i]][2]
  index<-index + 1
}

plot(unlist(vif3), unlist(minie3), xlab = "Variance Inflation Factor", ylab = "Minimum Eigenvalue of X'X", main = "Original Data p=3" )
abline(v = 10, col='blue', lty=2, lwd=2)
abline(h = 0.1, col= 'blue', lty=2, lwd=2)

### p = 4 ###
out4<-vector(mode="list",length = N*length(nlist2)*length(gamma))
index = 1 #initialize
for ( run in 1:N){
  for (n in nlist2){
    for (g in gamma){
      print(n)
      print(g)
      print(run)
      out4[[index]]<-VIFmontecarlo(n, 4 ,g)
      index<-index + 1
    }
  }
}

#extracting the minieigs from out4
minie4<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  minie4[[index]]<-out4[[i]][1]
  index<-index + 1
}



#extract just the VIF
vif4<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  vif4[[index]]<-out4[[i]][2]
  index<-index + 1
}

#now let's plot
plot(unlist(vif4), unlist(minie4), xlab = "Variance Inflation Factor", ylab = "Minimum Eigenvalue of X'X", main = "Original Data p=4" )
abline(v = 10, col='blue', lty=2, lwd=2)
abline(h = 0.1, col= 'blue', lty=2, lwd=2)

### p = 5 ###
out5<-vector(mode="list",length = N*length(nlist2)*length(gamma))
index = 1 #initialize
for ( run in 1:N){
  for (n in nlist2){
    for (g in gamma){
      print(n)
      print(g)
      print(run)
      out5[[index]]<-VIFmontecarlo(n, 5 ,g)
      index<-index + 1
    }
  }
}

#extracting the minieigs from out5
minie5<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  minie5[[index]]<-out5[[i]][1]
  index<-index + 1
}
#good


#extract just the VIF
vif5<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  vif5[[index]]<-out5[[i]][2]
  index<-index + 1
}

#now let's plot
plot(unlist(vif5), unlist(minie5), xlab = "Variance Inflation Factor", ylab = "Minimum Eigenvalue of X'X", main = "Original Data p=5" )
abline(v = 10, col='blue', lty=2, lwd=2)
abline(h = 0.1, col= 'blue', lty=2, lwd=2)

#######this concludes original data plots and calculations########################################

#now we move on to converting data to unit length
#Data are considered to be unit length when original uncentered data are 
#divided by the square root of the sum of every variable squared.

#now to set up the Monte Carlo simulation
plist = c(3,4,5)
gamma<- seq(0,0.95, by = 0.05)
nlist2 <- seq(15,200, by=5)


#generate W and X as defined in paper
set.seed(1001)
VIFmontecarlounit<-function(n,p,gamma){
  W<-matrix( rnorm(n*(p-1),mean=10,sd=100), n, (p-1))
  X<-matrix( , nrow = n, ncol = (p-1))
  
  for (i in 1:(p-1)){
    X[,i]<- (sqrt(1 - gamma^2) * W[,i]) + (gamma * W[, (p-1)])
  }
  
  ###new piece of code to convert data to unit length#########
  #denominator calculation 
  scalex<-vector(length = (p-1))
  square<-matrix(,nrow=n, ncol=(p-1))
  for (j in 1:(p-1)){ 
    #1 to p because we added intercept in to X matrix. 
    #We added intercept in because the VIF()  fxn by Salmeron requires it
    for(i in 1:n){
      square[i,j]<-(X[i,j]^2)
    }
    scalex[j]<-sqrt(sum(square[,j]))
  }
  #now converting x to U
  U<-sweep(X, 2, scalex, FUN = '/')
  
  ############################################################
  uu<-crossprod(U)
  eigvec<-eigen(uu)$vectors
  eigval<-eigen(uu)$values
  vifs = diag(solve(cor(U)))
  #getting correlation matrix of U and then inverting it, then extracting the diagonal components
  #I experienced multiple errors with the correlation matrix for U being computationally singular so I had to change solve() to Ginv()
  ##########################
  vifout<-max(vifs) #max vif
  minieig<-min(eigval) #minimum eigen of U'U
  listed<-c(minieig, vifout)
  return(listed)
}


#testing it out
VIFmontecarlounit(20,3,0.95)
#good to go


N = 1000 #number of runs
set.seed(1010)
out31<-vector(mode="list",length = N*length(nlist2)*length(gamma))

#i will run separate nested for loops for each value of p 
#I could add another nested for loop for 'p', but there are only three levels of p
#and we want to plot the VIF results for each 'p'' so it is simpler in my head
#to run it this way

### p = 3 ###
index = 1 #initialize
for ( run in 1:N){
  for (n in nlist2){
    for (g in gamma){
      print(n)
      print(g)
      print(run)
      out31[[index]]<-VIFmontecarlounit(n, 3 ,g)
      index<-index + 1
    }
  }
}

#extracting the minieigs from out3
minie31<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  minie31[[index]]<-out31[[i]][1]
  index<-index + 1
}
#good


#extract just the VIF
vif31<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  vif31[[index]]<-out31[[i]][2]
  index<-index + 1
}

plot(unlist(vif31), unlist(minie31), xlab = "Variance Inflation Factor", ylab = "Minimum Eigenvalue of X'X", main = "Unit Length Data p=3" )
abline(v = 10, col='blue', lty=2, lwd=2)
abline(h = 0.1, col='blue', lty=2, lwd=2)

### p = 4 ###
out41<-vector(mode="list",length = N*length(nlist2)*length(gamma))
index = 1 #initialize
for ( run in 1:N){
  for (n in nlist2){
    for (g in gamma){
      print(n)
      print(g)
      print(run)
      out41[[index]]<-VIFmontecarlounit(n, 4 ,g)
      index<-index + 1
    }
  }
}

#extracting the minieigs from out4
minie41<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  minie41[[index]]<-out41[[i]][1]
  index<-index + 1
}



#extract just the VIF
vif41<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  vif41[[index]]<-out41[[i]][2]
  index<-index + 1
}

#now let's plot
plot(unlist(vif41), unlist(minie41), xlab = "Variance Inflation Factor", ylab = "Minimum Eigenvalue of X'X", main = "Unit Length Data p=4" )
abline(v = 10, col='blue', lty=2, lwd=2)
abline(h = 0.1, col='blue', lty=2, lwd=2)


### p = 5 ###
out51<-vector(mode="list",length = N*length(nlist2)*length(gamma))
index = 1 #initialize
for ( run in 1:N){
  for (n in nlist2){
    for (g in gamma){
      print(n)
      print(g)
      print(run)
      out51[[index]]<-VIFmontecarlounit(n, 5 ,g)
      index<-index + 1
    }
  }
}
#extracting the minieigs from out3
minie51<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  minie51[[index]]<-out51[[i]][1]
  index<-index + 1
}
#good


#extract just the VIF
vif51<-vector(mode='list',length = N*length(nlist2)*length(gamma) )
index = 1
for (i in 1:760000){
  vif51[[index]]<-out51[[i]][2]
  index<-index + 1
}

#now let's plot
plot(unlist(vif51), unlist(minie51), xlab = "Variance Inflation Factor", ylab = "Minimum Eigenvalue of X'X", main = "Unit Length Data p=5" )
abline(v = 10, col='blue', lty=2, lwd=2)
abline(h = 0.1, col='blue', lty=2, lwd=2)
