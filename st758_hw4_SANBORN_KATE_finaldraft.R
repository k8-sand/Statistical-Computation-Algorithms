#SANBORN, KATE
#ST758 HOMEWORK 4
#17SEP2021
################################################################################
#Q2
#create sweep function and carry out simulation
#question wants matrix A and kth diagonal as input

#first let's create a sweep function

sweepr <- function(A,k) {
  piv <- A[k,k]    #select element i,i .this is the pivot
  A[k,] <- A[k,]/piv #take the ith row where pivot is and divide by the pivot 
  c <- A[,k]      #store the column where pivot is
  c[k] <- 0       #we don't want the pivot included in this to change...set to 0
  A[,k] <- -c/piv             # take th column elements where pivot is and divide by -pivot
  A <- A - outer(c,A[k,])  #the main sweep. this handles the elements of the matrix not in the same column or row as the pivot. Outer is outer product
  A[k,k] <- -1/piv             # diagonal elements
  regsweep <- A  #return sweeped matrix on the one pivot   
}


#now set up sim
plist=c(500,1000)
nlist=c(5000,10000,15000,20000)
N=1000 #1000 RESPONSE VECTORS TO SIMULATE

set.seed(1001)
simswp<-function(n,p){
  
  #collect<-matrix(ncol=N,nrow=n) #creating an empty matrix to collect 1000 response vectors
  results<-matrix(ncol=N, nrow=p) #creating an empty matrix to collect all estimates of beta
  
  err<-vector(mode="list", length=N)
  elapsed<-vector(mode='list', length=N)
  for(i in 1:N){
    x<-matrix(rnorm(n*p, mean=0,sd=1), n, p)
    e<-matrix(rnorm(p, mean=0,sd=1), n, 1)
    b<-matrix(rep(1,p))
    xx<-crossprod(x)
    startt<-proc.time() #start clock
    for (j in 1:p){
      xx<-sweepr(xx,j)
    }
    y<-x%*%b + e #true y values thrown into each column of empty matrix created
    bhat<-(xx)%*%(t(x)%*%y) #bhat thrown into each coumn of empty matrix created
    err[[i]]<-sum((b-bhat)^2)
    elapsed[i]<-proc.time() - startt
  }
  
  mylist<- list(elapsed,err) #creating a list of things I want my function to output
  return (mylist) #output from my function
  
}

simswp(10,5) #testing my function without a for loop...ok it works

#now to do this for all 8 combos

comp<-vector(mode="list", length=8) #empty list for my nested for loop
index=1 #initialization

#now running my function on all 8 combinations
for (n in nlist){
  for (p in plist){
    print(index)
    comp[[index]]<-simswp(n,p)
    index = index + 1
  }
}

#ok now I have a list of lists stored in comp...but it is not pretty
#need to extract proc.time elapsed and the error components for each of the 8 combinations


np1err<-as.matrix(comp[[1]][[1]])
np1err<-as.data.frame(np1err)
np2err<-as.matrix(comp[[2]][[1]])
np2err<-as.data.frame(np2err)
np3err<-as.matrix(comp[[3]][[1]])
np3err<-as.data.frame(np3err)
np4err<-as.matrix(comp[[4]][[1]])
np4err<-as.data.frame(np4err)
np5err<-as.matrix(comp[[5]][[1]])
np5err<-as.data.frame(np5err)
np6err<-as.matrix(comp[[6]][[1]])
np6err<-as.data.frame(np6err)
np7err<-as.matrix(comp[[7]][[1]])
np7err<-as.data.frame(np7err)
np8err<-as.matrix(comp[[8]][[1]])
np8err<-as.data.frame(np8err)

nperror<-rbind(np1err,np2err,np3err,np4err,np5err,np6err,np7err,np8err)

np1time<-as.matrix(comp[[1]][[2]])
np1time<-as.data.frame(np1time)
np2time<-as.matrix(comp[[2]][[2]])
np2time<-as.data.frame(np2time)
np3time<-as.matrix(comp[[3]][[2]])
np3time<-as.data.frame(np3time)
np4time<-as.matrix(comp[[4]][[2]])
np4time<-as.data.frame(np4time)
np5time<-as.matrix(comp[[5]][[2]])
np5time<-as.data.frame(np5time)
np6time<-as.matrix(comp[[6]][[2]])
np6time<-as.data.frame(np6time)
np7time<-as.matrix(comp[[7]][[2]])
np7time<-as.data.frame(np7time)
np8time<-as.matrix(comp[[8]][[2]])
np8time<-as.data.frame(np8time)

nptimeor<-rbind(np1time,np2time,np3time,np4time,np5time,np6time,np7time,np8time)


#now comp2 is filled with what I want. Let's rename V1 and V2 properly and add two columns referencing levels of n and p
#note that with my nested for loop R runs through by first picking 'n' then doing both values of p with that n. 
#then moves to the next n and then does both p with that n...etc
#keeping this in mind let's be careful about assigning n and p to each row

#NEED TO CREATE VECTORS OF N AND P COMBINATIONS SO I CAN CBIND THEM TO comp2

nlist3<-matrix(rep(5000,1000))
plist3<-matrix(rep(500,1000))
n4<-matrix(rep(5000,1000))
p4<-matrix(rep(1000,1000))
n5<-matrix(rep(10000,1000))
p5<-matrix(rep(500,1000))
n6<-matrix(rep(10000,1000))
p6<-matrix(rep(1000,1000))
n7<-matrix(rep(15000,1000))
p7<-matrix(rep(500,1000))
n8<-matrix(rep(15000,1000))
p8<-matrix(rep(1000,1000))
n9<-matrix(rep(20000,1000))
p9<-matrix(rep(500,1000))
n10<-matrix(rep(20000,1000))
p10<-matrix(rep(1000,1000))

nlisted<-rbind(nlist3,n4,n5,n6,n7,n8,n9,n10)
plisted<-rbind(plist3,p4,p5,p6,p7,p8,p9,p10)


outdata<-cbind(nlisted,plisted,nperror,nptimeor) #good now they are binded so i can create the requested graphs
outdata$timecomp<-outdata[,4]
outdata$errorstat<-outdata[,3]
outdata<-outdata[,c(1,2,5,6)]
outdata$timecomp<-as.numeric(outdata$timecomp)
outdata$errorstat<-as.numeric(outdata$errorstat)
write_xlsx(outdata, "outdata.xlsx")

library(ggplot2)
plot1<-ggplot(outdata, aes(nlisted,timecomp, colour = as.factor(plisted))) +
  geom_point() +
  
  xlab("n") +
  ylab("Computation Time (in sec)") +
  labs(title="n versus Time Elapsed for each 'p'") +
  labs(colour="p") +
  theme(plot.title = element_text(hjust = 0.5))
plot1


plot2<-ggplot(outdata, aes(timecomp,errorstat, colour = as.factor(plisted))) +
  geom_point() +
  
  xlab("Computation Time (in sec)") +
  ylab("Statistical Error for Beta") +
  labs(title="Elapsed Time versus Statistical Error for each 'p'") +
  labs(colour="p") +
  theme(plot.title = element_text(hjust = 0.5))
plot2
