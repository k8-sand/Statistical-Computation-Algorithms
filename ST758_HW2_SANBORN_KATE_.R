#ST758 HW 2
#AUTHOR: SANBORN, KATE
##########################
#Q1 HANDWRITTEN PROOF
##########################
#Q2 THE AIM OF THIS QUESTION IS TO EMPIRICALLY CHECK THE COMP TIME FOR BETA HAT VIA SIMULATIONS
#AND ALSO STUDY STAT ERROR VERSUS COMP COST TRADE-OFF
n1=5000
n2=10000
n3=15000
n4=20000
p1=50
p2=100
plist<- c(p1,p2)
nlist<-c(n1,n2,n3,n4)
N=1000


simul<-function(n,p){
  x<-matrix(rnorm(n*p, mean=0,sd=1), n, p)
  b<-matrix(rep(1,p))
  e<-matrix(rnorm(p, mean=0,sd=1), n, 1)
  collect<-matrix(ncol=N,nrow=n) #creating an empty matrix to collect 1000 response vectors
  results<-matrix(ncol=N, nrow=p) #creating an empty matrix to collect all estimates of beta
  startt<-proc.time() #start clock
  for (i in 1:N){
    collect[ ,i]<-x%*%b + e #true y values thrown into each column of empty matrix created 
    results[ ,i]<-solve(t(x)%*%x)%*%(t(x)%*%collect[ ,i]) #bhat thrown into each coumn of empty matrix created
  }
  for (i in 1:N){
    error<-sum((b-results[ ,i])^2) #getting the b - bhat error where b= vector of 1
  }
  elapsed<-proc.time() - startt #creating object to get elapsed time
  mylist<- list(elapsed,error) #creating a list of things I want my function to output
return (mylist) #output from my function
  
}

simul(10,5) #testing my function without a for loop...ok it works

#now to do this for all 8 combos

comp<-vector(mode="list", length=8) #empty list for my nested for loop
index=1 #initialization

#now running my function on all 8 combinations
 for (n in nlist){
        for (p in plist){
          comp[[index]]<-simul(n,p)
          index = index + 1
        }
      }

#ok now I have a list of lists stored in comp...but it is not pretty
#need to extract proc.time elapsed and the error components for each of the 8 combinations

#empty data frame to dump into
comp2<-matrix(ncol=2, nrow=8)

#fill empty matrix with necessary things from comp list of lists
for (i in 1:8){
  comp2[i,1]<-comp[[i]][[1]][["elapsed"]] #extracting elapsed time 
  comp2[i,2]<-comp[[i]][[2]] #extracting the error calculation
}

#now comp2 is filled with what I want. Let's rename V1 and V2 properly and add two columns referencing levels of n and p
#note that with my nested for loop R runs through by first picking 'n' then doing both values of p with that n. 
#then moves to the next n and then does both p with that n...etc
#keeping this in mind let's be careful about assigning n and p to each row

comp2<-data.frame(comp2) #make dataframe
comp2$proc.time<-comp2$X1 #rename first column 
comp2$error<-comp2$X2 #rename second column
comp2<-comp2[,c(3,4)] #get rid of first two columns which are the same as my new renamed columns

#NEED TO CREATE VECTORS OF N AND P COMBINATIONS SO I CAN CBIND THEM TO comp2
nlist2<-matrix(c(n1,
               n1,
               n2,
               n2,
               n3,
               n3,
               n4,
               n4),ncol=1, nrow=8, byrow = TRUE)
plist2<- matrix(c(p1,
                  p2,
                  p1,
                  p2,
                  p1,
                  p2,
                  p1,
                  p2), ncol=1, nrow=8, byrow=TRUE)

comp2<-cbind(nlist2,plist2,comp2) #good now they are binded so i can create the requested graphs


############################################
# q3 RECURRENCE RELATION FOR NON-CONSECUTIVE INTEGERS
#i call this vector of 500 fib because the recurrence relation is very reminiscent of 
#the Fibonnaci sum sequence
nfib=500
fib<-numeric(nfib) #empty vector of 500
fib[1]<-2 #first in sequence is 2
fib[2]<-3 #second in sequence is 3
for (i in 3:nfib) fib[i] <- fib[i - 2] + fib[i - 1] #now we have a full vector of the recurrence

#now we want to plot f(n+1)/f(n) and interpret the plot in light of the recurrence relation
ratiofib<-numeric(nfib)
ratiofib[500]<-NULL
for (i in 1:nfib-1) ratiofib[i]<-fib[i+1]/fib[i]
ratiofib<-ratiofib[1:499] #500th item is null because there is no term after f(500)

#now plot these 
# i will denote the x axis as the nth term and the y axis is the ratio

plotfib<-plot(c(1:499), ratiofib,  xlab="Nth term in sequence", ylab="f(n+1)/f(n) Golden Ratio")

#this is demonstrating that as n gets very large (actually happens early on in the sequence - 
#but in terms of as we go further and further in the sequence - we converge to 1.618034......)
#so in light of the recurrence relation we can roughly say that the gap between any consecutive elements in the set:
#is 1.618034. In other words: as we add one more integer to the set of {1,2,3,...n} the number of subsets
#of non-consecutive integers in that set compared to the prior set is 1.618 more on average.

#this is typically referred to as the GOLDEN RATIO as I recall from my Discrete Math class


##############################################################################################