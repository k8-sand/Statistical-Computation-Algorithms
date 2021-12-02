#ST758 - HW 1
#AUTHOR: SANBORN, KATE
########################

#QUESTION 1 PT2
#we want the probability that B>= 75 when n=200 and p=0.4 where B~Bin(n,p)

#first let's create a function to compute the factorial

fact <- function(n) {
  if (n==0){
    return(1)
  }
  f <- 1
  for (i in 1:n) {
    f <- f * i
  }
  return(f)
}


#now a function to get the combination - i.e. the binom. coeff
Combi<-function(n,k){
  nchoosek=1 #initialize
  for (i in 1:(n-k)){
    nchoosek = nchoosek*(k+i)/i #using code from class because computationally simpler
  }
  return(nchoosek)
}

#doing k:n because we want upper-tail probability
binp <- function(k, n, p) {
  sumz <- 0
  for (i in k:n) {
    sumz <- sumz + Combi(n, i) * (p**i) * ((1-p)**(n - i))
  }
  return(sumz)
}

binp(75,200,0.4)
#we get infinity - too large of a number. I wonder if we do lower tail and then do 1-lowertail

binlower <- function(k, n, p) {
  sumz <- 0
  for (i in 0:k) { #0 to k is lower tail
    sumz <- sumz + Combi(n, i) * (p**i) * ((1-p)**(n - i))
  }
  return(sumz)
}

1-binlower(75,200,0.4) # ok probability is close to 0.74104


#part c wants a normal approx
#recall that normal approx for binomial sets mean=np and standard error = sqrt(np(1-p)) 
#so Z= B-np / (sqrt(np(1-p))) converges to standard normal
n=200
k=75
p=0.4
mean=n*p #80
sig2=mean*(1-p) #48
#z score 
(75-80)/sqrt(sig2) #-0.72168

# P(B>=75) = P(-0.72168<=Z)
stdn <- function(x) {
  sumz <- 0
  for (i in x:10000000) { #set to some big number going to infinity
    sumz <- sumz + (1/sqrt(2*pi))*exp((-1)*((i)**2)/2)
  }
  return(sumz)
}
stdn(-0.72168) #0.8991515 

#this probability is larger than the probability we found with the binomial calculation.

##########################################
#QUESTION 2 EULER'S NUMBER
#if n=0 then e= 1/0! = 1.

#now if we sum forwards from n=0 to some large n...i picked n=200000 ... we get euler's number to be something smaller than 3
euler <- function(){
  options("warn"=-1)
  e <- 0
  for (n in 0:200000){
    e <- e+ 1/(fact(n))
  }
  return(e)
}
e <- euler()
print(e) #2.718282

#now let's do the backwards sum...so start with n=200000 and work backwards to n=0
eulerback <- function(){
  options("warn"=-1)
  e <- 0
  for (n in 200000:0){
    e <- e+ 1/(fact(n))
  }
  return(e)
}
e <- eulerback()
print(e)
#computation time took longer to go backwards
#note this uses the factorial function I created in question 1 so need to run that code before running this function

#i get the same estimate of 2.718282 due to my computer's storage capacity.

#reproduced factorial code 
fact <- function(n) {
  if (n==0){
    return(1)
  }
  f <- 1
  for (i in 1:n) {
    f <- f * i
  }
  return(f)
}


###############################################
#QUESTION 3 ON PAPER 
#these are the graphs to accompany intuition on condition number found

x1<-c(-1:20)
y1<-e^x
plot(x1,y1)
#very quick large changes

x2<-c(1:20)
y2<-log(x2)
plot(x2,y2)
#small slope change for small x change

x3<-c(0:20)
y3<-log(1+x3)
plot(x3,y3)
#similar...small changes

x4<-c(-20:20)
y4<-x4/sqrt(1+x4^2)
plot(x4,y4)
#not large changes again

#using these plots to visualize the condition number calculation
##############################################
#QUESTION 4 WORK ON PAPER
##############################################
#QUESTION 5

#using the Wikipedia page referenced in the question - I used the Iterative construction version of the Cantor function
#this was easier to construct via algorithm in R. I am sure there is a way to do it for converting base 3 to base 2 cleverly, 
#but I have no Computer Science background to know how to do that.

cantor <- function(x,n){
  if (n==0){
    return(x)
  }else{
    if(0 <= x & x <= 1/3){ #first 1/3
      return(1/2*cantor(3*x,n+1))
    }else if(1/3 < x & x < 2/3){ #second third
      return(1/2)
    }else{ #last third
      return(1/2+1/2*cantor(3*x-2,n+1))
    }
  }
}

#now to plot my function to see if it converged correctly to the Cantor function g(x) as expected.
#drawing a random sample of 1000 from Unif(0,1)
x=runif(1000,min=0,max=1)
#applying to all 1000 x's to get g(x)=y
y <- sapply(x, cantor, n=3) #just decided to just n= 3 replications

#now plot to see if we get the cantor function we expect (wikipedia gives a visual of what we expect)

plot(x,y,xlab="Random Uniform X Sample",ylab="Cantor g(x)",main="Plot of g(x) Cantor Function")
#GOOD TO GO. looks about right based on what I find on internet images.
