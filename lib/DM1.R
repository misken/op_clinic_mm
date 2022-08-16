vacationhyperexponCV2<-function(b)
{
# Returns the coefficient of variation squared for a certain hyperexponential distribution.
# The specific distribution is characterized by the vector of means, b.
# The probability that the service time is exponential with mean bi is bi/sum(b)
# I

    p<-b/sum(b)
    pb2<-p*b^2
    CV2<-(2*sum(pb2)/sum(p*b)^2)-1
    return(CV2)
}


DM1_Pn<-function(i,n,a,b)
{
# P[i,n,a,b] is the transient probability distribution of system size in the D/M/1 queue. 
# Interval n is [na,(n+1)a] where a is the fixed interarrival time and b is the mean service time. 
# Let a=1 (wolog) and we see that the 0'th interval is [0,1], the n=1 interval is [1,2] and the n=m interval is [m,m+1]. 
# P[i,n] is the probability that the number of customers in the system athe the START of the n'th interval is equal to i. 
# Since the first customer arrives at time 0 (the start of interval n=0), P[1,0] = 1 and P[i,0]=0 for i<>1. 
# Note that we can compute P[i,n,a,b] for any values of a and b (>0) and specifically for b>=a. 
# In other words, no stability condition is necessary since we are NOT computing steady state probabilities. 
# From the P's we can compute other performance measures such as the expected waiting time. 
# If we do have b<a, then we can compute the steady state probabilities P[i,a,b] when n goes to infinity.

	if(i==1) {
	  t1<-0
    if(n>=1) {
		  for(v in 0:(n-1)) {
		      t1 <- t1 + exp(-(v + i)*a/b)*((v + i)^(v - 1))*((a/b)^v)/factorial(v)
		  }
	  }
          p <- 1 - t1
	} else {
	    if(i == (n + 1) && i > 1) {
	         p <- exp(-a*n/b)
	    } else {
	        if(i > (n + 1) && i > 1) {
	            p <- 0
	        } else {
	            t1<-0
	            for(v in 0:(n-i+1)) {
	                t1 <- t1 + exp(-(v + i - 1)*a/b)*((v + i - 1)^(v - 1))*((a/b)^v)/factorial(v)
	            }
	            t2<-0
	            for(v in 0:(n-i)) {
	                t2 <- t2 + exp(-(v + i)*a/b)*((v + i)^(v - 1))*((a/b)^v)/factorial(v)
	            }
	            p <- (i-1)*t1 - i*t2
	        }
	    }    
	}
        return(p)

}

DM1_Wn<-function(n,a,b) 
{
# DM1_Wn is the expected wait time of the n'th arrival in a transient DM1 queue starting empty
# n is the customer number
# a is the time between arrivals
# b is the mean service time
    s<-0
    for(v in 1:(n+1)) s <- (s+v*DM1_Pn(v+1,n,a,b))
    return(b*s)

}



DM1_W<-function(N,a,b)
{
# DM1_W is the overall expected wait time in a transient DM1 queue starting empty
# N is the total number of arrivals
# a is the time between arrivals
# b is the mean service time

    m<-length(N)
    W<-numeric()
    for(u in 1:m) {
        s<-0
        for(v in 1:N[u]) s <- (s+DM1_Wn(v,a[u],b[u]))
        W[u]<-(s/N[u])
    }
    return(W)

}






