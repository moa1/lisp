a <- scan("~/lisp/cpu-cycles-no-other-process.txt", comment.char="#")

h <- hist(a,breaks=1000)
h <- hist(a[a<3.0e7],breaks=500)
h$density

b <- a - min(a) + 1
b <- b[b<1.5e6]
hist(b, breaks=1000)
abline(v=median(b),col="green")
abline(v=mean(b),col="blue")

# According to [http://en.wikipedia.org/wiki/Gamma_distribution] ("then k is approximately"), k can be approximated to within an error of 1.5% by performing the following calculations:
s <- log(mean(b)) - 1/length(b)*sum(log(b))
k <- (3 - s + sqrt((s-3)**2 + 24 * s))/(12*s)

# "With a shape parameter k and a mean parameter" mu=k/beta, and beta=1/theta (one line above, at 2.), therefore theta = mu/k; where mu is the mean of b
# Mode=(k-1)*theta

# in our example, the mode (is approximately equal to the median) should be at: median(b)/1.5e6=0.3524673 (1)
# therefore, using the above equation for the mode, theta=0.3524673/(k-1)=0.0849405. This is a bit small, let's scale it up 20 times. (I should modify equation (1) so that we see a scaled x interval [0;20], i.e. multiply it by 20.)



x <- (0:1000)/1000*10
y <- dgamma(x,k,1.69881)
#lines(x/10*1.5e6,y*1500,col="red")
# this doesn't seem to be a very good approximate of the shape, let's try if we become better by fiddling around:
lines(x[0:970]/7*1.5e6,y[31:1000]*1500,col="red")



# see cpu-cycles-no-other-process-fit.maxima, which tries to estimate the parameters of the gamma-distributed wait-time. For this program, we need the measured cumulative histogram of the gamma-part of the runtimes.

#h <- hist(a[a<2.2e7], breaks=20)
#h <- hist(a[a<0.9e4], breaks=100)
computehist <- function(bins) {
	modea <- median(a) #TODO: estimate the real mode
	mina <- min(a)
	breaks <- (0:(bins-1))*round((modea-mina)/(bins+2))+mina
	cat("breaks",breaks,"\n")
	h <- hist(a[a<breaks[bins]], breaks=breaks)
	return(h)
}
h <- computehist(30)

density <- h$counts/sum(h$counts)

histdens <- density
histcum <- cumsum(density)
histbreaks <- as.integer(h$breaks)


Rtomaxima <- function(list) {
	l <- length(list)
	s <- paste("[",paste(list[-l],",",sep="",collapse=" ")," ",list[l],"]",sep="")
	return(s)
}
cat("histcum: ",Rtomaxima(density),";\n",sep="")
cat("histbreaks: ",Rtomaxima(as.integer(h$breaks)),";\n",sep="")


# the values estimated (by hand) of the above hist data:
# i.e. k=5.175, theta=0.525 (i.e. rate=1/0.525), r=20322500, scale=0.85e-8
# pwaittime(x,k,theta,scale) := pgamma((x-r)*scale,k,theta);

k <- 5.175; theta <- 0.525; rate <- 1/theta; r <- 20322500; scale <- 0.85e-8

x <- (2033:2200)*10000
y <- dgamma((x-r)*scale*320,k,rate)*3e4
lines(x,y,col="red")



# from cpu-cycles-no-other-process-fit.maxima
#dwaittime(x,k,theta,r,scale) := pgamma((x-r)*scale,k,theta);
#ferror(i,k,theta,r,scale) := (dwaittime(histbreaks[i+1],k,theta,r,scale)-dwaittime(histbreaks[i],k,theta,r,scale)-histcum[i])^2;
#ferrorsum(k,theta,r,scale) := sum(ferror(i,k,theta,r,scale), i, 1, length(histcum));

dwaittime <- function(x,k,theta,r,scale) {
	return(dgamma((x-r)*scale,k,1/theta))
}

ferror <- function(i,k,theta,r,scale) {
	a <- (dwaittime(histbreaks[i],k,theta,r,scale)-histdens[i])^2
	return(a)
}

ferrorsum <- function(k,theta,r,scale) {
	return(sum(sapply(1:length(histcum), ferror, k,theta,r,scale)))
}

plotpars <- function(k,theta,r,scale) {
	plot(histbreaks[-length(histbreaks)], histcum, col="black")
	x <- histbreaks[-length(histbreaks)]
	y <- pgamma((x-r)*scale,k,1/theta)
	lines(x,y,col="red")
	# plot density
	points(x, histdens/max(histdens), col="grey")
#	points(x, histdens, col="grey")
	ydens <- dgamma((x-r)*scale,k,1/theta)
	lines(x,ydens/max(ydens),col="green")
#	lines(x,ydens,col="green")
	return(ferrorsum(k,theta,r,scale))
}

optimizepars <- function(k,theta,r,scale,maxchange=0.1) {
	oldsum <- ferrorsum(k,theta,r,scale)
	cat("errorsum",oldsum,"k",k,"theta",theta,"r",r,"scale",scale,"\n")
	while (TRUE) {
	  knew <- k * (2^runif(1,-maxchange,maxchange))
	  thetanew <- theta * (2^runif(1,-maxchange,maxchange))
	  rnew <- round(r * (2^runif(1,-maxchange,maxchange)))
	  scalenew <- scale * (2^runif(1,-maxchange,maxchange))
	  newsum <- ferrorsum(knew,thetanew,rnew,scalenew)
	  if (newsum < oldsum) {
	    k <- knew
	    theta <- thetanew
	    r <- rnew
	    scale <- scalenew
	    oldsum <- newsum
	    plotpars(k,theta,r,scale)
	    cat("errorsum",oldsum,"k",k,"theta",theta,"r",r,"scale",scale,"\n")
	    cat(k,", ",theta,", ",r,", ",scale,"\n",sep="")
	  }
	}
}

# optimizepars(5.175, 0.525, 20322500, 0.85e-8)
# optimizepars(5.175, 1.125, 20322500, 8.5e-6)
# optimizepars(5.175, 1.125, 20422500, 8.5e-6)
# plotpars(5.175, 1.125, 20422500, 8.5e-6)
# plotpars(7.534688, 1.810431, 20473988, 1.9888e-05)
# plotpars(19.91387, 5.332401, 20099556, 0.0001002135)

# plotpars(2, 0.5, min(a), 0.001)
# optimizepars(16.77494, 0.07895269, 6654, 0.004225396)
# optimizepars(27.51461, 0.4056076, 6301, 0.01423088)