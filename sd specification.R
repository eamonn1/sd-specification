
 
# If the standard deviation of the X replicated measurements is less than or equal to
## the stated specification, the error is considered consistent with the established test method error. 
## The baseline input required is a repeatability sd from a reliable source i.e. a precision study

##returns upper ONE SIDED specification 
set.seed(333) #

cv.spec <- function (input.sd, alph, n) {
  cv = input.sd
  alpha = alph
  sign = alpha/1;    ##note this is now one sided
  df <-n-1
  crv_q_u  = qchisq(1-sign, df)     
  ci_cv_u = cv*sqrt(crv_q_u /df);   
  return(ci_cv_u)
}


n=15
popsd=50#a
alph <- 0.025
cv.spec(input.sd=popsd, alph=alph, n=n)  # 3 times per million


##################################################################################
###method 2, verification with monte carlo, same inputs
#set.seed(123)
xx<-replicate(1e4, sd(rnorm( n,0,popsd)) )
#quantile(x, c(0.025,0.5,.975))
quantile(xx, c(1-alph))
########################### #######################################################

#xx <- replicate(1e4, sd(rnorm( n,0,1)) ) 
# Interpretation: 	As long as the SD stays below the calculated spec limit, it is considered to be			
# consistent with the previously established test method error.			
h <- hist(xx, breaks=1e4/10, xlab="SD", main="Distribution of SD", xlim=c(0,10), prob=T)
xfit <- seq(0, 40, length = 400)

curve( dchisq(x, df=n-1), col='red', main = "Chi-Square Density Graph",
           from=0,to=40)

# https://r.789695.n4.nabble.com/how-to-plot-chi-square-distribution-in-the-graph-td872614.html
#  https://stats.stackexchange.com/questions/76444/why-is-chi-square-used-when-creating-a-confidence-interval-for-the-variance

 

 
xx <-replicate(1e4, sd(rnorm( n=n, 0, sd=popsd)) )

z <- (xx^2)*(n-1)/popsd^2 ## this is chi square distributed.  A
 
hist(z, prob=TRUE, breaks=1000)
curve( dchisq(x, df=n-1) , col='green', add=TRUE)

# see my pdf
(popsd^2)* qchisq(.975, df=n-1)/(n-1)  # rearranging A
(popsd^2)* qchisq(.025, df=n-1)/(n-1)

 
## ok square this
cv.spec(input.sd=popsd, alph=.975, n)^2 # 3
cv.spec(input.sd=popsd, alph=.025, n)^2 # 3








