## Andrea Scalenghe
## Matteo Morabito


Yvariable <- function(beta, gamma, sigma, h) {
  Y<-vector()
  Y[1]<-0
  Z<-rnorm(T/h+1,mean=0,sd=sqrt(h))
  for(i in 2:(T/h+1)) Y[i]<- Y[i-1]-beta*(Y[i-1]-gamma)*h+sigma*Z[i-1]
  return(Y)
}

T<-15

Y_1<-Yvariable(2,1,3,0.01)
Y_2<-Yvariable(2,1,3,0.1)

plot(seq(0,T,0.01), Y_1, col = "blue", type = "l", xlab = "Time", ylab = "Y(t)")
lines(seq(0,T,0.1), Y_2, col = "red", type = "l")


X<-matrix(nrow = 1501, ncol = 4, byrow = FALSE)
g<-seq(2,5)
for(i in 2:5) {
  X[,i-1]<-Yvariable(2,i,3,0.01)
}

plot(seq(0,T,0.01), X[,1], type = "l", xlab = "Time", ylab = "Y(t)", col = "red")
lines(seq(0,T,0.01), X[,2], col = "magenta")
lines(seq(0,T,0.01), X[,3], col = "green")
lines(seq(0,T,0.01), X[,4], col = "blue")

#We can see that as gamma increases the value of the process also increases


beta<-1
gamma<-0
sigma<-1

#The theory states that Y(5) is distribuited as a normal random variable with 
#mean=0 and variance=(1-e^(-10))/2. I verify that simulating 10000 Y(5) and 
#analizing the results with the thoretical distribution.

T<-5

#Simulated random variable. We already prepare the random variable X_{2.5} for 
#the fourth exercise

simul<-c()
X_2dot5<-c()
for(i in 1:10000) {
  Y<-Yvariable(beta, gamma, sigma, 0.01)
  simul<-c(simul, Y[T/0.01+1])
  X_2dot5<-c(X_2dot5, Y[T/0.02+1])
}

simul_mean<-mean(simul)
simul_sd<-sd(simul)
#Theorical quantities

theor_mean<-gamma*(1-exp(-beta*T))
theor_sd<-sigma*sqrt(((1-exp(-2*beta*T))/(2*beta)))
theor_sample<-rnorm(10000,theor_mean,theor_sd)

#Descriptive analysis

mean_err<-abs(theor_mean-simul_mean)
sd_err<-abs(theor_sd-simul_sd)
print("Mean error of the simulated sample of size n=10000")
mean_err
print("Standard deviation error of the simulated sample of size n=10000")
sd_err

#We can also use the qqnorm function and plot an histogram and a boxplot of simul  
qqnorm(simul)
par(mfrow=c(1,2))
boxplot(simul)
hist(simul, freq = F, col = "blue")

#For a visually effective comparison we plot the two densities

par(mfrow=c(1,1))
plot(density(theor_sample), col = "red")
lines(density(simul), col = "blue")


X_5<-simul

P<-sum(X_5<X_2dot5)/10000
P 

