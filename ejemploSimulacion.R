N<- 1000
n <- seq(from=10, to=1000, by=10)


beta0<- 9
beta1<- 3
MSEbeta0<-c()
MSEbeta1<-c()

for(j in 1:length(n)){
  errbeta0<-c()
  errbeta1<-c()
  for(i in 1:N){
    x<- runif(n[j], min=1, max=30)
    epsilon<- rnorm(n[j],sd=5)
    y<- beta0 + beta1*x + epsilon
    ajuste<- lm(y~x)
    betahat<-as.numeric(coef(ajuste))
    errbeta0[i]<-beta0-betahat[1]
    errbeta1[i]<-beta1-betahat[2]
  }
  MSEbeta0[j]<-mean(errbeta0^2)
  MSEbeta1[j]<-mean(errbeta1^2)
}

par(mfrow=c(1,2))
plot(n,MSEbeta0)
plot(n,MSEbeta1)
par(mfrow=c(1,1))
