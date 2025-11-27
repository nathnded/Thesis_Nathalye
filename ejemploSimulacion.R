

N<- 1000
n <- c(30,40,50,60)


beta0<- 9
beta1<- 3
errbeta0<-c()
errbeta1<-c()

for(j in 1:length(n)){
  for(i in 1:N){
    x<- runif(n[j], min=1, max=30)
    epsilon<- rnorm(n[j],sd=5)
    y<- beta0+beta1*x+epsilon
    ajuste<- lm(y~x)
    betahat<-as.numeric(coef(ajuste))
    errbeta0[i]<-beta0-betahat[1]
    errbeta1[i]<-beta1-betahat[2]
  }
  MSEbeta0[j]<-mean(errbeta0^2)
  MSEbeta1[j]<-mean(errbeta1^2)
}
