library(splines)

rescale <- function(x, newMin, newMax){ 
  (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin 
}

create_simulated_data <- function(N=c(500,200,100),
                                 ages=list(c(10,70),c(5,25),c(50,100)),
                                 sigma=c(5,10,10),
                                 tau=50,
                                 mu=c(0,500,-350)
                                 ){
  sigma <- sigma * tau
  mu<-mu
  knot.age<-40
  
  Z<-Y<-Age<-vector('list',3)
  Age[[1]]<-rgamma(n=N[1],shape=5,rate=2)*10
  Age[[2]]<-rbeta(n=N[2],2,2)*20
  Age[[3]]<-rbeta(n=N[3],2,5)*20+60
  ##rescale age vectors
  Age[[1]]<-rescale(Age[[1]],ages[[1]][1],ages[[1]][2])
  Age[[2]]<-rescale(Age[[2]],ages[[2]][1],ages[[2]][2])
  Age[[3]]<-rescale(Age[[3]],ages[[3]][1],ages[[3]][2])
  
  age.vec<-unlist(Age)
  y.roi<-unlist(Y)
  
  site<-c(rep(1,N[1]),rep(2,N[2]),rep(3,N[3]))
  
  df<-data.frame(Site=site,Age=age.vec)
  df$Site<-as.factor(df$Site)
  
  for(s in 1:3){
    Z[[s]]<-roi_function(Age[[s]],linear='splines',a=-0.3,knot=knot.age) + 
      rnorm(N[s],0,sigma[s])
    Y[[s]]=Z[[s]]+mu[s]
  }
  
  df$y.roi.actual<-unlist(Z)
  y.roi<-unlist(Y)
  df$y.roi<-y.roi
  
  ##code for quadratic model
  # age.vec2<-age.vec^2
  # fit.lm<-lm(y.roi~age.vec+age.vec2+as.factor(site))
  # site.est<-rep(c(0,coef(fit.lm)[4],coef(fit.lm)[5]),unlist(N))
  ##code for splines model
  k<-quantile(age.vec,c(0.33,0.67))
  fit.lm<-lm(y.roi~bs(age.vec,knots=k)+as.factor(site))
  site.est<-rep(c(0,coef(fit.lm)[7],coef(fit.lm)[8]),unlist(N))
  
  df$Site.effect<-site.est
  df$y.roi.corrected<-y.roi-site.est
  
  return (df)
}
