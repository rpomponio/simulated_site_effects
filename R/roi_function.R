roi_function <- function(x,a=-1,b=5000,c=-0.8,knot=60,linear='linear'){
  if(linear=='linear')
    y<-a*(x+10)+b
  if(linear=='quadratic') 
    y<-a*(x+10)^2+b
  if(linear=='splines') 
    y<-a*(x+10)^2+c*(x>knot)*(x-knot)+b
}