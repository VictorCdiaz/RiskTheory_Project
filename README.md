# RiskTheory_Project
Final Project of the Risk Theory course

#Cab Díaz Víctor Manuel
#Martinez Rodriguez Erick 

#MIDTERM EXAM NUM.3


#Simulation of S's and Fs
simulation <- function(m){
  # m = Number of simulations or repetitions
  # N = Numeric object which represents the simulated value Ni
  # S = vector of the simulations of S = Y1 + ... + YN
  # Y = vector of the N claims
 
  S <- numeric(m)
  
  #N~BN(N,P), E(N)=100 & V(N)=225
  p<-100/225
  n<-100*p/(1-p)
  #Y~logNor(m,sdl) M(Y)=30, VaR(Y)=2500 with 95%
  #vemos donde se minimiza 
  FY<-function(x) { 
    M<-x[1]
    sdl<-x[2]
    (qlnorm(0.995,meanlog=m,sdlog=sdl)-2500)^2 + (qlnorm(0.5,meanlog=m,sdlog=sdl)-30)^2
    }
  nlm(FY,c(0,1))
  #obtenemos los valores de m y sdl
  M<-3.401419 
  sdl<-1.716972
  
  for(i in 1:m){
   N <- rnbinom(1, n, p)
   Y <- numeric(N)
   if(N > 1){
     for (j in 1:N) {
       Y[j] <- rlnorm(1,M,sdl)
       S[i] <- sum(Y)
     }
   }
   else 
    S[i] <- 0
  }
  return(S)
}
#Take a look to the density of the simulated data
x <- simulation(50000)
plot(density(x))

