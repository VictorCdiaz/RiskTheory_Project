# RiskTheory_Project
Final Project of the Risk Theory course

#Cab Díaz Víctor Manuel
#Martinez Rodriguez Erick Pechuga

#MIDTERM EXAM NUM.3


#Simulation of S's and Fs
simulation <- function(m){
  # m = Number of simulations or repetitions
  # N = Numeric object which represents the simulated value Ni
  # S = vector of the simulations of S = Y1 + ... + YN
  # Y = vector of the N claims
  S <- numeric(m)
  for(i in 1:m){
   N <- rnbinom(1, 1, mu = 100)
   Y <- numeric(N)
   if(N > 1){
     for (j in 1:N) {
       Y[j] <- rlnorm(1,0,1)
       S[i] <- sum(Y)
     }
   }
   else 
    S[i] <- 0
  }
  return(S)
}
