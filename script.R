
library(dplyr)
 library(readr)
 library(tibble)
 library(data.table)
 library(usethis)
 library(devtools)
 library(httr)
 library(RJSONIO)
 library(tidyverse)
 library(gitcreds)
 library(tikzDevice) 
 library(ggplot2)


 ## Parameters
 
a_0 = 50
a_1 = 0.75 
a_2 = 0.1 
delta = 0.1
rho = 0.1 
eps = 0.2
e_L = 1
e_K = 1
r_l = 0.04 
r_m = 0.04
w = 1

## Initialisation 

L <- M <- K <- Y <-YD <- P <- WB <- N <- C <- K_T <- I <- c()

L[1]=400                                    # first value for t = 0, random 0 value for not knowable variable
M[1]=400
K[1]=400
Y[1] = 0
K_T[1] = 0
I[1] = 0
N[1] = 0
WB[1] = 0 
P[1] = 0 
YD[1] = 0
C[1] = 0 



Y[2] = (a_0 + a_1*(r_m*M[1]-(r_l+rho)*L[1])+a_2*M[1]+K[1]*(delta-eps))/(1-a_1-eps/e_K)   # Init for t=1 (second value)
K_T[2] = Y[2]/e_K
I[2] = eps*(K_T[2]-K[1])+delta*K[1]
N[2] = Y[2]/e_L
WB[2] = w*N[2]
P[2] = Y[2]-WB[2]-(r_l+rho)*L[1]
YD[2] = WB[2]+P[2]+r_m*M[1]
C[2] = a_0 + a_1*YD[2]+a_2*M[1]
K[2] = K[1]*(1-delta)+I[2]
M[2] = M[1] +YD[2]-C[2]
L[2] = L[1]*(1-rho)+I[2]


## Resolution 

        # for a_0 = constant --------------------------------------------------------------------------------------
for (i in 3:51){
        Y[i] = (a_0 + a_1*(r_m*M[i-1]-(r_l+rho)*L[i-1])+a_2*M[i-1]+K[i-1]*(delta-eps))/(1-a_1-eps/e_K) 
        K_T[i] = Y[i]/e_K
        I[i] = eps*(K_T[i]-K[i-1])+delta*K[i-1]
        N[i] = Y[i]/e_L
        WB[i] = w*N[i]
        P[i] = Y[i]-WB[i]-(r_l+rho)*L[i-1]
        YD[i] = WB[i]+P[i]+r_m*M[i-1]
        C[i] = a_0 + a_1*YD[i]+a_2*M[i-1]
        K[i] = K[i-1]*(1-delta)+I[i]
        M[i] = M[i-1] +YD[i]-C[i]
        L[i] = L[i-1]*(1-rho)+I[i]    

}

plot(x = seq(1:51), y = Y, type = 'l', col = 'blue',xlab = '', ylab = '',ylim = c(0,600),xlim = c(1,50))
lines(x = seq(1:51), y = C, type = 'l', col = 'red')
lines(x = seq(1:51), y = I, type = 'l', col = 'green')
legend("topleft",bty='n',legend=c("Y", "C", "I"),title = "Evolution of main variables", col=c("blue", "red", "green"),lty=1)


 ### for a_0 changes at t = 10  ------------------------------------------------------------------------------------
for (i in 3:51){
        if (i<10){
        Y[i] = (a_0 + a_1*(r_m*M[i-1]-(r_l+rho)*L[i-1])+a_2*M[i-1]+K[i-1]*(delta-eps))/(1-a_1-eps/e_K) 
        K_T[i] = Y[i]/e_K
        I[i] = eps*(K_T[i]-K[i-1])+delta*K[i-1]
        N[i] = Y[i]/e_L
        WB[i] = w*N[i]
        P[i] = Y[i]-WB[i]-(r_l+rho)*L[i-1]
        YD[i] = WB[i]+P[i]+r_m*M[i-1]
        C[i] = a_0 + a_1*YD[i]+a_2*M[i-1]
        K[i] = K[i-1]*(1-delta)+I[i]
        M[i] = M[i-1] +YD[i]-C[i]
        L[i] = L[i-1]*(1-rho)+I[i]  }
        else{
                a_0=60
                Y[i] = (a_0 + a_1*(r_m*M[i-1]-(r_l+rho)*L[i-1])+a_2*M[i-1]+K[i-1]*(delta-eps))/(1-a_1-eps/e_K) 
                K_T[i] = Y[i]/e_K
                I[i] = eps*(K_T[i]-K[i-1])+delta*K[i-1]
                N[i] = Y[i]/e_L
                WB[i] = w*N[i]
                P[i] = Y[i]-WB[i]-(r_l+rho)*L[i-1]
                YD[i] = WB[i]+P[i]+r_m*M[i-1]
                C[i] = a_0 + a_1*YD[i]+a_2*M[i-1]
                K[i] = K[i-1]*(1-delta)+I[i]
                M[i] = M[i-1] +YD[i]-C[i]
                L[i] = L[i-1]*(1-rho)+I[i] 
        }
}

plot(x = seq(1:51), y = Y, type = 'l', col = 'blue',xlab = '', ylab = '',ylim = c(0,620),xlim = c(1,50))
lines(x = seq(1:51), y = C, type = 'l', col = 'red')
lines(x = seq(1:51), y = I, type = 'l', col = 'green')
legend("topright",bty='n',legend=c("Y", "C", "I"),title = "Evolution of main variables", col=c("blue", "red", "green"),lty=1)






