 library(dplyr)
 library(readr)
 library(tibble)
 library(data.table)
 library(usethis)
 library(devtools)
 library(httr)
 library(RJSONIO)
 library(tidyverse)
 library(wbstats)
 library(gitcreds)

 
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



Y[2] = (a_0 - a_1*(r_l+rho)*L[1]+a_2*M[1]-eps*K[1]+delta*K[1])/(1-a_1-eps/e_K)   # Init for t=1 (second value)
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

for (i in 3:51){
        Y[i] = (a_0 - a_1*(r_l+rho)*L[i-1]+a_2*M[i-1]-eps*K[i-1]+delta*K[i-1])/(1-a_1-eps/e_K) 
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






