library(R2jags)
library(rjags)
library(GDINA)
library(tidyverse)
library(truncnorm)
library(parallel)

K <- 3
Q_all <-  matrix(c(1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1,
               1,0,0,
               0,1,0,
               0,0,1,
               1,1,0,
               1,0,1,
               0,1,1,
               1,1,1), 
             ncol = K, byrow = TRUE)

latent <- function(I){
    for (N in c(100,200,1000)){
        for (r in 1:100){
            K <- 3
            Q <- Q_all[1:I,]

            possible.pattern <- c(0,0,0)
            possible.pattern <- as.matrix(rbind(possible.pattern,Q[1:7,]))
            random.index <- sample(1:8, N, replace = TRUE)
            attribute_profile <- possible.pattern[random.index,]

                alpha <- rnorm(I,0,1)
                # main
                beta.1.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.2.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.3.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.4.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.4.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.5.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.5.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.6.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.6.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.7.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.7.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.7.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)

                beta.8.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.9.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.10.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.11.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.11.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.12.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.12.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.13.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.13.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.14.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.14.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.14.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)

                beta.15.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.16.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.17.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.18.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.18.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.19.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.19.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.20.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.20.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.21.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.21.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.21.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)

                beta.22.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.23.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.24.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.25.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.25.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.26.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.26.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.27.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.27.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.28.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.28.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.28.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)

                beta.29.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.30.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.31.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.32.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.32.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.33.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.33.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.34.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.34.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.35.1 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.35.2 <- rtruncnorm(n=1,b=0,mean=0,sd=1)
                beta.35.3 <- rtruncnorm(n=1,b=0,mean=0,sd=1)

                # interactive
                beta.4.12 <- rnorm(1,0,1)
                beta.5.13 <- rnorm(1,0,1)
                beta.6.23 <- rnorm(1,0,1)
                beta.7.12 <- rnorm(1,0,1)
                beta.7.13 <- rnorm(1,0,1)
                beta.7.23 <- rnorm(1,0,1)
                beta.7.123 <- rnorm(1,0,1)

                beta.11.12 <- rnorm(1,0,1)
                beta.12.13 <- rnorm(1,0,1)
                beta.13.23 <- rnorm(1,0,1)
                beta.14.12 <- rnorm(1,0,1)
                beta.14.13 <- rnorm(1,0,1)
                beta.14.23 <- rnorm(1,0,1)
                beta.14.123 <- rnorm(1,0,1)

                beta.18.12 <- rnorm(1,0,1)
                beta.19.13 <- rnorm(1,0,1)
                beta.20.23 <- rnorm(1,0,1)
                beta.21.12 <- rnorm(1,0,1)
                beta.21.13 <- rnorm(1,0,1)
                beta.21.23 <- rnorm(1,0,1)
                beta.21.123 <- rnorm(1,0,1)

                beta.25.12 <- rnorm(1,0,1)
                beta.26.13 <- rnorm(1,0,1)
                beta.27.23 <- rnorm(1,0,1)
                beta.28.12 <- rnorm(1,0,1)
                beta.28.13 <- rnorm(1,0,1)
                beta.28.23 <- rnorm(1,0,1)
                beta.28.123 <- rnorm(1,0,1)

                beta.32.12 <- rnorm(1,0,1)
                beta.33.13 <- rnorm(1,0,1)
                beta.34.23 <- rnorm(1,0,1)
                beta.35.12 <- rnorm(1,0,1)
                beta.35.13 <- rnorm(1,0,1)
                beta.35.23 <- rnorm(1,0,1)
                beta.35.123 <- rnorm(1,0,1)


                inv_epsilon <- rtruncnorm(n=I,a=0,mean=1.875,sd=1)
                epsilon <- 1/inv_epsilon
                logRT <- matrix(NA,nrow=N,ncol=I)
                for (i in 1:N){
                        logRT[i,1] <- rnorm(1,alpha[1] + beta.1.1 * attribute_profile[i,1],epsilon[1])
                        logRT[i,2] <- rnorm(1,alpha[2] + beta.2.2 * attribute_profile[i,2],epsilon[2])
                        logRT[i,3] <- rnorm(1,alpha[3] + beta.3.3 * attribute_profile[i,3],epsilon[3])
                        logRT[i,4] <- rnorm(1,alpha[4] + beta.4.1 * attribute_profile[i,1] + beta.4.2 * attribute_profile[i,2] + beta.4.12 * attribute_profile[i,1]* attribute_profile[i,2],epsilon[4])
                        logRT[i,5] <- rnorm(1,alpha[5] + beta.5.1 * attribute_profile[i,1] + beta.5.3 * attribute_profile[i,3] + beta.5.13 * attribute_profile[i,1]* attribute_profile[i,3],epsilon[5])
                        logRT[i,6] <- rnorm(1,alpha[6] + beta.6.2 * attribute_profile[i,2] + beta.6.3 * attribute_profile[i,3] + beta.6.23 * attribute_profile[i,2]* attribute_profile[i,3],epsilon[6])
                        logRT[i,7] <- rnorm(1,alpha[7] + beta.7.1 * attribute_profile[i,1] + beta.7.2 * attribute_profile[i,2] + beta.7.3 * attribute_profile[i,3] + 
                                        beta.7.12 * attribute_profile[i,1]* attribute_profile[i,2] + beta.7.13 * attribute_profile[i,1]* attribute_profile[i,3] + beta.7.23 * attribute_profile[i,2]* attribute_profile[i,3] + 
                                        beta.7.123 * attribute_profile[i,1]* attribute_profile[i,2] * attribute_profile[i,3], epsilon[7])

                        logRT[i,8] <- rnorm(1,alpha[8] + beta.8.1 * attribute_profile[i,1],epsilon[8])
                        logRT[i,9] <- rnorm(1,alpha[9] + beta.9.2 * attribute_profile[i,2],epsilon[9])
                        logRT[i,10] <- rnorm(1,alpha[10] + beta.10.3 * attribute_profile[i,3],epsilon[10])
                        logRT[i,11] <- rnorm(1,alpha[11] + beta.11.1 * attribute_profile[i,1] + beta.11.2 * attribute_profile[i,2] + beta.11.12 * attribute_profile[i,1]* attribute_profile[i,2],epsilon[11])
                        logRT[i,12] <- rnorm(1,alpha[12] + beta.12.1 * attribute_profile[i,1] + beta.12.3 * attribute_profile[i,3] + beta.12.13 * attribute_profile[i,1]* attribute_profile[i,3],epsilon[12])
                        logRT[i,13] <- rnorm(1,alpha[13] + beta.13.2 * attribute_profile[i,2] + beta.13.3 * attribute_profile[i,3] + beta.13.23 * attribute_profile[i,2]* attribute_profile[i,3],epsilon[13])
                        logRT[i,14] <- rnorm(1,alpha[14] + beta.14.1 * attribute_profile[i,1] + beta.14.2 * attribute_profile[i,2] + beta.14.3 * attribute_profile[i,3] + 
                                        beta.14.12 * attribute_profile[i,1]* attribute_profile[i,2] + beta.14.13 * attribute_profile[i,1]* attribute_profile[i,3] + beta.14.23 * attribute_profile[i,2]* attribute_profile[i,3] + 
                                        beta.14.123 * attribute_profile[i,1]* attribute_profile[i,2] * attribute_profile[i,3], epsilon[14])
            
                        logRT[i,15] <- rnorm(1,alpha[15] + beta.15.1 * attribute_profile[i,1],epsilon[15])
                        logRT[i,16] <- rnorm(1,alpha[16] + beta.16.2 * attribute_profile[i,2],epsilon[16])
                        logRT[i,17] <- rnorm(1,alpha[17] + beta.17.3 * attribute_profile[i,3],epsilon[17])
                        logRT[i,18] <- rnorm(1,alpha[18] + beta.18.1 * attribute_profile[i,1] + beta.18.2 * attribute_profile[i,2] + beta.18.12 * attribute_profile[i,1]* attribute_profile[i,2],epsilon[18])
                        logRT[i,19] <- rnorm(1,alpha[19] + beta.19.1 * attribute_profile[i,1] + beta.19.3 * attribute_profile[i,3] + beta.19.13 * attribute_profile[i,1]* attribute_profile[i,3],epsilon[19])
                        logRT[i,20] <- rnorm(1,alpha[20] + beta.20.2 * attribute_profile[i,2] + beta.20.3 * attribute_profile[i,3] + beta.20.23 * attribute_profile[i,2]* attribute_profile[i,3],epsilon[20])
                        logRT[i,21] <- rnorm(1,alpha[21] + beta.21.1 * attribute_profile[i,1] + beta.21.2 * attribute_profile[i,2] + beta.21.3 * attribute_profile[i,3] + 
                                        beta.21.12 * attribute_profile[i,1]* attribute_profile[i,2] + beta.21.13 * attribute_profile[i,1]* attribute_profile[i,3] + beta.21.23 * attribute_profile[i,2]* attribute_profile[i,3] + 
                                        beta.21.123 * attribute_profile[i,1]* attribute_profile[i,2] * attribute_profile[i,3], epsilon[21])

                        logRT[i,22] <- rnorm(1,alpha[22] + beta.22.1 * attribute_profile[i,1],epsilon[22])
                        logRT[i,23] <- rnorm(1,alpha[23] + beta.23.2 * attribute_profile[i,2],epsilon[23])
                        logRT[i,24] <- rnorm(1,alpha[24] + beta.24.3 * attribute_profile[i,3],epsilon[24])
                        logRT[i,25] <- rnorm(1,alpha[25] + beta.25.1 * attribute_profile[i,1] + beta.25.2 * attribute_profile[i,2] + beta.25.12 * attribute_profile[i,1]* attribute_profile[i,2],epsilon[25])
                        logRT[i,26] <- rnorm(1,alpha[26] + beta.26.1 * attribute_profile[i,1] + beta.26.3 * attribute_profile[i,3] + beta.26.13 * attribute_profile[i,1]* attribute_profile[i,3],epsilon[26])
                        logRT[i,27] <- rnorm(1,alpha[27] + beta.27.2 * attribute_profile[i,2] + beta.27.3 * attribute_profile[i,3] + beta.27.23 * attribute_profile[i,2]* attribute_profile[i,3],epsilon[27])
                        logRT[i,28] <- rnorm(1,alpha[28] + beta.28.1 * attribute_profile[i,1] + beta.28.2 * attribute_profile[i,2] + beta.28.3 * attribute_profile[i,3] + 
                                           beta.28.12 * attribute_profile[i,1]* attribute_profile[i,2] + beta.28.13 * attribute_profile[i,1]* attribute_profile[i,3] + beta.28.23 * attribute_profile[i,2]* attribute_profile[i,3] + 
                                           beta.28.123 * attribute_profile[i,1]* attribute_profile[i,2] * attribute_profile[i,3], epsilon[28])

                        logRT[i,29] <- rnorm(1,alpha[29] + beta.29.1 * attribute_profile[i,1],epsilon[29])
                        logRT[i,30] <- rnorm(1,alpha[30] + beta.30.2 * attribute_profile[i,2],epsilon[30])
                        logRT[i,31] <- rnorm(1,alpha[31] + beta.31.3 * attribute_profile[i,3],epsilon[31])
                        logRT[i,32] <- rnorm(1,alpha[32] + beta.32.1 * attribute_profile[i,1] + beta.32.2 * attribute_profile[i,2] + beta.32.12 * attribute_profile[i,1]* attribute_profile[i,2],epsilon[32])
                        logRT[i,33] <- rnorm(1,alpha[33] + beta.33.1 * attribute_profile[i,1] + beta.33.3 * attribute_profile[i,3] + beta.33.13 * attribute_profile[i,1]* attribute_profile[i,3],epsilon[33])
                        logRT[i,34] <- rnorm(1,alpha[34] + beta.34.2 * attribute_profile[i,2] + beta.34.3 * attribute_profile[i,3] + beta.34.23 * attribute_profile[i,2]* attribute_profile[i,3],epsilon[34])
                        logRT[i,35] <- rnorm(1,alpha[35] + beta.35.1 * attribute_profile[i,1] + beta.35.2 * attribute_profile[i,2] + beta.35.3 * attribute_profile[i,3] + 
                                           beta.35.12 * attribute_profile[i,1]* attribute_profile[i,2] + beta.35.13 * attribute_profile[i,1]* attribute_profile[i,3] + beta.35.23 * attribute_profile[i,2]* attribute_profile[i,3] + 
                                           beta.35.123 * attribute_profile[i,1]* attribute_profile[i,2] * attribute_profile[i,3], epsilon[35])
                }

                real <- list(alpha=alpha,beta.1.1=beta.1.1,beta.2.2=beta.2.2,beta.3.3=beta.3.3,beta.4.1=beta.4.1,beta.4.2=beta.4.2,beta.5.1=beta.5.1,beta.5.3=beta.5.3,
                            beta.6.2=beta.6.2,beta.6.3=beta.6.3,beta.7.1=beta.7.1,beta.7.2=beta.7.2,beta.7.3=beta.7.3,beta.8.1=beta.8.1,beta.9.2=beta.9.2,
                            beta.10.3=beta.10.3,beta.11.1=beta.11.1,beta.11.2=beta.11.2,beta.12.1=beta.12.1,beta.12.3=beta.12.3,beta.13.2=beta.13.2,
                            beta.13.3=beta.13.3,beta.14.1=beta.14.1,beta.14.2=beta.14.2,beta.14.3=beta.14.3,beta.15.1=beta.15.1,beta.16.2=beta.16.2,
                            beta.17.3=beta.17.3,beta.18.1=beta.18.1,beta.18.2=beta.18.2,beta.19.1=beta.19.1,beta.19.3=beta.19.3,beta.20.2=beta.20.2,
                            beta.20.3=beta.20.3,beta.21.1=beta.21.1,beta.21.2=beta.21.2,beta.21.3=beta.21.3,beta.22.1=beta.22.1,beta.23.2=beta.23.2,
                            beta.24.3=beta.24.3,beta.25.1=beta.25.1,beta.25.2=beta.25.2,beta.26.1=beta.26.1,beta.26.3=beta.26.3,beta.27.2=beta.27.2,
                            beta.27.3=beta.27.3,beta.28.1=beta.28.1,beta.28.2=beta.28.2,beta.28.3=beta.28.3,beta.29.1=beta.29.1,beta.30.2=beta.30.2,
                            beta.31.3=beta.31.3,beta.32.1=beta.32.1,beta.32.2=beta.32.2,beta.33.1=beta.33.1,beta.33.3=beta.33.3,beta.34.2=beta.34.2,
                            beta.34.3=beta.34.3,beta.35.1=beta.35.1,beta.35.2=beta.35.2,beta.35.3=beta.35.3,beta.4.12=beta.4.12,beta.5.13=beta.5.13,
                            beta.6.23=beta.6.23,beta.7.12=beta.7.12,beta.7.13=beta.7.13,beta.7.23=beta.7.23,beta.7.123=beta.7.123,beta.11.12=beta.11.12,
                            beta.12.13 =beta.12.13,beta.13.23 =beta.13.23,beta.14.12=beta.14.12,beta.14.13=beta.14.13,beta.14.23=beta.14.23,beta.14.123=beta.14.123,
                            beta.18.12=beta.18.12 ,beta.19.13 =beta.19.13 ,beta.20.23 =beta.20.23 ,beta.21.12 =beta.21.12,beta.21.13 =beta.21.13,
                            beta.21.23=beta.21.23,beta.21.123=beta.21.123,beta.25.12=beta.25.12,beta.26.13=beta.26.13,beta.27.23=beta.27.23,beta.28.12=beta.28.12,
                            beta.28.13=beta.28.13,beta.28.23=beta.28.23,beta.28.123=beta.28.123,beta.32.12=beta.32.12,beta.33.13=beta.33.13,beta.34.23=beta.34.23,
                            beta.35.12=beta.35.12,beta.35.13=beta.35.13,beta.35.23=beta.35.23,beta.35.123=beta.35.123,epsilon=epsilon,profile=attribute_profile)

                filename <- paste("simulation-latent/","real-","rep", as.character(r),"I",as.character(I),"N",as.character(N),".rds",collapse="",sep="")
                saveRDS(real,filename)

                jags.data <- list(I=nrow(logRT),J=ncol(logRT),K=K,all.pattern=possible.pattern,delta=rep(1,nrow(possible.pattern)),
                                  logRT=logRT,C=nrow(possible.pattern))
                parameters <- c("profile","c")
                # parameters <- c("alpha","beta.1.1","beta.2.2","beta.3.3","beta.4.1","beta.4.2","beta.5.1","beta.5.3","beta.6.2","beta.6.3","beta.7.1","beta.7.2","beta.7.3",
                #                 "beta.8.1","beta.9.2","beta.10.3","beta.11.1","beta.11.2","beta.12.1","beta.12.3","beta.13.2","beta.13.3","beta.14.1","beta.14.2","beta.14.3",
                #                 "beta.15.1","beta.16.2","beta.17.3","beta.18.1","beta.18.2","beta.19.1","beta.19.3","beta.20.2","beta.20.3","beta.21.1","beta.21.2","beta.21.3",
                #                 "beta.22.1","beta.23.2","beta.24.3","beta.25.1","beta.25.2","beta.26.1","beta.26.3","beta.27.2","beta.27.3","beta.28.1","beta.28.2","beta.28.3",
                #                 "beta.29.1","beta.30.2","beta.31.3","beta.32.1","beta.32.2","beta.33.1","beta.33.3","beta.34.2","beta.34.3","beta.35.1","beta.35.2","beta.35.3",
                #                 "beta.4.12","beta.5.13","beta.6.23","beta.7.12","beta.7.13","beta.7.23","beta.7.123","beta.11.12","beta.12.13","beta.13.23","beta.14.12","beta.14.13",
                #                 "beta.14.23","beta.14.123","beta.18.12","beta.19.13","beta.20.23","beta.21.12","beta.21.13","beta.21.23","beta.21.123",
                #                 "beta.25.12","beta.26.13","beta.27.23","beta.28.12","beta.28.13","beta.28.23","beta.28.123","beta.32.12","beta.33.13","beta.34.23",
                #                 "beta.35.12","beta.35.13","beta.35.23","beta.35.123","epsilon","c","profile")

                model.fit <- jags(model.file='sim_latent_35.txt',data=jags.data,parameters.to.save=parameters,n.iter=12000,n.chains=4,n.burnin=10000,jags.seed=1234)
                filename <- paste("simulation-latent/","rep", as.character(r),"I",as.character(I),"N",as.character(N),".rds",collapse="",sep="")
                saveRDS(model.fit,filename)
            }


        }
}

latent(35)




getmode <- function(v) {
                uniqv <- unique(v)
                uniqv[which.max(tabulate(match(v, uniqv)))]
}


result <- c()
for (I in c(35)){
        for (N in c(100,200,1000)){
            for (r in 1:100){
            all.pattern <- c(0,0,0)
            all.pattern <- as.matrix(rbind(all.pattern,Q_all[1:I,]))

            filename <- paste("simulation-latent/","real-","rep", as.character(r),"I",as.character(I),"N",as.character(N),".rds",collapse="",sep="")
            real <- readRDS(filename)
            real.attribute.profile <- real$profile
            filename <- paste("simulation-latent/","rep", as.character(r),"I",as.character(I),"N",as.character(N),".rds",collapse="",sep="")
            est <- readRDS(filename)

            # posterior.c <- apply(est$BUGSoutput$sims.list$c,2,getmode)
            # est.profile <- c()
            # for (i in 1:N){
            #     est.profile <- rbind(est.profile, all.pattern[posterior.c[i],])
            # }
            est.profile <- est$BUGSoutput$median$profile

        match <- (real.attribute.profile == est.profile)
        ans <- rbind(result, c(round(mean(match[,1]),3),
                        round(mean(match[,2]),3),
                        round(mean(match[,3]),3),
                        round(mean(apply(match,1,sum) == 3),3)))
        }
        result <- rbind(result,round(apply(ans,2,median),3))
    }
}


