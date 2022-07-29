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
    set.seed(3356)
    for (N in c(100,200,1000)){
        for (r in 1:100){
            K <- 3
            Q <- Q_all[1:I,]

            possible.pattern <- c(0,0,0)
            possible.pattern <- as.matrix(rbind(possible.pattern,Q[1:7,]))
            random.index <- sample(1:8, N, replace = TRUE)
            attribute_profile <- possible.pattern[random.index,]

                alpha <- rnorm(I,0,1)
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
                beta.4.12 <- rnorm(1,0,1)
                beta.5.13 <- rnorm(1,0,1)
                beta.6.23 <- rnorm(1,0,1)
                beta.7.12 <- rnorm(1,0,1)
                beta.7.13 <- rnorm(1,0,1)
                beta.7.23 <- rnorm(1,0,1)
                beta.7.123 <- rnorm(1,0,1)
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
                    }
                real <- list(alpha=alpha,beta.1.1=beta.1.1,beta.2.2=beta.2.2,beta.3.3=beta.3.3,
                            beta.4.1=beta.4.1,beta.4.2=beta.4.2,beta.5.1=beta.5.1,beta.5.3=beta.5.3,
                            beta.6.2=beta.6.2,beta.6.3=beta.6.3,beta.7.1=beta.7.1,beta.7.2=beta.7.2,
                            beta.7.3=beta.7.3,beta.4.12=beta.4.12,beta.5.13=beta.5.13,beta.6.23=beta.6.23,
                            beta.7.12=beta.7.12,beta.7.13=beta.7.13,beta.7.23=beta.7.23,beta.7.123=beta.7.123,
                            epsilon=epsilon,profile=attribute_profile)
                filename <- paste("simulation-latent/","real-","rep", as.character(r),"I",as.character(I),"N",as.character(N),".rds",collapse="",sep="")
                saveRDS(real,filename)

                jags.data <- list(I=nrow(logRT),J=ncol(logRT),K=K,all.pattern=possible.pattern,delta=rep(1,nrow(possible.pattern)),
                                  logRT=logRT,C=nrow(possible.pattern))
                parameters <- c("profile","c")
                # parameters <- c("alpha","beta.1.1","beta.2.2","beta.3.3","beta.4.1","beta.4.2","beta.5.1","beta.5.3","beta.6.2","beta.6.3","beta.7.1","beta.7.2","beta.7.3",
                #                 "beta.4.12","beta.5.13","beta.6.23","beta.7.12","beta.7.13","beta.7.23","beta.7.123","epsilon","c")
                model.fit <- jags(model.file='sim_latent_7.txt',data=jags.data,parameters.to.save=parameters,n.iter=12000,n.chains=4,n.burnin=10000,jags.seed=1234)
                filename <- paste("simulation-latent/","rep", as.character(r),"I",as.character(I),"N",as.character(N),".rds",collapse="",sep="")
                saveRDS(model.fit,filename)



           
        }

    }
}

latent(7)


getmode <- function(v) {
                uniqv <- unique(v)
                uniqv[which.max(tabulate(match(v, uniqv)))]
}




result <- c()
for (I in c(7)){
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


