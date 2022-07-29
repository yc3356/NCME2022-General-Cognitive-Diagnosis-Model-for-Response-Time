library(R2jags)
library(rjags)
library(GDINA)
library(tidyverse)
library(truncnorm)



robust <- function(N=100,errorP=0.05){
    performance <- c()
    R <- 100
    for (r in 1:R){
        I <- 10
        K <- 3
        Q <- matrix(c(1,0,0,
                    0,1,0,
                    0,0,1,
                    1,1,0,
                    1,0,1,
                    0,1,1,
                    1,1,1,
                    1,1,0,
                    0,1,1,
                    1,0,1), 
                    I, K, byrow = TRUE)
        possible.pattern <- c(0,0,0)
        possible.pattern <- as.matrix(rbind(possible.pattern,Q[1:7,]))
        random.index <- sample(1:8, N, replace = TRUE)
        manifest_attribute_profile <- possible.pattern[random.index,]
        manifest_attribute_profile_v <- as.vector(manifest_attribute_profile)
        indexs <- sample(1:length(manifest_attribute_profile_v),length(manifest_attribute_profile_v)*errorP,replace=FALSE)
        manifest_attribute_profile_v[indexs] <- ifelse(manifest_attribute_profile_v[indexs]==1,0,1)
        fake_attribute_profile <- matrix(manifest_attribute_profile_v,
                                            byrow=FALSE,
                                            ncol=ncol(manifest_attribute_profile),
                                            nrow=nrow(manifest_attribute_profile))


        alpha <- runif(I,0,5)
        beta.1.1 <- -runif(1,0,2)
        beta.2.2 <- -runif(1,0,2)
        beta.3.3 <- -runif(1,0,2)
        beta.4.1 <- -runif(1,0,2)
        beta.4.2 <- -runif(1,0,2)
        beta.5.1 <- -runif(1,0,2)
        beta.5.3 <- -runif(1,0,2)
        beta.6.2 <- -runif(1,0,2)
        beta.6.3 <- -runif(1,0,2)
        beta.7.1 <- -runif(1,0,2)
        beta.7.2 <- -runif(1,0,2)
        beta.7.3 <- -runif(1,0,2)
        beta.8.1 <- -runif(1,0,2)
        beta.8.2 <- -runif(1,0,2)
        beta.9.2 <- -runif(1,0,2)
        beta.9.3 <- -runif(1,0,2)
        beta.10.1 <- -runif(1,0,2)
        beta.10.3 <- -runif(1,0,2)
        # interactive
        beta.4.12 <- rnorm(1,0,1)
        beta.5.13 <- rnorm(1,0,1)
        beta.6.23 <- rnorm(1,0,1)
        beta.7.12 <- rnorm(1,0,1)
        beta.7.13 <- rnorm(1,0,1)
        beta.7.23 <- rnorm(1,0,1)
        beta.7.123 <- rnorm(1,0,1)
        beta.8.12 <- rnorm(1,0,1)
        beta.9.23 <- rnorm(1,0,1)
        beta.10.13 <- rnorm(1,0,1)

        inv_epsilon <- rtruncnorm(n=I,a=0,mean=1.875,sd=1)
        epsilon <- 1/inv_epsilon

        logRT <- matrix(NA,nrow=N,ncol=I)
        for (i in 1:N){
                logRT[i,1] <- rnorm(1,alpha[1] + beta.1.1 * fake_attribute_profile[i,1],epsilon[1])
                logRT[i,2] <- rnorm(1,alpha[2] + beta.2.2 * fake_attribute_profile[i,2],epsilon[2])
                logRT[i,3] <- rnorm(1,alpha[3] + beta.3.3 * fake_attribute_profile[i,3],epsilon[3])
                logRT[i,4] <- rnorm(1,alpha[4] + beta.4.1 * fake_attribute_profile[i,1] + beta.4.2 * fake_attribute_profile[i,2] + beta.4.12 * fake_attribute_profile[i,1]* fake_attribute_profile[i,2],epsilon[4])
                logRT[i,5] <- rnorm(1,alpha[5] + beta.5.1 * fake_attribute_profile[i,1] + beta.5.3 * fake_attribute_profile[i,3] + beta.5.13 * fake_attribute_profile[i,1]* fake_attribute_profile[i,3],epsilon[5])
                logRT[i,6] <- rnorm(1,alpha[6] + beta.6.2 * fake_attribute_profile[i,2] + beta.6.3 * fake_attribute_profile[i,3] + beta.6.23 * fake_attribute_profile[i,2]* fake_attribute_profile[i,3],epsilon[6])
                logRT[i,7] <- rnorm(1,alpha[7] + beta.7.1 * fake_attribute_profile[i,1] + beta.7.2 * fake_attribute_profile[i,2] + beta.7.3 * fake_attribute_profile[i,3] + 
                                beta.7.12 * fake_attribute_profile[i,1]* fake_attribute_profile[i,2] + beta.7.13 * fake_attribute_profile[i,1]* fake_attribute_profile[i,3] + beta.7.23 * fake_attribute_profile[i,2]* fake_attribute_profile[i,3] + 
                                beta.7.123 * fake_attribute_profile[i,1]* fake_attribute_profile[i,2] * fake_attribute_profile[i,3], epsilon[7])
                logRT[i,8] <- rnorm(1,alpha[8] + beta.8.1 * fake_attribute_profile[i,1] + beta.8.2 * fake_attribute_profile[i,2] + beta.8.12 * fake_attribute_profile[i,1]* fake_attribute_profile[i,2],epsilon[8])
                logRT[i,9] <- rnorm(1,alpha[9] + beta.9.2 * fake_attribute_profile[i,2] + beta.9.3 * fake_attribute_profile[i,3] + beta.9.23 * fake_attribute_profile[i,2]* fake_attribute_profile[i,3],epsilon[9])
                logRT[i,10] <- rnorm(1,alpha[10] + beta.10.1 * fake_attribute_profile[i,1] + beta.10.3 * fake_attribute_profile[i,3] + beta.10.13 * fake_attribute_profile[i,1]* fake_attribute_profile[i,3],epsilon[10])
                
            }


        jags.data <- list(I=nrow(logRT),J=ncol(logRT),profile=manifest_attribute_profile,logRT=logRT)
        parameters <- c("alpha","beta.1.1","beta.2.2","beta.3.3","beta.4.1","beta.4.2","beta.5.1","beta.5.3","beta.6.2","beta.6.3","beta.7.1","beta.7.2","beta.7.3","beta.8.1","beta.8.2","beta.9.2","beta.9.3","beta.10.1","beta.10.3",
                        "beta.4.12","beta.5.13","beta.6.23","beta.7.12","beta.7.13","beta.7.23","beta.7.123","beta.8.12","beta.9.23","beta.10.13","epsilon")
        manifest.model.fit <- jags(model.file='sim_manifest.txt',data=jags.data,parameters.to.save=parameters,n.iter=12000,n.chains=4,n.burnin=10000,jags.seed=1234)
        filename <- paste("simulation-manifest/","rep", as.character(r),".rds",collapse="",sep="")
        #saveRDS(manifest.model.fit,filename)
        est.alpha <- manifest.model.fit$BUGSoutput$median$alpha
        est.beta.1.1   <- manifest.model.fit$BUGSoutput$median$beta.1.1
        est.beta.2.2   <- manifest.model.fit$BUGSoutput$median$beta.2.2
        est.beta.3.3   <- manifest.model.fit$BUGSoutput$median$beta.3.3 
        est.beta.4.1   <- manifest.model.fit$BUGSoutput$median$beta.4.1
        est.beta.4.2   <- manifest.model.fit$BUGSoutput$median$beta.4.2
        est.beta.5.1   <- manifest.model.fit$BUGSoutput$median$beta.5.1
        est.beta.5.3   <- manifest.model.fit$BUGSoutput$median$beta.5.3
        est.beta.6.2   <- manifest.model.fit$BUGSoutput$median$beta.6.2
        est.beta.6.3   <- manifest.model.fit$BUGSoutput$median$beta.6.3
        est.beta.7.1   <- manifest.model.fit$BUGSoutput$median$beta.7.1
        est.beta.7.2   <- manifest.model.fit$BUGSoutput$median$beta.7.2
        est.beta.7.3   <- manifest.model.fit$BUGSoutput$median$beta.7.3
        est.beta.8.1   <- manifest.model.fit$BUGSoutput$median$beta.8.1
        est.beta.8.2   <- manifest.model.fit$BUGSoutput$median$beta.8.2
        est.beta.9.2   <- manifest.model.fit$BUGSoutput$median$beta.9.2
        est.beta.9.3   <- manifest.model.fit$BUGSoutput$median$beta.9.3
        est.beta.10.1  <- manifest.model.fit$BUGSoutput$median$beta.10.1
        est.beta.10.3  <- manifest.model.fit$BUGSoutput$median$beta.10.3
        est.beta.4.12  <- manifest.model.fit$BUGSoutput$median$beta.4.12
        est.beta.5.13  <- manifest.model.fit$BUGSoutput$median$beta.5.13
        est.beta.6.23  <- manifest.model.fit$BUGSoutput$median$beta.6.23
        est.beta.7.12  <- manifest.model.fit$BUGSoutput$median$beta.7.12
        est.beta.7.13  <- manifest.model.fit$BUGSoutput$median$beta.7.13
        est.beta.7.23  <- manifest.model.fit$BUGSoutput$median$beta.7.23
        est.beta.7.123 <- manifest.model.fit$BUGSoutput$median$beta.7.123
        est.beta.8.12  <- manifest.model.fit$BUGSoutput$median$beta.8.12
        est.beta.9.23  <- manifest.model.fit$BUGSoutput$median$beta.9.23
        est.beta.10.13 <- manifest.model.fit$BUGSoutput$median$beta.10.13 
        est.epsilon <- manifest.model.fit$BUGSoutput$median$epsilon 


        main <- c(beta.1.1,beta.2.2,beta.3.3,beta.4.1,beta.4.2,beta.5.1,beta.5.3,beta.6.2,beta.6.3,beta.7.1,beta.7.2,beta.7.3,
                    beta.8.1,beta.8.2,beta.9.2,beta.9.3,beta.10.1,beta.10.3)
        main.est <- c(est.beta.1.1, est.beta.2.2,est.beta.3.3,est.beta.4.1,est.beta.4.2,est.beta.5.1,est.beta.5.3,est.beta.6.2,
                    est.beta.6.3,est.beta.7.1,est.beta.7.2,est.beta.7.3,est.beta.8.1,est.beta.8.2,est.beta.9.2,est.beta.9.3,
                    est.beta.10.1,est.beta.10.3)
        twoway <- c(beta.4.12,beta.5.13,beta.6.23,beta.7.12,beta.7.13,beta.7.23,beta.8.12,beta.9.23,beta.10.13)
        twoway.est <- c(est.beta.4.12,est.beta.5.13,est.beta.6.23,est.beta.7.12,est.beta.7.13,est.beta.7.23,est.beta.8.12,est.beta.9.23,est.beta.10.13)

        mean.abs.bias <- c(mean(abs(alpha - est.alpha)),
        mean(abs(main-main.est)),
        mean(abs(twoway-twoway.est)),
        mean(abs(beta.7.123- est.beta.7.123)),
        mean(abs(sqrt(epsilon) - est.epsilon)))
        performance <- rbind(performance,mean.abs.bias)
    }
    filename <- paste("simulation-manifest/","mean.abs.bias","N",as.character(N),"p",as.character(errorP),".rds",collapse="",sep="")
    saveRDS(performance,filename)

}


#performance <- readRDS(filename)
#round(apply(performance,2,mean),3)


# for (n in c(100,500,1000)){
#     for (p in c(0.05,0.2,0.5)){
#         robust(N=n,errorP=p)
#     }
# }

ans <- c()
for (n in c(100,500,1000)){
    for (p in c(0.05,0.2,0.5)){
        filename <- paste("simulation-manifest/","mean.abs.bias","N",as.character(n),"p",as.character(p),".rds",collapse="",sep="")
        result <- readRDS(filename)
        ans <- rbind(ans,apply(result,2,mean))
    }
}
ans <- round(ans,3)
write.csv(ans,"ans.csv")
