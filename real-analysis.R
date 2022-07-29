library(R2jags)
library(rjags)
library(GDINA)
library(tidyverse)
library(jagsUI)

data <- readRDS(file = "dat.rds")
RA <- data %>% select(starts_with("CM"))
RT <- data %>% select(starts_with("time"))
logRT <- log(RT)
I <- 10
K <- 7
Q <- matrix(c(0,1,0,0,1,0,0,
              1,0,0,0,1,0,0,
              1,0,0,0,1,0,0,
              0,0,1,0,0,0,1,
              0,0,1,0,0,0,1,
              0,0,1,0,0,0,1,
              0,0,1,0,0,0,1,
              0,0,0,1,0,1,0,
              0,0,0,1,0,1,0,
              0,0,0,1,0,1,0), 
              I, K, byrow = TRUE)


## manifest attribute profile
est <- GDINA(dat = RA, Q = Q, model = "GDINA", verbose = 0)
Qv <- Qval(est,method = "Wald")
Qv
CA(est)
manifest_attribute_profile <- personparm(est)

# jags.data <- list(I=nrow(RA),J=ncol(RA),profile=manifest_attribute_profile,
#     logRT=logRT)
# manifest.model.fit <- jags(model.file="LM_manifest.txt",data=jags.data,parameters.to.save=c("alpha","beta"),n.iter=12000,n.chains=4,n.burnin=10000,jags.seed=1234)
# saveRDS(manifest.model.fit,"manifest.model.fit.rds")
# manifest.model.fit <- readRDS("manifest.model.fit.rds")

# manifest.alpha <- manifest.model.fit$BUGSoutput$median$alpha
# manifest.beta <- as.data.frame(manifest.model.fit$BUGSoutput$median$beta)
# table <- cbind(round(exp(manifest.alpha),3),round(exp(manifest.beta),3))
# write.csv(table,"table.csv")
# table <- cbind(round(manifest.alpha,3),round(manifest.beta,3))
# for (i in 1:10){
#     alpha_sd = round(manifest.model.fit$BUGSoutput$sd$alpha,3)
#     cstring = c(as.character(table[i,1]), " ", "(", as.character(alpha_sd[i]), ")")
#     table[i,1] = paste(cstring,collapse="")
#     beta_sd = round(manifest.model.fit$BUGSoutput$sd$beta,3)
#     for (j in 2:4){
#         cstring = c(as.character(table[i,j]), " ", "(", as.character(beta_sd[i,(j-1)]), ")")
#         table[i,j] = paste(cstring,collapse="")
#     }
# }


# write.csv(table,"table.csv")









#### latent attribute profile
all.pattern <- rep(0,K)
for (k in 1:K){
    combine <- combn(K,k)
    for (i in 1:ncol(combine)){
        temp <- rep(0,K)
        temp[combine[,i]] <- 1
        all.pattern <- rbind(all.pattern,temp)
    }
}
all.pattern <- as.matrix(all.pattern)
delta <- rep(1,nrow(all.pattern))


latent.jags.data <- list(I=nrow(RA),J=ncol(RA),K=K,all.pattern=all.pattern,delta=delta,logRT=logRT,C=length(delta))
latent.model.fit <-  autojags(model.file="LM_latent.txt",data=latent.jags.data,parameters.to.save=c("alpha","beta","epsilon","profile"),n.chains = 2,Rhat.limit = 1.3)
saveRDS(latent.model.fit,"latent.model.fit.rds")
latent.model.fit <- readRDS("latent.model.fit.rds")

# latent.alpha <- latent.model.fit$BUGSoutput$median$alpha
# latent.beta <- as.data.frame(latent.model.fit$BUGSoutput$median$beta)
# table <- cbind(round(exp(latent.alpha),3),round(exp(latent.beta),3))
# write.csv(table,"table.csv")
# table <- cbind(round(latent.alpha,3),round(latent.beta,3))
# for (i in 1:10){
#     alpha_sd = round(latent.model.fit$BUGSoutput$sd$alpha,3)
#     #cstring = c(as.character(table[i,1]), " ", "(", as.character(alpha_sd[i]), ")")
#     table[i,1] = paste(cstring,collapse="")
#     beta_sd = round(latent.model.fit$BUGSoutput$sd$beta,3)
#     # for (j in 2:4){
#     #     cstring = c(as.character(table[i,j]), " ", "(", as.character(beta_sd[i,(j-1)]), ")")
#     #     table[i,j] = paste(cstring,collapse="")
#     # }
# }
# write.csv(table,"table.csv")

## matchness
latent.profile <- latent.model.fit$BUGSoutput$median$profile
library(ggplot2)
UNI <- rowMeans((manifest_attribute_profile == 1) & (latent.profile == 0))
UNI <- data.frame(UNI)
ggplot(UNI, aes(x=UNI)) + 
 theme_gray(base_size = 14) +
  geom_histogram() 
ggsave('UNI.png')


RNI <- rowMeans((manifest_attribute_profile == 0) & (latent.profile == 1))
RNI <- data.frame(RNI)
ggplot(RNI, aes(x=RNI)) + 
 theme_gray(base_size = 14) +
  geom_histogram() 
ggsave('RNI.png')


UAI <- rowMeans((manifest_attribute_profile == 1) & (latent.profile == 0))
UAI <- data.frame(UAI)
ggplot(UAI, aes(x=UAI)) + 
 theme_gray(base_size = 14) +
  geom_histogram() 
ggsave('UAI.png')


match = latent.profile == manifest_attribute_profile









# manifest.item.param <- c(manifest.alpha,as.vector(as.matrix(manifest.beta)))
# item.param <- c(latent.alpha,as.vector(as.matrix(latent.beta)))

# cor(manifest.item.param,item.param)
# cor(manifest.item.param,item.param,method="kendall")



# getmode <- function(v) {
#    uniqv <- unique(v)
#    uniqv[which.max(tabulate(match(v, uniqv)))]
# }


# posterior.c <- apply(latent.model.fit$BUGSoutput$sims.list$c,2,getmode)
# profile <- c()
# for (i in 1:nrow(RA)){
#     profile <- rbind(profile, all.pattern[posterior.c[i],])
# }


# mean(apply(profile,1,sum) == 7)
# sum(apply(profile,1,sum) == 7)
# sum(apply(profile,1,sum) == 0)
# mean(apply(profile,1,sum) == 0)

# mean(profile[,1])
# sum(profile[,1])

# mean(profile[,2])
# sum(profile[,2])

# mean(profile[,3])
# sum(profile[,3])

# mean(profile[,4])
# sum(profile[,4])

# mean(profile[,5])
# sum(profile[,5])

# mean(profile[,6])
# sum(profile[,6])

# mean(profile[,7])
# sum(profile[,7])

# profile.alternative <- latent.model.fit$BUGSoutput$median$profile

# contingency <- table(manifest_attribute_profile,profile)
# contingency / sum(contingency)

# sum(diag(contingency)) / sum(contingency)

# result <- c()
# for (k in 1:K){
#     contingency <- as.character(table(manifest_attribute_profile[,k],profile[,k]) )
#     relative_contingency <-  as.character(round(100*table(manifest_attribute_profile[,k],profile[,k]) / sum(table(manifest_attribute_profile[,k],profile[,k])),2))
#     result <- rbind(result,c(paste(c(contingency[1], " ", "(", as.character(relative_contingency[1]),"%",")"),collapse=""),paste(c(contingency[3], " ", "(", as.character(relative_contingency[3]),"%",")"),collapse="")))
#     result <- rbind(result,c(paste(c(contingency[2], " ", "(", as.character(relative_contingency[2]),"%",")"),collapse=""),paste(c(contingency[4], " ", "(", as.character(relative_contingency[4]),"%",")"),collapse="")))
#     print(as.numeric(contingency[3])/as.numeric(contingency[2]))
# }

# write.csv(result,"table.csv")

# hist(apply(profile,1,sum))


# ## analysis the correaltions
# manifest.model.fit <- readRDS("manifest.model.fit.rds")
# latent.model.fit <- readRDS("latent.model.fit.rds")


# manifest.alpha <- manifest.model.fit$BUGSoutput$median$alpha
# latent.alpha <- latent.model.fit$BUGSoutput$median$alpha
# cor(exp(manifest.alpha),exp(latent.alpha))

# manifest.beta <- manifest.model.fit$BUGSoutput$median$beta
# latent.beta <- latent.model.fit$BUGSoutput$median$beta
# cor(as.vector(manifest.beta[,1:2]),as.vector(latent.beta[,1:2]))
# cor(exp(as.vector(manifest.beta[,1:2])),exp(as.vector(latent.beta[,1:2])))


# cor(as.vector(manifest.beta[,3]),as.vector(latent.beta[,3]))
# cor(exp(as.vector(manifest.beta[,3])),exp(as.vector(latent.beta[,3])))


# manifest.table <- cbind(round(exp(manifest.alpha),3),round(exp(manifest.beta),3))
# latent.table <- cbind(round(exp(latent.alpha),3),round(exp(latent.beta),3))


# cor(apply(manifest.table,1,sum),apply(latent.table,1,sum))

