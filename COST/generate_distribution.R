rm(list = ls())
library(PoisNor)
doparContext <- RxForeachDoPar()
rxSetComputeContext(doparContext)
#-------------------------------------------------
set.seed(8)               # this makes the example reproducible
iter = 100000              # this is how many data you want
probs = c(.6,.9)          # these are *cumulative* probabilities; since they 
#   necessarily sum to 1, the last would be redundant
dists = runif(iter)          # here I'm generating random variates from a uniform
#   to select the relevant distribution
# 
feature_names <- c("Service.Model","Nr.Trades","Nr.CCY","Nr.Positions","AuM","Cash","random")
# these are the feature of the distribution
# 
M=c(0.28,0.43,0.13,0.04,0.01,0.58,0.08,0.07,0.02,0.20,0.06,0.01,0.26,0.02,0.01)
# the 'empirical' correlation coefficients to generate the distributions
N=diag(6) 
N[lower.tri(N)]=M 
TV=N+t(N)
diag(TV)<-1
# Lambda of Nr.Trades, Nr, CCY and Nr. Positions are Poisson distributions with given lambda
lamvec1  <- c(1,2,4) 
lamvec2  <- c(5,3,10)
lamvec3  <- c(10,5,15) 
# Mean of AuM, Cash, Random :)
meanvec1 <-c(500000,50000,0)
meanvec2 <- c(1000000,150000,0.5)
meanvec3 <- c(4000000,300000,0.7)
#  sd of AuM, Cash, Random 
sdvec1   <- c(5e5,1e5,1)
sdvec2   <- c(7e5,1.5e5,1)
sdvec3   <- c(2e6,2e5,1)
#
cstar1   <- cmat.star(no.pois=3, no.norm=3, TV, lamvec1)
cstar2   <- cmat.star(no.pois=3, no.norm=3, TV, lamvec2)
cstar3   <- cmat.star(no.pois=3, no.norm=3, TV, lamvec3)
#   
#df <- data.frame(y=character(),
#                x=matrix(ncol = 6,nrow=0),
#                 stringsAsFactors = FALSE)
colnames(df) <- feature_names
# the empty dataframe to sample from the distributions
colnames(TV) <- feature_names[2:length(feature_names)]
rownames(TV) <- feature_names[2:length(feature_names)]
print(TV)
# the 'empirical' correlation matrix. Input for the generation of the multivariate Poisson-Normal distribution
# 
# 
cl <- makeCluster(4)
registerDoParallel(cl)
final <- foreach(i = 1:iter, .combine=rbind, .packages = c("PoisNor")) %dopar% {
  # this is where the actual data are generated, it's just some if->then
  if(dists[i]<probs[1]){
    mydata=data.frame(sm="light",as.data.frame(t(genPoisNor(n=2, no.norm=3, no.pois=3, cmat.star=cstar1, lamvec1, sd.vec=sdvec1, mean.vec=meanvec1)[1,])))
    #sm <- "light"
  } else if(dists[i]<probs[2]){
    mydata=data.frame(sm="standard",as.data.frame(t(genPoisNor(n=2, no.norm=3, no.pois=3, cmat.star=cstar2, lamvec2, sd.vec=sdvec2, mean.vec=meanvec2)[1,])))
    #sm <- "standard"
  } else {
   mydata=data.frame(sm="plus",as.data.frame(t(genPoisNor(n=2, no.norm=3, no.pois=3, cmat.star=cstar3, lamvec3, sd.vec=sdvec3, mean.vec=meanvec3)[1,])))
    #sm <- "plus"
  }
}
stopCluster(cl)
names(final) <- feature_names
clean_data <- final
write.csv(clean_data,"Rscripts\\COST\\clean_data.csv")
