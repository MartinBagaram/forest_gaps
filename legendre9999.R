###########Test coefficient significance through permutation
library(combinat)
corperm<-function(x,y,N=9999, plot=F){
  reps<-replicate(N, cor(sample(x),y, use = "pairwise.complete.obs"))
  obs<-cor(x,y,use = "pairwise.complete.obs")
  p<-mean(abs(obs)<abs(reps))
  if(plot){
    hist(reps)
    abline(v=obs,col="red")
  }
  p
}