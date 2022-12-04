
library("ggplot2")
library("UBL")
library("MASS")
library("reticulate")
library("randomForest")
library("ks")
library("mclust")
library("kernelboot")
library("smotefamily")
library("synthpop")
library("mgcv")
library("Metrics")
library(earth)
library("mda")
library("beepr")
library("plotly")


## Dataset simulation 

### Population

n_pop = 10000
alpha_pop = 5
beta_pop = 5
X_pop = rbeta (n_pop,alpha_pop, beta_pop)

pop = data.frame(X1=X_pop)

for (i in (1:nrow(pop))){
  pop$Y[i] = rnorm(1,sin(pop$X1[i]*7 - 0.5) +10 ,0.1)  
}

Pt = function(x){
  dbeta(x,alpha_pop,beta_pop)
}


### sampling & initialization

n_ech = 1000
n_tir = 50
n_dataset  = 16

GAM_devExp = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)
GAM_rmse = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)
GAM_R2 = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)

RF_rsq = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)
RF_rmse = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)
RF_R2 = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)

MARS_rmse = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)
MARS_R2 = matrix(rep(0,n_dataset*n_tir),n_tir,n_dataset)

test = pop[sample(nrow(pop),1000),]
y_test = test$Y
test$Y = NULL

ech_rep = pop[sample(nrow(pop),1000),]
nb = 0


###################################################### LOOP

while (nb < n_tir){
  
    tir_ech = sample(nrow(pop),n_ech,prob = dbeta(pop$X1,9,9))
    ech = pop[tir_ech,]
    
    verif = inherits(try({
    
              ### Weighted Resampling (WR) algorithm
              # Resampling weights definition
              X = ech$X1
              pe = density(X,n=length(X))
              ech$pe = approx(pe$x,pe$y,xout=ech$X1)$y
              ech$pt = Pt(X)
              ech$w = ech$pt / ech$pe
              ech$q = ech$w / sum(ech$w)
              plot(ech$X1,ech$q)
              ech0=ech
              # Drawing
              ech_add = sample(seq(1, nrow(ech)),1000,replace=T,prob = ech$q)
              ech_add = ech[ech_add,]
              pe = density(ech_add$X1,n=length(ech_add$X1))
              ech_add$pe = approx(pe$x,pe$y,xout=ech_add$X1)$y
              ech_add$pt = Pt(ech_add$X1)
              ech_add$w = ech_add$pt / ech_add$pe
              ech_add$q = ech_add$w / sum(ech_add$w)
              
              # Filter
              filtre_var = c("X1","Y")
              ech_add = ech_add[,filtre_var]
              ech0 = ech0[,filtre_var]
              
                                                                                                                                                                                                                                                                                                                                 
              ## Data Augmentation - Weighted Resampling algorithm
              
              
              ### Generation by random noise
              #### Gaussian Noise (GN)
              ##### Gaussian Noise on the augmented dataset and application of WR algorithm
              N = 100000
              pert = 0.5 
              
              for (tir in seq(round(N/nrow(ech_add)))){
                ech_GN=ech_add
                for (j in (1:ncol(ech_add))) {
                  ech_GN[, j] = ech_add[, j] + rnorm(nrow(ech_add), 0, sd(ech0[,colnames(ech0)[j]]) * pert)
                }
                if (tir==1){
                  ech_GN_SC = ech_GN   
                }else{
                  ech_GN_SC = rbind(ech_GN_SC,ech_GN)
                }
              }
              
              # Drawing weights
              X = ech_GN_SC$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_GN_SC$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_GN_SC)),1000,replace=T,prob = q)
              ech_GN_SC = ech_GN_SC[ind,filtre_var]
              
          
              
              ## Gaussian Noise on the augmented dataset, by cluster and application of WR algorithm
              N = 100000
              DMC = densityMclust(ech0, plot=F,G=6)
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              cl = max(ech_MC$cluster)
    
              for (tir in seq(round(N/nrow(ech_add)))){
                for (i in (1:cl)){
                  temp = ech_MC[ech_MC$cluster == i,]
                  ech_add_temp = merge(temp, ech_add, on ="X1")
                  N = nrow(ech_add_temp)
                  ech_GN=ech_add_temp
                  for (j in (1:ncol(ech_add_temp))) {
                    ech_GN[, j] = ech_add_temp[, j] + 
                    rnorm(nrow(ech_add_temp), 0, sd(temp[,colnames(temp)[j]]) * pert)
                  }
                  if (tir==1 & i==1){
                    GN_GMM = ech_GN   
                  }else{
                    GN_GMM = rbind(GN_GMM,ech_GN)
                  }
                }
              }
            
              # Drawing weights
              X = GN_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=GN_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(GN_GMM)),1000,replace=T,prob = q)
              GN_GMM = GN_GMM[ind,filtre_var]
    
      
              #### Multivariate kernel density estimator (ROSE & KDE)
              ##### Estimator inspired by ROSE
            
              hmult=1
              n = nrow(ech0)
              q = ncol(ech0)
              n.new <- nrow(ech_add)
              cons.kernel <- (4/((q+2)*n))^(1/(q+4))
              if(q!=1){
                H <- hmult*cons.kernel*diag(apply(ech0, 2, sd), q)
              }else {
                H <- hmult*cons.kernel*sd(ech0)}
              Xnew.num <- matrix(rnorm(n.new*q), n.new, q)%*%H
              ech_ROSE_SC =data.frame(Xnew.num + ech_add)
              
              # Drawing weights
              X = ech_ROSE_SC$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_ROSE_SC$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_ROSE_SC)),1000,replace=T,prob = q)
              ech_ROSE_SC = ech_ROSE_SC[ind,filtre_var]
              
              # Application by cluster
              ech_MC = cbind(ech0[,c("X1","Y")],"cluster" = DMC$classification)
              ech_add_MC = merge(ech_MC, ech_add, on = filtre_var)
              cl = max(ech_MC$cluster) 
              N = 100000
              
              for (tir in seq(round(N/nrow(ech_add)))){
                for (i in (1:cl)){
                  temp = ech_MC[ech_MC$cluster == i,]
                  temp_add = ech_add_MC[ech_add_MC$cluster == i,]
                  n = nrow(temp)
                  q = ncol(temp)
                  n.new <- nrow(temp_add)
                  cons.kernel <- (4/((q+2)*n))^(1/(q+4))
                  if(q!=1){
                    H <- hmult*cons.kernel*diag(apply(temp, 2, sd), q)
                  }else {
                    H <- hmult*cons.kernel*sd(temp)}
                  Xnew.num <- matrix(rnorm(n.new*q), n.new, q)%*%H
                  ech_ROSE =data.frame(Xnew.num + temp_add)
                  if (tir == 1 & i == 1) {
                    ROSE_GMM = ech_ROSE
                  } else {ROSE_GMM = rbind(ROSE_GMM, ech_ROSE)}
                }
              }
              
              
              # Drawing weights
              X = ROSE_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ROSE_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ROSE_GMM)),1000,replace=T,prob = q)
              ROSE_GMM = ROSE_GMM[ind,filtre_var]
              
              
              
              #### Smoothed Bootstrap (KDE)
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              kde_boot = data.frame(rmvg(100000, ech0, weights =  q))
              
              # Drawing weights
              X = kde_boot$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=kde_boot$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(kde_boot)),1000,replace=T,prob = q)
              kde_boot = kde_boot[ind,filtre_var]
              
              # Application by cluster
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              N = 100000
              ech_MC = cbind(ech0[,c("X1","Y")],"cluster" = DMC$classification)
              ech_add_MC = merge(ech_MC, ech_add, on = filtre_var)
              cl = max(ech_MC$cluster)
              try(
                for (tir in seq(round(N/nrow(ech_add)))){
                  for (i in (1:cl)){
                    temp = data.frame(rmvg(sum(ech_add_MC$cluster == i), ech_MC[ech_MC$cluster == i,filtre_var], weights =  q[ech_MC$cluster==i]))
                    if (tir == 1 & i ==1) {
                      kde_boot_GMM = temp
                    } else {kde_boot_GMM = rbind(kde_boot_GMM, temp)}
                  }
                }
              )  
              
              
              # Drawing weights
              X = kde_boot_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=kde_boot_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(kde_boot_GMM)),1000,replace=T,prob = q)
              kde_boot_GMM = kde_boot_GMM[ind,filtre_var]
              
                     
                     
              ### Mixture model approach (latent structure model)
              #### Gaussian Mixture Model (GMM)
                     
              DMC = densityMclust(ech0, plot=F,G=6)
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              
              DS_GMM = as.data.frame(sim(modelName=DMC$modelName,parameters=DMC$parameters,n=100000))
              colnames(DS_GMM) = c("cluster",colnames(ech0))
              DS_GMM0 = DS_GMM
              # Drawing weights
              X = DS_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=DS_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(DS_GMM)),1000,replace=T,prob = q)
              DS_GMM = DS_GMM[ind,filtre_var]
               
              
              #### Factor analysis (FA)
              
              ##### clustering via GMM
              DMC = densityMclust(ech0, plot=F,G=6)
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              
              ech = ech_MC
              if ("cluster" %in% colnames(ech)) {
                cl = max(ech$cluster)
                p = length(colnames(ech))-1
              }else {
                cl = 1
                p = length(colnames(ech))
              }
              phi = array(rep(0,cl*p),dim=c(1,p,cl))
              W = array(rep(0,cl*p*(p-1)),dim=c(p-1,p,cl))
              mu = array(rep(0,cl*p),dim=c(1,p,cl))
              for (i in (1:cl)){
                if ("cluster" %in% colnames(ech)) {
                  temp = ech[ech$cluster == i,]
                } else {temp = ech}
                temp = temp[,c("X1","Y")]
                p = length(colnames(temp))
                p_py = r_to_py(p,convert=TRUE)
                py_run_string("from sklearn.decomposition import FactorAnalysis")
                ech_py = r_to_py(temp,convert=TRUE)
                py_run_string("p=r.ech_py.shape[1]")
                py_run_string("transformer = FactorAnalysis(n_components=p-1, random_state=0)")
                py_run_string("X_transformed = transformer.fit_transform(r.ech_py)")
                py_run_string("noise_var = transformer.noise_variance_")
                py_run_string("mu = transformer.mean_")
                py_run_string("comp = transformer.components_")
                phi[,,i] = py$noise_var
                W[,,i] = py$comp
                mu[,,i] = py$mu
              }
              # Data generation
              for (i in (1:cl)){
                n_ds = sum(ech$cluster == i)*1000
                Z = mvrnorm(n_ds,rep(0,p-1),diag(rep(1,p-1)))
                temp = Z%*%W[,,i]  +  t(matrix(rep(mu[,,i],n_ds),p,n_ds)) + mvrnorm(n_ds,rep(0,p),diag(phi[,,i]))
                temp = as.data.frame(cbind(temp,cluster = i))
                if (i == 1) {
                  DS_FA = temp
                }else {DS_FA = rbind(DS_FA,temp)}
              }
              DS_FA_GMM = as.data.frame(DS_FA)
              colnames(DS_FA_GMM) = c(colnames(ech0),"cluster")
              
              # Drawing weights
              X = DS_FA_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=DS_FA_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(DS_FA_GMM)),1000,replace=T,prob = q)
              DS_FA_GMM = DS_FA_GMM[ind,filtre_var]
            
              
              ### Copula
              
              py_run_string("from sdv.tabular import GaussianCopula")
              ech_py = r_to_py(ech0,convert=TRUE)
              py_run_string("copule = GaussianCopula()")
              py_run_string("copule.fit(r.ech_py)")
              py_run_string("ech_copule_py = copule.sample(100000)")
              ech_copule = py$ech_copule_py
              
              # Drawing weights
              X = ech_copule$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_copule$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_copule)),1000,replace=T,prob = q)
              ech_copule = ech_copule[ind,filtre_var]
              
              
              ### Conditional Generative Adversarial Net (GAN)
              # Application by clustering 
              ech_MC = cbind(ech0[,c("X1","Y")],"cluster" = DMC$classification)
              py_run_string("from ctgan import CTGANSynthesizer")
              ech_MC_py = r_to_py(ech_MC,convert=TRUE)
              py_run_string("discrete_columns = ['cluster']")
              py_run_string("ctganData = CTGANSynthesizer(epochs=3000)")
              py_run_string("ctganData.fit(r.ech_MC_py,discrete_columns)")
              py_run_string("ech_ctgan_py = ctganData.sample(100000)")
              ech_ctganSynth_GMM = py$ech_ctgan_py
              
              # Drawing weights
              X = ech_ctganSynth_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_ctganSynth_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_ctganSynth_GMM)),1000,replace=T,prob = q)
              ech_ctganSynth_GMM = ech_ctganSynth_GMM[ind,filtre_var]
              
              
              ### k-NN Interpolation
              #### interpolation inspired by SMOTE (SMOTE)
              
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              ech = ech0
              
              k=3
              ech$id=seq(nrow(ech))
              
              n_pop = 10000 #nb obs de la pop fictive
              for (i in (1:n_pop)){
                  id = sample(nrow(ech0),1,prob = q)
                  knn_id = smotefamily::knearest(ech,ech[id,], n_clust=k)
                  kppv = knn_id[sample(seq(k),1)]
                  lambda = runif(1)
                  new_obs = ech[id,] + lambda * (ech[kppv,]-ech[id,])
                  rownames(new_obs)=i
                  if (i==1){
                    ech_smote = new_obs
                  }else{
                    ech_smote = rbind(ech_smote,new_obs)
                  } 
              }
              ech_smote$id = NULL
              
              # Drawing weights
              X = ech_smote$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_smote$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_smote)),1000,replace=T,prob = q)
              ech_smote = ech_smote[ind,filtre_var]
              
              
              #### Application post-clustering
              
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              # Application by cluster
              ech = ech0
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              cl = max(ech_MC$cluster)
              n_pop = 10000 
              for (i in (1:n_pop)){
                  id = sample(nrow(ech0),1,prob = q)
                  temp = ech_MC[ech_MC$cluster == ech_MC$cluster[id],]
                  knn_id = smotefamily::knearest(temp,ech_MC[id,], n_clust=k)
                  kppv = knn_id[sample(seq(k),1)]
                  lambda = runif(1)
                  new_obs = ech_MC[id,] + lambda * (temp[kppv,]-ech_MC[id,])
                  rownames(new_obs)=i
                  if (i==1){
                    ech_smote_GMM = new_obs
                  }else{
                    ech_smote_GMM = rbind(ech_smote_GMM,new_obs)
                  } 
              }
              ech_smote_GMM$id = NULL
              
              
              # Drawing weights
              X = ech_smote_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_smote_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_smote_GMM)),1000,replace=T,prob = q)
              ech_smote_GMM = ech_smote_GMM[ind,filtre_var]
              
              
              
              ### Random Forest (RF)
              
              ech_RF2 = syn(ech_MC[,c("X1","Y","cluster")],method="rf", visit.sequence = c("cluster","X1","Y"),k=10000)
              ech_RF2 = ech_RF2$syn
              
              # Drawing weights
              X = ech_RF2$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_RF2$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_RF2)),1000,replace=T,prob = q)
              ech_RF2 = ech_RF2[ind,filtre_var]
        
    
    }, silent=TRUE), "try-error")  
    
    if (verif == FALSE){
      
      
              # Save
              if (nb > 0) {
                mat_ech = cbind(mat_ech,tir_ech)
              } else {
                mat_ech = as.data.frame(tir_ech) 
              }
              nb = nb + 1
    
              # Drawing weights
              X = ech$X1
              pe = density(X,n=length(X))
              ech$pe = approx(pe$x,pe$y,xout=ech$X1)$y
              ech$pt = Pt(X)
              ech$w = ech$pt / ech$pe
              ech$q = ech$w / sum(ech$w)
              plot(ech$X1,ech$q)
              ech0=ech
              # Drawing
              ech_add = sample(seq(1, nrow(ech)),1000,replace=T,prob = ech$q)
              ech_add = ech[ech_add,]
              pe = density(ech_add$X1,n=length(ech_add$X1))
              ech_add$pe = approx(pe$x,pe$y,xout=ech_add$X1)$y
              ech_add$pt = Pt(ech_add$X1)
              ech_add$w = ech_add$pt / ech_add$pe
              ech_add$q = ech_add$w / sum(ech_add$w)
              
              # Filter
              filtre_var = c("X1","Y")
              ech_add = ech_add[,filtre_var]
              ech0 = ech0[,filtre_var]
              
              
              ## Data Augmentation - Weighted Resampling algorithm
              
              
              ### Generation by random noise
              #### Gaussian Noise (GN)
              ##### Gaussian Noise on the augmented dataset and application of WR algorithm
              
              N = 100000
              pert = 0.5 
              
              for (tir in seq(round(N/nrow(ech_add)))){
                ech_GN=ech_add
                for (j in (1:ncol(ech_add))) {
                  ech_GN[, j] = ech_add[, j] + rnorm(nrow(ech_add), 0, sd(ech0[,colnames(ech0)[j]]) * pert)
                }
                if (tir==1){
                  ech_GN_SC = ech_GN   
                }else{
                  ech_GN_SC = rbind(ech_GN_SC,ech_GN)
                }
              }
              
              # Drawing weights
              X = ech_GN_SC$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_GN_SC$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_GN_SC)),1000,replace=T,prob = q)
              ech_GN_SC = ech_GN_SC[ind,filtre_var]
              
              
              
              ## Gaussian Noise on the augmented dataset, by cluster and application of WR algorithm
              N = 100000
              DMC = densityMclust(ech0, plot=F,G=6)
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              cl = max(ech_MC$cluster)
              
              for (tir in seq(round(N/nrow(ech_add)))){
                for (i in (1:cl)){
                  temp = ech_MC[ech_MC$cluster == i,]
                  ech_add_temp = merge(temp, ech_add, on ="X1")
                  N = nrow(ech_add_temp)
                  ech_GN=ech_add_temp
                  for (j in (1:ncol(ech_add_temp))) {
                    ech_GN[, j] = ech_add_temp[, j] + 
                      rnorm(nrow(ech_add_temp), 0, sd(temp[,colnames(temp)[j]]) * pert)
                  }
                  if (tir==1 & i==1){
                    GN_GMM = ech_GN   
                  }else{
                    GN_GMM = rbind(GN_GMM,ech_GN)
                  }
                }
              }
              
              # Drawing weights
              X = GN_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=GN_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(GN_GMM)),1000,replace=T,prob = q)
              GN_GMM = GN_GMM[ind,filtre_var]
              
              
              #### Multivariate kernel density estimator (ROSE & KDE)
              ##### Estimator inspired by ROSE
              
              hmult=1
              n = nrow(ech0)
              q = ncol(ech0)
              n.new <- nrow(ech_add)
              cons.kernel <- (4/((q+2)*n))^(1/(q+4))
              if(q!=1){
                H <- hmult*cons.kernel*diag(apply(ech0, 2, sd), q)
              }else {
                H <- hmult*cons.kernel*sd(ech0)}
              Xnew.num <- matrix(rnorm(n.new*q), n.new, q)%*%H
              ech_ROSE_SC =data.frame(Xnew.num + ech_add)
              
              # Drawing weights
              X = ech_ROSE_SC$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_ROSE_SC$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_ROSE_SC)),1000,replace=T,prob = q)
              ech_ROSE_SC = ech_ROSE_SC[ind,filtre_var]
              
              # Application by cluster
              ech_MC = cbind(ech0[,c("X1","Y")],"cluster" = DMC$classification)
              ech_add_MC = merge(ech_MC, ech_add, on = filtre_var)
              cl = max(ech_MC$cluster) 
              N = 100000
              
              for (tir in seq(round(N/nrow(ech_add)))){
                for (i in (1:cl)){
                  temp = ech_MC[ech_MC$cluster == i,]
                  temp_add = ech_add_MC[ech_add_MC$cluster == i,]
                  n = nrow(temp)
                  q = ncol(temp)
                  n.new <- nrow(temp_add)
                  cons.kernel <- (4/((q+2)*n))^(1/(q+4))
                  if(q!=1){
                    H <- hmult*cons.kernel*diag(apply(temp, 2, sd), q)
                  }else {
                    H <- hmult*cons.kernel*sd(temp)}
                  Xnew.num <- matrix(rnorm(n.new*q), n.new, q)%*%H
                  ech_ROSE =data.frame(Xnew.num + temp_add)
                  if (tir == 1 & i == 1) {
                    ROSE_GMM = ech_ROSE
                  } else {ROSE_GMM = rbind(ROSE_GMM, ech_ROSE)}
                }
              }
              
              
              # Drawing weights
              X = ROSE_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ROSE_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ROSE_GMM)),1000,replace=T,prob = q)
              ROSE_GMM = ROSE_GMM[ind,filtre_var]
              
              
              
              ### smoothedbootstrap
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              kde_boot = data.frame(rmvg(100000, ech0, weights =  q))
              
              # Drawing weights
              X = kde_boot$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=kde_boot$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(kde_boot)),1000,replace=T,prob = q)
              kde_boot = kde_boot[ind,filtre_var]
              
              # Application post-clustering : 
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              N = 100000
              ech_MC = cbind(ech0[,c("X1","Y")],"cluster" = DMC$classification)
              ech_add_MC = merge(ech_MC, ech_add, on = filtre_var)
              cl = max(ech_MC$cluster)
              try(
                for (tir in seq(round(N/nrow(ech_add)))){
                  for (i in (1:cl)){
                    temp = data.frame(rmvg(sum(ech_add_MC$cluster == i), ech_MC[ech_MC$cluster == i,filtre_var], weights =  q[ech_MC$cluster==i]))
                    if (tir == 1 & i ==1) {
                      kde_boot_GMM = temp
                    } else {kde_boot_GMM = rbind(kde_boot_GMM, temp)}
                  }
                }
              )  
              
              
              # Drawing weights
              X = kde_boot_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=kde_boot_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(kde_boot_GMM)),1000,replace=T,prob = q)
              kde_boot_GMM = kde_boot_GMM[ind,filtre_var]
              
              
              
              ### Mixture model approach (latent structure model)
              #### Gaussian Mixture Model (GMM)
              
              
              DMC = densityMclust(ech0, plot=F,G=6)
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              
              DS_GMM = as.data.frame(sim(modelName=DMC$modelName,parameters=DMC$parameters,n=100000))
              colnames(DS_GMM) = c("cluster",colnames(ech0))
              DS_GMM0 = DS_GMM
              # Drawing weights
              X = DS_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=DS_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(DS_GMM)),1000,replace=T,prob = q)
              DS_GMM = DS_GMM[ind,filtre_var]
              
              
              #### Factor analysis (FA)
              
              ##### clustering via GMM
              DMC = densityMclust(ech0, plot=F,G=6)
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              ech = ech_MC
              
              if ("cluster" %in% colnames(ech)) {
                cl = max(ech$cluster)
                p = length(colnames(ech))-1
              }else {
                cl = 1
                p = length(colnames(ech))
              }
              phi = array(rep(0,cl*p),dim=c(1,p,cl))
              W = array(rep(0,cl*p*(p-1)),dim=c(p-1,p,cl))
              mu = array(rep(0,cl*p),dim=c(1,p,cl))
              for (i in (1:cl)){
                if ("cluster" %in% colnames(ech)) {
                  temp = ech[ech$cluster == i,]
                } else {temp = ech}
                temp = temp[,c("X1","Y")]
                p = length(colnames(temp))
                p_py = r_to_py(p,convert=TRUE)
                py_run_string("from sklearn.decomposition import FactorAnalysis")
                ech_py = r_to_py(temp,convert=TRUE)
                py_run_string("p=r.ech_py.shape[1]")
                py_run_string("transformer = FactorAnalysis(n_components=p-1, random_state=0)")
                py_run_string("X_transformed = transformer.fit_transform(r.ech_py)")
                py_run_string("noise_var = transformer.noise_variance_")
                py_run_string("mu = transformer.mean_")
                py_run_string("comp = transformer.components_")
                phi[,,i] = py$noise_var
                W[,,i] = py$comp
                mu[,,i] = py$mu
              }
              # Data generation
              for (i in (1:cl)){
                n_ds = sum(ech$cluster == i)*1000
                Z = mvrnorm(n_ds,rep(0,p-1),diag(rep(1,p-1)))
                temp = Z%*%W[,,i]  +  t(matrix(rep(mu[,,i],n_ds),p,n_ds)) + mvrnorm(n_ds,rep(0,p),diag(phi[,,i]))
                temp = as.data.frame(cbind(temp,cluster = i))
                if (i == 1) {
                  DS_FA = temp
                }else {DS_FA = rbind(DS_FA,temp)}
              }
              DS_FA_GMM = as.data.frame(DS_FA)
              colnames(DS_FA_GMM) = c(colnames(ech0),"cluster")
              
              # Drawing weights
              X = DS_FA_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=DS_FA_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(DS_FA_GMM)),1000,replace=T,prob = q)
              DS_FA_GMM = DS_FA_GMM[ind,filtre_var]
              
              
              ### Copula
              py_run_string("from sdv.tabular import GaussianCopula")
              ech_py = r_to_py(ech0,convert=TRUE)
              py_run_string("copule = GaussianCopula()")
              py_run_string("copule.fit(r.ech_py)")
              py_run_string("ech_copule_py = copule.sample(100000)")
              ech_copule = py$ech_copule_py
              
              # Drawing weights
              X = ech_copule$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_copule$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_copule)),1000,replace=T,prob = q)
              ech_copule = ech_copule[ind,filtre_var]
              
              
              ### Conditional Generative Adversarial Net (GAN)
              # Application by clustering 
              ech_MC = cbind(ech0[,c("X1","Y")],"cluster" = DMC$classification)
              py_run_string("from ctgan import CTGANSynthesizer")
              ech_MC_py = r_to_py(ech_MC,convert=TRUE)
              py_run_string("discrete_columns = ['cluster']")
              py_run_string("ctganData = CTGANSynthesizer(epochs=3000)")
              py_run_string("ctganData.fit(r.ech_MC_py,discrete_columns)")
              py_run_string("ech_ctgan_py = ctganData.sample(50000)")
              ech_ctganSynth_GMM = py$ech_ctgan_py
              
              # Drawing weights
              X = ech_ctganSynth_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_ctganSynth_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_ctganSynth_GMM)),1000,replace=T,prob = q)
              ech_ctganSynth_GMM = ech_ctganSynth_GMM[ind,filtre_var]
              
              
              ### k-NN Interpolation
              #### interpolation inspired by SMOTE (SMOTE)
              
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              ech = ech0
              k=3
              ech$id=seq(nrow(ech))
              
              n_pop = 10000 
              for (i in (1:n_pop)){
                id = sample(nrow(ech0),1,prob = q)
                knn_id = smotefamily::knearest(ech,ech[id,], n_clust=k)
                kppv = knn_id[sample(seq(k),1)]
                lambda = runif(1)
                new_obs = ech[id,] + lambda * (ech[kppv,]-ech[id,])
                rownames(new_obs)=i
                if (i==1){
                  ech_smote = new_obs
                }else{
                  ech_smote = rbind(ech_smote,new_obs)
                } 
              }
              ech_smote$id = NULL
              
              # Drawing weights
              X = ech_smote$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_smote$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_smote)),1000,replace=T,prob = q)
              ech_smote = ech_smote[ind,filtre_var]
              
              
              #### Application post-clustering
              
              # Drawing weights
              X = ech0$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech0$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              
              # Application by cluster
              ech = ech0
              ech_MC = cbind(ech0,"cluster" = DMC$classification)
              cl = max(ech_MC$cluster)
              n_pop = 10000 
              for (i in (1:n_pop)){
                id = sample(nrow(ech0),1,prob = q)
                temp = ech_MC[ech_MC$cluster == ech_MC$cluster[id],]
                knn_id = smotefamily::knearest(temp,ech_MC[id,], n_clust=k)
                kppv = knn_id[sample(seq(k),1)]
                lambda = runif(1)
                new_obs = ech_MC[id,] + lambda * (temp[kppv,]-ech_MC[id,])
                rownames(new_obs)=i
                if (i==1){
                  ech_smote_GMM = new_obs
                }else{
                  ech_smote_GMM = rbind(ech_smote_GMM,new_obs)
                } 
              }
              ech_smote_GMM$id = NULL
              
              
              # Drawing weights
              X = ech_smote_GMM$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_smote_GMM$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_smote_GMM)),1000,replace=T,prob = q)
              ech_smote_GMM = ech_smote_GMM[ind,filtre_var]
              
              
              
              ### Random Forest (RF)
              
              ech_RF2 = syn(ech_MC[,c("X1","Y","cluster")],method="rf", visit.sequence = c("cluster","X1","Y"),k=10000)
              ech_RF2 = ech_RF2$syn
              
              # Drawing weights
              X = ech_RF2$X1
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=ech_RF2$X1)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              # Drawing
              ind = sample(seq(1, nrow(ech_RF2)),1000,replace=T,prob = q)
              ech_RF2 = ech_RF2[ind,filtre_var]
              
              
              
                  
              ################################################## Performance evaluation
              
              
              ## Prediction
              
              ### Generalized Additive Model Learning
               
              l = 1
              
              #### Balanced sample
              dat = ech_rep
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### Imbalanced sample
              dat = ech0
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
               
              #### Weighted Resampling (WR) sample
              dat = ech_add
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GN-WR sample
              dat = ech_GN_SC
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GN_GMM-WR sample
              dat = GN_GMM
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### ROSE-WR sample
              dat = ech_ROSE_SC
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### ROSE_GMM-WR sample
              dat = ROSE_GMM
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### KDE-WR sample
              dat = kde_boot
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### KDE_GMM-WR sample
              dat = kde_boot_GMM
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GMM-WR sample
              dat = DS_GMM
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### FA_GMM-WR sample
              dat = DS_FA_GMM
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### Copula-WR sample
              dat = ech_copule
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GAN_GMM-WR sample
              dat = ech_ctganSynth_GMM
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### SMOTE-WR sample
              dat = ech_smote
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
          
              
              #### SMOTE_GMM-WR sample
              dat = ech_smote_GMM
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### RF-WR sample
              dat = ech_RF2
              # Predictions
              reg = gam(Y ~  s(X1) , data = dat)
              y_pred = predict(reg,test)
              res = summary(reg)
              # Indicators
              GAM_devExp[nb,l] = res$dev.expl 
              GAM_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              GAM_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              
              
              
              
              ### Random Forest Learning
              
              l = 1
              
              #### Balanced sample
              dat = ech_rep
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### Imbalanced sample
              dat = ech0
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### Weighted Resampling (WR) sample
              dat = ech_add
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GN-WR sample
              dat = ech_GN_SC
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GN_GMM-WR sample
              dat = GN_GMM
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### ROSE-WR sample
              dat = ech_ROSE_SC
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### ROSE_GMM-WR sample
              dat = ROSE_GMM
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### KDE-WR sample
              dat = kde_boot
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### KDE_GMM-WR sample
              dat = kde_boot_GMM
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GMM-WR sample
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### FA_GMM-WR sample
              dat = DS_FA_GMM
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### Copula-WR sample
              dat = ech_copule
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### GAN_GMM-WR sample
              dat = ech_ctganSynth_GMM
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### SMOTE-WR sample
              dat = ech_smote
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### SMOTE_GMM-WR sample
              dat = ech_smote_GMM
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              #### RF-WR sample
              dat = ech_RF2
              # Predictions
              X = dat$X
              pe = density(X,n=length(X))
              pe = approx(pe$x,pe$y,xout=dat$X)$y
              pt = Pt(X)
              w = pt / pe
              q = w / sum(w)
              reg = randomForest( Y ~ ., data = dat, weights = q,nodesize=2)
              y_pred = predict(reg,test)
              # Indicators
              RF_rsq[nb,l] = mean(reg$rsq)
              RF_rmse[nb,l] = rmse(y_test,as.numeric(y_pred))
              RF_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2
              l = l + 1
              
              
              
              
              
              
              ### Multivariate Adaptative Regression Spline Learning
              
              l = 1
              
              #### Balanced sample
              dat = ech_rep
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### Imbalanced sample
              dat = ech0
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### Weighted Resampling (WR) sample
              dat = ech_add
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### GN-WR sample
              dat = ech_GN_SC
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### GN_GMM-WR sample
              dat = GN_GMM
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### ROSE-WR sample
              dat = ech_ROSE_SC
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### ROSE_GMM-WR sample
              dat = ROSE_GMM
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### KDE-WR sample
              dat = kde_boot
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1 
              
              
              #### KDE_GMM-WR sample
              dat = kde_boot_GMM
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1 
              
              
              #### GMM-WR sample
              dat = DS_GMM
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1 
              
              
              #### FA_GMM-WR sample
              dat = DS_FA_GMM
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### Copula-WR sample
              dat = ech_copule
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1 
              
              
              #### GAN_GMM-WR sample
              dat = ech_ctganSynth_GMM
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1 
              
              
              #### SMOTE-WR sample
              dat = ech_smote
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### SMOTE_GMM-WR sample
              dat = ech_smote_GMM
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1
              
              
              #### RF-WR sample
              dat = ech_RF2
              # Predictions
              reg = mars(dat$X1 ,dat$Y)
              y_pred = predict(reg,test)
              # Indicators
              MARS_rmse[nb,l] = rmse(y_test,as.numeric(y_pred)) 
              MARS_R2[nb,l] = cor(y_test,as.numeric(y_pred))^2 
              l = l + 1

    }
}


beep(8)
getwd()
setwd("C:/Users/user/OneDrive/Perso/Thèse/Travaux/Exogenous Sampling/Illustration/Sorties Boucle DAUR")



GAM_devExp  = as.data.frame(GAM_devExp)
GAM_rmse    = as.data.frame(GAM_rmse)
GAM_R2  = as.data.frame(GAM_R2)
RF_rsq = as.data.frame(RF_rsq)
RF_rmse = as.data.frame(RF_rmse)
RF_R2 = as.data.frame(RF_R2)
MARS_rmse = as.data.frame(MARS_rmse)
MARS_R2 = as.data.frame(MARS_R2)

colnames(GAM_devExp) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                          "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")
colnames(GAM_rmse) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                         "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")
colnames(GAM_R2) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                         "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")
colnames(RF_rsq) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                         "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")
colnames(RF_rmse) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                         "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")
colnames(RF_R2) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                         "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")
colnames(MARS_rmse) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                         "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")
colnames(MARS_R2) = c("ech_rep","ech0","ech_add","ech_GN_SC","GN_GMM","ech_ROSE_SC","ROSE_GMM","kde_boot",
                         "kde_boot_GMM","DS_GMM","DS_FA_GMM","ech_copule","ech_ctganSynth_GMM","ech_smote","ech_smote_GMM","ech_RF2")

#############################################################################################################
#####################             DATAVIZ


library("plotly")
library("ggplot2")
setwd("C:/Users/user/OneDrive/Perso/Thèse/Travaux/Exogenous Sampling/Illustration")

res = GAM_rmse
res$X = NULL
colnames(res) = c("Db (bal)","Di (imb)","WR","GN","GN-GMM","ROSE","ROSE-GMM","KDE","KDE-GMM",
                  "GMM","FA_GMM","Copule","GAN","SMOTE","SMOTE-GMM","RF") 
for (i in seq(ncol(res))){
  temp = data.frame(RMSE = res[,colnames(res)[i]],dataset = colnames(res)[i])
  if (i ==1){
    res_bp = temp
  } else {
    res_bp = rbind(res_bp,temp)
  }
}

res_bp$dataset <- factor(res_bp$dataset, levels = colnames(res))
res_bp = res_bp[res_bp$RMSE<0.5,]

fig <- plot_ly(res_bp, y = ~ RMSE, color = ~ dataset, type = "box", main = "MARS_rmse")  
fig 

#fig %>% layout(legend = list(orientation = "v",xanchor = "center", y=0.3))

# Other visualisations 
ggplot(res_bp, aes(x=dataset, y=RMSE, fill = dataset, x)) + geom_boxplot() + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank())

ggplot(res_bp, aes(x=dataset, y=RMSE, fill = dataset, x)) + geom_boxplot() + 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ theme(legend.position="none")



