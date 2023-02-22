Data Augmentation for Imbalanced Regression - Illustration
================
Samuel STOCKSIEKER
25/01/2023

This code refers to the following paper : http://arxiv.org/abs/2302.09288

*Import library*

<style>
body {
text-align: justify}
</style>

check packages version

``` r
sessionInfo()
```

    ## R version 4.2.2 (2022-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 22621)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
    ## [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
    ## [5] LC_TIME=French_France.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] latex2exp_0.9.6      plotly_4.10.1        seewave_2.2.0       
    ##  [4] statip_0.2.3         beepr_1.3            pdp_0.8.1           
    ##  [7] mda_0.5-3            class_7.3-20         earth_5.3.1         
    ## [10] plotmo_3.6.2         TeachingDemos_2.12   plotrix_3.8-2       
    ## [13] Formula_1.2-4        Metrics_0.1.4        mgcv_1.8-41         
    ## [16] nlme_3.1-160         synthpop_1.8-0       smotefamily_1.3.1   
    ## [19] kernelboot_0.1.9     mclust_6.0.0         ks_1.14.0           
    ## [22] reticulate_1.27      MASS_7.3-58.1        UBL_0.0.7           
    ## [25] randomForest_4.7-1.1 automap_1.0-16       sp_1.5-1            
    ## [28] gstat_2.1-0          MBA_0.1-0            ggplot2_3.4.0       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] TH.data_1.1-1       colorspace_2.0-3    modeltools_0.2-23  
    ##  [4] mipfp_3.2.1         rmutil_1.1.10       clue_0.3-63        
    ##  [7] rstudioapi_0.14     proxy_0.4-27        listenv_0.9.0      
    ## [10] audio_0.1-10        fansi_1.0.3         mvtnorm_1.1-3      
    ## [13] coin_1.4-2          ranger_0.14.1       codetools_0.2-18   
    ## [16] splines_4.2.2       libcoin_1.0-9       knitr_1.41         
    ## [19] jsonlite_1.8.4      cluster_2.1.4       png_0.1-8          
    ## [22] httr_1.4.4          compiler_4.2.2      lazyeval_0.2.2     
    ## [25] Matrix_1.5-1        fastmap_1.1.0       strucchange_1.5-3  
    ## [28] cli_3.5.0           htmltools_0.5.4     tools_4.2.2        
    ## [31] gtable_0.3.1        glue_1.6.2          broman_0.80        
    ## [34] dplyr_1.0.10        Rcpp_1.0.9          vctrs_0.5.1        
    ## [37] iterators_1.0.14    xfun_0.36           stringr_1.5.0      
    ## [40] globals_0.16.2      proto_1.0.0         lifecycle_1.0.3    
    ## [43] cmm_0.12            future_1.30.0       polspline_1.1.22   
    ## [46] zoo_1.8-11          scales_1.2.1        parallel_4.2.2     
    ## [49] sandwich_3.0-2      yaml_2.3.6          rpart_4.1.19       
    ## [52] reshape_0.8.9       stringi_1.7.8       maptools_1.1-6     
    ## [55] foreach_1.5.2       e1071_1.7-12        truncnorm_1.0-8    
    ## [58] tuneR_1.4.2         intervals_0.15.2    rlang_1.0.6        
    ## [61] pkgconfig_2.0.3     matrixStats_0.63.0  Rsolnp_1.16        
    ## [64] pracma_2.4.2        evaluate_0.19       lattice_0.20-45    
    ## [67] purrr_1.0.0         htmlwidgets_1.6.1   tidyselect_1.2.0   
    ## [70] parallelly_1.34.0   plyr_1.8.8          magrittr_2.0.3     
    ## [73] R6_2.5.1            generics_0.1.3      multcomp_1.4-20    
    ## [76] pillar_1.8.1        foreign_0.8-83      withr_2.5.0        
    ## [79] xts_0.12.2          survival_3.4-0      nnet_7.3-18        
    ## [82] tibble_3.1.8        future.apply_1.10.0 spacetime_1.2-8    
    ## [85] KernSmooth_2.23-20  utf8_1.2.2          party_1.3-11       
    ## [88] rmarkdown_2.19      grid_4.2.2          data.table_1.14.6  
    ## [91] FNN_1.1.3.1         digest_0.6.31       classInt_0.4-8     
    ## [94] tidyr_1.2.1         numDeriv_2016.8-1.1 signal_0.7-7       
    ## [97] stats4_4.2.2        munsell_0.5.0       viridisLite_0.4.1

R version 4.2.0 (2022-04-22 ucrt) Platform: x86_64-w64-mingw32/x64
(64-bit) Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale: \[1\] LC_COLLATE=French_France.utf8 LC_CTYPE=French_France.utf8
LC_MONETARY=French_France.utf8 \[4\] LC_NUMERIC=C
LC_TIME=French_France.utf8

attached base packages: \[1\] stats graphics grDevices utils datasets
methods base

other attached packages: \[1\] seewave_2.2.0 statip_0.2.3 beepr_1.3
pdp_0.8.1 mda_0.5-3  
\[6\] class_7.3-20 earth_5.3.1 plotmo_3.6.1 TeachingDemos_2.12
plotrix_3.8-2  
\[11\] Formula_1.2-4 Metrics_0.1.4 mgcv_1.8-40 nlme_3.1-157
synthpop_1.7-0  
\[16\] smotefamily_1.3.1 kernelboot_0.1.7 mclust_5.4.9 ks_1.13.5
reticulate_1.25  
\[21\] MASS_7.3-57 UBL_0.0.7 randomForest_4.7-1 automap_1.0-16
sp_1.4-7  
\[26\] gstat_2.0-9 MBA_0.0-9 ggplot2_3.4.0

loaded via a namespace (and not attached): \[1\] TH.data_1.1-1
colorspace_2.0-3 ellipsis_0.3.2 modeltools_0.2-23 mipfp_3.2.1  
\[6\] rprojroot_2.0.3 clue_0.3-61 rstudioapi_0.14 proxy_0.4-26
listenv_0.8.0  
\[11\] audio_0.1-10 fansi_1.0.3 mvtnorm_1.1-3 coin_1.4-2 ranger_0.13.1  
\[16\] codetools_0.2-18 splines_4.2.0 libcoin_1.0-9 knitr_1.39
jsonlite_1.8.0  
\[21\] cluster_2.1.3 png_0.1-7 compiler_4.2.0 Matrix_1.5-3
strucchange_1.5-2  
\[26\] cli_3.3.0 tools_4.2.0 gtable_0.3.0 glue_1.6.2 dplyr_1.0.9  
\[31\] rappdirs_0.3.3 Rcpp_1.0.8.3 vctrs_0.5.1 iterators_1.0.14
xfun_0.31  
\[36\] stringr_1.4.0 globals_0.15.0 proto_1.0.0 lifecycle_1.0.3
cmm_0.12  
\[41\] future_1.25.0 polspline_1.1.20 zoo_1.8-10 scales_1.2.0
parallel_4.2.0  
\[46\] sandwich_3.0-1 rpart_4.1.16 reshape_0.8.9 stringi_1.7.6
maptools_1.1-4  
\[51\] foreach_1.5.2 e1071_1.7-9 truncnorm_1.0-8 tuneR_1.4.0
intervals_0.15.2  
\[56\] rlang_1.0.6 pkgconfig_2.0.3 matrixStats_0.62.0 Rsolnp_1.16
pracma_2.3.8  
\[61\] lattice_0.20-45 purrr_0.3.4 tidyselect_1.1.2 here_1.0.1
parallelly_1.31.1  
\[66\] plyr_1.8.7 magrittr_2.0.3 R6_2.5.1 generics_0.1.2
multcomp_1.4-19  
\[71\] pillar_1.7.0 foreign_0.8-82 withr_2.5.0 xts_0.12.1
survival_3.3-1  
\[76\] nnet_7.3-17 tibble_3.1.7 future.apply_1.9.0 spacetime_1.2-6
crayon_1.5.1  
\[81\] KernSmooth_2.23-20 utf8_1.2.2 party_1.3-10 grid_4.2.0 FNN_1.1.3  
\[86\] digest_0.6.29 classInt_0.4-3 numDeriv_2016.8-1.1 signal_0.7-7
stats4_4.2.0  
\[91\] munsell_0.5.0

# Dataset simulation

## Population

We consider a bi-dimensional initial population
$\mathcal{D}^p = (X^p,Y^p)$, of size $n^p = 10,000$ such as:  
$X \sim F_0 = \mathcal{B}\text{eta}(5,5)$
$Y \sim \mathcal{N}(\sin(7X -0.5)+10,0.1)$ where ${\cal B}$ denotes the
Beta distribution and ${\cal N}$ denotes the Gaussian distribution.

``` r
if (rerun == T){
  n_pop = 10000 # size of the population
  alpha_pop = 5 # parameter of X distribution in the population
  beta_pop = 5 # parameter of X distribution in the population
  X_pop = rbeta (n_pop,alpha_pop, beta_pop) # simulation of X in the population
  hist(X_pop,breaks = 100)
  pop = data.frame(X=X_pop)
}
```

simulation of $Y$ as a function of $X$ according to the non-linear
relationship :

``` r
if (rerun == T){
  for (i in (1:nrow(pop))){
    pop$Y[i] = rnorm(1,sin(pop$X[i]*7 - 0.5) +10 ,0.1)
  }
}
plot(pop$X,pop$Y)
```

![](DAIR_Illustration_files/figure-gfm/genPop_Y-1.png)<!-- -->

## Sampling

The test, balanced, and imbalanced samples are all of size $n=1,000$.

### Test sample

From this population, we uniformly draw a test sample $\mathcal{D}^t$.

``` r
if (rerun == T){
  base=pop
  n_ech = 1000
  ind_test = sample(nrow(base),n_ech)
  test = base[ind_test,]
  base = base[-ind_test,]
  y_test = test$Y
  test$Y = NULL
}
```

### Balanced sample

From the remaining population, $\mathcal{D}^p$ \\ $\mathcal{D}^t$, we
uniformly draw a balanced sample $\mathcal{D}^b$, supposed to be
representative of the population.

``` r
if (rerun == T){
  ech_rep = base[sample(nrow(base),n_ech),]
}
```

### Imbalanced sample

Finally, we draw an imbalanced sample $\mathcal{D}^i$ from this
remaining population. The draw weights to construct this imbalanced
sample are defined by the distribution $F = \mathcal{B}\text{eta}(9,9)$
in order to get less observations on the sides.

``` r
plot(dbeta(x=seq(0,1,0.01), 9,9 ), col="darkred", type = 'l')
lines(dbeta(x=seq(0,1,0.01),alpha_pop ,beta_pop ), col="green3")
```

![](DAIR_Illustration_files/figure-gfm/genDist_Imbalanced-1.png)<!-- -->

``` r
if (rerun == T){
  ech = base[sample(nrow(base),n_ech,prob = dbeta(base$X,9,9)),]
  X_ech = ech$X
  ech0 = ech
}
```

Graphical analysis of the imbalanced sample

``` r
df_train=data.frame(ech$X,rep("imb",length(ech$X)))
df_test=data.frame(pop$X,rep("pop",length(test$X)))
colnames(df_train)=c("X","dataset")
colnames(df_test)=c("X","dataset")
df = rbind(df_train,df_test)

ggplot(df, aes(X, color=dataset, fill=dataset)) + 
  geom_histogram(alpha = 0.5,aes(y = ..density..),bins=100,position="identity")+
  geom_density(alpha=0.5)+
  scale_color_manual(values = c("pop" = "azure4", "imb" = "darkred"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^p$)"))))) + 
  scale_fill_manual(values = c("pop" = "azure4", "imb" = "darkred"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^p$)"))))) +
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0))
```

![](DAIR_Illustration_files/figure-gfm/Graph_Imbalanced-1.png)<!-- -->

``` r
ggsave("Sorties_illustration/comp_X_Ech0-vs-Pop-Dens.png",width=7.29, height=4.5)

ggplot(df, aes(X, color=dataset, fill=dataset)) + 
  geom_histogram(alpha = 0.5,aes(y = ..density..),bins=100,position="identity")+
  scale_color_manual(values = c("pop" = "azure4", "imb" = "darkred"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^p$)"))))) + 
  scale_fill_manual(values = c("pop" = "azure4", "imb" = "darkred"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^p$)"))))) +
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0))
```

![](DAIR_Illustration_files/figure-gfm/Graph_Imbalanced-2.png)<!-- -->

``` r
ggsave("Sorties_illustration/comp_X_Ech0-vs-Pop.png",width=7.29, height=4.5)

ggplot() + 
  geom_point(aes(x=pop$X, y=pop$Y, colour="pop")) + 
  geom_point(aes(x=ech0$X, y=ech0$Y, colour="imb"))+
  scale_color_manual(name = "dataset",values = c("pop" = "azure4", "imb" = "darkred"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^p$)"))))) + 
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0)) +
  xlab("X") + ylab("Y")
```

![](DAIR_Illustration_files/figure-gfm/Graph_Imbalanced-3.png)<!-- -->

``` r
ggsave("Sorties_illustration/comp_Y_Ech0-vs-Pop.png",width=7.29, height=4.5)


df_train=data.frame(ech$X,rep("imb",length(ech$X)))
df_test=data.frame(test$X,rep("test",length(test$X)))
colnames(df_train)=c("X","dataset")
colnames(df_test)=c("X","dataset")
df = rbind(df_train,df_test)

ggplot(df, aes(X, color=dataset, fill=dataset)) + 
  geom_histogram(alpha = 0.5,aes(y = ..density..),bins=100,position="identity")+
  scale_color_manual(values = c("darkred","darkgreen"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^t$)"))))) + 
  scale_fill_manual(values = c("darkred","darkgreen"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^t$)"))))) +
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0))
```

![](DAIR_Illustration_files/figure-gfm/Graph_Imbalanced-4.png)<!-- -->

Imbalanced analysis for the definition of Imbalanced Covariates
Regression in the paper.

``` r
# threshold 
a = 0.3
b = 0.7

# alpha 
(1-pbeta(b,5,5))/(1-pbeta(b,9,9)) - 1
```

    ## [1] 1.453232

``` r
# beta
pbeta(a,5,5)
```

    ## [1] 0.09880866

``` r
1-pbeta(b,5,5)
```

    ## [1] 0.09880866

we face an imbalanced regression: a $(\alpha, \beta)$-imbalanced problem
with, for example,
$\lvert \frac{\widehat{\mathbb{P}}(\boldsymbol{X}\in\chi)}{\mathbb{P}_0(\boldsymbol{X}\in\chi)} - 1 \rvert > \alpha$
when $\chi:= [0,0.3]$ or $\chi:= [0.7,1]$ and
($\alpha \leq 0.59, \beta \leq 0.09$). Here we are in the situation
where $F$ and $F_0$ have the same support and the imbalanced problem
will be less with $n$ large.

The impacts of the imbalanced sample on the prediction are visible on
the part “Performance Evaluation”

# DA-WR approach

To handle the imbalance, we want to draw a $n^*$-sample,
$\{(\boldsymbol{X}^*_i,Y^*_i)_{i=1,\cdots,n^*}\}$ from the initial
$n$-sample $\{(\boldsymbol{X}_i,Y_i)_{i=1,\cdots,n}\}$, such that the
cdf $F^*$ of $\boldsymbol{X^*}$ converges to the target $F_0$ with
associated probability $\mathbb{P}_0$.

We wish to redress the initial distribution of $X$ (in black) as follows
(in red):

``` r
ggplot(ech, aes(X)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..,color= "imb"),position = 'identity',bins=100, fill="darkred")+
  geom_density(alpha=0.5, fill = "darkred")+
  geom_line(aes(y=dbeta(ech$X,alpha_pop,beta_pop),colour="f0"), lwd=2,linetype = 1)+
  scale_color_manual(name = "legend",values = c("f0" = "darkgreen", "imb" = "darkred"),labels = c(unname(TeX(c(r"($\textit{f}_0$)"))), unname(TeX(c(r"($\textit{D}^i$)"))))) +
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0))
```

![](DAIR_Illustration_files/figure-gfm/histInit_cible-1.png)<!-- -->

``` r
ggsave("Sorties_illustration/Hist_X_Ech0-vs-Tgt.png",width=7.29, height=4.5)
```

## Weighted Resampling (WR) algorithm

Resampling weights definition

``` r
# Target dstribution : population
Pt = function(x){
  dbeta(x,alpha_pop,beta_pop)
}

X = ech$X
pe = density(X,n=length(X)) # emmpiric distribution  : kernel estimator
ech$pe = approx(pe$x,pe$y,xout=ech$X)$y
ech$pt = Pt(X)
ech$w = ech$pt / ech$pe
ech$q = ech$w / sum(ech$w)

ggplot(ech, aes(x=X)) + geom_point(aes(y=q), colour="darkred") + labs(y = "weight")
```

![](DAIR_Illustration_files/figure-gfm/WB-1.png)<!-- -->

``` r
ggsave("Sorties_illustration/weights-ech0.png",width=7.29, height=4.5)
```

Drawing

``` r
if (rerun == T){
  # Drawing
  ech_add = sample(seq(1, nrow(ech)),n_ech,replace=T,prob = ech$q)
  ech_add = ech[ech_add,]
  
  # Variable selection
  filtre_var = c("X","Y")
  ech_add = ech_add[,filtre_var]
  ech0 = ech0[,filtre_var]
}
```

Obtained distribution after resampling

``` r
ggplot(ech_add, aes(X)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..,color= "WR"),position = 'identity',bins=100, fill="darkred")+
  geom_density(alpha=0.5, fill = "darkred")+
  geom_line(aes(y=dbeta(ech_add$X,alpha_pop,beta_pop),colour="f0"), lwd=2,linetype = 1)+
  scale_color_manual(name = "legend",values = c("f0" = "darkgreen", "WR" = "darkred"),labels = c(unname(TeX(c(r"($\textit{f}_0$)"))), unname(TeX(c(r"($\textit{D}^*(WR)$)"))))) +
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0))
```

![](DAIR_Illustration_files/figure-gfm/histWB_cible-1.png)<!-- -->

``` r
ggsave("Sorties_illustration/Hist_X_Ech_add-vs-Tgt.png",width=7.29, height=4.5)
```

This last figure shows the new sample $\mathcal{D}^*$ which is closer to
target distribution than the initial one. However, the values on the
sides, quite rare, are naturally very drawn. We see there are still some
parts of the support without observation, especially on the right side.
This situation could lead to an overfitting phenomenon because the same
observations are replicated several times and could lead to an
over-fitting effect. To improve the weighted resampling, we propose to
combine it with a synthetic data generation.

Definition of a WR function to reuse it

``` r
WR = function(ech,X,N){
  # Drawing weights
  pe = density(X,n=length(X))
  pe = approx(pe$x,pe$y,xout=ech$X)$y
  pt = Pt(X)
  w = pt / pe
  q = w / sum(w)
  # Drawing
  ind = sample(seq(1, nrow(ech)),N,replace=T,prob = q)
  ech[ind,]
}
```

## Data Augmentation - Weighted Resampling algorithm

### Generation by random noise

#### Gaussian Noise (GN)

**Gaussian Noise on the augmented dataset and application of WR
algorithm**

``` r
if (rerun == T){
  ### Approach GN (cf fonction Gn.exsClassif : https://rdrr.io/github/paobranco/UBL/src/R/gaussNoiseClassif.R
  N = 100000
  pert = 0.3 
  
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
  
  # WR algorithm
  ech_GN_SC = WR(ech_GN_SC,ech_GN_SC$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = ech_GN_SC
name_synth = "ech_GN_SC"

graph = function (ech_synth,name_synth){
  # Covariate analysis
  print(ggplot(ech_synth, aes(X)) + 
    geom_histogram(alpha = 0.5, aes(y = ..density..,color= "new"),position = 'identity',bins=100, fill="deepskyblue4")+
    geom_density(alpha=0.5, fill = "deepskyblue4")+
    geom_line(aes(y=dbeta(ech_synth$X,alpha_pop,beta_pop),colour="f0"), lwd=2,linetype = 1)+
    scale_color_manual(name = "legend",values = c("f0" = "darkgreen", "new" = "deepskyblue4"),labels = c(unname(TeX(c(r"($\textit{f}_0$)"))), unname(TeX(c(r"($\textit{D}^*$)"))))) +
    theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0)))
  ggsave(paste0("Sorties_illustration/Hist_X_",name_synth,"-vs-Tgt.png"),width=7.29, height=4.5)
  
  df_add=data.frame(ech_add$X,rep("WR",length(ech$X)))
  df_synth=data.frame(ech_synth$X,rep("new",length(ech_synth$X)))
  colnames(df_add)=c("X","dataset")
  colnames(df_synth)=c("X","dataset")
  df = rbind(df_add,df_synth)
  print(ggplot(df, aes(X, color=dataset, fill=dataset)) + 
    geom_histogram(alpha = 0.5, aes(y = ..density..),position = 'identity',bins=100)+
      #geom_density(alpha=0.5)+
    scale_color_manual(values=c("WR" = "darkred", "new" = "deepskyblue4"),labels = c(unname(TeX(c(r"($\textit{D}^*(DA-WR)$)"))), unname(TeX(c(r"($\textit{D}^*(WR)$)")))))+
    scale_fill_manual(values=c("WR" = "darkred", "new" = "deepskyblue4"),labels = c(unname(TeX(c(r"($\textit{D}^*(DA-WR)$)"))), unname(TeX(c(r"($\textit{D}^*(WR)$)")))))+
    theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0)))
  ggsave(paste0("Sorties_illustration/Hist_X_",name_synth,"-vs-ech_add.png"),width=7.29, height=4.5)
  
  # Target variable analysis
  print(ggplot() + 
    geom_point(aes(x=ech_synth$X, y=ech_synth$Y, colour="new")) + 
    geom_point(aes(x=ech0$X, y=ech0$Y, colour="imb"))+
    scale_color_manual(name = "dataset",values = c("new" = "deepskyblue4", "imb" = "darkred"),labels = c(unname(TeX(c(r"($\textit{D}^i$)"))), unname(TeX(c(r"($\textit{D}^*$)"))))) + 
    theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0))+
  xlab("X") + ylab("Y"))
  ggsave(paste0("Sorties_illustration/comp_Y_",name_synth,"-vs-ech0.png"),width=7.29, height=4.5)

}
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/GN2-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GN2-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GN2-3.png)<!-- -->

**Gaussian Noise on the augmented dataset, by cluster and application of
WR algorithm**

``` r
if (rerun == T){
  N = 100000
  DMC = densityMclust(ech0, plot=F,G=1:6)
  ech_MC = cbind(ech0,"cluster" = DMC$classification)
  cl = max(ech_MC$cluster)
  for (tir in seq(round(N/nrow(ech_add)))){
    for (i in (1:cl)){
        temp = ech_MC[ech_MC$cluster == i,]
        ech_add_temp = merge(temp, ech_add, on ="X")
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
  
  # WR algorithm
  GN_GMM = WR(GN_GMM,GN_GMM$X,n_ech)
}
```

Graphical analysis of clustering

``` r
# Clustering analysis
ggplot(ech_MC, aes(x=X, y=Y, color=cluster)) + geom_point() + 
    theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=12,hjust=0))
```

![](DAIR_Illustration_files/figure-gfm/cluster-1.png)<!-- -->

``` r
ggsave(paste0("Sorties_illustration/clustering.png"),width=7.29, height=4.5)
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = GN_GMM
name_synth = "GN_GMM"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/GN-GMM2-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GN-GMM2-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GN-GMM2-3.png)<!-- -->

**Application of the Gaussian Noise for regression imbalanced, on the
augmented dataset and application of WR algorithm**

``` r
if (rerun == T){
  ech_GN_clust = GaussNoiseClassif(cluster ~ .,ech_MC, pert,C.perc = "balance")
}
hist(ech_GN_clust$X,breaks=100, col = "antiquewhite",freq=F,right = T)
points(ech_GN_clust$X,dbeta(ech_GN_clust$X,alpha_pop,beta_pop),col="red")
lines(density(ech_GN_clust$X),col="black",lwd=2)
```

![](DAIR_Illustration_files/figure-gfm/GN-classif-1.png)<!-- -->

``` r
hist(ech_GN_clust$X, col=rgb(0,0,1,1/4),breaks = 100,prob=T) 
hist(ech_add$X, col=rgb(1,0,0,1/4),breaks = 100,prob=T, add=T) 
hist(ech0$X,col=rgb(1,0,0,1/4),breaks = 100,prob=T, add=T)
```

![](DAIR_Illustration_files/figure-gfm/GN-classif-2.png)<!-- -->

``` r
plot(ech_GN_clust$X,ech_GN_clust$Y)
points(ech0$X,ech0$Y,col="red")
```

![](DAIR_Illustration_files/figure-gfm/GN-classif-3.png)<!-- -->

Not relevant

#### Multivariate kernel density estimator (ROSE & KDE)

*Estimator inspired by ROSE* source code :
<https://rdrr.io/cran/ROSE/man/ROSE-package.html>

``` r
if (rerun == T){
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
  
  # WR algorithm
  ech_ROSE_SC = WR(ech_ROSE_SC,ech_ROSE_SC$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = ech_ROSE_SC
name_synth = "ech_ROSE_SC"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/ROSE_AnalyseY-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/ROSE_AnalyseY-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/ROSE_AnalyseY-3.png)<!-- -->

*application by cluster*

``` r
if (rerun == T){
  ech_MC = cbind(ech0[,c("X","Y")],"cluster" = DMC$classification)
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
  
  # WR algorithm
  ROSE_GMM = WR(ROSE_GMM,ROSE_GMM$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = ROSE_GMM
name_synth = "ROSE_GMM"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/ROSE_GMM_res-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/ROSE_GMM_res-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/ROSE_GMM_res-3.png)<!-- -->

*Smoothed Bootstrap (KDE)* Same approach than ROSE but with another
bandwidth matrix

``` r
if (rerun == T){
  # Drawing weights
  X = ech0$X
  pe = density(X,n=length(X))
  pe = approx(pe$x,pe$y,xout=ech0$X)$y
  pt = Pt(X)
  w = pt / pe
  q = w / sum(w)
  
  kde_boot = data.frame(rmvg(100000, ech0, weights =  q))
  
  # WR algorithm
  kde_boot = WR(kde_boot,kde_boot$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = kde_boot
name_synth = "kde_boot"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/smoothedbootstrap%20result-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/smoothedbootstrap%20result-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/smoothedbootstrap%20result-3.png)<!-- -->

*application by cluster*

``` r
if (rerun == T){
  # Drawing weights
  X = ech0$X
  pe = density(X,n=length(X))
  pe = approx(pe$x,pe$y,xout=ech0$X)$y
  pt = Pt(X)
  w = pt / pe
  q = w / sum(w)
  
  N = 100000
  ech_MC = cbind(ech0[,c("X","Y")],"cluster" = DMC$classification)
  ech_add_MC = merge(ech_MC, ech_add, on = filtre_var)
  cl = max(ech_MC$cluster)
  for (tir in seq(round(N/nrow(ech_add)))){
    for (i in (1:cl)){
      temp = data.frame(rmvg(sum(ech_add_MC$cluster == i), ech_MC[ech_MC$cluster == i,filtre_var], weights =  q[ech_MC$cluster==i]))
      if (tir == 1 & i ==1) {
      kde_boot_GMM = temp
    } else {kde_boot_GMM = rbind(kde_boot_GMM, temp)}
    }
  }
  
  # WR algorithm
  kde_boot_GMM = WR(kde_boot_GMM,kde_boot_GMM$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = kde_boot_GMM
name_synth = "kde_boot_GMM"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/smoothedbootstrap3-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/smoothedbootstrap3-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/smoothedbootstrap3-3.png)<!-- -->

### Mixture model approach (latent structure model)

#### Gaussian Mixture Model (GMM)

``` r
if (rerun == T){
  DMC = densityMclust(ech0, plot=F,G=1:6)
  ech_MC = cbind(ech0,"cluster" = DMC$classification)
}
```

``` r
ggplot(ech_MC, aes(x=X, y=Y, color=cluster)) + geom_point()
```

![](DAIR_Illustration_files/figure-gfm/GMM2-1.png)<!-- -->

``` r
if (rerun == T){
  DS_GMM = as.data.frame(sim(modelName=DMC$modelName,parameters=DMC$parameters,n=100000))
  colnames(DS_GMM) = c("cluster",colnames(ech0))

  # WR algorithm
  GMM = WR(DS_GMM,DS_GMM$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
### Graphics
ech_synth = GMM
name_synth = "GMM"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/GMM_sim2-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GMM_sim2-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GMM_sim2-3.png)<!-- -->

#### Factor analysis (FA)

``` r
if (rerun == T){
  ech = ech0
  
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
    temp = temp[,c("X","Y")]
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
    if ("cluster" %in% colnames(ech)) {
      n_ds = sum(ech$cluster == i)*10
    }else{n_ds = nrow(ech)*10}
    Z = mvrnorm(n_ds,rep(0,p-1),diag(rep(1,p-1)))
    temp = Z%*%W[,,i]  +  t(matrix(rep(mu[,,i],n_ds),p,n_ds)) + mvrnorm(n_ds,rep(0,p),diag(phi[,,i]))
    temp = as.data.frame(cbind(temp,cluster = i))
    if (i == 1) {
      DS_FA = temp
    }else {DS_FA = rbind(DS_FA,temp)}
  }
  DS_FA = as.data.frame(DS_FA)
  colnames(DS_FA) = c(colnames(ech0),"cluster")
  
  # WR algorithm
  DS_FA = WR(DS_FA,DS_FA$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
plot(DS_FA$X,DS_FA$Y)
points(ech0$X,ech0$Y,col="red")
```

![](DAIR_Illustration_files/figure-gfm/FA_simple_res-1.png)<!-- -->

*application by cluster*

``` r
if (rerun == T){  
  DMC = densityMclust(ech0, plot=F,G=1:6)
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
    temp = temp[,c("X","Y")]
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
    n_ds = sum(ech$cluster == i)*100
    Z = mvrnorm(n_ds,rep(0,p-1),diag(rep(1,p-1)))
    temp = Z%*%W[,,i]  +  t(matrix(rep(mu[,,i],n_ds),p,n_ds)) + mvrnorm(n_ds,rep(0,p),diag(phi[,,i]))
    temp = as.data.frame(cbind(temp,cluster = i))
    if (i == 1) {
      DS_FA = temp
    }else {DS_FA = rbind(DS_FA,temp)}
  }
  DS_FA_GMM = as.data.frame(DS_FA)
  colnames(DS_FA_GMM) = c(colnames(ech0),"cluster")
  
  # WR algorithm
  FA_GMM = WR(DS_FA_GMM,DS_FA_GMM$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
### Graphics
ech_synth = FA_GMM
name_synth = "FA_GMM"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/FA_cluster_res-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/FA_cluster_res-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/FA_cluster_res-3.png)<!-- -->

### Copula

``` r
if (rerun == T){
  py_run_string("from sdv.tabular import GaussianCopula")
  ech0_1000 = ech0 
  ech0_py = r_to_py(ech0_1000,convert=TRUE)
  py_run_string("copule = GaussianCopula()")
  py_run_string("copule.fit(r.ech0_py)")
  py_run_string("ech_copule_py = copule.sample(100000)")
  ech_copule = py$ech_copule_py
  
  # WR algorithm
  ech_copule = WR(ech_copule,ech_copule$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
### Graphics
ech_synth = ech_copule
name_synth = "ech_copule"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/copula_res-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/copula_res-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/copula_res-3.png)<!-- -->

By definition, the copula simulations do not go beyond the min-max
values

### Conditional Generative Adversarial Net (GAN)

package CTGAN from SDV

*no conditional*

``` r
if (rerun == T){
  py_run_string("from sdv.tabular import CTGAN")
  ech0_py = r_to_py(ech0,convert=TRUE)
  py_run_string("ctgan = CTGAN(epochs=1000)")
  py_run_string("ctgan.fit(r.ech0_py)")
  py_run_string("ech_ctgan_py = ctgan.sample(100000)")
  ech_GAN = py$ech_ctgan_py
  
  # WR algorithm
  ech_GAN = WR(ech_GAN,ech_GAN$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
### Graphics
ech_synth = ech_GAN
name_synth = "ech_GAN"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/GAN_res-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GAN_res-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GAN_res-3.png)<!-- -->

*application by cluster*

``` r
if (rerun == T){
  ech_MC = cbind(ech0[,c("X","Y")],"cluster" = DMC$classification)
  py_run_string("from ctgan import CTGANSynthesizer")
  ech_MC_py = r_to_py(ech_MC,convert=TRUE)
  py_run_string("discrete_columns = ['cluster']")
  py_run_string("ctgan = CTGANSynthesizer(epochs=10000)")
  py_run_string("ctgan.fit(r.ech_MC_py,discrete_columns)")
  py_run_string("ech_ctgan_py = ctgan.sample(50000)")
  ech_ctganSynth_GMM = py$ech_ctgan_py

  # WR algorithm
  ech_ctganSynth_GMM = WR(ech_ctganSynth_GMM,ech_ctganSynth_GMM$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
### Graphics
ech_synth = ech_ctganSynth_GMM
name_synth = "ech_GAN_GMM"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/GAN_cond_res-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GAN_cond_res-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/GAN_cond_res-3.png)<!-- -->

### k-NN Interpolation

#### interpolation inspired by SMOTE (SMOTE)

``` r
if (rerun == T){
  # Drawing weights
  X = ech0$X
  pe = density(X,n=length(X))
  pe = approx(pe$x,pe$y,xout=ech0$X)$y
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
  
  # WR algorithm
  ech_smote = WR(ech_smote,ech_smote$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = ech_smote
name_synth = "ech_smote"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/Smote_graph-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/Smote_graph-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/Smote_graph-3.png)<!-- -->

*application by cluster*

``` r
if (rerun == T){
  # Drawing weights
  X = ech0$X
  pe = density(X,n=length(X))
  pe = approx(pe$x,pe$y,xout=ech0$X)$y
  pt = Pt(X)
  w = pt / pe
  q = w / sum(w)
  
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
  
  # WR algorithm
  ech_smote_GMM = WR(ech_smote_GMM,ech_smote_GMM$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
ech_synth = ech_smote_GMM
name_synth = "ech_smote_GMM"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/Smote_clust_graph-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/Smote_clust_graph-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/Smote_clust_graph-3.png)<!-- -->

*SMOTE on the initial sample, binarised by clusters*

``` r
if (rerun == T){
  ech_smote_clust = SmoteClassif(cluster ~ .,ech_MC)
}
hist(ech_smote_clust$X,breaks=100, col = "antiquewhite",freq=F,right = T)
points(ech_smote_clust$X,dbeta(ech_smote_clust$X,alpha_pop,beta_pop),col="red")
```

![](DAIR_Illustration_files/figure-gfm/SmoteOrig_clust-1.png)<!-- -->

``` r
hist(ech_smote_clust$X, col=rgb(0,0,1,1/4),breaks = 100,prob=T) 
hist(ech_add$X, col=rgb(1,0,0,1/4),breaks = 100,prob=T, add=T) 
hist(ech0$X,col=rgb(1,0,0,1/4),breaks = 100,prob=T, add=T)
```

![](DAIR_Illustration_files/figure-gfm/SmoteOrig_clust-2.png)<!-- -->

``` r
plot(ech_smote_clust$X,ech_smote_clust$Y)
points(ech0$X,ech0$Y,col="red")
```

![](DAIR_Illustration_files/figure-gfm/SmoteOrig_clust-3.png)<!-- -->

Results non relevant

#### Approachs Utulity-Based Learning

1)  Error : “All the points have relevance 0. Please, redefine your
    relevance function!” 2 ) No possibility to define q as utility
    function : Error in GaussNoiseRegress(form = Y \~ ., dat = ech0, rel
    = rel) : All the points have relevance 1. Please, redefine your
    relevance function!
2)  too complicated to define q as a utility function (UBL an R package
    for … : p.50)

On $Y$ :

``` r
if (rerun == T){
  try({
    ech_smoGN_Y = SMOGNRegress(Y ~ .,ech0)
    hist(ech_smoGN_Y$X,breaks=50, col = "antiquewhite",freq=F,right = T)
    points(ech_smoGN_Y$X,dbeta(ech_smoGN_Y$X,alpha_pop,beta_pop),col="red")
    hist(ech_smoGN_Y$X, col=rgb(0,0,1,1/4),breaks = 50,prob=T)
    hist(ech_add$X, col=rgb(1,0,0,1/4),breaks = 50,prob=T, add=T)
    hist(ech0$X,col=rgb(1,0,0,1/4),breaks = 50,prob=T, add=T)
    plot(ech_smoGN_Y$X,ech_smoGN_Y$Y)
    points(ech0$X,ech0$Y,col="red")
  })
}
```

On $X$ :

``` r
if (rerun == T){
  ech_smoGN = SMOGNRegress(X ~ .,ech0)
}
try({
  hist(ech_smoGN$X,breaks=50, col = "antiquewhite",freq=F,right = T)
  points(ech_smoGN$X,dbeta(ech_smoGN$X,alpha_pop,beta_pop),col="red")
  hist(ech_smoGN$X, col=rgb(0,0,1,1/4),breaks = 50,prob=T)
  hist(ech_add$X, col=rgb(1,0,0,1/4),breaks = 50,prob=T, add=T)
  hist(ech0$X,col=rgb(1,0,0,1/4),breaks = 50,prob=T, add=T)
  plot(ech_smoGN$X,ech_smoGN$Y)
  points(ech0$X,ech0$Y,col="red")
})
```

![](DAIR_Illustration_files/figure-gfm/SmoGN-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/SmoGN-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/SmoGN-3.png)<!-- -->
*Error* : “Error in SMOGNRegress(X \~ ., ech0) : All the points have
relevance 0. Please, redefine your relevance function!”

In both cases, this proposed solution does not meet the objective and
the generation of DS is not interesting: low values of $Y$ in the first
case and no orientation in the second.

### Random Forest (RF)

``` r
if (rerun == T){
  ech_RF=syn(ech0,method="rf", visit.sequence = c("X","Y"),k=1000, proper=T)
  ech_RF = ech_RF$syn
}
summary(ech_RF$X)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2431  0.4388  0.5039  0.5034  0.5681  0.7909

``` r
summary(ech0$X)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2431  0.4363  0.5028  0.5023  0.5678  0.7909

``` r
qqplot(ech_RF$X,ech0$X)
```

![](DAIR_Illustration_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
hist(ech_RF$X, col=rgb(0,0,1,1/4),breaks = 100,prob=T) 
hist(ech0$X, col=rgb(1,0,0,1/4),breaks = 100,prob=T, add=T) 
```

![](DAIR_Illustration_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
if (rerun == T){
  ech_RF = syn(ech0,method="rf", visit.sequence = c("X","Y"),k=10000)
  ech_RF = ech_RF$syn
}
hist(ech_RF$X,breaks=100, col = "antiquewhite",freq=F,right = T)
points(ech_RF$X,dbeta(ech_RF$X,alpha_pop,beta_pop),col="red")
```

![](DAIR_Illustration_files/figure-gfm/Synthpop-RF-1.png)<!-- -->

``` r
hist(ech_RF$X, col=rgb(0,0,1,1/4),breaks = 100,prob=T)  
hist(ech_add$X, col=rgb(1,0,0,1/4),breaks = 100,prob=T, add=T) 
hist(ech0$X,col=rgb(1,0,0,1/4),breaks = 100,prob=T, add=T)
```

![](DAIR_Illustration_files/figure-gfm/Synthpop-RF-2.png)<!-- -->

``` r
plot(ech_RF$X,ech_RF$Y)
points(ech0$X,ech0$Y,col="red")
```

![](DAIR_Illustration_files/figure-gfm/Synthpop-RF-3.png)<!-- -->

*Conditionnal application by cluster*

``` r
if (rerun == T){
  ech_RF2 = syn(ech_MC[,c("X","Y","cluster")],method="rf", visit.sequence = c("cluster","X","Y"),k=10000)
  ech_RF2 = ech_RF2$syn
  
  # WR algorithm
  ech_RF2 = WR(ech_RF2,ech_RF2$X,n_ech)
}
```

Graphical analysis of the rebalanced sample

``` r
### Graphics
ech_synth = ech_RF2
name_synth = "ech_RF"
graph(ech_synth,name_synth)
```

![](DAIR_Illustration_files/figure-gfm/Synthpop-RF2_res-1.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/Synthpop-RF2_res-2.png)<!-- -->![](DAIR_Illustration_files/figure-gfm/Synthpop-RF2_res-3.png)<!-- -->

# Performance evaluation

## Generalized Additive Model Learning

### Balanced sample

``` r
dat = ech_rep
name_dat = "GAM_ech_rep"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit=T)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)

y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 97.7"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 10.24"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 97.89"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y = function (name_dat){
  ## target variable analysis
  print(ggplot() +
    geom_line(aes(x=test$X, y=y_pred, colour="pred"), lwd = 1.5) +
      geom_line(aes(x=test$X, y=upr, colour="conf_int"), lwd=1,linetype = "dashed") +
      geom_line(aes(x=test$X, y=lwr, colour="conf_int"), lwd=1,linetype = "dashed") +
      geom_point(alpha=0.5,aes(x=test$X, y=y_test),colour="chartreuse4", shape=19, size=1)+
      stat_smooth(aes(x=test$X, y=y_test,colour="test"),method = loess, lwd=1.5)+
      scale_color_manual(name = "dataset",values = c("pred" = "deepskyblue4", "test" = "darkgreen", "conf_int" = "deepskyblue3")) +   
      theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0)))
  ggsave(paste0("Sorties_illustration/pred_Y_",name_dat,"-vs-test.png"),width=7.29, height=4.5)
}
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_pop-1.png)<!-- -->

#### Imbalanced Sample

``` r
dat = ech0
name_dat = "GAM_ech0"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 96.47"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.32"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 96.61"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech0-1.png)<!-- -->

#### Weighted Resampling (WR) sample

``` r
dat = ech_add
name_dat = "GAM_ech_add"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 97.46"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 12.72"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 96.89"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_add-1.png)<!-- -->

#### GN-WR sample

``` r
dat = ech_GN_SC
name_dat = "GAM_ech_GN_SC"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 89.56"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 15.06"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 95.53"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_GN_SC-1.png)<!-- -->

#### GN_GMM-WR sample

``` r
dat = GN_GMM
name_dat = "GAM_GN_GMM"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 95.37"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 12.25"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 97.05"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_GN_GMM-1.png)<!-- -->

#### ROSE-WR sample

``` r
dat = ech_ROSE_SC
name_dat = "GAM_ech_ROSE_SC"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 87.51"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 16.84"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 94.5"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_ROSE_SC-1.png)<!-- -->

#### ROSE_GMM-WR sample

``` r
dat = ROSE_GMM
name_dat = "GAM_ROSE_GMM"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 93.14"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.29"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 96.55"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ROSE_GMM-1.png)<!-- -->

#### KDE-WR sample

``` r
dat = kde_boot
name_dat = "GAM_kde_boot"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 96.6"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 21.83"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 92.02"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_kde_boot-1.png)<!-- -->

#### KDE_GMM-WR sample

``` r
dat = kde_boot_GMM
name_dat = "GAM_kde_boot_GMM"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 97.81"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 96.91"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_kde_boot_GMM-1.png)<!-- -->

#### GMM-WR sample

``` r
dat = GMM
name_dat = "GAM_GMM"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 97.55"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 15.06"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 95.92"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_GMM-1.png)<!-- -->

#### FA_GMM-WR sample

``` r
dat = FA_GMM
name_dat = "GAM_FA_GMM"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 97.64"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 14.3"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 96.15"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_FA_GMM-1.png)<!-- -->

#### Copula-WR sample

``` r
dat = ech_copule
name_dat = "GAM_ech_copule"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 95.41"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 25.55"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 90.11"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_copule-1.png)<!-- -->

#### GAN-WR sample

``` r
dat = ech_GAN
name_dat = "GAM_ech_GAN"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 93.91"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 24.39"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 91.49"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_GAN-1.png)<!-- -->

#### GAN_GMM-WR sample

``` r
dat = ech_ctganSynth_GMM
name_dat = "GAM_ech_ctganSynth_GMM"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 85.91"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 18.19"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 93.39"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_ctganSynth_GMM-1.png)<!-- -->

#### SMOTE-WR sample

``` r
dat = ech_smote
name_dat = "GAM_ech_smote"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 97.43"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.44"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 96.52"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_smote-1.png)<!-- -->

#### SMOTE_GMM-WR sample

``` r
dat = ech_smote_GMM
name_dat = "GAM_ech_smote_GMM"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 98.2"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 11.24"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 97.48"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_smote_GMM-1.png)<!-- -->

#### RF-WR sample

``` r
dat = ech_RF2
name_dat = "GAM_ech_RF2"

## Predictions
reg = gam(Y ~  s(X) , data = dat)
p <- predict(reg, test, se.fit = TRUE)
upr <- p$fit + (2 * p$se.fit)
lwr <- p$fit - (2 * p$se.fit)
y_pred = p$fit
res = summary(reg)
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
gam_devExp[k] = res$dev.expl ; print(paste0("deviance expliquee : " , round(gam_devExp[k]*100,2)))
```

    ## [1] "deviance expliquee : 87.96"

``` r
gam_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(gam_rmse[k]*100,2)))
```

    ## [1] "RMSE : 17.66"

``` r
gam_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(gam_R2[k]*100,2)))
```

    ## [1] "R2 : 94.17"

``` r
gam_hell_X[k] = hellinger(pop$X, dat$X)
gam_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
gam_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
gam_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predGAM_ech_RF2-1.png)<!-- -->

## Random Forest Learning

#### Balanced sample

``` r
dat = ech_rep
name_dat = "ech_rep"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 96.5"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 12.09"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 97.07"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF = function (name_dat){
  ## target variable analysis 
  print(ggplot() +
      stat_smooth(aes(x=test$X, y=y_pred, colour="pred"), lwd=1.5, method='loess') + 
      stat_smooth(aes(x=test$X, y=upr, colour="conf_int"), lwd=1,linetype = "dashed", method='loess') +
      stat_smooth(aes(x=test$X, y=lwr, colour="conf_int"), lwd=1,linetype = "dashed", method='loess') +
      geom_point(alpha=0.5,aes(x=test$X, y=y_test),colour="chartreuse4", shape=19, size=1)+
      stat_smooth(aes(x=test$X, y=y_test,colour="test"),method = loess, lwd=1.5)+
      scale_color_manual(name = "dataset",values = c("pred" = "deepskyblue4", "test" = "darkgreen", "conf_int" = "deepskyblue3")) +   
      theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.title = element_text(face = "bold"),legend.text = element_text(size=15,hjust=0)))
 ggsave(paste0("Sorties_illustration/pred_Y_RF_",name_dat,"-vs-test.png"),width=7.29, height=4.5)
}
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_pop-1.png)<!-- -->

#### Imbalanced sample

``` r
dat = ech0
name_dat = "ech0"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 94.61"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 15.49"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 95.59"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech0-1.png)<!-- -->

#### Weighted Resampling (WR) sample

``` r
dat = ech_add
name_dat = "ech_add"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 98.25"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 16.06"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 95.21"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_add-1.png)<!-- -->

#### GN-WR sample

``` r
dat = ech_GN_SC
name_dat = "ech_GN_SC"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})

ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 85.11"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 21.75"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 91.04"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_GN_SC-1.png)<!-- -->

#### GN_GMM-WR sample

``` r
dat = GN_GMM
name_dat = "GN_GMM"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 93.17"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 16.05"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 95.02"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_GN_GMM-1.png)<!-- -->

#### ROSE-WR sample

``` r
dat = ech_ROSE_SC
name_dat = "ech_ROSE_SC"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 90.69"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 24.61"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 88.77"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_ROSE_SC-1.png)<!-- -->

#### ROSE_GMM-WR sample

``` r
dat = ROSE_GMM
name_dat = "ROSE_GMM"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 89.71"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 18.36"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 93.57"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ROSE_GMM-1.png)<!-- -->

#### KDE-WR sample

``` r
dat = kde_boot
name_dat = "kde_boot"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 95.12"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 22.24"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 91.74"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_kde_boot-1.png)<!-- -->

#### KDE_GMM-WR sample

``` r
dat = kde_boot_GMM
name_dat = "kde_boot_GMM"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})

ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 96.91"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 15.11"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 95.9"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_kde_boot_GMM-1.png)<!-- -->

#### GMM-WR sample

``` r
dat = GMM
name_dat = "GMM"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 96.32"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 16.6"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 95.15"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_GMM-1.png)<!-- -->

#### FA_GMM-WR sample

``` r
dat = FA_GMM
name_dat = "FA_GMM"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.02"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 16.41"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 94.96"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_FA_GMM-1.png)<!-- -->

#### Copula-WR sample

``` r
dat = ech_copule
name_dat = "ech_copule"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 93.44"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 22.5"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 91.73"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_copule-1.png)<!-- -->

#### GAN-WR sample

``` r
dat = ech_GAN
name_dat = "ech_GAN"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 91.16"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 26.26"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 90.05"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_GAN-1.png)<!-- -->

#### GAN_GMM-WR sample

``` r
dat = ech_ctganSynth_GMM
name_dat = "ech_ctganSynth_GMM"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 78.9"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 24.21"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 88.36"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_ctganSynth_GMM-1.png)<!-- -->

#### SMOTE-WR sample

``` r
dat = ech_smote
name_dat = "ech_smote"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 96.68"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 16.07"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 94.88"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_smote-1.png)<!-- -->

#### SMOTE-GMM-WR sample

``` r
dat = ech_smote_GMM
name_dat = "ech_smote_GMM"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.73"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 14.75"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 95.91"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_smote_GMM-1.png)<!-- -->

#### RF-WR sample

``` r
dat = ech_RF2
name_dat = "ech_RF2"

## Predictions
X = dat$X
pe = density(X,n=length(X))
pe = approx(pe$x,pe$y,xout=dat$X)$y
pt = Pt(X)
w = pt / pe
q = w / sum(w)
reg = randomForest( Y ~ X, data = dat, weights = q,nodesize=2)
p = predict(reg, test, predict.all=TRUE)
y_pred = p$aggregate
upr = apply(p$individual, 1, function(x) {quantile(x, 0.975)})
lwr = apply(p$individual, 1, function(x) {quantile(x, 0.025)})
ymax=max(y_pred,y_test)
ymin=min(y_pred,y_test)

## Indicators
RF_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(RF_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 84.53"

``` r
RF_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(RF_rmse[k]*100,2)))
```

    ## [1] "RMSE : 20.41"

``` r
RF_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(RF_R2[k]*100,2)))
```

    ## [1] "R2 : 91.79"

``` r
RF_hell_X[k] = hellinger(pop$X, dat$X)
RF_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
RF_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
RF_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y_RF(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predRF_ech_RF2-1.png)<!-- -->

## Multivariate Adaptative Regression Spline Learning

#### Balanced sample

``` r
dat = ech_rep
name_dat = "MARS_ech_rep"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.37"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 10.73"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 97.68"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_pop-1.png)<!-- -->

#### Imbalanced sample

``` r
dat = ech0
name_dat = "MARS_ech0"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 96.4"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 16.52"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 95.02"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech0-1.png)<!-- -->

#### Weighted Resampling (WR) sample

``` r
dat = ech_add
name_dat = "MARS_ech_add"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.36"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.9"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 96.35"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_add-1.png)<!-- -->

#### GN-WR sample

``` r
dat = ech_GN_SC
name_dat = "MARS_ech_GN_SC"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 89.5"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 15.15"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 95.5"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_GN_SC-1.png)<!-- -->

#### GN_GMM-WR sample

``` r
dat = GN_GMM
name_dat = "MARS_GN_GMM"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 95.21"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.31"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 96.56"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_GN_GMM-1.png)<!-- -->

#### ROSE-WR sample

``` r
dat = ech_ROSE_SC
name_dat = "MARS_ech_ROSE_SC"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 87.51"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 17.66"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 93.98"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_ROSE_SC-1.png)<!-- -->

#### ROSE_GMM-WR sample

``` r
dat = ROSE_GMM
name_dat = "MARS_ROSE_GMM"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 93.08"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.71"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 96.34"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ROSE_GMM-1.png)<!-- -->

#### KDE-WR sample

``` r
dat = kde_boot
name_dat = "MARS_kde_boot"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 96.47"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 19.9"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 93.34"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_kde_boot-1.png)<!-- -->

#### KDE_GMM-WR sample

``` r
dat = kde_boot_GMM
name_dat = "MARS_kde_boot_GMM"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.73"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.17"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 96.85"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_kde_boot_GMM-1.png)<!-- -->

#### GMM-WR sample

``` r
dat = GMM
name_dat = "MARS_GMM"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.51"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 15.07"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 95.94"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_GMM-1.png)<!-- -->

#### FA_GMM-WR sample

``` r
dat = FA_GMM
name_dat = "MARS_FA_GMM"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.59"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 14.48"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 96.11"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_FA_GMM-1.png)<!-- -->

#### Copula-WR sample

``` r
dat = ech_copule
name_dat = "MARS_ech_copule"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 95.37"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 25.11"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 90.35"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_copule-1.png)<!-- -->

#### GAN-WR sample

``` r
dat = ech_GAN
name_dat = "ech_GAN"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 93.85"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 24.66"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 91.33"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_GAN-1.png)<!-- -->

#### GAN_GMM-WR sample

``` r
dat = ech_ctganSynth_GMM
name_dat = "MARS_ech_ctganSynth_GMM"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 85.84"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 19.01"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 92.74"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_ctganSynth_GMM-1.png)<!-- -->

#### SMOTE-WR sample

``` r
dat = ech_smote
name_dat = "MARS_ech_smote"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 97.37"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 14.22"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 96.01"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_smote-1.png)<!-- -->

#### SMOTE_GMM-WR sample

``` r
dat = ech_smote_GMM
name_dat = "MARS_ech_smote_GMM"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 98.09"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 13.55"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 96.44"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_smote_GMM-1.png)<!-- -->

#### RF-WR sample

``` r
dat = ech_RF2
name_dat = "MARS_ech_RF2"

## Predictions
reg = earth(dat$X ,dat$Y,nfold=10, ncross=30, varmod.method='gam')
p = predict(reg,test,interval="pint")
upr <- p$upr
lwr <- p$lwr
y_pred = p$fit
ymax=max(y_pred,ymax)
ymin=min(y_pred,ymin)

## Indicators
mars_rsq[k] = mean(reg$rsq) ; print(paste0("pseudo-R2 : " , round(mars_rsq[k]*100,2)))
```

    ## [1] "pseudo-R2 : 87.95"

``` r
mars_rmse[k] = rmse(y_test,as.numeric(y_pred)) ; print(paste0("RMSE : " , round(mars_rmse[k]*100,2)))
```

    ## [1] "RMSE : 17.31"

``` r
mars_R2[k] = cor(y_test,as.numeric(y_pred))^2 ; print(paste0("R2 : " , round(mars_R2[k] *100,2)))
```

    ## [1] "R2 : 94.48"

``` r
mars_hell_X[k] = hellinger(pop$X, dat$X)
mars_hell_Y[k] = hellinger(y_test, y_pred)
f_pop = densityfun(pop$X)
f_dat = densityfun(dat$X)
mars_kl_X[k] = kl.dist(f_pop(pop$X), f_dat(pop$X))$D
mars_kl_Y[k] = kl.dist(y_test,y_pred)$D
k = k + 1

## Graphics
graph_Y(name_dat)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](DAIR_Illustration_files/figure-gfm/predMARS2_ech_RF2-1.png)<!-- -->

``` r
beepr::beep(8)
```

*Calcul distance X*

``` r
KSD_X = data.frame("ech" = rep(" ",16), "KSD" = rep(0,16))
KSD_X$ech = c("Di (imb)","WR","GN","GN-GMM","ROSE","ROSE-GMM","KDE","KDE-GMM",                  "GMM","FA_GMM","Copule","GAN","GAN-GMM","SMOTE","SMOTE-GMM","RF")


KSD_X$KSD[1] = ks.test(ech0$X,ech_rep$X)$statistic
KSD_X$KSD[2] = ks.test(ech_add$X,ech_rep$X)$statistic
KSD_X$KSD[3] = ks.test(ech_GN_SC$X,ech_rep$X)$statistic
KSD_X$KSD[4] = ks.test(GN_GMM$X,ech_rep$X)$statistic
KSD_X$KSD[5] = ks.test(ech_ROSE_SC$X,ech_rep$X)$statistic
KSD_X$KSD[6] = ks.test(ROSE_GMM$X,ech_rep$X)$statistic
KSD_X$KSD[7] = ks.test(kde_boot$X,ech_rep$X)$statistic
KSD_X$KSD[8] = ks.test(kde_boot_GMM$X,ech_rep$X)$statistic
KSD_X$KSD[9] = ks.test(GMM$X,ech_rep$X)$statistic
KSD_X$KSD[10] = ks.test(FA_GMM$X,ech_rep$X)$statistic
KSD_X$KSD[11] = ks.test(ech_copule$X,ech_rep$X)$statistic
KSD_X$KSD[12] = ks.test(ech_GAN$X,ech_rep$X)$statistic
KSD_X$KSD[13] = ks.test(ech_ctganSynth_GMM$X,ech_rep$X)$statistic
KSD_X$KSD[14] = ks.test(ech_smote$X,ech_rep$X)$statistic
KSD_X$KSD[15] = ks.test(ech_smote_GMM$X,ech_rep$X)$statistic
KSD_X$KSD[16] = ks.test(ech_RF2$X,ech_rep$X)$statistic
# 
# label_x =  factor(KSD_X$ech, levels = KSD_X$ech)
# fig <- plot_ly(KSD_X,x= label_x,y = ~ KSD, color = ~ ech, type = "bar", main = "KS distance X with respect to the balanced sample")  
# fig 
```
