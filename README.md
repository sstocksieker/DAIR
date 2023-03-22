

# <p align="center">Data Augmentation for Imbalanced Regression</p>
  

## <p align="center">Introduction</p>
  
This repository refers to the following paper : *Data Augmentation for Imbalanced Regression* : https://arxiv.org/abs/2302.09288. 

__Abstract__

In this work, we consider the problem of imbalanced data in a regression framework when the imbalanced phenomenon concerns continuous or discrete covariates. Such a situation can lead to biases in the estimates. In this case, we propose a data augmentation algorithm that combines a weighted resampling (WR) and a data augmentation (DA) procedure. In a first step, the DA procedure permits exploring a wider support than the initial one. In a second step, the WR method drives the exogenous distribution to a target one. We discuss the choice of the DA procedure through a numerical study that illustrates the advantages of this approach. Finally, an actuarial application is studied.



## <p align="center">Illustration</p>
  
We illustrate our approach numerically by first showing the
different generated data obtained with the WR and the DAWR algorithms. Then, we analyze the impact of bias selection in regression when an imbalanced sample is used for
training the learning model. At last, we measure the benefit
obtained with both WR and DA-WR algorithms and compare the results obtained by the different generators. In this
illustration, the algorithms are intentionally evaluated on a
complicated case with two inflection points on the border
of the observed support to get a strong impact on the regression in order to assess the method.


To avoid a sampling effect and give more robustness to the
results, we test our method on 100 imbalanced samples.


## <p align="center">Application</p>
  
We test our approach on a portfolio of automobile insurance described in a dataset of driver telematics from (So et al. (2021)).

dataset : http://www2.math.uconn.edu/~valdez/data.html 

paper : Synthetic Dataset Generation of Driver Telematics : https://arxiv.org/pdf/2102.00252.pdf
    

    
