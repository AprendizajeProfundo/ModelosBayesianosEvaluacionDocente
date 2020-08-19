#
############################################################################
# Multifaceted Model for evaluations of Professors at classroom
# By 
# A. Montenegro, Universidad Nacional dea Colombia
# H. Rosas, UNiversidad de Valparaíso
# C.      , Universidad Nacional dea Colombia
# J.      , Universidad Nacional dea Colombia
#
# Date: September, 2018
#
# Source. Data from a survey applied to students of Facultad de Ciencias
# to evaluate the perfomance of the profesors in classroom
# Date 2015-2
# 
# In this Script the Chains from Stan are loaded and GOF computations are made
# by Stan
# 
# utility functions
# working functions
# asuume logistic orderd model
#sampler
 r_ord_logistic = function(theta,gamma,beta){
  # Number of categories 
  # first generate the ordered logistic distribution
  K = length(beta)+1
  g = numeric(K)
  pred.right = 1
  #
  for(k in 1:(K-1)){
    pred.left  = pred.right
    pred.right = 1/(1+exp(-(theta-gamma-beta[k])))
    g[k]= pred.left - pred.right
    }
  g[K]= pred.right
  # Now generate the sample 
  y = sample(1:K,size=1, prob=g)
  return(y)
}# end function r_ord_logistic
#test
 r_ord_logistic(1.2,2.0, c(-1,0,1,2))

# probability
 p_ord_logistic = function(y,theta,gamma,beta,loga=TRUE){
   # Number of categories 
   # first generate the ordered logistic distribution
   K = length(beta)+1
   # calculation
   if (y==1){pr = 1-1/(1+exp(-(theta-gamma-beta[1])))}
   if (y==K){pr = 1/(1+exp(-(theta-gamma-beta[K-1])))}
   if (y>1 && y<K){pr = 1/(1+exp(-(theta-gamma-beta[y-1])))-
                        1/(1+exp(-(theta-gamma-beta[y])))}
   if (loga) {return(log(pr))} else{
      return (pr)}
 }# end function p_ord_logistic
 #test
 
 p_ord_logistic(1,0.68,-1.0, c(-1,0,1.2)) 
 
 
#library to RStan
library(rstan)
#
# professor data
# replace the next line according to your file system
path = "D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data"
setwd(path)
professor_data = read.csv("D4_professor_data_sample.txt",header=TRUE,sep=";")
# omit self performance column
professor_data = professor_data[,-4]
# load the r object containing the Stan fit object
load(file="Model_student_professor_item.Rdata")
#
# Extract the chains
#la = extract(prof_fit_2, permuted = TRUE)
la = extract(prof_fit_2, permuted = TRUE)
names(la)
# "theta"        "gamma"        "beta"         "mu_beta"      "sigma_beta"   "sigma_gamma" 
# [7] "mu_item_beta" "lp__" 

attach(la)
# professor latent trait chains
dim(theta)
# 4000  124
# student latent trait chains 
dim(gamma)  
# 4000 1380
dim(beta)
#4000   15    3

# main
# Bayesian sample size
BSZ = nrow(theta) # 4000
# data size
DZ = nrow(professor_data)
# number of professors
NP = 124
# number of students 
NS =1380
# number of items
NI = 15
# number of categories
NC = 4 # fixed in the current application, really is not required
# lp professor 
lp_prof   = array(0.0,dim =NP)
lp_prof_r = array(0.0,dim =NP) # for replicate data 
# lp item
lp_item   = array(0.0,dim =NI)
lp_item_r = array(0.0,dim =NI) # for replicate data
# Bayesian pvalue profesor
pval_prof =array(0.0,dim =NP)
# Bayesian pvalue item
pval_item =array(0.0,dim =NI)
# global pval
pval_g =0.0

# working bucle
BSZ = 2000  #prov
#for each Bayesian sample
for (t in 1:BSZ){ 
  # for each observed data 
  if (t%%10 ==1) cat(t," pval_g = ",pval_g/t,"\n")
  for (l in 1:DZ){
    # current response
    y         = professor_data[l,3] # category
    item      = professor_data[l,4]
    student   = professor_data[l,1]
    professor =  professor_data[l,2]
     # replicate the observation
    yp =  r_ord_logistic(theta[t,professor],gamma[t,student],beta[t,item,])
    # lpy
    lpy = p_ord_logistic(y,theta[t,professor],gamma[t,student], beta[t,item,]) 
    # lpyp
    lpyp = p_ord_logistic(yp,theta[t,professor],gamma[t,student], beta[t,item,]) 
    # acumulate the professor lp, item lp
    lp_prof[professor] = lp_prof[professor] + lpy
    lp_prof_r[professor] = lp_prof_r[professor] + lpyp
    lp_item[item]      = lp_item[item] + lpy
    lp_item_r[item]      = lp_item_r[item] + lpyp
  }# end for l
 # update p_value data
  pval_prof = pval_prof + ifelse(-2*lp_prof < -2*lp_prof_r,1,0)
  pval_item = pval_item + ifelse(-2*lp_item < -2*lp_item_r,1,0) 
  lpp = -2*sum(lp_prof)
  lpr = -2*sum(lp_prof_r)
  pval_g = pval_g +ifelse(lpp < lpr,1,0)
  # reset lp values
  # lp professor 
  lp_prof   = array(0.0,dim =NP)
  lp_prof_r = array(0.0,dim =NP) 
  # lp item
  lp_item   = array(0.0,dim =NI)
  lp_item_r = array(0.0,dim =NI) 
}# end for t

# compute Bayesian p-values
pval_prof= pval_prof/BSZ
pval_item= pval_item/BSZ
pval_g = pval_g/BSZ
pval_prof
pval_item
pval_g

write.csv2(pval_prof,file="pval_prof.txt")
write.csv2(pval_item,file="pval_item.txt")
write.csv2(pval_g,file="pval_g.txt")