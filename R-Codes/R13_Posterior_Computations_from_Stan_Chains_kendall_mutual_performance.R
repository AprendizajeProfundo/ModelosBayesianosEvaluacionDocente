#
############################################################################
# Multifaceted Model for evaluations of Professors at classroom
# By 
# A. MOntenegro, Universidad Nacional dea Colombia
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
# In this Script the Chains from Stan are loaded and some computations are made
# of kendall correlation and mutual information from the chains produced 
# by Stan
# 
# professor data
# replace the next line according to your file system
path = "D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data"
setwd(path)

# library to information theory computations
library(infotheo)
#library to RStan
library(rstan)

# load the r object containing the Stan fit object
load(file="Model_student_professor_performance_item.Rdata")
#summary objects for general analysis
# the object name is prof_fit_2
prof_fit_summary <- summary(prof_fit_2)
prof_summary =prof_fit_summary$summary
View(prof_summary)
#
#names of fit_summary
#print(names(fit_summary_02))
# summary"   "c_summary"
#In fit_summary$summary all chains are merged whereas 
#fit_summary$c_summary contains summaries for each chain individually. 
#Typically we want the summary for all chains merged,
#
# summary of theta (professor)
#num professors
NP=124
prof_theta_mean = prof_summary[1:NP,1]
summary(prof_theta_mean)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-2.898751 -0.598566  0.182545  0.001712  0.610713  2.512942 
hist(prof_theta_mean)
plot(density(prof_theta_mean))

# summary of gamma (student)
# num students
NS=1380
prof_student_mean = prof_summary[(NP+1):(NP+NS),1]
summary(prof_student_mean)
#Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-2.803968 -0.723143  0.016133  0.000182  0.668619  3.965242 


hist(prof_student_mean)
plot(density(prof_student_mean))

#convergence statistics(Rhat)
summary(prof_summary[,10])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.999   1.000   1.001   1.001   1.002   1.016 

# Extract the chains
la = extract(prof_fit_2, permuted = TRUE)
names(la)
# [1] theta"        "gamma"        "xi"           "beta"         "mu_beta"      "sigma_beta"   "sigma_gamma" 
# [8] "sigma_xi"     "mu_item_beta" "lp__"  

attach(la)
# professor latent trait chains
dim(theta)
# 4000  124
# student latent trait chains 
> dim(gamma)  
# 4000 1380

# load the complete recode sample data including estimations, harmonic mean..
final_data = read.csv(file="D25_Procesed_Data_for_Analysis.txt",header=TRUE,sep=";",dec = ",")
#
# sort the dataframes by  Id_Student_Stan
final_data = final_data[order(final_data[,1]),]

#index  professor
idsp = final_data[,5]

# Number of students
NCG = ncol(gamma)
NN = nrow(gamma)
# discretization of gamma. Transpose data
gamma_disc=matrix(NA,NCG,NN)
for(i in 1:NN){
  gamma_disc[,i] = discretize( gamma[i,], disc="equalfreq", nbins=round(NCG^(1/3)))[,1]
}

# discretization of theta. Transpose data
NCT =ncol(theta)
theta_disc=matrix(NA,NCT,NN)
for(i in 1:NN){
  theta_disc[,i] = discretize( theta[i,], disc="equalfreq", nbins=round(NCT^(1/3)))[,1]
}
# extend theta to fit with student data
theta_disc_exp = matrix(NA,NCG,NN)
for(j in 1:NN){
  for (i in 1:NCG){
    theta_disc_exp[i,j] = theta_disc[idsp[i],j ]
  }
}

# discretize harmonic mean
harmonic_disc = discretize( final_data[,8], disc="equalfreq", nbins=round(NCT^(1/3)))[,1]
# selfperformance
self_perform = final_data[,2] 

# working data matrix
dat = data.frame(matrix(NA,NS,4))
colnames(dat) =c("theta","gamma","harmonic","selfper")
#
dat[,3] = harmonic_disc
dat[,4] = self_perform

#dat[,1] = theta_disc_exp[,4]
#dat[,2] = gamma_disc[,4]
#cor(dat,method = "kendall")
#I=mutinformation(dat,method= "mm")
#

# matrix to kendall and mutual information
kendall.chain = matrix(NA,NN,10)
mutual.chain =  matrix(NA,NN,10)
n.names = c("th_th","th_gam","th_har","th_self", "gam_gam","gam_har", "gam_self",
            "har_har", "har_self","self_self")
colnames(kendall.chain) = colnames(mutual.chain) = n.names
#
# working loop
for (j in 1:NN){
  if (j%%100 == 1) print(j)
  dat[,1] = theta_disc_exp[,j]
  dat[,2] = gamma_disc[,j]

  # compute kendall correlation
  kendall = cor(dat,method="kendall")
  # compute mutual information
  I = mutinformation(dat,method= "mm")
  #I2 <- mutinformation(z,method= "emp")
 kendall.chain[j,1:4] = kendall[1,1:4]
 kendall.chain[j,5:7] = kendall[2,2:4]
 kendall.chain[j,8:9] = kendall[3,3:4]
 kendall.chain[j,10] = kendall[4,4]
 mutual.chain[j,1:4] = I[1,1:4]
 mutual.chain[j,5:7] = I[2,2:4]
 mutual.chain[j,8:9] = I[3,3:4]
 mutual.chain[j,10] = I[4,4]
 
}# end for j



# statistics of kendall correlation
 prob = c(0.025,0.25,0.5,0.75,0.975)
k_theta_gamma = quantile(kendall.chain[,2],probs=prob)
k_theta_gamma = c(k_theta_gamma, mean(kendall.chain[,2]))
#
k_theta_harmonic = quantile(kendall.chain[,3],probs=prob)
k_theta_harmonic = c(k_theta_harmonic, mean(kendall.chain[,3]))
#
k_theta_self = quantile(kendall.chain[,4],probs=prob)
k_theta_self = c(k_theta_self, mean(kendall.chain[,4]))
#
k_gamma_harmonic = quantile(kendall.chain[,6],probs=prob)
k_gamma_harmonic = c(k_gamma_harmonic, mean(kendall.chain[,6]))
#
k_gamma_self = quantile(kendall.chain[,7],probs=prob)
k_gamma_self = c(k_gamma_self, mean(kendall.chain[,7]))
#
k_harmonic_self = quantile(kendall.chain[,9],probs=prob)
k_harmonic_self = c(k_harmonic_self, mean(kendall.chain[,9]))
#
kendall_stat = rbind(k_theta_gamma,k_theta_harmonic,k_theta_self,
                     k_gamma_harmonic, k_gamma_self)
kendall_stat = cbind(kendall_stat[,1:3],kendall_stat[,6],kendall_stat[,4:5])
colnames(kendall_stat) =c("q2.5","q25","q50","mean","q75","q97.5")
#
# statistics of mutual information
prob = c(0.025,0.25,0.5,0.75,0.975)
m_theta_theta = quantile(mutual.chain[,1],probs=prob)
m_theta_theta = c(m_theta_theta, mean(mutual.chain[,1]))
#
m_theta_gamma = quantile(mutual.chain[,2],probs=prob)
m_theta_gamma = c(m_theta_gamma, mean(mutual.chain[,2]))
#
m_theta_harmonic = quantile(mutual.chain[,3],probs=prob)
m_theta_harmonic = c(m_theta_harmonic, mean(mutual.chain[,3]))
#
m_theta_self = quantile(mutual.chain[,4],probs=prob)
m_theta_self = c(m_theta_self, mean(mutual.chain[,4]))
#
m_gamma_gamma = quantile(mutual.chain[,5],probs=prob)
m_gamma_gamma = c(m_gamma_gamma, mean(mutual.chain[,5]))
#
m_gamma_harmonic = quantile(mutual.chain[,6],probs=prob)
m_gamma_harmonic = c(m_gamma_harmonic, mean(mutual.chain[,6]))
#
m_gamma_self = quantile(mutual.chain[,7],probs=prob)
m_gamma_self = c(m_gamma_self, mean(mutual.chain[,7]))
#
m_harmonic_harmonic = quantile(mutual.chain[,8],probs=prob)
m_harmonic_harmonic = c(m_harmonic_harmonic, mean(mutual.chain[,8]))
#
m_harmonic_self = quantile(mutual.chain[,9],probs=prob)
m_harmonic_self = c(m_harmonic_self, mean(mutual.chain[,9]))
#
m_self_self = quantile(mutual.chain[,10],probs=prob)
m_self_self = c(m_self_self, mean(mutual.chain[,10]))
#
mutual_stat = rbind(m_theta_theta,m_theta_gamma,m_theta_harmonic,m_theta_self,
                    m_gamma_gamma, m_gamma_harmonic, m_gamma_self, m_harmonic_harmonic,
                    m_harmonic_self,m_self_self)

mutual_stat = cbind(mutual_stat[,1:3],mutual_stat[,6],mutual_stat[,4:5])
colnames(mutual_stat) =c("q2.5","q25","q50","mean","q75","q97.5")
mutual_stat

# normalized mutual information
norm_mutual.chain= matrix(NA, nrow(mutual.chain),ncol(mutual.chain))
colnames(norm_mutual.chain) = colnames(mutual.chain)
norm_mutual.chain[,1] = norm_mutual.chain[,5] = norm_mutual.chain[,8] = norm_mutual.chain[,10] =
                      rep(1,times=nrow(mutual.chain))

for (i in 1:nrow(mutual.chain)){
  norm_mutual.chain[i,2]= mutual.chain[i,2]/sqrt(mutual.chain[i,1]*mutual.chain[i,5])
  norm_mutual.chain[i,3]= mutual.chain[i,3]/sqrt(mutual.chain[i,1]*mutual.chain[i,8])
  norm_mutual.chain[i,4]= mutual.chain[i,4]/sqrt(mutual.chain[i,1]*mutual.chain[i,10])
  norm_mutual.chain[i,6]= mutual.chain[i,6]/sqrt(mutual.chain[i,5]*mutual.chain[i,8])
  norm_mutual.chain[i,7]= mutual.chain[i,7]/sqrt(mutual.chain[i,5]*mutual.chain[i,10])
  norm_mutual.chain[i,9]= mutual.chain[i,9]/sqrt(mutual.chain[i,8]*mutual.chain[i,10])
}
  
# statistics of normalized mutual information
prob = c(0.025,0.25,0.5,0.75,0.975)
n_m_theta_theta = quantile(norm_mutual.chain[,1],probs=prob)
n_m_theta_theta = c(n_m_theta_theta, mean(norm_mutual.chain[,1]))
#
n_m_theta_gamma = quantile(norm_mutual.chain[,2],probs=prob)
n_m_theta_gamma = c(n_m_theta_gamma, mean(norm_mutual.chain[,2]))
#
n_m_theta_harmonic = quantile(norm_mutual.chain[,3],probs=prob)
n_m_theta_harmonic = c(n_m_theta_harmonic, mean(norm_mutual.chain[,3]))
#
n_m_theta_self = quantile(norm_mutual.chain[,4],probs=prob)
n_m_theta_self = c(n_m_theta_self, mean(norm_mutual.chain[,4]))
#
n_m_gamma_gamma = quantile(norm_mutual.chain[,5],probs=prob)
n_m_gamma_gamma = c(n_m_gamma_gamma, mean(norm_mutual.chain[,5]))
#
n_m_gamma_harmonic = quantile(norm_mutual.chain[,6],probs=prob)
n_m_gamma_harmonic = c(n_m_gamma_harmonic, mean(norm_mutual.chain[,6]))
#
n_m_gamma_self = quantile(norm_mutual.chain[,7],probs=prob)
n_m_gamma_self = c(n_m_gamma_self, mean(norm_mutual.chain[,7]))
#
n_m_harmonic_harmonic = quantile(norm_mutual.chain[,8],probs=prob)
n_m_harmonic_harmonic = c(n_m_harmonic_harmonic, mean(norm_mutual.chain[,8]))
#
n_m_harmonic_self = quantile(norm_mutual.chain[,9],probs=prob)
n_m_harmonic_self = c(n_m_harmonic_self, mean(norm_mutual.chain[,9]))
#
n_m_self_self = quantile(norm_mutual.chain[,10],probs=prob)
n_m_self_self = c(n_m_self_self, mean(norm_mutual.chain[,10]))
#
norm_mutual_stat = rbind(n_m_theta_theta,n_m_theta_gamma,n_m_theta_harmonic,n_m_theta_self,
                    n_m_gamma_gamma, n_m_gamma_harmonic, n_m_gamma_self, n_m_harmonic_harmonic,
                    n_m_harmonic_self,n_m_self_self)

norm_mutual_stat = cbind(norm_mutual_stat[,1:3],norm_mutual_stat[,6],norm_mutual_stat[,4:5])
colnames(norm_mutual_stat) =c("q2.5","q25","q50","mean","q75","q97.5")
norm_mutual_stat

# save chain objects
save(kendall.chain,file="kendall_chain.Rdata")
save(mutual.chain,file="mutual_chain.Rdata")
save(norm_mutual.chain,file="norm_mutual_chain.Rdata")

##
# xtable to the paper
library(xtable)
zz=round(kendall_stat,3)
xtable(zz,digits=3,caption="Summary of Bayesian estimation of Kendall correlation estimation , in the professor performance estimation",
                         label ="kendall_professor_table",align ="ccccccc")
zz=round(mutual_stat,3)
xtable(zz,digits=3,caption="Bayesian estimation of mutual information, in the professor performance estimation",
       label ="mutual_professor_table",align ="ccccccc")
zz=round(norm_mutual_stat[c(2:4,6:7,9),],3)
xtable(zz,digits=3,caption="Bayesian estimation of normalized mutual information, in the professor performance estimation",
       label ="normal_mutual_professor_table",align ="ccccccc")

#
