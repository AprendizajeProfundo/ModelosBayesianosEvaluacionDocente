#
############################################################################
# Multifaceted Model for evaluations of Professors at classroom
# By 
# A. MOntenegro, Universidad Nacional dea Colombia
# H. Rosas, UNiversidad de Valparaíso
# C.      , Universidad Nacional dea Colombia
# J.      , Universidad Nacional dea Colombia
#
# Date: March, 2018
#
# Source. Data from a survey applied to students of Facultad de Ciencias
# to evaluate the perfomance of the profesors in classroom
# Date 2015-2
#
# Model Description
#  Model 1: relevance estimation:
# Subjects  items, students
# data prepared from a text file object, with data ready to use in Stan.There are no missing data 
# the object subject_data is upload. It is a 5503 by 5 matrix
# Columns: Y_sub (responses), Student, subject, Item
# There are 4 items. All items  have 4 categories. 
##################################### ########################################
#

# rstan library
#
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#

# Read the data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
subject_data = read.csv("D5_subject_data_sample.txt",header=TRUE,sep=";")
head(subject_data)

# total parameters
NN = nrow(subject_data) #5503; num. registers
NS = length(unique(subject_data[,1])) #1380; num. students
NJ = length(unique(subject_data[,2])) #123;  num. subjects
NC = max(subject_data[,3]) #4; number of response  categories
NR = length(unique(subject_data[,4])) #5;  num. categories selfperformance
NI = length(unique(subject_data[,5])) #4;   num. items

# 
# go to Stan
dat = list(student = subject_data[,1], subject=subject_data[,2], y=subject_data[,3],
           item=subject_data[,5], N=NN, N_stud=NS, N_sub=NJ, 
           N_cat=NC, N_item=NI)

################
#first compile and save the fitted model for re-using
# data are taken from the current R works
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Stan-codes")
prof_fit_p_4 <- stan(file = 'S6_Multi_Faceted_Ciencias_subject_student_item.stan', data = dat,iter = 4, chains = 1)

# now sample using the compiled model
prof_fit_4<- stan(fit = prof_fit_p_4,  data =dat, iter = 2000, chains = 4)

# save the stan object
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
save(prof_fit_4,file="Model_student_subject_item.Rdata")
# to load the object
#load(file="Model_student_subject_item.Rdata")

#review the results
#summary object
fit_summary_04 <- summary(prof_fit_4)
#names of fit_summary
print(names(fit_summary_04))
# summary"   "c_summary"
#In fit_summary$summary all chains are merged whereas 
#fit_summary$c_summary contains summaries for each chain individually. 
#Typically we want the summary for all chains merged,

print(fit_summary$summary_04)
summary_04 =fit_summary_04$summary
fix( summary_04)
colnames(summary_04)
# "mean"    "se_mean" "sd"      "2.5%"    "25%"     "50%"     "75%"     "97.5%"   "n_eff"  "Rhat"

# summary of theta (professor)
theta_mean_04 = summary_04[1:NP,1]
summary(theta_mean_04)
#Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-2.6997778 -0.5350104  0.0098986  0.0004556  0.5784284  2.2332802 
hist(theta_mean_04)
plot(density(theta_mean_04))

# summary of gamma (student)
student_mean_04 = summary_04[1:NP,1]
summary(student_mean_04)
#Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-2.6997778 -0.5350104  0.0098986  0.0004556  0.5784284  2.2332802 
hist(student_mean_04)
plot(density(student_mean_04))



#convergence statistics(Rhat)
summary(summary_04[,10])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.9992  1.0014  1.0031  1.0039  1.0052  1.0163 

#plot(prof_fit)
#pairs(prof_fit, pars = c("alpha", "beta","sigma", "lp__"))

# extract the posterior draws
# 
la <- extract(prof_fit, permuted = TRUE) # return a list of arrays 
theta.chain <- la$theta
beta.chain = la$beta
mu.beta = la$mu_beta
sigma.beta.chain = la$sigma_beta

# extract as a matrix, better to ts plots
#chains = as.matrix(mult_reg_fit)
#plot(as.ts(chains[,1]))
# ok

################################################
# save the stan object to future analysis
################################################

save(prof_fit_4,file="Model_4_prof_fit_4_pr_it_st_co_su_ca.Rdata")
# to load the object
#prof_fit_4 =load(file="Model_4_prof_fit_4_pr_it_st_co_su_ca.Rdata")


