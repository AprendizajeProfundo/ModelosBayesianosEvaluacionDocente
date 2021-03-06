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
# Model 1: Performance estimation:
# Professors,  items, students
# data prepared from a text file object, with data ready to use in Stan.There are no missing data 
# the object professor_data is upload. It is a 20617 by 4 matrix
# Columns: Y_prof (responses), Student, Professor, Item
# There are 15 items. All items  have 4 categories. 
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
professor_data = read.csv("D4_professor_data_sample.txt",header=TRUE,sep=";")
head(professor_data)

# total parameters
NN = nrow(professor_data) #20606; num. registers
NS = length(unique(professor_data[,1])) #1380; num. students
NP = length(unique(professor_data[,2])) #124;  num. professors
NC = max(professor_data[,3]) #4; number of response  categories
NR = length(unique(professor_data[,4])) #5;  num. categories selfperformance
NI = length(unique(professor_data[,5])) #15;   num. items

# 
# go to Stan
dat = list(student = professor_data[,1], professor=professor_data[,2], y=professor_data[,3],
            item=professor_data[,5], N=NN, N_stud=NS, N_prof=NP, 
           N_cat=NC, N_item=NI)

################
#first compile and save the fitted model for re-using
# data are taken from the current R works
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Stan-codes")
prof_fit_p_ <- stan(file = 'S3_Multi_Faceted_Ciencias_professor_student_item.stan', data = dat,iter = 4, chains = 1)

# now sample using the compiled model
prof_fit_2<- stan(fit = prof_fit_p_,  data =dat, iter = 2000, chains = 4, control = list(max_treedepth = 12))
# save the stan object
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
save(prof_fit_2,file="Model_student_professor_item.Rdata")
# to load the object
#load(file="Model_2_prof_fit_2_pr_it_st.Rdata")
#load(file="Model_student_professor_item.Rdata")

#review the results
#summary object
prof_fit_summary <- summary(prof_fit_2)
prof_summary =prof_fit_summary$summary
fix(prof_summary)
#names of fit_summary
#print(names(fit_summary_02))
# summary"   "c_summary"
#In fit_summary$summary all chains are merged whereas 
#fit_summary$c_summary contains summaries for each chain individually. 
#Typically we want the summary for all chains merged,
#
# summary of theta (professor)
prof_theta_mean = prof_summary[1:NP,1]
summary(prof_theta_mean)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-2.6901509 -0.6136038  0.0965673  0.0006738  0.6196237  2.7087729 
hist(prof_theta_mean)
plot(density(prof_theta_mean))

# summary of gamma (student)
prof_student_mean = prof_summary[(NP+1):(NP+NS),1]
summary(prof_student_mean)
#Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-3.055600 -0.783952 -0.001659 -0.001363  0.752513  4.499214 
hist(prof_student_mean)
plot(density(prof_student_mean))

#convergence statistics(Rhat)
summary(prof_summary[,10])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.9991  0.9996  1.0000  1.0002  1.0005  1.0082

#plot(prof_fit)
#pairs(prof_fit, pars = c("alpha", "beta","sigma", "lp__"))

# extract the posterior draws
la <- extract(prof_fit_2, permuted = TRUE)
## return a list of arrays 
theta.chain <- la$theta
beta.chain = la$beta
mu.beta = la$mu_beta
sigma.beta.chain = la$sigma_beta

# extract as a matrix, better to ts plots
#chains = as.matrix(mult_reg_fit)
#plot(as.ts(chains[,1]))
# ok



