#
############################################################################
# Multifaceted Model for evaluations of Professors at classroom
# By 
# A. Montenegro, Universidad Nacional dea Colombia
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
# In this script we take the Stan output of professor´performance estimation to make files to advance analysis
# The data are prepare to be load in a database engine to produce the procesed data to analysis
###############################################################################################
#
library(rstan)
# Read the Stan Output Data for professor performance. 2 models
#
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
# professor data
load(file="Model_student_professor_item.Rdata")
# object prof_fit_2
fit_student_professor_item = prof_fit_2
rm(prof_fit_2)
load(file="Model_student_professor_performance_item.Rdata")
# object prof_fit_2
fit_student_professor_performance_item = prof_fit_2
rm(prof_fit_2)
#
fit_student_professor_item_summary = summary(fit_student_professor_item)
student_professor_item_summary = round(fit_student_professor_item_summary$summary,3)
View(student_professor_item_summary)
#
fit_student_professor_performance_item_summary = summary(fit_student_professor_performance_item)
student_professor_performance_item_summary = round(fit_student_professor_performance_item_summary$summary,3)
View(student_professor_performance_item_summary)

colnames(student_professor_item_summary )
# spi_mean, spi_se_mean_sd, spi_sd, spi_2_5, spi_25, spi_50, spi_50, spi_75, spi_97_5, psi_n_eff,psi_rhat
spi_names = c("spi_mean", "spi_se_mean_sd", "spi_sd", "spi_2_5", "spi_25", "spi_50",  
                 "spi_75", "spi_97_5",  "spi_n_eff", "spi_rhat")

colnames(student_professor_item_summary) = spi_names 

spipernames = c("spiper_mean", "spiper_se_mean_sd", "spiper_sd", "spiper_2_5", "spiper_25", "spiper_50",  
              "spiper_75", "spiper_97_5",  "spiper_n_eff", "spiper_rhat")


colnames(student_professor_performance_item_summary)  = spipernames

# save complete  processed data
#setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
# write.csv(fit_student_professor_item_summary, file="D10_fit_student_professor_item_summary.txt",header=TRUE,sep=";")
# write.csv(fit_student_professor_performance_item_summary,file="D11_fit_student_professor_performance_item_summary.txt",header=TRUE,sep=";")

# split the data into facets
#
# professor
# read the professor's id
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
id_prof = read.csv2("D6_professor_indices_sample.txt",header=TRUE,sep=";")
# Num professors
NP = 124
professor_fit_data = cbind(id_prof,student_professor_item_summary[1:NP,],
                           student_professor_performance_item_summary[1:NP,])                           )
# save the professor fit data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(professor_fit_data,file="D10_professor_fit_dat.txt")
#
# student
# read the students's id
id_student = read.csv("D7_student_indices_sample.txt",header=TRUE,sep=";")
# number of students
NS = 1380
student_prof_fit_data = cbind(id_student,student_professor_item_summary[(NP+1):(NP+NS),],
                           student_professor_performance_item_summary[(NP+1):(NP+NS),])                           )
# save the student fit data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(student_prof_fit_data, file="D11_student_professor_fit_dat.txt")
#
# item parameters
NBE = 45
N_MU_BE = 15
N_sIG_BE = 2
N_sIG_MU = 1
N_XI = 5
N_SIG_XI = 1

beta_fit_data_1= rbind(student_professor_item_summary[(NP+NS+1):(NP+NS+NBE),],
                     student_professor_item_summary[(NP+NS+NBE+4):(NP+NS+NBE+3+N_MU_BE),])
beta_fit_data_2= rbind(student_professor_performance_item_summary[(NP+NS+N_XI+1):(NP+NS+N_XI+NBE),],
                       student_professor_performance_item_summary[(NP+NS+N_XI+NBE+5):(NP+NS+N_XI+NBE+4+N_MU_BE),])
beta_fit_data = cbind(beta_fit_data_1,beta_fit_data_2)
# save the student fit data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(beta_fit_data , file="D12_beta_professor_fit_dat.txt")

# performance 
xi_fit_data = student_professor_performance_item_summary[(NP+NS+1):(NP+NS+N_XI),]
# save the performance fit data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(xi_fit_data, file="D13_performance_professor_fit_dat.txt")

# variances
sigma_fit = student_professor_item_summary[(NP+NS+NBE+2):(NP+NS+NBE+3),]
sigma_fit = cbind(sigma_fit,
                    student_professor_performance_item_summary[(NP+NS+NBE+N_XI+2):(NP+NS+NBE+N_XI+3),])
sigma_xi = c(rep(NA,times =10),
                  student_professor_performance_item_summary[(NP+NS+NBE+N_XI+4),])
sigma_fit = rbind(sigma_fit,sigma_xi)
# save the variance fit data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(sigma_fit, file="D14_variance_professor_fit_dat.txt")

# log-likehood (lp)
lp_fit = student_professor_item_summary[1568,]
lp_fit =t(c(lp_fit,student_professor_performance_item_summary[1574,]))
row.names(lp_fit)=("lp")

# save the lp fit data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(lp_fit, file="D15_lp_professor_fit_dat.txt")

