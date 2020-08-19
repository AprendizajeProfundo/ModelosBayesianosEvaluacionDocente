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
# professor fit data
beta_prof_fit_data = read.csv2(file="D12_beta_professor_fit_dat.txt", header=TRUE,sep=";",dec=",")
beta_prof_fit_data_n= beta_prof_fit_data[,c(1:10)]
library(xtable)
xtable(beta_prof_fit_data_n,caption="Estimate")

# subject data
# replace the next line according to your file system
path = "D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data"
setwd(path)
# subject fit data
beta_prof_fit_data = read.csv2(file="D17_beta_subject_fit_dat.txt", header=TRUE,sep=";",dec=",")
beta_prof_fit_data_n= beta_prof_fit_data[,c(1,2,4:10)]
library(xtable)
xtable(beta_prof_fit_data_n,caption="Estimate")
