#
############################################################################
# Multifaceted Model for evaluations of professors at classroom
# By 
# A. Montenegro, Universidad Nacional dea Colombia
# H. Rosas, UNiversidad de Valparaíso
# C.      , Universidad Nacional dea Colombia
# J.      , Universidad Nacional dea Colombia
#
# Date: March, 2018
#
# Source. Data from a survey applied to students of Facultad de Ciencias
# to evaluate the perfomance of the professors in classroom
# Date 2015-2
#
# In this script we take the Stan output of subject relevance estimation to make files to advance analysis
# The data are prepare to be load in a database engine to produce the procesed data to analysis
###############################################################################################
#
# read final data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
final_data = read.csv("D25_Procesed_Data_for_Analysis.txt",header=TRUE,sep=";",dec = ",")
#number of professors
NP= 124
#number of items
NI = 15


# harmonic mean of professor's performance
prof_cat_data = final_data[,c(5,13:18,22:29,35)]
harmonic = matrix(NA,NP,NI)
for(i in 2:(NI+1)){
  harmonic[,i-1] = by (prof_cat_data[,i], prof_cat_data[,1],mean,na.rm=TRUE,simplify= TRUE)
}
harmonic_mean = round(1/(apply(1/harmonic,1,mean)),3)
# save the harmonic mena  data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(harmonic_mean, file="D26_harmonic_mean_professor.txt")




# select some initial variables
idx = c(2,3,4,6,7,9,10,11,12)
data_cor = final_data[,idx]
correlation =cor(data_cor,method="kendall")
View(correlation)
                   