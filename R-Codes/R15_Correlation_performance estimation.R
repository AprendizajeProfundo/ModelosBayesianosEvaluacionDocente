
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
final_data = read.csv("D10_professor_fit_dat.txt",header=TRUE,sep=";",dec = ",")

core = final_data[,c(4,14,24)]
cor(core,method="kendal")
