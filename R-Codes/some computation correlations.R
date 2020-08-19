3 computatuions correltions


professor_1 =student_professor_item_summary[1:NP,]
professor_2_per =student_professor_performance_item_summary[1:NP,]
professor_fit_data = cbind(professor_1,professor_2_per)

prof1 = student_professor_item_summary[1:NP,1]
prof2 = student_professor_performance_item_summary[1:NP,1]
plot(prof1,prof2)
cor(prof1,prof2,method="kendall")
#0.9017705

stud1 = student_professor_item_summary[(NP+1):(NP+NS),]
stud2 = student_professor_performance_item_summary[(NP+1):(NP+NS),]

plot(stud1[,1],stud2[,1])
cor(stud1[,1],stud2[,1],method="kendall")
#0.8293316

# read recode data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
datos = read.csv("D3_Complete_Recode_Data_Sample.txt",header=TRUE,sep=";")
View(datos)
#order by student id
data = datos[order(datos[,2]),]

cor(stud1[,1], data[,5],method = "kendall")
#-0.2012666
cor(stud2[,1], data[,5],method = "kendall")
#-0.01041535