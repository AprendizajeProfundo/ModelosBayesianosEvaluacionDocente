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
# Data Preparation
# In this script we recode re resposes to have less categories,
# according to the preliminary analysis
# Also we recode the data in vector form to use in Stan
# Inside the code, we explain the construction
#
# Data description
# student data
# datos[,1]: Id_Student. This is the key of the table
# datos[,2]: student sex
# datos[,3]: Id_Fingerprint. Groups of simmilar characteristics
# datos[,4]: Id_Carrer. Id of the student carrer
# datos[,5]: Number_Enrollment
# datos[,6]: Number_Repetition
# datos[,7]: Number_Lost
# datos[,8]: Self_Perfomance
# subject data
# datos[,9]: Id_Subject
# datos[,10]: Id_Course
# datos[,11]: Group
# datos[,12]: Shared. 1= shared course; 0 = no shared course
# professor data
# datos[,13]: Id_professor. Id of the professor of the course
# datos[,14]: professor_sex. Id of the professor of the course
# datos[,15]: Vinculacion prof: Planta, ocasional
# datos[,16]: Fecha_Ingreso UN
# datos[,17]: professor_cat: category of professor
# datos[,18]: professor_dedic: dedication of the prof.
# datos[,19]: professor_Dept: Departament of the prof.
# item responses
# datos[,20:43]: items: C_12 to C_35; professor: C_12:C_17; C_22:C_28; C_34;
#                                       subject: C_18:C_21; C_35
#                            Physical resources: C_29:C31
#                                       Monitor: C_32:C_33
# textual responses 
# datos[,44:45]: questions C_36 (Strength) and C_37 (Weaknesses)
#
##################################################################
#
# Read the original sample data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
# Use this sample for experiments
# 1380  registers, corresponding to 123 subjects, with 4 to 20 registers, 
# 36 carrers with 3 to 223 registers, 124 professors wit 3 to 36 registers.  
datos = read.csv("D2_Sample_Data.txt",header=TRUE,sep=";")
head(datos)
#
# order the data by the key(Id student) for easy and put in a new container
data = datos[order(datos[,1]),]
#
# extract the category responses
Y = as.matrix(data[,20:43]) # 1380 x 24
# Item topics
idx_prof = c(1:7,11:17,23) # 14 items
idx_sub =  c(8:10,24) # 5 items
idx_phy =  c(18:20) # 3 items
idx_mon =  c(21,22) # 2 items
# 
#
# compute the response frequency to define the number of categories by item
#name_es = c( "C_12","C_13","C_14","C_15",
#             "C_16","C_17","C_18","C_19","C_20",
#             "C_21", "C_22", "C_23", "C_24" ,"C_25",         
#             "C_26","C_27","C_28","C_29","C_30",
#             "C_31","C_32","C_33","C_34","C_35")
#names(Y)=name_es
#id1 =c(1:20,23,24)
#n1 = Y[,id1]
#frq= matrix(0,ncol(id1),5)
#for (i in 1:length(id1)){
#  frq[i,]= as.array(table(n1[,i]))
#}
#table(Y[,21])
#table(Y[,22])
#frq

#     [,1] [,2] [,3] [,4] [,5]  id  Topic categories
#[1,]    5   26   85  332  927  12  pr    4 or 3
#[2,]    4   20   70  299  985  13  pr    4 or 3
#[3,]   21   26   90  253  988  14  pr    4 or 3
#[4,]   12   27   78  303  957  15  pr    4 or 3
#[5,]   26   54  128  364  803  16  pr    4 or 3
#[6,]   12   23   90  296  947  17  pr    4 or 3
#[7,]    9   20  176  551  624  18  pr    4 or 3
#[8,]    6   15  132  425  801  19  su    4 or 3
#[9,]    5   11   80  350  933  20  su    4 or 3
#[10,]   26   49  216  442  647 21  pr    4 or 3
#[11,]  307  130  200  300  432 22  su    4 or 5
#[12,]   83  100  252  382  558 23  pr    4 or 5 # doubt
#[13,]   25   43  180  408  716 24  pr    4 or 3
#[14,]    8   19  130  418  794 25  pr    4 or 3
#[15,]   96   67  134  286  792 26  pr    4 or 5  # doubt 
#[16,]   84   90  191  402  610 27  pr    4 or 5  # doubt 
#[17,]   46   44  108  313  858 28  pr    4 or 5  # doubt
#[18,]   29   39  248  729  331 29  re    4 or 5 # physical resources
#[19,]   23   52  255  685  364 30  re    4 or 5 # physical resources
#[20,]   13   39  105  571  648 31  re    4 or 5 # physical resources
#[21,]  290 1087                32  mo    2 # monitor (binary)
#[22,]  623  752                33  mo    2 # monitor (binary)
#[23,]   10   22  100  562  675 34  pr    4 or 3 # global professor eval
#[24,]   13   23  139  602  588 35  su    4 or 3 # global subject eval
#
# recode the responses (to have less categories)
# items 1:20,23:24 in four categories


iidx = c(1:20,23:24)
for (i in iidx){
  z = Y[,i]
  z = ifelse(z==1 |z==2,1,z)
  z = ifelse(z==3,2,z)
  z = ifelse(z==4,3,z)
  z = ifelse(z==5,4,z)
  Y[,i]=z
} 
################################################
# save recode reponse data (optional)
################################################
#data[,20:43] = Y
#setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
#write.csv2(data,file="D3_Sample_Data_recode.txt", row.names = FALSE)

# subsets of data
# professor
y_prof = Y[,idx_prof]
# subject 
y_sub  = Y[,idx_sub]
# physical resources
y_phy  = Y[,idx_phy ]

# PCA analysis
library(FactoMineR)
# professor
y_prof = Y[,idx_prof]
ACP_prof<- PCA(y_prof, graph=TRUE)
par(mfrow=c(1,2))
barplot(ACP_prof$eig[,1]) # max 2-dimension, is a scale factor
#CMC 
# requires complete data
# select the registers without NA's
nana = numeric()
for (j in 1:ncol(y_prof)){
  nana = union(nana,which(is.na(y_prof[,j])))
}
nana=unique(nana)
y_prof_n =y_prof[-nana,]

# plot in the same figure than the barplot
Curva.alpha<- CMC::alpha.curve(y_prof_n)
ltm::cronbach.alpha(y_prof_n)

# subject 
y_sub = Y[,idx_sub]
ACP_sub<- PCA(y_sub, graph=TRUE)
barplot(ACP_sub$eig[,1]) # 1-dimension
nana = numeric()
for (j in 1:ncol(y_sub)){
  nana = union(nana,which(is.na(y_sub[,j])))
}
nana=unique(nana)
y_sub_n =y_sub[-nana,]
Curva.alpha <- CMC::alpha.curve(y_sub_n)
ltm::cronbach.alpha(y_sub_n)

# Making correct indices to Stan
# first order the dataframe by Id_Student which is a primary key.
# data = datos[order(datos[,1]),] #done previously
# only consider student, professor, subject and selfperformance

# original indices
Id_Stu_or = sort(unique(data$Id_Student))
Id_Pro_or = sort(unique(data$Id_Professor))
Id_Sub_or = sort(unique(data$Id_Subject))
Id_Sel_or = sort(unique(data$Self_Perfomance))

#
L1 = length(Id_Stu_or) # 1380
L2 = length(Id_Pro_or) # 124
L3 = length(Id_Sub_or) # 123
L4 = length(Id_Sel_or)# 5


#new indices
Id_Stu_new = 1:L1
Id_Pro_new = 1:L2
Id_Sub_new = 1:L3
Id_Sel_new = 1:L4


# new data
N= nrow(Y)
Student   = c(Id_Stu_new)
Professor = array(dim=N)
Subject   = array(dim=N)
SelfPerf  = array(dim=N)
#
for(i in 1:L2){
  w = which(data$Id_Professor==Id_Pro_or[i])
  Professor[w] = Id_Pro_new[i]
}

for(i in 1:L3){
  w = which(data$Id_Subject==Id_Sub_or[i])
  Subject[w] = Id_Sub_new[i]
}


for(i in 1:L4){
  w = which(data$Self_Perfomance ==Id_Sel_or[i])
  SelfPerf[w] = Id_Sel_new[i]
}

#
# complete recode data
key_Student=Id_Stu_or
complete_data = cbind(key_Student,Student, Professor, Subject,SelfPerf,Y, datos[,c(44:45,2:7, 10:12, 14:19)])
# save the data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(complete_data,file="D3_Complete_Recode_Data_Sample.txt", row.names = FALSE)

#
# profesor data
# Preparing data in vectors
# number of items
N = nrow(y_prof) #1380
J = length(idx_prof) #15
Student   = c(Id_Stu_new)# 20700
Student   = rep(Student,J)#20700
Professor = rep(Professor,J)#20700
SelfPerf_pr  = rep(SelfPerf,J)#20700
Y_prof    = as.vector(y_prof) #  vectorized responses
Item = rep(1,N)
for(i in 2:J){
  Item = c(Item, rep(i,N))
}
# omit missing data
no.missing = which(!is.na(Y_prof))
Student   = Student[no.missing] #20606
Professor = Professor[no.missing]#20606
Item      = Item [no.missing]#20606
SelfPerf_pr  = SelfPerf_pr[no.missing]#20606
Y_prof    = Y_prof[no.missing]#20606
# 
professor_data = cbind(Student,Professor,Y_prof,SelfPerf_pr,Item) #20606   x  5, 15 items
#
# save the data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(professor_data,file="D4_professor_data_sample.txt", row.names = FALSE)
#


#
# subject data
# Preparing data in vectors
# number of items
N = nrow(y_sub) #1380
J = length(idx_sub) #4
Student   = c(Id_Stu_new)
Student   = rep(Student,J)
Subject = rep(Subject,J)
SelfPerf_su  = rep(SelfPerf,J)
Y_sub     = as.vector(y_sub) # vectorized responses

Item = rep(1,N)
for(i in 2:J){
  Item = c(Item, rep(i,N))
}

# missing data
no.missing = which(!is.na(Y_sub))
Student   = Student[no.missing]
Subject = Subject[no.missing]
Item      = Item [no.missing]
SelfPerf_su =SelfPerf_su[no.missing]
Y_sub    = Y_sub[no.missing]
# 
subject_data = cbind(Student,Subject, Y_sub,SelfPerf_su,Item) #5503 rows by 5 cols, 4 items

# save the data
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")
write.csv2(subject_data,file="D5_subject_data_sample.txt", row.names = FALSE)
#
# Save the indices to recover original information
professor_indices = cbind(Id_Pro_or, Id_Pro_new)
student_indices   = cbind(Id_Stu_or,Id_Stu_new)
subject_indices   = cbind(Id_Sub_or,Id_Sub_new)
selfPerf_indices  = cbind(Id_Sel_or,Id_Sel_new)
#
setwd("D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data")

write.csv2(professor_indices,file="D6_professor_indices_sample.txt", row.names = FALSE)
write.csv2(student_indices,file="D7_student_indices_sample.txt", row.names = FALSE)
write.csv2(subject_indices,file="D8_subject_indices_sample.txt", row.names = FALSE)
write.csv2(selfPerf_indices,file="D9_selfPerf_indices_sample.txt", row.names = FALSE)

# end preparing data


