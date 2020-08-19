
#
############################################################################
# Sterategic diagram
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
#  Output
# Strategic Diagram
#
# Data description
# Subject_Professor_Index[,1]: Id_Subject
# Subject_Professor_Index[,2]: Subject_Name
# Subject_Professor_Index[,3]: Professor: Id Professor
# Subject_Professor_Index[,4]: Subj_Index: Subject Index
# Subject_Professor_Index[,5]: Prof_Index: Professor Index
# Subject_Professor_Index[,6]: Department: Department which offer the subject
# Subject_Professor_Index[,7]: Num_Stud: Number of students
# Subject_Professor_Index[,8]: Percent_H: Percent of males
##################################################################
#
# Read the original sample data
path = "D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data"
setwd(path)
Subject_Professor_Index = read.csv(file="D30_professor_subject_estimation.txt",header=TRUE, sep=";",dec = ",")
names(Subject_Professor_Index)
# "Professor"   "performance" "Subject"     "relevance"   

#order by subject index
Subject_Professor_Index =Subject_Professor_Index[order(Subject_Professor_Index[,4]),]


attach(Subject_Professor_Index)
cor(performance,relevance)
# 0.7108083
plot(relevance,performance,main=c("Strategic Diagram"),xlab=c("Subject Measure"),
                                   ylab=c("Professor Performance"))
#abline(0,0,col="red")
abline(h=0,v=0,col="red")
text (x=-2.2,y=0.2, label ="correlation = 0.71")
# quadrant numbers
xx = c(1.4,-1.3,-1.3,1.4)
yy = c(1.3,1.3,-1.3,-1.3)
lxy =c("I","II", "III","IV")
text(x=xx,y=yy, labels =lxy,col="red",cex=2)
# quadrant legends
xx = c(1.3,-1.3,-1.3,1.3)
yy = c(2.5,2.5,-2.5,-2.5)
lxy =c("high high","low high", "low low","high low")
text(x=xx,y=yy, labels =lxy,col="red",cex=1)

# subject legends
lxy =c("Mult. Desc. Stats; P080","Gen. Biology; P33 and P36",
         "Dif. Calculus; P45,P46", "Teaching chemistry; P74")
xx=c(1.6,-0.5,-2.7,1.15)
yy = c(2.8,1.55,-2.2,-0.85)
text(x=xx,y=yy, labels =lxy,col="black",cex=0.8)

detach(Subject_Professor_Index)



#
########################################################################3
# Dynamic location
#library(manipulate)
#identify(Prof_Index,Subj_Index, labels=Subject_Name) # identify points 
#coords <- locator(type="p") # add lines
#coords # display list


manipulate(
  plot(relevance,performance,main=c("Strategic Diagram"),xlab=c("Subject Index"),
       ylab=c("Professor Index"), xlim = c(-3.5, 3.5), type = type, ann = label),
  x.min = slider(-3.5, 3.5, step=0.5, initial = -3.5),
  x.max = slider(0, 3.5, step=0.5, initial = 3.5),
  type = picker("Points" = "p", "Line" = "l", "Step" = "s"),
  label = checkbox(TRUE, "Draw Labels"))


# Interacting with a scatterplot 
#attach(mydata)
#plot(x, y) # scatterplot
#identify(x, y, labels=row.names(mydata)) # identify points 
#coords <- locator(type="l") # add lines
#coords # display list
