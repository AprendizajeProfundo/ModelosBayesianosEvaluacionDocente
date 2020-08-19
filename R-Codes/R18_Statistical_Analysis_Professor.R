#
############################################################################
#  Anlaysis of professor latent traits
# By 
# A. Montenegro, Universidad Nacional dea Colombia
# H. Rosas, UNiversidad de Valparaíso
# C.      , Universidad Nacional dea Colombia
# J.      , Universidad Nacional dea Colombia
#
# Date: March, 2018
#
# Source. Data from a survey applied to students of Facultad de Ciencias
# to evaluate the Performance of the profesors in classroom
# Date 2015-2
#
# Output
# In this script we analyze the output data from the prliminary fit.
# Descritive simple statistics and ggplots
#

#
##################################################################
#
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# Read the original sample data
# professor data
# replace the next line according to your file system
path = "D:/Alvaro/Alvaro_2018/Papers/Eval_IRT_Facet/Suplementary Material/Data"
setwd(path)
# professor fit data
professor_data= read.csv2(file="D27_professor_fit_data_completed.txt", header=TRUE,sep=";",dec=",")
# item data
item_data =read.csv2(file="D12_beta_professor_fit_dat.txt", header=TRUE,sep=";",dec=",")
beta_data = item_data[1:45,]
# sort the dataframes by  Id_Pro_Stan
professor_data = professor_data[order(professor_data[,3]),]

# correlation between Harmonic_Mean and Prof_index
theta = professor_data[,4]
rho  =  professor_data[,24]
cor(theta,rho)
#  0.95204443, not valid
cor(theta,rho,method="kendall")
# 0.8451339


#############################################################
# ggplots
#############################################################
library(ggplot2)
#library(grid)
#library(gridExtra)
Order = 1:nrow(professor_data)
# sort the dataframes by  Professor'_Index's performance
professor_data = professor_data[order(professor_data[,4]),]
# create data frame to ggplots
professor_plot_data = cbind.data.frame(Order,professor_data[,c(1,3:4,24,30,17,19,21)], substr(professor_data[,30], 1, 4))
colnames(professor_plot_data) = c("Order","Parameter","Professor","Performance","Harmonic_Mean","Department","q_025","Median","q_975","Dep")



# Plot 1. Distribution of Professor Performance (Latent Trait)
p1 = ggplot(data=professor_plot_data,aes(Performance))
p1 = p1 + geom_histogram(binwidth=0.5,color="black", fill="yellow")
p1 = p1 + labs(x="Performance",y="Frequency")
p1 = p1 + ggtitle("Performance distribution")
#p1
  
# plot 2. Professor's performance with 95%-credibility bands
p2 = ggplot(data=professor_plot_data, aes(x=Order, y=Performance)) 
p2 = p2 + geom_point() + geom_errorbar(aes(ymin=q_025, ymax=q_975,col="red"))   
p2 = p2 + ggtitle("95% Credibility bands")  
p2 = p2 + xlab("Professor")
p2 = p2 + theme(legend.position="none" ) # to omit the legend
#p2

# plot 3. Boxplots by Department
p3 = ggplot(data=professor_plot_data, aes(x= Dep,y=Performance,group=Department,fill=Department))
p3 = p3 + geom_boxplot() + stat_boxplot(geom="errorbar")
p3 = p3 + ggtitle("Performance by department") 
p3 = p3 + labs(x="Department")
p3 = p3 + theme(legend.position="none",axis.text.x = element_text(angle=90, hjust=1) ) # to omit the legend
#p3

# plot 4. Comparing with harmonic mean
#p4 = ggplot(data=professor_plot_data, aes(x= Performance,y=Harmonic_Mean, color =1))
#p4 = p4 + geom_point() +geom_smooth(method="lm") 
#p4 = p4 + ggtitle("Comparison with the harmonic mean") 
#p4 = p4 + labs(y="Harmonic mean")
#p4 = p4 + theme(legend.position="none")
#p4 = p4 + annotate("text",x=2,y=1.55,label="correlation = 0.976",colour = "red")
#p4

# plot 4 beta params plot
# item professor
n_item =15
k  = 3

item_prof=c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,
            6,6,6,7,7,7,8,8,8,9,9,9,10,10,10,
            11,11,11,12,12,12,13,13,13,14,14,14,15,15,15)

item_professor = data.frame(item_data[1:45,2], item_prof)
names(item_professor) = c("beta_k","item")
#
p4 = ggplot(data=item_professor, aes(x= beta_k,y=item,group=item))
for (i in 1:15){
  p4 = p4 +geom_hline(yintercept=i,alpha=0.5,colour="blue")
}
p4 = p4 + ggtitle("Item params in professor measurement") 
p4 = p4+ geom_point(size =1,colour="red") 
p4 = p4 + xlim(-5.5,2)
p4 = p4 + labs(x="beta-parameters")
#p4

multiplot(p1,p3,p2,p4,cols=2)

