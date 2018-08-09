library(ggpubr)
library(ggplot2)
library(readr)
gapData <- read_csv("D:/rStudio/caprarola/gapDataWithin.csv")
necromassDf <- read_csv("D:/rStudio/caprarola/deadwood caprarola_simple.csv")
gapData<-merge(gapData,necromassDf[1:2], by="Plot")
df1<-subset(gapData, Area_m2>=1)
df2<-subset(gapData, Area_m2>=2)

###############################
# Plot one of the patch metrics
##############################
ggdensity(df2, "Roundness")
#############################
names(df1)[names(df1) == "Forest Type"] <- "ForType"
names(df2)[names(df2) == "Forest Type"] <- "ForType"
names(gapData)[names(gapData) == "Forest Type"] <- "ForType"
df3<-gapData
ggdensity(df1, "Area_m2", fill = "ForType")

f2<-subset(df2,ForType=="Fagus")
st.f2<-summary(f2$Area_m2)
a.f2<-paste(names(st.f2), format(st.f2, digits = 2), collapse = "\n")
q2<-subset(df2,ForType=="Quercus")
st.q2<-summary(q2$Area_m2)
a.q2<-paste(names(st.q2), format(st.q2, digits = 2), collapse = "\n")
m2<-subset(df2,ForType=="Mixed")
st.m2<-summary(m2$Area_m2)
a.m2<-paste(names(st.m2), format(st.m2, digits = 2), collapse = "\n")

f1<-subset(df1,ForType=="Fagus")
st.f1<-summary(f1$Area_m2)
a.f1<-paste(names(st.f1), format(st.f1, digits = 2), collapse = "\n")
q1<-subset(df1,ForType=="Quercus")
st.q1<-summary(q1$Area_m2)
a.q1<-paste(names(st.q1), format(st.q1, digits = 2), collapse = "\n")
m1<-subset(df1,ForType=="Mixed")
st.m1<-summary(m1$Area_m2)
a.m1<-paste(names(st.m1), format(st.m1, digits = 2), collapse = "\n")


gg1<-ggdensity(df1, "Area_m2", fill = "ForType", color = "ForType", alpha = 0.3 )+ 
  guides(color=FALSE)+
  scale_fill_discrete(labels=c("Fagus Forest","Mixed Forest","Quercus Forest"))+
  labs(xlab(bquote('Area ('*m^2*')'))) + 
  annotate("text",x=10,y=.15,label=a.f1,hjust=0)+
  annotate("text",x=28,y=.15,label=a.m1,hjust=0)+
  annotate("text",x=46,y=.15,label=a.q1,hjust=0)+
  annotate("text",x=10,y=.29,label="Fagus",hjust=0,size=5)+
  annotate("text",x=28,y=.29,label="Mixed",hjust=0,size=5)+
  annotate("text",x=46,y=.29,label="Quercus",hjust=0,size=5)+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.text = element_text(size = rel(1.5)),
        legend.position = c(.2,.5),
        axis.title = element_text(face = "bold",size = rel(1.2)),
        axis.text = element_text(size = rel(1.1)))

gg2<-ggdensity(df2, "Area_m2", fill = "ForType", color = "ForType" ,alpha = 0.3)+ 
  guides(color=FALSE)+
  scale_fill_discrete(labels=c("Fagus Forest","Mixed Forest","Quercus Forest"))+
  labs(xlab(bquote('Area ('*m^2*')'))) + 
  annotate("text",x=10,y=.13,label=a.f2,hjust=0)+
  annotate("text",x=28,y=.13,label=a.m2,hjust=0)+
  annotate("text",x=46,y=.13,label=a.q2,hjust=0)+
  annotate("text",x=10,y=.22,label="Fagus",hjust=0,size=5)+
  annotate("text",x=28,y=.22,label="Mixed",hjust=0,size=5)+
  annotate("text",x=46,y=.22,label="Quercus",hjust=0,size=5)+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = rel(1.5)),
        legend.direction = "horizontal",
        legend.position = c(.2,.5),
        axis.title = element_text(face = "bold",size = rel(1.2)),
        axis.text = element_text(size = rel(1.1)))

png("D:/densisty3.png",res = 300,width = 2800,height = 2000)
ggarrange(gg1,gg2,nrow = 2,labels = c("A","B"),common.legend = T,legend = "bottom")+
  theme(legend.text = element_text(size = rel(1.8)))
dev.off()

sum_all_within.1m <- read_csv("GapData/within_1m2/all_within_1m2_sum.csv")
sum_all_within.1m<-sum_all_within.1m[,-c(1,21)]
colnames(sum_all_within.1m)<-c("SUM_Area_m2","SUM_Border_length","SUM_Lenght","SUM_Width",
  "SUM_Border_Index","SUM_GSCI","SUM_PFD","SUM_FD","SUM_FDI","SUM_Asymmetry",
  "SUM_Elliptic_Fit","SUM_Density","SUM_RLE",
  "SUM_Rect_fit","SUM_LengthWidt","SUM_RSE",
  "SUM_Shape_index","SUM_Compactnes","SUM_Roundness")

cv_all_within.1m<-read_csv("GapData/within_1m2/all_within_1m2_cv.csv")
cv_all_within.1m<-cv_all_within.1m[,-c(1,21)]
colnames(cv_all_within.1m)<- c("cv_Area_m2","cv_Border_length","cv_Length","cv_Width",
  "cv_Border_Index","cv_GSCI","cv_PFD","cv_FD","cv_FDI","cv_Asymmetry",
  "cv_Elliptic_Fit","cv_Density","cv_RLE",
  "cv_Rect_fit","cv_LengthWidt","cv_RSE",
  "cv_Shape_index","cv_Compactnes","cv_Roundness")
install.packages("corrplot")
library(corrplot)
corrplot(cor(cv_all_within.1m, use = "pairwise.complete.obs"), method = "circle")
corrplot(cor(sum_all_within.1m, use = "pairwise.complete.obs"), method = "circle")

png(filename="D:/myname.png")

dev.off()

####Models
sum_all_within.1m <- read_csv("GapData/within_1m2/all_within_1m2_sum.csv")
mean_all_within.1m<-read_csv("GapData/within_1m2/all_within_1m2_mean.csv")
sd_all_within.1m<-read_csv("GapData/within_1m2/all_within_1m2_sd.csv")
median_all_within.1m<-read.csv("GapData/within_1m2/all_within_1m2_median.csv")
cv_all_within.1m<-read_csv("GapData/within_1m2/all_within_1m2_cv.csv")

sum_all_within.2m <- read_csv("D:/rStudio/caprarola/Results correlation/Necromass/within_2m2/all_within_sum.csv")
mean_all_within.2m<-read_csv("D:/rStudio/caprarola/Results correlation/Necromass/within_2m2/all_within_mean.csv")
sd_all_within.2m<-read_csv("D:/rStudio/caprarola/Results correlation/Necromass/within_2m2/all_within_sd.csv")
median_all_within.2m<-read.csv( "D:/rStudio/caprarola/Results correlation/Necromass/within_2m2/all_within_median.csv")
cv_all_within.2m<-read_csv("D:/rStudio/caprarola/Results correlation/Necromass/within_2m2/all_within_cv.csv")
#######

namesAvg<-c("avg_Area_m2","avg_Border_length","avg_Lenght","avg_Width",
            "avg_Border_Index`","avg_GSCI","avg_PFD","avg_FD","avg_FDI","avg_Asymmetry",
            "avg_Elliptic_Fit","avg_Density","avg_RLE",
            "avg_Rect_fit","avg_LengthWidt","avg_RSE",
            "avg_Shape_index","avg_Compactnes","avg_Roundness")
namesCv<-c("cv_Area_m2","cv_Border_length","cv_Length","cv_Width",
           "cv_Border_Index","cv_GSCI","cv_PFD","cv_FD","cv_FDI","cv_Asymmetry",
           "cv_Elliptic_Fit","cv_Density","cv_RLE",
           "cv_Rect_fit","cv_LengthWidt","cv_RSE",
           "cv_Shape_index","cv_Compactnes","cv_Roundness")
namesSd<-c("SD_Area_m2","SD_Border_length","SD_Lenght","SD_Width",
           "SD_Border_Index","SD_GSCI","SD_PFD","SD_FD","SD_FDI","SD_Asymmetry",
           "SD_Elliptic_Fit","SD_Density","SD_RLE",
           "SD_Rect_fit","SD_LengthWidt","SD_RSE",
           "SD_Shape_index","SD_Compactnes","SD_Roundness")
namesMdn<-c("mdn_Area_m2","mdn_Border_length","mdn_Lenght","mdn_Width",
            "mdn_Border_Index","mdn_GSCI","mdn_PFD","mdn_FD","mdn_FDI","mdn_Asymmetry",
            "mdn_Elliptic_Fit","mdn_Density","mdn_RLE",
            "mdn_Rect_fit","mdn_LengthWidt","mdn_RSE",
            "mdn_Shape_index","mdn_Compactnes","mdn_Roundness")
namesSum<-c("SUM_Area_m2","SUM_Border_length","SUM_Lenght","SUM_Width",
            "SUM_Border_Index`","SUM_GSCI","SUM_PFD","SUM_FD","SUM_FDI","SUM_Asymmetry",
            "SUM_Elliptic_Fit","SUM_Density","SUM_RLE",
            "SUM_Rect_fit","SUM_LengthWidt","SUM_RSE",
            "SUM_Shape_index","SUM_Compactnes","SUM_Roundness")

cv_all_within.1m<-cv_all_within.1m[,-c(1,21)]
colnames(cv_all_within.1m)<-namesCv 
sd_all_within.1m<-sd_all_within.1m[,-c(1,21)]
colnames(sd_all_within.1m)<-namesSd
sum_all_within.1m<-sum_all_within.1m[,-c(1,21)]
colnames(sum_all_within.1m)<-namesSum
mean_all_within.1m<-mean_all_within.1m[,-c(1,21)]
colnames(mean_all_within.1m)<-namesAvg
median_all_within.1m<-median_all_within.1m[,-c(1,21)]
colnames(median_all_within.1m)<-namesMdn

patchMetric.1<-cbind(mean_all_within.1m,median_all_within.1m,sum_all_within.1m,
                     sd_all_within.1m,cv_all_within.1m)
rm(mean_all_within.1m,median_all_within.1m,sum_all_within.1m,
   sd_all_within.1m,cv_all_within.1m)

cv_all_within.2m<-cv_all_within.2m[,-c(1,21)]
colnames(cv_all_within.2m)<-namesCv 
sd_all_within.2m<-sd_all_within.2m[,-c(1,21)]
colnames(sd_all_within.2m)<-namesSd
sum_all_within.2m<-sum_all_within.2m[,-c(1,21)]
colnames(sum_all_within.2m)<-namesSum
mean_all_within.2m<-mean_all_within.2m[,-c(1,21)]
colnames(mean_all_within.2m)<-namesAvg
median_all_within.2m<-median_all_within.2m[,-c(1,21)]
colnames(median_all_within.2m)<-namesMdn

patchMetric.2<-cbind(mean_all_within.2m,median_all_within.2m,sum_all_within.2m,
                     sd_all_within.2m,cv_all_within.2m)
rm(mean_all_within.2m,median_all_within.2m,sum_all_within.2m,
   sd_all_within.2m,cv_all_within.2m)

Undestorey <- read_csv("D:/rStudio/caprarola/understory_final.csv")

reg<-cbind(Undestorey,patchMetric.1)
reg2<-cbind(Undestorey,patchMetric.2)
#Fagus
reg.f1<-subset(reg,`Forest Type`=="Fagus")
reg.f2<-subset(reg2,`Forest Type`=="Fagus")
mf1<-lm(N_SPECIES~cv_Roundness, data = reg.f1)
mf2<-lm(I_SHANNON~SD_Roundness, data = reg.f1)
mf3<-lm(I_PIELOU~mdn_PFD, data = reg.f1)

mf4<-lm(MEAN_HTOT~cv_Length, data = reg.f2)

#Quercus
reg.q1<-subset(reg,`Forest Type`=="Quercus")
reg.q2<-subset(reg2,`Forest Type`=="Quercus")
mq1<-lm(N_PLANTS~mdn_GSCI, data = reg.q1)
mq2<-lm(MEAN_DBH~avg_Rect_fit, data = reg.q1)
mq3<-lm(I_PIELOU~SD_Density, data = reg.q1)
mq4<-lm(V_TOT~mdn_Asymmetry, data = reg.q1)
mq5<-lm(G_TOT~avg_Asymmetry, data = reg.q1)

mq6<-lm(N_SPECIES~SD_Rect_fit, data = reg.q2)
mq7<-lm(I_SHANNON~SD_Rect_fit, data = reg.q2)
mq8<-lm(MEAN_HTOT~mdn_Border_Index, data = reg.q2)

###Mixed
reg.m1<-subset(reg,`Forest Type`=="Mixed")
reg.m2<-subset(reg2,`Forest Type`=="Mixed")
mm1<-lm(N_SPECIES~mdn_RSE, data = reg.m1)
mm2<-lm(I_SHANNON~mdn_RSE, data = reg.m1)

mm3<-lm(N_PLANTS~avg_Roundness, data = reg.m2)
mm4<-lm(I_PIELOU~avg_RLE, data = reg.m2)
mm5<-lm(MEAN_HTOT~avg_Asymmetry, data = reg.m2)
mm6<-lm(MEAN_DBH~SUM_Width, data = reg.m2)
mm7<-lm(G_TOT~SD_Asymmetry, data = reg.m2)
mm8<-lm(V_TOT~avg_Compactnes, data = reg.m2)

# Put model estimates into temporary data.frames:
model1F <- data.frame(Variable = rownames(summary(mf1)$coef),
                          Coefficient = summary(mf1)$coef[, 1],
                          SE = summary(mf1)$coef[, 2],
                          modelName = "Fagus",
                          Parm=as.character(mf1$terms[[2]]))
model2F <- data.frame(Variable = rownames(summary(mf2)$coef),
                          Coefficient = summary(mf2)$coef[, 1],
                          SE = summary(mf2)$coef[, 2],
                          modelName = "Fagus",
                          Parm=as.character (mf2$terms[[2]]))
model3F <- data.frame(Variable = rownames(summary(mf3)$coef),
                          Coefficient = summary(mf3)$coef[, 1],
                          SE = summary(mf3)$coef[, 2],
                          modelName = "Fagus",
                          Parm=as.character(mf3$terms[[2]]))
model4F <- data.frame(Variable = rownames(summary(mf4)$coef),
                      Coefficient = summary(mf4)$coef[, 1],
                      SE = summary(mf4)$coef[, 2],
                      modelName = "Fagus",
                      Parm=as.character(mf4$terms[[2]]))

model1q <- data.frame(Variable = rownames(summary(mq1)$coef),
                      Coefficient = summary(mq1)$coef[, 1],
                      SE = summary(mq1)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq1$terms[[2]]))
model2q <- data.frame(Variable = rownames(summary(mq2)$coef),
                      Coefficient = summary(mq2)$coef[, 1],
                      SE = summary(mq2)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq2$terms[[2]]))
model3q <- data.frame(Variable = rownames(summary(mq3)$coef),
                      Coefficient = summary(mq3)$coef[, 1],
                      SE = summary(mq3)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq3$terms[[2]]))
model4q <- data.frame(Variable = rownames(summary(mq4)$coef),
                      Coefficient = summary(mq4)$coef[, 1],
                      SE = summary(mq4)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq4$terms[[2]]))
model5q <- data.frame(Variable = rownames(summary(mq5)$coef),
                      Coefficient = summary(mq5)$coef[, 1],
                      SE = summary(mq5)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq5$terms[[2]]))
model6q <- data.frame(Variable = rownames(summary(mq6)$coef),
                      Coefficient = summary(mq6)$coef[, 1],
                      SE = summary(mq6)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq6$terms[[2]]))
model7q <- data.frame(Variable = rownames(summary(mq7)$coef),
                      Coefficient = summary(mq7)$coef[, 1],
                      SE = summary(mq7)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq7$terms[[2]]))
model8q <- data.frame(Variable = rownames(summary(mq8)$coef),
                      Coefficient = summary(mq8)$coef[, 1],
                      SE = summary(mq8)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mq8$terms[[2]]))

model1m <- data.frame(Variable = rownames(summary(mm1)$coef),
                      Coefficient = summary(mm1)$coef[, 1],
                      SE = summary(mm1)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm1$terms[[2]]))
model2m <- data.frame(Variable = rownames(summary(mm2)$coef),
                      Coefficient = summary(mm2)$coef[, 1],
                      SE = summary(mm2)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm2$terms[[2]]))
model3m <- data.frame(Variable = rownames(summary(mm3)$coef),
                      Coefficient = summary(mm3)$coef[, 1],
                      SE = summary(mm3)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm3$terms[[2]]))
model4m <- data.frame(Variable = rownames(summary(mm4)$coef),
                      Coefficient = summary(mm4)$coef[, 1],
                      SE = summary(mm4)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm4$terms[[2]]))
model5m <- data.frame(Variable = rownames(summary(mm5)$coef),
                      Coefficient = summary(mm5)$coef[, 1],
                      SE = summary(mm5)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm5$terms[[2]]))
model6m <- data.frame(Variable = rownames(summary(mm6)$coef),
                      Coefficient = summary(mm6)$coef[, 1],
                      SE = summary(mm6)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm6$terms[[2]]))
model7m <- data.frame(Variable = rownames(summary(mm7)$coef),
                      Coefficient = summary(mm7)$coef[, 1],
                      SE = summary(mm7)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm7$terms[[2]]))
model8m <- data.frame(Variable = rownames(summary(mm8)$coef),
                      Coefficient = summary(mm8)$coef[, 1],
                      SE = summary(mm8)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mm8$terms[[2]]))


# Combine these data.frames
allModelFrame <- data.frame(rbind(model1F, model2F, model3F,model4F,
                                  model1m,model2m,model3m,model4m,model5m,model6m,
                                  model7m,model8m,
                                  model1q, model2q, model3q,model4q,model5q,model6q,
                                  model7q,model8q))  # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
rm(zp1)
zp1 <- ggplot(allModelFrame, aes(colour = modelName)) + guides(fill = F)
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
# zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
#                                 ymax = Coefficient + SE*interval1),
#                             lwd = 1, position = position_dodge(width = .5))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE,
                                 ymax = Coefficient + SE, shape = modelName),
                             size=1, position = position_dodge(width = .5),
                             fatten = 2.5)+
  #labs(aesthetic = "Forest Type")+
  facet_wrap(~Parm, scales = "free")
  #scale_fill_discrete(name="Forest Type")
zp1 <- zp1 + coord_flip() + 
  xlab("Gap Patch Metrics")+ylab("")+
  scale_shape_discrete(name="Forest Type")+
  scale_color_discrete(name="Forest Type")+
  theme_bw()+
  theme(legend.position = c(.85,.15),
        legend.title = element_text(face = "bold",size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        axis.line = element_line(size=.5, colour = "black"),
        axis.text=element_text(colour="black", size = rel(1.1)),
        axis.title = element_text(face = "bold",size = rel(1.3)),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(size = rel(1.2),colour = "black"),
        plot.title = element_text(family = "xkcd-Regular"),
        text=element_text(family="xkcd-Regular"))
png(filename ="D:/under_regression.png", height=2000, width=3000,res=300 )
print(zp1) # The trick to these is position_dodge().
dev.off()

################
linvingDf <- read_csv("D:/rStudio/caprarola/living_trees_final.csv")

regL1<-cbind(linvingDf,patchMetric.1)
regL2<-cbind(linvingDf,patchMetric.2)
#Fagus

regL.f2<-subset(regL2,`Forest Type`=="Fagus")
mLf1<-lm(HAB~cv_PFD, data = regL.f2)
mLf2<-lm(`%HAB`~avg_RSE, data = regL.f2)


#Quercus
regL.q1<-subset(regL1,`Forest Type`=="Quercus")
regL.q2<-subset(regL2,`Forest Type`=="Quercus")
mLq1<-lm(MEAN_HTOT~avg_Rect_fit, data = regL.q1)
mLq2<-lm(MEAN_DBH~SUM_Rect_fit, data = regL.q1)

mLq3<-lm(`%HAB`~SUM_RLE, data = regL.q2)
mLq4<-lm(HAB~SUM_Rect_fit, data = regL.q2)
mLq5<-lm(I_PRETZSCH~SD_Density, data = regL.q2)
mLq6<-lm(N_SPECIES~SD_Rect_fit, data = regL.q2)
mLq7<-lm(I_SHANNON~cv_Density, data = regL.q2)
mLq8<-lm(I_MARGALEF~SD_Rect_fit, data = regL.q2)

###Mixed
regL.m1<-subset(regL1,`Forest Type`=="Mixed")
regL.m2<-subset(regL2,`Forest Type`=="Mixed")
mLm1<-lm(MEAN_HTOT~SD_Asymmetry, data = regL.m1)
mLm2<-lm(`%HAB`~avg_RSE, data = regL.m1)

mLm3<-lm(HAB~cv_Length, data = regL.m2)
mLm4<-lm(MEAN_DBH~avg_Roundness, data = regL.m2)
# mm5<-lm(MEAN_HTOT~avg_Asymmetry, data = reg.m2)
# mm6<-lm(MEAN_DBH~SUM_Width, data = reg.m2)
# mm7<-lm(G_TOT~SD_Asymmetry, data = reg.m2)
# mm8<-lm(V_TOT~avg_Compactnes, data = reg.m2)

# Put model estimates into temporary data.frames:
model1LF <- data.frame(Variable = rownames(summary(mLf1)$coef),
                      Coefficient = summary(mLf1)$coef[, 1],
                      SE = summary(mLf1)$coef[, 2],
                      modelName = "Fagus",
                      Parm=as.character(mLf1$terms[[2]]))
model2LF <- data.frame(Variable = rownames(summary(mLf2)$coef),
                      Coefficient = summary(mLf2)$coef[, 1],
                      SE = summary(mLf2)$coef[, 2],
                      modelName = "Fagus",
                      Parm=as.character (mLf2$terms[[2]]))
# model3F <- data.frame(Variable = rownames(summary(mf3)$coef),
#                       Coefficient = summary(mf3)$coef[, 1],
#                       SE = summary(mf3)$coef[, 2],
#                       modelName = "Fagus",
#                       Parm=as.character(mf3$terms[[2]]))
# model4F <- data.frame(Variable = rownames(summary(mf4)$coef),
#                       Coefficient = summary(mf4)$coef[, 1],
#                       SE = summary(mf4)$coef[, 2],
#                       modelName = "Fagus",
#                       Parm=as.character(mf4$terms[[2]]))

model1Lq <- data.frame(Variable = rownames(summary(mLq1)$coef),
                      Coefficient = summary(mLq1)$coef[, 1],
                      SE = summary(mLq1)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq1$terms[[2]]))
model2Lq <- data.frame(Variable = rownames(summary(mLq2)$coef),
                      Coefficient = summary(mLq2)$coef[, 1],
                      SE = summary(mLq2)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq2$terms[[2]]))
model3Lq <- data.frame(Variable = rownames(summary(mLq3)$coef),
                      Coefficient = summary(mLq3)$coef[, 1],
                      SE = summary(mLq3)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq3$terms[[2]]))
model4Lq <- data.frame(Variable = rownames(summary(mLq4)$coef),
                      Coefficient = summary(mLq4)$coef[, 1],
                      SE = summary(mLq4)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq4$terms[[2]]))
model5Lq <- data.frame(Variable = rownames(summary(mLq5)$coef),
                      Coefficient = summary(mLq5)$coef[, 1],
                      SE = summary(mLq5)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq5$terms[[2]]))
model6Lq <- data.frame(Variable = rownames(summary(mLq6)$coef),
                      Coefficient = summary(mLq6)$coef[, 1],
                      SE = summary(mLq6)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq6$terms[[2]]))
model7Lq <- data.frame(Variable = rownames(summary(mLq7)$coef),
                      Coefficient = summary(mLq7)$coef[, 1],
                      SE = summary(mLq7)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq7$terms[[2]]))
model8Lq <- data.frame(Variable = rownames(summary(mLq8)$coef),
                      Coefficient = summary(mLq8)$coef[, 1],
                      SE = summary(mLq8)$coef[, 2],
                      modelName = "Quercus",
                      Parm=as.character(mLq8$terms[[2]]))

model1Lm <- data.frame(Variable = rownames(summary(mLm1)$coef),
                      Coefficient = summary(mLm1)$coef[, 1],
                      SE = summary(mLm1)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mLm1$terms[[2]]))
model2Lm <- data.frame(Variable = rownames(summary(mLm2)$coef),
                      Coefficient = summary(mLm2)$coef[, 1],
                      SE = summary(mLm2)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mLm2$terms[[2]]))
model3Lm <- data.frame(Variable = rownames(summary(mLm3)$coef),
                      Coefficient = summary(mLm3)$coef[, 1],
                      SE = summary(mLm3)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mLm3$terms[[2]]))
model4Lm <- data.frame(Variable = rownames(summary(mLm4)$coef),
                      Coefficient = summary(mLm4)$coef[, 1],
                      SE = summary(mLm4)$coef[, 2],
                      modelName = "Mixed",
                      Parm=as.character(mLm4$terms[[2]]))
# model5m <- data.frame(Variable = rownames(summary(mm5)$coef),
#                       Coefficient = summary(mm5)$coef[, 1],
#                       SE = summary(mm5)$coef[, 2],
#                       modelName = "Mixed",
#                       Parm=as.character(mm5$terms[[2]]))
# model6m <- data.frame(Variable = rownames(summary(mm6)$coef),
#                       Coefficient = summary(mm6)$coef[, 1],
#                       SE = summary(mm6)$coef[, 2],
#                       modelName = "Mixed",
#                       Parm=as.character(mm6$terms[[2]]))
# model7m <- data.frame(Variable = rownames(summary(mm7)$coef),
#                       Coefficient = summary(mm7)$coef[, 1],
#                       SE = summary(mm7)$coef[, 2],
#                       modelName = "Mixed",
#                       Parm=as.character(mm7$terms[[2]]))
# model8m <- data.frame(Variable = rownames(summary(mm8)$coef),
#                       Coefficient = summary(mm8)$coef[, 1],
#                       SE = summary(mm8)$coef[, 2],
#                       modelName = "Mixed",
#                       Parm=as.character(mm8$terms[[2]]))


# Combine these data.frames
allModelFrameL <- data.frame(rbind(model1LF, model2LF,
                                  model1Lm,model2Lm,model3Lm,model4Lm,
                                  model1Lq, model2Lq, model3Lq,model4Lq,model5Lq,model6Lq,
                                  model7Lq,model8Lq))  # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot

zp2 <- ggplot(allModelFrameL, aes(colour = modelName))+
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+
  geom_pointrange(aes(shape = modelName, x = Variable, y = Coefficient, ymin = Coefficient - SE,
                                 ymax = Coefficient + SE),
                             size=1, position = position_dodge(width = .5),
                             fill = "WHITE", fatten = 2.5)+
  facet_wrap(~Parm, scales = "free", ncol = 3)+ coord_flip() + 
  xlab("Gap Patch Metrics")+ylab("")+
  scale_color_discrete(name="Forest Type")+
  scale_shape_discrete(name="Forest Type")+
  theme_bw()+
  theme(legend.position = c(.85,.15),
        strip.text = element_text(size = rel(1.4)),
        legend.title = element_text(face = "bold",size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        axis.line = element_line(size=.5, colour = "black"),
        axis.text=element_text(colour="black", size = rel(1.3)),
        axis.title = element_text(face = "bold",size = rel(1.3)),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
png(filename ="D:/regL1.png", height=2000, width=3000,res=300 )
print(zp2) # The trick to these is position_dodge().
dev.off()

##################### understorey
#plot(reg.q2$I_SHANNON,predict(mq7))
g1<-data.frame(actual=reg.q2$I_SHANNO,
                    predicted=predict(mq7),
                    variable="Shonnon")
g2<-data.frame(actual=reg.q1$MEAN_DBH,
                    predicted=predict(mq2),
                    variable="MEAN_DBH")
g3<-data.frame(actual=reg.m2$MEAN_HTOT,
                    predicted=predict(mm5),
                    variable="MEAN_HTOT")
g4<-data.frame(actual=reg.m2$N_PLANTS,
                    predicted=predict(mm3),
                    variable="N_PLANTS")
# #gg<-rbind(g1,g2,g3,g4)
# #l_df<-data.frame(variable=c("Shonnon","MEAN_DBH","MEAN_HTOT","N_PLANTS"),
#                  r2=c("R^2==0.52","R^2==0.60","R^2==0.87","R^2==0.62"),
#                  rmse=c("RMSE==0.21","RMSE==1.3","RMSE==0.43","RMSE==3.84"))
p1<-ggplot(g1,aes(x=actual,y=predicted))+
  geom_point(pch=19,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  xlab("Actual  Shannon Index")+
  ylab("Predicted  Shannon Index")+
  scale_x_continuous(breaks = seq(.5,2,.5))+
  scale_y_continuous(breaks = seq(.5,2,.5))+
  annotate("text",x=.72,y=1.68,label="Quercus", hjust=0, vjust=0)+
  annotate("text",x=.72,y=1.62,label="R^2==0.52", parse=T, hjust=0, vjust=0)+
  annotate("text",x=.72,y=1.55,label="RMSE==0.21", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

p2<-ggplot(g2,aes(x=actual,y=predicted))+
  geom_point(pch=19,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  # facet_wrap(~variable,scales = "free")+
  # geom_text(x=.72,y=1.6,aes(label=r2), data = l_df,parse = T,hjust=1, vjust=.5)+
  # geom_text(x=.72,y=1.5,aes(label=rmse), data = l_df,parse = T,hjust=0, vjust=0)+
  xlab("Actual MEAN_DBH")+
  ylab("Predicted MEAN_DBH")+
  scale_x_continuous(breaks = seq(5,13,2))+
  scale_y_continuous(breaks = seq(5,13,2))+
  annotate("text",x=5.5,y=12,label="Quercus", hjust=0, vjust=0)+
  annotate("text",x=5.5,y=11.5,label="R^2==0.60", parse=T, hjust=0, vjust=0)+
  annotate("text",x=5.5,y=11,label="RMSE==1.3", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

p3<-ggplot(g3,aes(x=actual,y=predicted))+
  geom_point(pch=1, size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  # facet_wrap(~variable,scales = "free")+
  # geom_text(x=.72,y=1.6,aes(label=r2), data = l_df,parse = T,hjust=1, vjust=.5)+
  # geom_text(x=.72,y=1.5,aes(label=rmse), data = l_df,parse = T,hjust=0, vjust=0)+
  xlab("Actual MEAN_HTOT")+
  ylab("Predicted MEAN_HTOT")+
  scale_x_continuous(breaks = seq(5,13,2))+
  scale_y_continuous(breaks = seq(5,13,2))+
  annotate("text",x=4.5,y=8.3,label="Mixed forest", hjust=0, vjust=0)+
  annotate("text",x=4.5,y=8,label="R^2==0.87", parse=T, hjust=0, vjust=0)+
  annotate("text",x=4.5,y=7.7,label="RMSE==0.43", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

p4<-ggplot(g4,aes(x=actual,y=predicted))+
  geom_point(pch=1,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  xlab("Actual N_PLANTS")+
  ylab("Predicted N_PLANTS")+
  annotate("text",x=3,y=21,label="Mixed forest", hjust=0, vjust=0)+
  annotate("text",x=3,y=19.5,label="R^2==0.62", parse=T, hjust=0, vjust=0)+
  annotate("text",x=3,y=18,label="RMSE==3.84", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))
####
p5<-ggarrange(p1,p2,p3,p4,nrow = 1,ncol=4,labels = c("A","B","C","D"))
png("D:/compar.png",res=300,width = 4000,height = 1000)
p5
dev.off()

svg("D:/svgfile.svg")
p5
dev.off()

for(i in 1:4) print(p,i)

####predict vs actual living trees
gL1<-data.frame(actual=regL.q2$HAB,
               predicted=predict(mLq4),
               variable="HAB")
gL2<-data.frame(actual=regL.q2$I_PRETZSCH,
               predicted=predict(mLq5),
               variable="PRETZSCH index")
gL3<-data.frame(actual=regL.m2$HAB,
               predicted=predict(mLm3),
               variable="HAB")
gL4<-data.frame(actual=regL.m1$`%HAB`,
               predicted=predict(mLm2),
               variable="% HAB")
gL5<-data.frame(actual=regL.m1$MEAN_HTOT,
                predicted=predict(mLm1),
                variable="MEAN_HTOT")




ggL1<-ggplot(gL1,aes(x=actual,y=predicted))+
  geom_point(pch=1,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  xlab("Actual HAB")+
  ylab("Predicted HAB")+
  annotate("text",x=1,y=21,label="Quercus forest", hjust=0, vjust=0)+
  annotate("text",x=1,y=19.5,label="R^2==0.52", parse=T, hjust=0, vjust=0)+
  annotate("text",x=1,y=18,label="RMSE==3.89", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

ggL2<-ggplot(gL2,aes(x=actual,y=predicted))+
  geom_point(pch=1,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  xlab("Actual I_PRETZSCH")+
  ylab("Predicted I_PRETZSCH")+
  scale_x_continuous(breaks = seq(1,2,.5))+
  scale_y_continuous(breaks = seq(1,2,.5))+
  annotate("text",x=1.3,y=2.4,label="Quercus forest", hjust=0, vjust=0)+
  annotate("text",x=1.3,y=2.3,label="R^2==0.74", parse=T, hjust=0, vjust=0)+
  annotate("text",x=1.3,y=2.2,label="RMSE==0.40", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

ggL3<-ggplot(gL3,aes(x=actual,y=predicted))+
  geom_point(pch=19,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  xlab("Actual HAB")+
  ylab("Predicted HAB")+
  scale_x_continuous(breaks = seq(2,14,4))+
  scale_y_continuous(breaks = seq(2,14,4))+
  annotate("text",x=1.2,y=12.6,label="Mixed forest", hjust=0, vjust=0)+
  annotate("text",x=1.2,y=11.5,label="R^2==0.79", parse=T, hjust=0, vjust=0)+
  annotate("text",x=1.2,y=10.4,label="RMSE==1.60", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

ggL4<-ggplot(gL4,aes(x=actual,y=predicted))+
  geom_point(pch=19,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  xlab("Actual % HAB")+
  ylab("Predicted % HAB")+
  scale_x_continuous(breaks = seq(0,80,20))+
  scale_y_continuous(breaks = seq(0,80,20))+
  annotate("text",x=5,y=67,label="Mixed forest", hjust=0, vjust=0)+
  annotate("text",x=5,y=63,label="R^2==0.64", parse=T, hjust=0, vjust=0)+
  annotate("text",x=5,y=59,label="RMSE==12.53", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

ggL5<-ggplot(gL5,aes(x=actual,y=predicted))+
  geom_point(pch=19,size=2)+
  geom_abline(intercept = 0,slope = 1, lty=2)+
  xlab("Actual MEAN_HTOT")+
  ylab("Predicted MEAN_HTOT")+
  scale_x_continuous(breaks = seq(12,24,4))+
  scale_y_continuous(breaks = seq(12,24,4))+
  annotate("text",x=13,y=21.7,label="Mixed forest", hjust=0, vjust=0)+
  annotate("text",x=13,y=21,label="R^2==0.67", parse=T, hjust=0, vjust=0)+
  annotate("text",x=13,y=20.3,label="RMSE==1.86", parse=T,hjust=0,vjust=0)+
  theme_bw()+
  theme(panel.border = element_blank(),
        axis.line = element_line(size = .5, color = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(face = "bold",size = rel(1.2)))

####
pp5<-ggarrange(ggL1,ggL2,ggL3,ggL4,ggL5,nrow = 1,ncol=5,labels = c("A","B","C","D","E"))
png("D:/livingComp.png",res = 300,width = 5000,height = 1000)
pp5
dev.off()








ggplot(df1,aes(y=Area_m2, x=ForType))+ 
  geom_violin(aes(fill = ForType, color=ForType))
  guides(color=FALSE)+
  scale_fill_discrete(labels=c("Fagus Forest","Mixed Forest","Quercus Forest"))+
  labs(xlab(bquote('Area ('*m^2*')'))) + 
  annotate("text",x=10,y=.13,label=a.f2,hjust=0)+
  annotate("text",x=24,y=.13,label=a.m2,hjust=0)+
  annotate("text",x=38,y=.13,label=a.q2,hjust=0)+
  annotate("text",x=10,y=.22,label="Fagus",hjust=0,size=5)+
  annotate("text",x=24,y=.22,label="Mixed",hjust=0,size=5)+
  annotate("text",x=38,y=.22,label="Quercus",hjust=0,size=5)+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(.2,.5),
        axis.title = element_text(face = "bold",size = rel(1.2)),
        axis.text = element_text(size = rel(1.1)))

  
  
  #####################################################
  ### Bootstrapping
  ###################################################
  
  
  # number of repetitions
  set.seed(8793)
  boot_r_square_cal<-function(df, field_parm, gap_metr,  R = 500) {
    N = length(df[,1])
    r_squares <- rep(0, R)
    field_parm<-deparse(substitute(field_parm))
    gap_metr<-deparse(substitute(gap_metr))
    for(i in 1:R){
      idx = sample(1:N, N, replace = TRUE)
      boot.df <- df[idx,]
      #summary(lm(boot.df[[MEAN_HTOT]]~boot.df[[avg_Asymmetry]]))$adj.r.squared
      r_squares[i]<- summary(lm(boot.df[[field_parm]]~boot.df[[gap_metr]]))$adj.r.squared
    }
    return(r_squares)
  }
  
  ##  understorey
  
r_sq_mixed_MEAN_HTOT<-boot_r_square_cal(reg.m2, MEAN_HTOT, avg_Asymmetry)
r_sq_mixed_N_PLANTS<-boot_r_square_cal(reg.m2, N_PLANTS, avg_Roundness)
r_sq_mixed_MEAN_DBH<-boot_r_square_cal(reg.m2, MEAN_DBH, SUM_Width)
r_sq_quercus_MEAN_DBH<-boot_r_square_cal(reg.q1, MEAN_DBH, avg_Rect_fit)
r_sq_quercus_I_SHANNON<-boot_r_square_cal(reg.q2, I_SHANNON, SD_Rect_fit)

rs_un<-c(summary(lm(MEAN_HTOT~avg_Asymmetry, data = reg.m2))$adj.r.squared,
         summary(lm(N_PLANTS~avg_Roundness, data = reg.m2))$adj.r.squared,
         summary(lm(MEAN_DBH~SUM_Width, data = reg.m2))$adj.r.squared,
         summary(lm(MEAN_DBH~avg_Rect_fit, data = reg.q1))$adj.r.squared,
         summary(lm(I_SHANNON~SD_Rect_fit, data = reg.q2))$adj.r.squared)

R_SQ<-list("mixed MEAN_HTOT"=r_sq_mixed_MEAN_HTOT, "mixed N_PLANTS" = r_sq_mixed_N_PLANTS,
    "mixed MEAN_DBH" =  r_sq_mixed_MEAN_DBH, "quercus MEAN_DBH"= r_sq_quercus_MEAN_DBH,
    "quercus I_SHANNON"=r_sq_quercus_I_SHANNON)

und_R_sq<-as.data.frame(matrix(NA, length(R_SQ),7))
names(und_R_sq)<- c("Variable", "boot R2", "sd", "lower ci", "upper ci", "R2", "bias")
for(i in 1:length(R_SQ)){
  avg = lapply(R_SQ[i], mean)[[1]]
  und_R_sq[i,]=c(names(R_SQ)[i],avg, lapply(R_SQ[i], sd)[[1]], 
                 t.test(R_SQ[[i]], conf.level = .95)$conf.int, rs_un[i], (rs_un[i]-avg))
}
und_R_sq[["data"]]<-"Umderstorey"
rm(R_SQ,r_sq_mixed_N_PLANTS,r_sq_mixed_MEAN_DBH,r_sq_quercus_MEAN_DBH,
   r_sq_quercus_I_SHANNON, rs_un)
##########   Living trees

set.seed(9093)
r_sq_quercus_PRETZSCH<-boot_r_square_cal(regL.q2, I_PRETZSCH, SD_Density)
r_sq_quercus_HAB<-boot_r_square_cal(regL.q2, HAB, SUM_Rect_fit)
r_sq_mixed_MEAN_HTOT<-boot_r_square_cal(regL.m1, MEAN_HTOT, SD_Asymmetry)
r_sq_mixed_HAB<-boot_r_square_cal(regL.m2, HAB, cv_Length)
r_sq_mixed_perc_HAB<-boot_r_square_cal(regL.m1, `%HAB`, avg_RSE)
r_sq_mixed_MEAN_DBH<-boot_r_square_cal(regL.m1, MEAN_DBH, avg_Roundness)

rs_un<-c(summary(lm(I_PRETZSCH~SD_Density, data = regL.q2))$adj.r.squared,
         summary(lm(HAB~SUM_Rect_fit, data = regL.q2))$adj.r.squared,
         summary(lm(MEAN_HTOT~SD_Asymmetry, data = regL.m1))$adj.r.squared,
         summary(lm(HAB~cv_Length, data = regL.m2))$adj.r.squared,
         summary(lm(`%HAB`~avg_RSE, data = regL.m1))$adj.r.squared,
         summary(lm(MEAN_DBH~avg_Roundness, data = regL.m1))$adj.r.squared)



R_SQ_L<-list("quercus PRETZSCH"=r_sq_quercus_PRETZSCH, "quercus HAB" = r_sq_quercus_HAB,
           "mixed MEAN_HTOT" =  r_sq_mixed_MEAN_HTOT,"mixed HAB"= r_sq_mixed_HAB, 
           "mixed %HAB"= r_sq_mixed_perc_HAB, "mixed MEAN_DBH"=r_sq_mixed_MEAN_DBH)

L_R_sq<-as.data.frame(matrix(NA, length(R_SQ_L),7))
names(L_R_sq)<- c("Variable", "boot R2", "sd", "lower ci", "upper ci", "R2", "bias")
for(i in 1:length(R_SQ_L)){
  avg = lapply(R_SQ_L[i], mean)[[1]]
  L_R_sq[i,]=c(names(R_SQ_L)[i], avg, lapply(R_SQ_L[i], sd)[[1]], 
                 t.test(R_SQ_L[[i]], conf.level = .95)$conf.int, rs_un[i], (rs_un[i]-avg))
}
L_R_sq[["data"]]<-"Living"
rm(R_SQ_L,r_sq_mixed_MEAN_DBH,r_sq_mixed_perc_HAB,r_sq_mixed_HAB,r_sq_mixed_MEAN_HTOT,
   r_sq_quercus_HAB, r_sq_quercus_PRETZSCH, rs_un)

boot.df_R<-rbind(und_R_sq, L_R_sq)
getwd()
write.csv(boot.df_R, "./Article/boot2_R2.csv")
