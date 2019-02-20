### plots for samsung paper
accs<-c(svm_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results[2,1],
        rf_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results[2,1],
        nnet_results_16sec_upper_samsung$complete_model_on_test_results[2,1],
        ctree_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results[2,1],
        svm_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results$kappa[2],
        rf_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results$kappa[2],
        nnet_results_16sec_upper_samsung$complete_model_on_test_results$kappa[2],
        ctree_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results$kappa[2])

accs<-round(accs,3)
lab<-c(rep("acc",4), rep("kap",4))
modls<-c("SVM","RF","NNET","Tree","SVM","RF","NNET","Tree")
mydata<-data.frame(accs,lab,modls)
mydata<-as.matrix(mydata)
mydata[,1]<-as.numeric(mydata[,1])
mydata<-table(accs, lab, modls)
barplot(as.matrix(mydata), col = c("darkblue","red"))

par(mfrow=c(2,2))

plot(accs[1:4], ylim=c(0,1), pch=7, xaxt='n', xlab="Method", ylab="Accuracy", cex.axis=1.2, cex.lab=1.3, cex.main=2, pty="s")
points(accs[5:8], pch=3)
axis(side=1, labels = c("SVM","RF","NNET","Tree"), at = c(1,2,3,4), cex.axis=1.2, cex.lab=1.3)
title(main="Body movement location", cex.main=1.4)
legend(legend = c("Accuracy","Kappa"), "bottomright", pch = c(7,3))

par(mar=c(2.5,2.5,2.5,2.5))








accs<-c(svm_results_16sec_samsung$complete_model_on_test_results[2,1],
        rf_results_samsung_16sec$complete_model_on_test_results[2,1],
        nnet_results_16sec_samsung$complete_model_on_test_results[2,1],
        ctree_results_samsung_16sec$complete_model_on_test_results[2,1],
        svm_results_16sec_samsung$complete_model_on_test_results$kappa[2],
        rf_results_samsung_16sec$complete_model_on_test_results$kappa[2],
        nnet_results_16sec_samsung$complete_model_on_test_results$kappa[2],
        ctree_results_samsung_16sec$complete_model_on_test_results$kappa[2])

accs<-round(accs,3)
lab<-c(rep("acc",4), rep("kap",4))
modls<-c("SVM","RF","NNET","Tree","SVM","RF","NNET","Tree")
mydata<-data.frame(accs,lab,modls)
mydata<-as.matrix(mydata)
mydata[,1]<-as.numeric(mydata[,1])
mydata<-table(accs, lab, modls)
barplot(as.matrix(mydata), col = c("darkblue","red"))

par(mfrow=c(1,1))
plot(accs[1:4], ylim=c(0,1), pch=7, xaxt='n', xlab="Method", ylab="Accuracy", cex.axis=1.2, cex.lab=1.3, cex.main=2, pty="s")
points(accs[5:8], pch=3)
axis(side=1, labels = c("SVM","RF","NNET","Tree"), at = c(1,2,3,4), cex.axis=1.2, cex.lab=1.3)
title(main="Activity recognition", cex.main=1.4)
legend(legend = c("Accuracy","Kappa"), "bottomright", pch = c(7,3))


#### plot for balanced accuracy for rf
rf<-rf_results_samsung_16sec_alldatafortest
rf2<-rf_results_16sec
ba<-vector()
ba2<-vector()
#for i in 1:15
delete<-c(mean(rf$complete_model_on_test_results$BalancedAccuracy_1,na.rm = TRUE),mean(rf$complete_model_on_test_results$BalancedAccuracy_2, na.rm = TRUE),
          mean(rf$complete_model_on_test_results$BalancedAccuracy_3,na.rm = TRUE),mean(rf$complete_model_on_test_results$BalancedAccuracy_4,na.rm = TRUE),
          mean(rf$complete_model_on_test_results$BalancedAccuracy_5, na.rm = TRUE),mean(rf$complete_model_on_test_results$BalancedAccuracy_6,na.rm = TRUE),
          mean(rf$complete_model_on_test_results$BalancedAccuracy_7,na.rm = TRUE),mean(rf$complete_model_on_test_results$BalancedAccuracy_8, na.rm = TRUE),
          mean(rf$complete_model_on_test_results$BalancedAccuracy_9,na.rm = TRUE),mean(rf$complete_model_on_test_results$BalancedAccuracy_10,na.rm = TRUE),
          mean(rf$complete_model_on_test_results$BalancedAccuracy_11,na.rm = TRUE),mean(rf$complete_model_on_test_results$BalancedAccuracy_12,na.rm = TRUE),
          mean(rf$complete_model_on_test_results$BalancedAccuracy_13,na.rm = TRUE),mean(rf$complete_model_on_test_results$BalancedAccuracy_14,na.rm = TRUE),
          mean(rf$complete_model_on_test_results$BalancedAccuracy_15,na.rm = TRUE))d
delete<-confusionMatrix(rf_results_samsung_16sec_alldatafortest$predicted,rf_results_samsung_16sec_alldatafortest$actual)
delete<-delete$byClass[,11]
for (i in 1:15) {
  ba[i]<-delete[i]
} 
delete2<-c(mean(rf2$complete_model_on_test_results$BalancedAccuracy_1,na.rm = TRUE),mean(rf2$complete_model_on_test_results$BalancedAccuracy_2, na.rm = TRUE),
          mean(rf2$complete_model_on_test_results$BalancedAccuracy_3,na.rm = TRUE),mean(rf2$complete_model_on_test_results$BalancedAccuracy_4,na.rm = TRUE),
          mean(rf2$complete_model_on_test_results$BalancedAccuracy_5, na.rm = TRUE),mean(rf2$complete_model_on_test_results$BalancedAccuracy_6,na.rm = TRUE),
          mean(rf2$complete_model_on_test_results$BalancedAccuracy_7,na.rm = TRUE),mean(rf2$complete_model_on_test_results$BalancedAccuracy_8, na.rm = TRUE),
          mean(rf2$complete_model_on_test_results$BalancedAccuracy_9,na.rm = TRUE),mean(rf2$complete_model_on_test_results$BalancedAccuracy_10,na.rm = TRUE),
          mean(rf2$complete_model_on_test_results$BalancedAccuracy_11,na.rm = TRUE),mean(rf2$complete_model_on_test_results$BalancedAccuracy_12,na.rm = TRUE),
          mean(rf2$complete_model_on_test_results$BalancedAccuracy_13,na.rm = TRUE),mean(rf2$complete_model_on_test_results$BalancedAccuracy_14,na.rm = TRUE),
          mean(rf2$complete_model_on_test_results$BalancedAccuracy_15,na.rm = TRUE))
for (i in 1:15) {
  ba2[i]<-delete2[i]
} 
#ba[15]<-mean(rf$complete_model_on_test_results$BalancedAccuracy_15,na.rm=TRUE)
cts<-array(dim=c(15,2))
cts<-as.data.frame(cts)
cts[,1]<-ba
cts[,2]<-c("Computer Work","Heavy Lifting","Home Maintenance","Ironing","Laundry Washing","Mopping","Replacing Bed Sheet","Shopping",
           "Trash Removal","Leisure walk","Rapid walk","Walk at RPE 1","Walk at RPE 5","Washing Windows","Yoga")
colnames(cts)<-c("Accuracy","Activity")
cts2<-array(dim=c(15,2))
cts2<-as.data.frame(cts2)
cts2[,1]<-ba2
cts2[,2]<-c("Computer Work","Heavy Lifting","Home Maintenance","Ironing","Laundry Washing","Mopping","Replacing Bed Sheet","Shopping",
           "Trash Removal","Leisure walk","Rapid walk","Walk at RPE 1","Walk at RPE 5","Washing Windows","Yoga")
colnames(cts2)<-c("Accuracy","Activity")
cts<-rbind(cts, cts2)
cts$device<-c(rep("SGS",15),rep("GT3X+",15))

bp2<-ggplot(cts[1:15,], aes(x=c(1:15),Accuracy, fill="black"))
bp2<-bp2+geom_bar(stat="identity", position = "dodge")+
  theme(legend.position="none")+
  theme (text=element_text(size = 25, debug = FALSE))#,margin = margin(unit = "pt",b=0,t=0,l=5,r=10)
bp2<-bp2+scale_fill_grey()
bp2
bp3<-bp2+scale_x_discrete(name="Activity", limits=cts$Activity)
bp3<-bp3+theme(text = element_text(size=25),
               axis.text.x = element_text(angle=90,color = "black", vjust = 0.9, hjust=0.9, size=22),
               axis.text.y=element_text(color = "black"))
#bp3<-bp3+ ggtitle("Balanced accuracy of each activity")
bp3
bp3+theme(panel.background = element_rect(fill = 'gray94'))
### for normazlied confusion matrix
delete2<-array(dim = c(15,15))
for(i in 1:15){
  for(j in 1:15){
    delete2[i,j]<-round(delete$table[i,j]/(sum(delete$table[,j])),2)
  }
}
##### plot for variable importance
par(mar=c(5,5,5,5))
par(mfrow=c(6,1))
vi1
layout(matrix(c(1,2,3,4,5), nrow=1, byrow=TRUE))
layout.show(n=5)
vi1
vi2;vi3;vi4;vi5
table=importance(rf_results_16sec_intensity_samsung_alldatafortest$complete_model[[4]])
table<-as.data.frame(table); table$feature<-c("dom.freq","pow.625","mean","mean_a","sd_a",
                                              "covariance","kurtosis","entropy","cv","cor_x_y",
                                              "cor_y_z","cor_x_z")
  #c("df","p625","mvm","mangle","sdangle","cov",
   #                                           "geary","entropy","cv","cor(x,y)","cor(y,z)","cor(x,z)")
vi1<-ggplot(data = table, aes(x=c(1:12), y=MeanDecreaseGini, width=0.3))
vi1<-vi1+geom_bar(stat = "identity")
vi1<-vi1+scale_x_discrete(name="Feature", limits=table$feature)
vi1<-vi1+labs(y= "Mean decrease in Gini")
vi1<-vi1+coord_flip()
vi1<-vi1+ggtitle("Intensity")
vi1=vi1+theme(axis.text=element_text(size=24, face = "bold"))#,axis.text.y = element_text(size=14)))
vi1=vi1+theme(axis.title=element_text(size=24, face = "bold"))
vi1=vi1+theme(title=element_text(size=24, face="bold"))
#vi1+theme(aspect.ratio = 1.5)

table=importance(rf_results_16sec_loco_samsung_alldatafortest$complete_model[[3]])
table<-as.data.frame(table); table$feature<-c("dom.freq","pow.625","mean","mean_a","sd_a",
                                              "covariance","skewness","kurtosis","entropy","cv","cor_x_y",
                                              "cor_y_z","cor_x_z")
vi2<-ggplot(data = table, aes(x=c(1:13), y=MeanDecreaseGini, width=0.3))
vi2<-vi2+geom_bar(stat = "identity")
vi2<-vi2+scale_x_discrete(name="Feature", limits=table$feature)
vi2<-vi2+labs(y= "Mean decrease in Gini")
vi2<-vi2+coord_flip()
vi2<-vi2+ggtitle("Locomotion")
vi2=vi2+theme(axis.text=element_text(size=24, face = "bold"))#,axis.text.y = element_text(size=16)))
vi2=vi2+theme(axis.title=element_text(size=24, face = "bold"))
vi2=vi2+theme(title=element_text(size=24, face="bold"))

table=importance(rf_results_16sec_seden_samsung_alldatafortest$complete_model[[3]])
table<-as.data.frame(table); table$feature<-c("dom.freq","pow.625","mean","mean_a","sd_a",
                                              "covariance","kurtosis","entropy","cv","cor_x_y",
                                              "cor_y_z","cor_x_z")
vi3<-ggplot(data = table, aes(x=c(1:12), y=MeanDecreaseGini, width=0.3))
vi3<-vi3+geom_bar(stat = "identity")
vi3<-vi3+scale_x_discrete(name="Feature", limits=table$feature)
vi3<-vi3+labs(y= "Mean decrease in Gini")
vi3<-vi3+coord_flip()
vi3<-vi3+ggtitle("Sedentary")
vi3=vi3+theme(axis.text=element_text(size=24, face = "bold"))#,axis.text.y = element_text(size=16)))
vi3=vi3+theme(axis.title=element_text(size=24, face = "bold"))
vi3=vi3+theme(title=element_text(size=24, face="bold"))

table=importance(rf_results_16sec_upper_samsung_alldatafortest$complete_model[[1]])
table<-as.data.frame(table); table$feature<-c("dom.freq","pow.625","mean","mean_a","sd_a",
                                               "covariance","kurtosis","entropy","cv","cor_x_y",
                                               "cor_y_z","cor_x_z")
vi4<-ggplot(data = table, aes(x=c(1:12), y=MeanDecreaseGini, width=0.3))
vi4<-vi4+geom_bar(stat = "identity")
vi4<-vi4+scale_x_discrete(name="Feature", limits=table$feature)
vi4<-vi4+labs(y= "Mean decrease in Gini")
vi4<-vi4+coord_flip()
vi4<-vi4+ggtitle("Body movement location")
vi4=vi4+theme(axis.text=element_text(size=24,face = "bold" ))#,axis.text.y = element_text(size=16)))
vi4=vi4+theme(axis.title=element_text(size=24, face = "bold"))
vi4=vi4+theme(title=element_text(size=24, face="bold"))

table=importance(rf_results_samsung_16sec_alldatafortest$complete_model[[1]])
table<-as.data.frame(table); table$feature<-c("dom.freq","pow.625","mean","mean_a","sd_a",
                                              "covariance","skewness","kurtosis","entropy","cv","cor_x_y",
                                              "cor_y_z","cor_x_z")
vi5<-ggplot(data = table, aes(x=c(1:13), y=MeanDecreaseGini, width=0.3))
vi5<-vi5+geom_bar(stat = "identity")
vi5<-vi5+scale_x_discrete(name="Feature", limits=table$feature)
vi5<-vi5+labs(y= "Mean decrease in Gini")
vi5<-vi5+coord_flip()
vi5<-vi5+ggtitle("Activity recognition")
vi5=vi5+theme(axis.text=element_text(size=24, face = "bold"))#,axis.text.y = element_text(size=16)))
vi5=vi5+theme(axis.title=element_text(size=24, face = "bold"))
vi5=vi5+theme(title=element_text(size=24, face="bold"))

table=importance(rf_results_16sec_met$complete_model[[4]])
table<-as.data.frame(table); table$feature<-c("dom.freq","pow.625","mean_a","sd_a",
                                              "covariance","kurtosis","entropy","cv","cor_x_y",
                                              "cor_y_z","cor_x_z")
vi6<-ggplot(data = table, aes(x=c(1:11), y=IncNodePurity, width=0.3))
vi6<-vi6+geom_bar(stat = "identity")
vi6<-vi6+scale_x_discrete(name="Feature", limits=table$feature)
vi6<-vi6+labs(y= "IncNodePurity")
vi6<-vi6+coord_flip()
vi6<-vi6+ggtitle("METs estimation")
vi6=vi6+theme(axis.text=element_text(size=24, face = "bold"))#,axis.text.y = element_text(size=14)))
vi6=vi6+theme(axis.title=element_text(size=24, face = "bold"))
vi6=vi6+theme(title=element_text(size=24, face="bold"))
#vi1+theme(aspect.ratio = 1.5)


multiplot(vi1,vi2,vi3,vi4,vi5,vi6,cols =3)



### plot for simple vs. complex vs. all activity accuracy
## simple and complex activities
table<-array(dim=c(12,2))
table<-as.data.frame(table)
colnames(table)<-c("Accuracy","Type")
table$Accuracy<-c(complex, simple, total)
table$Type<-c(rep("complex",4), rep("simple",4),rep("total",4))
table$model<-c(rep(c("SVM","RF","NNET","CTREE"),3))
b1<-ggplot(table,aes(c(1:4),model))
colour="black"
b2<-b1+geom_bar(stat="identity",colour="black", position = "dodge")
b2
#bp2<-bp2+scale_fill_grey()
b3<-b2+scale_x_discrete(name="Type", limits=table$model)
b3<-b3+theme(text = element_text(size=25, face = "bold"),
             axis.text.x = element_text(angle=90))
#bp3<-bp3+ ggtitle("Balanced accuracy of each activity")
b3
b=ggplot(data=table, aes(x=table$model, y=table$Accuracy, fill=Type))+
  geom_bar(stat = "identity", position = position_dodge())+
  scale_fill_grey()
b=b+scale_x_discrete(name="Model")
b=b+scale_y_continuous(name="Accuracy")
b=b+theme(text = element_text(size=20))
b
simple<-c(0.77,0.94,0.88,0.82)
complex<-c(0.38,0.54,0.49,0.35)
total<-c(0.48,0.65,0.59,0.48)


delete1<-confusionMatrix(rf_results_samsung_16sec_alldatafortest$predicted, 
                         rf_results_samsung_16sec_alldatafortest$actual)
delete2<-confusionMatrix(svm_results_16sec_samsung_alldatafortest$predicted, 
                         svm_results_16sec_samsung_alldatafortest$actual)
delete3<-confusionMatrix(ctree_results_samsung_16sec_alldatafortest$predicted,
                         ctree_results_samsung_16sec_alldatafortest$actual)
delete4<-confusionMatrix(nnet_results_samsung_16sec$predicted, 
                         nnet_results_samsung_16sec$actual)
## for simple accuracies
good<-0
whole<-0
for(i in c(1,10:13)){
  good=good+delete4$table[i,i]
  print(good)
  whole=whole+sum(delete4$table[,i])
  print(whole)
  print(i)
}
good/whole
####
### device comparisons
activ<-confusionMatrix(rf_results_samsung_16sec$predicted,
                rf_results_samsung_16sec$actual)
seden<-confusionMatrix(rf_results_16sec_seden_samsung_alldatafortest$predicted,
                       rf_results_16sec_seden_samsung_alldatafortest$actual)
intensity<-confusionMatrix(rf_results_16sec_intensity_samsung_alldatafortest$predicted,
                           rf_results_16sec_intensity_samsung_alldatafortest$actual)
upper<-confusionMatrix(rf_results_16sec_upper_samsung_alldatafortest$predicted,
                       rf_results_16sec_upper_samsung_alldatafortest$actual)
loco<-confusionMatrix(rf_results_16sec_loco_samsung_alldatafortest$predicted,
                      rf_results_16sec_loco_samsung_alldatafortest$actual)


activ_a<-confusionMatrix(rf_results_16sec$predicted,
                       rf_results_16sec$actual)
seden_a<-confusionMatrix(rf_results_16sec_seden_actigraph_alldatafortest$predicted,
                       rf_results_16sec_seden_actigraph_alldatafortest$actual)
intensity_a<-confusionMatrix(rf_results_16sec_intensity_actigraph_alldatafortest$predicted,
                           rf_results_16sec_intensity_actigraph_alldatafortest$actual)
upper_a<-confusionMatrix(rf_results_16sec_upper_actigraph_alldatafortest$predicted,
                       rf_results_16sec_upper_actigraph_alldatafortest$actual)
loco_a<-confusionMatrix(rf_results_16sec_loco_actigraph_alldatafortest$predicted,
                      rf_results_16sec_loco_actigraph_alldatafortest$actual)


table<-array(dim=c(10,3))
table<-as.data.frame(table)
colnames(table)<-c("Device","Accuracy","SD")
table$Device<-c(rep("SGS",5), rep("GT3X+",5))
table$Accuracy<-c(mean(rf_results_16sec_intensity_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec_loco_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec_seden_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_samsung_16sec_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec_intensity_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec_upper_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec_loco_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec_seden_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  mean(rf_results_16sec$complete_model_on_test_results[1][,1], na.rm = TRUE))
                  

table$SD<-c(sd(rf_results_16sec_intensity_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_16sec_upper_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_16sec_loco_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_16sec_seden_samsung_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_samsung_16sec_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),                  
                  sd(rf_results_16sec_intensity_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_16sec_upper_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_16sec_loco_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_16sec_seden_actigraph_alldatafortest$complete_model_on_test_results[1][,1], na.rm = TRUE),
                  sd(rf_results_16sec$complete_model_on_test_results[1][,1], na.rm = TRUE))

table$task<-rep(c("Inten detec","Location detec","Loco detec","Seden detec","Activ recog"),2)

b1<-ggplot(table,aes(x=task,y=Accuracy, fill=Device))+
  geom_bar(stat="identity", width=0.5, position=position_dodge())+
  theme(axis.text.x = element_text(angle=45, vjust=0.8, hjust=0.7))+
  theme(text = element_text(size = 20))+
  geom_errorbar(aes(ymin=Accuracy-SD, ymax=Accuracy+SD), width=.2,
                position=position_dodge(.5))+
  scale_x_discrete(name="Classification task")+
  scale_fill_grey()
