source("FUNC_CreateClassifier.R")
source("SETUP_CreateClassifier.R")


files = list.files("~/Desktop/ClassifierData/Classified/Knautia/Classifier/", 
                   full.names = TRUE, all.files = T, recursive = T, patter = "LEAF.xyz")

files


#newClassifier = createClassifier(files, scales)

getPerformances = createClassifier(files, scales)



accuracy.list = NULL
f1_0.list = NULL
f1_1.list = NULL

for (i in 1:12 ) {
 
  if (i %% 2 == 1) {
    print(i)
    accuracy.list = rbind(accuracy.list,getPerformances[i,]$overall[1])
    f1_0.list = rbind(f1_0.list, getPerformances[i,]$byClass[7])
    f1_1.list = rbind(f1_1.list, getPerformances[i+1,]$byClass[7])
  }
   else{
     next
   }
 

}

accuracy = NULL

accuracy = as.data.frame(c(10,20,40,60,80,100), 
                         row.names = c("10", "20", "40", "60", "80", "100"))

accuracy = cbind(accuracy, round(accuracy.list*100,2), round(f1_0.list,3), 
                 round(f1_1.list,3))

colnames(accuracy) = c("Trees", "Accuracy", "F1_Leaf", "F1_NotLeaf")


library(ggplot2)

ggplot(accuracy, aes(x = Trees, y = Accuracy))+
  geom_point(size = 1.5) +
  geom_line(size = 0.2) +
  ylim(94,97) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  xlab("Number of Trees") +
  ylab("Accuracy [%]")



#saveRDS(newClassifier, file = "Knautia.rds")
