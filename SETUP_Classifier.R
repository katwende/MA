library(data.table)
library(caTools)
set.seed(134)
library(randomForest)

cc_dir = "/Applications/CloudCompare.app/Contents/MacOS/CloudCompare"

cloudcompare <- cc_dir
run<-function(x) rstudioapi::terminalExecute(x)

scales = c(0.005, 0.02, 0.05)
#scales = c(0.01, 0.05, 0.1)
#scales = c(0.005, 0.05, 0.1)
#scales = c(0.03, 0.10, 0.2)
#scales = c(0.001, 0.01, 0.02)
#scales = c(0.02, 0.04, 0.08, 0.1)

classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/Geranium_Classifier.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/GerLeu_Classifier.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/Geranium_fourR.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/Geranium_V2_Classifier.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/Geranium_V3_Classifier.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/Geranium_OBB081_Classifier.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/Plantago_Classifier.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/GerFlower.rds")
#classifier<-readRDS("~/Desktop/MA/Code/ML_LeavesOfGrass/Knautia.rds")