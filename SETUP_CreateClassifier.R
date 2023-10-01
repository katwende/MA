library(data.table)
library(caTools)
library(caret)
set.seed(134)
library(randomForest)

cc_dir = "/Applications/CloudCompare.app/Contents/MacOS/CloudCompare"

cloudcompare <- cc_dir
run<-function(x) rstudioapi::terminalExecute(x)

scales = c(0.005, 0.02, 0.05)
#scales = c(0.02, 0.04, 0.08, 0.1)
#scales = c(0.01, 0.05, 0.1)