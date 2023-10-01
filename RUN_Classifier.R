#plantCode.list = c("OBC121", "OBC102", "OBC128", "OBB081", 
 #                  "OBB091", "OBA017", "OBC109")

#plantCode.list = c("OBA017")

#plantCode.list = c("OBA020")
#plantCode.list = c("OBC100", "OBC099", "OBA033")
#plantCode.list = c("OBA007")

#plantCode.list = c("OBC095", "OBC130", "OBC111", "OBA035", "OBB056")

#plantCode.list = c("OBA016")

plantCode.list = c("OBB071", "OBC093", "OBC125", "OBA037", "OBB062", "OBB065", "OBA003", "OBA005", "OBC115")


source("FUNC_Classifier.R")
source("SETUP_Classifier.R")

for (i in plantCode.list) {
  plantCode = i
  classPrep(plantCode)
}


for (i in plantCode.list) {
  plantCode = i
  classifyRealData(plantCode, classifier)
}


