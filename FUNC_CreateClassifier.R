createClassifier <- function(files, scales){
  
  print(paste(c("Scales for classifying:", scales), sep = " "))
  
  for (i in files) {
    print(c("Calculating Normals for...", basename(i)))
    #ifelse(file.exists(c(gsub(".xyz","_0_005_NORM.xyz", i),gsub(".xyz","_0_02_NORM.xyz", i),gsub(".xyz","_0_05_NORM.xyz", i))), next(), getThreeNormals(i, scales))
    
    if(!file.exists(gsub(".xyz","_0_005_NORM.xyz", i))
       |!file.exists(gsub(".xyz","_0_02_NORM.xyz", i))
       |!file.exists(gsub(".xyz","_0_05_NORM.xyz", i)))
      getThreeNormals(i, scales)
    
    while(!file.exists(gsub(".xyz","_0_05_NORM.xyz", i)))Sys.sleep(5)
    print("done")
  }
  
  print("Reading Normal Files...")
  leaf_files<-list.files("~/Desktop/ClassifierData/Classified/Knautia/", full.names = TRUE, pattern = "_LEAF_0_", recursive = T)
  notLeaf_files<-list.files("~/Desktop/ClassifierData/Classified/Knautia/", full.names = TRUE, pattern = "_NOTLEAF_0_", recursive = T)
  #flower_files<-list.files("~/Desktop/ClassifierData/Classified/Geranium", full.names = TRUE, pattern = "_FLOWER_0_", recursive = T)
  
  print("done.")
  
  
  
  leaf = getDf(leaf_files)
  notLeaf = getDf(notLeaf_files)
  #flower = getDf(flower_files)

  leaf$Category = factor(0)
  notLeaf$Category = factor(1)
  #flower$Category = factor(2)

  #df = rbind(leaf, notLeaf, flower)
  df = rbind(leaf, notLeaf)

  split = sample.split(df$Category, SplitRatio = 0.8)
  trainings_set = subset(df, split == TRUE)
  test_set = subset(df, split == FALSE)


  print("Creating Classifier ...")
  
   trees = c(10, 20, 40, 60, 80, 100)
   #trees = c(60)
   
   
  performances =  NULL
   
   for (tree in trees) {
    
    classifier = randomForest(x = trainings_set[, 4:12],
                              y = trainings_set$Category,
                              ntree = tree)
    
    print("done.")
    
    
    print("Calculation Prediction and Confusion matrix...")
    print(paste("Trees : ", tree))
   
   system.time({y_pred = predict(classifier, newdata = test_set[,4:12])})
   

    #cm = table(unlist(test_set[, "Category"]), y_pred)
    
    #print(cm)
    
    score0 = confusionMatrix(y_pred, test_set[, "Category"],
                    mode = "everything")
    
    score1 = confusionMatrix(y_pred, test_set[, "Category"],
                             mode = "everything", positive = "1")


  

    performances = rbind(performances, score0, score1)
  
 
   }
  
  return(performances)
  
  
}

getThreeNormals <- function(input_file, scales){
  
  term<-run(paste(cloudcompare, # call Cloud Compare. The .exe file folder must be in the system PATH
                  "-SILENT",
                  "-C_EXPORT_FMT", "ASC", "-PREC", 8, "-EXT", ".xyz", #Set asc as export format
                  "-NO_TIMESTAMP",
                  "-AUTO_SAVE OFF",
                  "-O", input_file, #open the subsampled file
                  "-OCTREE_NORMALS", scales[1], "-MODEL LS",
                  "-SAVE_CLOUDS","FILE", gsub(".xyz","_0_005_NORM.xyz", input_file),
                  "-OCTREE_NORMALS", scales[2], "-MODEL LS",
                  "-SAVE_CLOUDS","FILE", gsub(".xyz","_0_02_NORM.xyz", input_file),
                  "-OCTREE_NORMALS", scales[3], "-MODEL LS",
                  "-SAVE_CLOUDS", "FILE", gsub(".xyz","_0_05_NORM.xyz", input_file),
                  "-CLEAR",
                  sep = " "))
  
  print("waiting for normal files...")
  while(!file.exists(gsub(".xyz","_0_005_NORM.xyz", input_file))) Sys.sleep(5)
  print("0_005_NORM.xyz done")
  while(!file.exists(gsub(".xyz","_0_02_NORM.xyz", input_file))) Sys.sleep(5)
  print("0_02_NORM.xyz done")
  while(!file.exists(gsub(".xyz","_0_05_NORM.xyz", input_file))) Sys.sleep(5)
  print("0_05_NORM.xyz done")
  
}

readNormFiles <- function(files, pattern){
  
  df = data.frame()
  print("Cleaning up Normal files...")
  for (file in files) {
    if (grepl(pattern, file, fixed = T)){
      df = rbind(df, fread(file))
      
     

      file.remove(file)
    }
    else{
      next
    }
  }
  
  return(df)
  
}

getDf <- function(files){
  normals_005 = readNormFiles(files, "0_005_")
  normals_02 = readNormFiles(files, "0_02_")
  normals_05 = readNormFiles(files, "0_05_")
  
  normals = (as.data.frame(cbind(normals_005, normals_02[,4:6], normals_05[,4:6])))
  
  colnames(normals) = c("X", "Y", "Z", 
                        "nX005", "nY005", "nZ005", 
                        "nX02", "nY02", "nZ02", 
                        "nX05", "nY05", "nZ05")
  
  return(normals)
  
}

