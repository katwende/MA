
preprocessingOriginals <- function(output_file){
  
  term<-run(paste(cloudcompare, # call Cloud Compare. The .exe file folder must be in the system PATH
                  "-SILENT",
                  "-C_EXPORT_FMT", "ASC", "-PREC", 6, "-EXT", ".xyz", 
                  "-NO_TIMESTAMP",
                  "-AUTO_SAVE OFF",
                  "-O", output_file,
                  "-SOR", 6, 1.0,
                  "-NOISE RADIUS 0.002 REL 1 RIP",
                  "-OCTREE_NORMALS", 0.002 , "-MODEL LS",
                  "-SAVE_CLOUDS","FILE", gsub(".xyz","_preprocessed.xyz", output_file),
                  "-CLEAR",
                  sep = " "))
  
  print("waiting for preprocessed file...")
  while(!file.exists(gsub(".xyz","_preprocessed.xyz", output_file))) Sys.sleep(5)
  print(paste(gsub(".xyz","_preprocessed.xyz", output_file), "done", sep = " "))  
  file.remove(output_file)
} 

classPrep <- function(plantCode){
  
  files<-list.files("~/Desktop/ClassifierData/Original/", full.names = TRUE, pattern = plantCode, recursive = T)
  output_dir = "~/Desktop/ClassifierData/Preprocessed/"
  
  
  for (i in files) {
    
    print(c("Pre-Processing ...", basename(i)))
    
    output_file = paste(output_dir, basename(i), sep="")
    
    if (!file.exists(gsub(".xyz","_preprocessed.xyz", output_file))) {
      fwrite(fread(i, select = 3:5, header = F), file = output_file,)
      
      preprocessingOriginals(output_file)
      
      print("done")
    }else{
      print("Pre-Processed file exists ... next")
    }
    
  }
}

# createClassifier <- function(files, scales){
#   
#   print(paste(c("Scales for classifying:", scales), sep = " "))
#   
#   for (i in files) {
#     print(c("Calculating Normals for...", basename(i)))
#     #ifelse(file.exists(c(gsub(".xyz","_0_005_NORM.xyz", i),gsub(".xyz","_0_02_NORM.xyz", i),gsub(".xyz","_0_05_NORM.xyz", i))), next(), getThreeNormals(i, scales))
#     
#     if(!file.exists(gsub(".xyz","_0_005_NORM.xyz", i))|!file.exists(gsub(".xyz","_0_02_NORM.xyz", i))|!file.exists(gsub(".xyz","_0_05_NORM.xyz", i)))
#       getThreeNormals(i, scales)
#     
#     while(!file.exists(gsub(".xyz","_0_05_NORM.xyz", i)))Sys.sleep(5)
#     print("done")
#   }
#   
#   print("Reading Normal Files...")
#   leaf_files<-list.files("~/Desktop/ClassifierData/Classified/Geranium", full.names = TRUE, pattern = "_Leaf_0_", recursive = T)
#   notLeaf_files<-list.files("~/Desktop/ClassifierData/Classified/Geranium", full.names = TRUE, pattern = "_NotLeaf_0_", recursive = T)
#   print("done.")
#   
#   
#   
#   leaf = getDf(leaf_files)
#   notLeaf = getDf(notLeaf_files)
#   
#   leaf$Category = factor(0)
#   notLeaf$Category = factor(1)
#   
#   df = rbind(leaf, notLeaf)
#   
#   print(c("DF:", str(df)))
#   
#   split = sample.split(df$Category, SplitRatio = 0.75)
#   trainings_set = subset(df, split == TRUE)
#   test_set = subset(df, split == FALSE)
#   
#   
#   print("Creating Classifier ...")
#   classifier = randomForest(x = trainings_set[, 4:12],
#                             y = trainings_set$Category,
#                             ntree = 100)
#   print("done.")
#   
#   
#   print("Calculation Prediction and Confusion matrix...")
#   y_pred = predict(classifier, newdata = test_set[,4:12])
#   
#   cm = table(unlist(test_set[, "Category"]), y_pred)
#   
#   print(cm)
#   
#   return(classifier)
#   
#   
# }

getThreeNormals <- function(input_file, scales){
  
  term<-run(paste(cloudcompare, # call Cloud Compare. The .exe file folder must be in the system PATH
                  "-SILENT",
                  "-C_EXPORT_FMT", "ASC", "-PREC", 8, "-EXT", ".xyz", #Set asc as export format
                  "-NO_TIMESTAMP",
                  "-AUTO_SAVE OFF",
                  "-O", input_file, #open the subsampled file
                  "-SS SPATIAL", 0.002,
                  "-OCTREE_NORMALS", scales[1], "-MODEL LS",
                  "-SAVE_CLOUDS","FILE", gsub(".xyz","_0_005_NORM.xyz", input_file),
                  "-OCTREE_NORMALS", scales[2], "-MODEL LS",
                  "-SAVE_CLOUDS","FILE", gsub(".xyz","_0_02_NORM.xyz", input_file),
                  "-OCTREE_NORMALS", scales[3], "-MODEL LS",
                  "-SAVE_CLOUDS", "FILE", gsub(".xyz","_0_05_NORM.xyz", input_file),
                  "-CLEAR",
                  sep = " "))
  
  print("waiting for normal files...")
  while(!file.exists(gsub(".xyz","_0_005_NORM.xyz", input_file))) Sys.sleep(10)
  print("0_005_NORM.xyz done")
  while(!file.exists(gsub(".xyz","_0_02_NORM.xyz", input_file))) Sys.sleep(10)
  print("0_02_NORM.xyz done")
  while(!file.exists(gsub(".xyz","_0_05_NORM.xyz", input_file))) Sys.sleep(10)
  print("0_05_NORM.xyz done")
  
}

readNormFiles <- function(files, pattern){
 
  df = data.frame()
  
  for (file in files) {
    if (grepl(pattern, file, fixed = T)){
      df = rbind(df, fread(file))
      
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

classify <- function(month, classifier, plantCode){
  normal_files = list.files("~/Desktop/ClassifierData/Preprocessed/" , full.names = TRUE, pattern = paste(month, plantCode, sep=""), recursive = T)
  outputfile = paste("~/Desktop/ClassifierData/Classified/Ranunculus///",plantCode,"/", month, plantCode, "_classified.xyz", sep="")
  
  if (!file.exists(outputfile)) {
    print(paste("Classifying...", paste(month, plantCode, sep = ""), sep = " "))
    real_005 = readNormFiles(normal_files, "0_005_")
    real_02 = readNormFiles(normal_files, "0_02_")
    real_05 = readNormFiles(normal_files, "0_05_")

    
    colnames(real_005) = c("X", "Y", "Z", "nX005", "nY005", "nZ005")
    colnames(real_02) = c("X", "Y", "Z", "nX02", "nY02", "nZ02")
    colnames(real_05) = c("X", "Y", "Z", "nX05", "nY05", "nZ05")

    
    real_df = (as.data.frame(cbind(real_005, real_02[,4:6], real_05[,4:6])))
    
    real_data = real_df
    real_data$Category = NA
    real_data$Category = factor(real_data$Category)
    
    real_df$Category = predict(classifier, newdata = real_data[,4:12])
    
    
    fwrite(real_df[, c("X", "Y", "Z", "Category")],
           file = outputfile)
    
    print("done classifying")
    print("cleaning up")
    while(!file.exists(paste("~/Desktop/ClassifierData/Preprocessed/", month, plantCode,"_preprocessed_0_05_NORM.xyz", sep = ""))) Sys.sleep(5)
    
    file.remove(paste("~/Desktop/ClassifierData/Preprocessed/", month, plantCode,"_preprocessed_0_005_NORM.xyz", sep = ""), 
                paste("~/Desktop/ClassifierData/Preprocessed/", month, plantCode,"_preprocessed_0_02_NORM.xyz", sep = ""),
                paste("~/Desktop/ClassifierData/Preprocessed/", month, plantCode,"_preprocessed_0_05_NORM.xyz", sep = "")
    )
    
  }else{
    file.remove(paste("~/Desktop/ClassifierData/Preprocessed/", month, plantCode,"_preprocessed_0_005_NORM.xyz", sep = ""), 
                paste("~/Desktop/ClassifierData/Preprocessed/", month, plantCode,"_preprocessed_0_02_NORM.xyz", sep = ""),
                paste("~/Desktop/ClassifierData/Preprocessed/", month, plantCode,"_preprocessed_0_05_NORM.xyz", sep = "")
    )
    
    print(paste(basename(outputfile),"does exist... next", sep = " "))
  }
  
}

classifyRealData <- function(plantCode, classifier){
  
  pattern = paste(plantCode,"_preprocessed.xyz", sep="" )
  
  real_data_files = list.files("~/Desktop/ClassifierData/Preprocessed/" , full.names = TRUE, pattern = pattern, recursive = T)
  
  for (i in real_data_files) {
    
    print(paste("Normal Calculation for ...", basename(i), sep = " " ))
    
    if(!file.exists(gsub(".xyz","_0_005_NORM.xyz", i))|
       !file.exists(gsub(".xyz","_0_02_NORM.xyz", i))|
       !file.exists(gsub(".xyz","_0_05_NORM.xyz", i)))
      getThreeNormals(i, scales)
    
  }
  
  
  may14 = "14_05_"
  august14 = "14_08_"
  may15 = "15_05_"
  august15 = "15_08_"
  
  months = c(may14 , august14, may15, august15)
  
  for (i in months) {
    classify(i, classifier, plantCode)
  }
  
} 
