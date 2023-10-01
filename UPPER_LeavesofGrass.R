################################################################
# Set Up - directory for Cloud Compare and libraries, terminal, input file
###############################################################

cc_dir = "/Applications/CloudCompare.app/Contents/MacOS/CloudCompare"

source("FUNC_LeavesofGrassUPPER.R")

library(filesstrings)
library(data.table)
library(lidR)
library(raster)
library(geometry)
library(ggplot2)
library(randomForest)
library(fitdistrplus)
library(vroom)
library(gstat)

cloudcompare <- cc_dir
run<-function(x) rstudioapi::terminalExecute(x)

voxRes=0.1
minVoxDensity= 5 
correct.topography=F
superDF=TRUE
clean=F

overwrite = TRUE

#plantCode.list = c("OBC095", "OBC130", "OBC111", "OBA035", "OBB056")



plantcode =  "OBA005"
pattern = paste(plantcode, "_LEAF.xyz", sep = "")
#pattern = "_LEAF.xyz"

input_files = list.files("~/Desktop/ClassifierData/Classified/Ranunculus/", pattern = pattern, recursive = T, full.names = T)

input_files



df.angles = NULL

for (i in input_files) {
 
  
  label = str_split(basename(i), "_", simplify = T  )
  label = paste(label[1], label[2], label[3], sep = "_" )
  
  if (!file.exists(gsub(".xyz","_NORM.xyz", i))) {
    normalCalc(i)
  }else{
    print(paste(basename(gsub(".xyz","_NORM.xyz", i)), "does exist", sep = " "))
  }
  


  df = fread(gsub(".xyz","_NORM.xyz", i))

  norm_file = gsub(".xyz","_NORM.xyz", i)


  TLSLeAF.dat = LeavesofGrass(norm_file,
                              overwrite,
                              center,
                              scatterLim,
                              rf_model,
                              correct.topography,
                              voxRes,
                              minVoxDensity,
                              superDF,
                              clean)

print(
  ggplot(TLSLeAF.dat@LAD,
         aes(a))+
    geom_density(fill="lightgreen") +
    xlim(c(0,90)) +
    geom_vline(xintercept= median(TLSLeAF.dat@LAD$a), size=0.5, color="orange") +
    geom_text(aes(x=10, label=(paste("Median", round(median(TLSLeAF.dat@LAD$a), digits = 2), sep = " ")), y = 0.04)) +
    ggtitle(paste(label, sep = " "), subtitle = "Ranunculus acris")
)
  
  #df.angles[[basename(i)]] = TLSLeAF.dat@LAD$a
  
  df.angles[[gsub("_LEAF.xyz", "", basename(i) )]] = TLSLeAF.dat@LAD$a
}

boxplot(df.angles) 

