library(data.table)
library(stringr)
library(r2r)

df = data.frame(NULL)

load("SEM_Structure_metrics_temperature_August.RData") #newdata_aug AUG 2014-> SR, Mean Leaf Temp
load("SEM_Structure_metrics_temperature_May.Rdata") #newdata_new MAY 2014->  SR, Mean Leaf Temp
leafTemp_2015 = readRDS("leafTemp_2015.rds") # Mean Leaf Temp 2015 MAY+AUG
load("daily_weather_saale.RData") # Airtemp, Rh, Rain for days of measurments -> VPD
load("legacy_weather_data_saale.RData") # Mean Airtemp and Rain Spring and Summer 2014+2015
load("CWM_FDis_May_2014.Rdata")
load("CWM_FDis_Aug_2014.Rdata")
load("FD_df.RData")

# df.2015 = cbind(Tabelle1_Tabelle_1[,2], Tabelle1_Tabelle_1[,]$`5/19/15`, Tabelle1_Tabelle_1[,]$`8/22/15`)
# 
# df.2015 = df.2015[4:nrow(df.2015),]
# 
# df.2015[,2] = as.numeric(gsub(",", ".", df.2015[,2]))
# df.2015[,3] = as.numeric(gsub(",", ".", df.2015[,3]))

#colnames(data_2015) = c("Plot", "May_2015", "Aug_2015")

#saveRDS(data_2015, "leafTemp_2015.rds")


# FILE DIRECTORY

dir = "~/Desktop/ClassifierData/Classified/"
geranium_plots = list.files("~/Desktop/ClassifierData/Classified/Geranium/", pattern = "OB")
knautia_plots = list.files("~/Desktop/ClassifierData/Classified/Knautia//", pattern = "OB")
ranunculus_plots = list.files("~/Desktop/ClassifierData/Classified/Ranunculus//", pattern = "OB")
pattern = "LAD.txt"

plots <- hashmap()

plots[["Geranium"]] = geranium_plots
plots[["Knautia"]] = knautia_plots
plots[["Ranunculus"]] = ranunculus_plots


for (key in keys(plots)) {
  species.list = plots[key]
  
  for (plot.list in species.list) {
    
    for (i in plot.list) {
      
      dir.plot = paste(dir, key, "/" , i, sep="")
      list = list.files(dir.plot, full.names = TRUE, pattern = pattern)
      
      block = str_sub(i, 1, 3)
      
      for (j in list) {
        
        
        SR = newdata_aug[newdata_aug$Plot == i,]$SR
        file = fread(j)
        split = str_split(basename(j), "_", simplify = TRUE)
        date = paste(split[1], split[2], sep = "_")
        
        if (date == "14_05") {
              vec = c(i, date, "LOW"  , 0.394 , key, mean(file$a), median(file$a), sd(file$a), 
                      SR, FD.df[i,]$FDis_May14, FD.df[i,]$PCA1_May14,
                      mean(daily_weather_saale$Temp_MAY_2014)/mean(daily_weather_saale$Rh_MAY_14),
                      newdata_new[newdata_new$Plot == i, ]$Tmean,
                      legacy_weather_data$Temp_SPRING_2014, legacy_weather_data$Rain_SPRING_2014)

              df = rbind(df, vec)
            }
            else if (date == "14_08") {
              vec = c(i, date, "MEDIUM" , 0.5 ,key, mean(file$a), median(file$a), sd(file$a), 
                      SR, FD.df[i,]$FDis_Aug14, FD.df[i,]$PCA1_Aug14,
                      mean(daily_weather_saale$Temp_AUG_2014)/mean(daily_weather_saale$Rh_AUG_14),
                      newdata_aug[newdata_aug$Plot == i, ]$Tmean,
                      legacy_weather_data$Temp_SUMMER_2014, legacy_weather_data$Rain_SUMMER_2014)

              df = rbind(df, vec)
            }
            else if(date == "15_05"){
              
              number = str_sub(i, -3, -1)
              
              vec = c(i, date, "HIGH" , 0.765 ,key, mean(file$a), median(file$a), sd(file$a), 
                      SR, FD.df[i,]$FDis_May15, FD.df[i,]$PCA1_May15,
                      mean(daily_weather_saale$Temp_MAY_2015)/mean(daily_weather_saale$Rh_MAY_15),
                      leafTemp_2015[leafTemp_2015$Plot == number,]$May_2015,
                      legacy_weather_data$Temp_SPRING_2015, legacy_weather_data$Rain_SPRING_2015)

              df = rbind(df, vec)

            }
            else if(date == "15_08"){
              
              number = str_sub(i, -3, -1)

              vec = c(i, date, "VERY HIGH" , 0.954 ,key, mean(file$a), median(file$a), sd(file$a), 
                      SR, FD.df[i,]$FDis_Aug15, FD.df[i,]$PCA1_Aug15,
                      mean(daily_weather_saale$Temp_AUG_2015)/mean(daily_weather_saale$Rh_AUG_15),
                      leafTemp_2015[leafTemp_2015$Plot == number,]$Aug_2015,
                      legacy_weather_data$Temp_SUMMER_2015, legacy_weather_data$Rain_SUMMER_2015)

              df = rbind(df, vec)

            }
            else{
             print("ERROR")
            }



          }
      }
      
     
      
    }
    
  }
  


colnames(df) = c("PLOT", "DATE", "DROUGHT_LEVEL", "SPEI", "FOCUS_SPECIES", "LAD_Mean", "LAD_Median", "LAD_SD", "SR" ,"FDis", "PCA1",
                 "VPD", "MeanLeafT", "Legacy_Temp", "Legacy_Rain")



pSEM.df = df




save(pSEM.df, file = "pSEM.RData")
