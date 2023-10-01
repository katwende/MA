library(nlme)
#install.packages("vegan")
library(vegan)
library(corrplot)
library(dplyr)

load("pSEM.RData")
#df = pSEM.df %>% filter(pSEM.df$FOCUS_SPECIES == "Geranium")

df = pSEM.df

# 
# df_14 = df %>%  filter(DATE == "14_05" | DATE == "14_08")
# df_15 = df %>%  filter(DATE == "15_05" | DATE == "15_08")


df <- df %>% mutate_at(c("LAD_Mean", "LAD_Median", "LAD_SD", "SR" ,"FDis", "PCA1",
                          "MeanLeafT", "SPEI" ), as.numeric)

df <- df %>% mutate_at(c( "DROUGHT_LEVEL", "PLOT","DATE","FOCUS_SPECIES" ,"VPD", "Legacy_Temp", "Legacy_Rain"), 
                       as.factor)


str(df)


corr <- cor(df[,c("LAD_Mean", "LAD_Median", "LAD_SD")])
corrplot(corr, method = "number", type = "lower", diag = F,
         cl.pos = "n")



################################################################################
### Model selection LA_Mean
################################################################################

full.model.mean = lm(LAD_Mean ~ PCA1 + FDis + log(SR) + MeanLeafT + SPEI  + 
          PCA1*SPEI + FDis*SPEI + log(SR)*SPEI +
          PCA1*MeanLeafT + FDis*MeanLeafT + log(SR)*MeanLeafT +
            MeanLeafT*SPEI, 
        na.action = na.omit , df)

summary(full.model.mean)

bestfit.model.mean = step(full.model.mean, direction = "backward")

summary(bestfit.model.mean)

final.model.mean = lm(LAD_Mean ~ PCA1 + FDis + log(SR) + MeanLeafT + SPEI + 
         PCA1:SPEI + FDis*MeanLeafT + MeanLeafT*SPEI, 
       na.action = na.omit , df)

summary(final.model.mean)

anova(final.model.mean, test="F")

anova(full.model.mean, bestfit.model.mean, final.model.mean)
AIC(full.model.mean, bestfit.model.mean, final.model.mean)



################################################################################
### Model selection LA_Median
################################################################################

full.model.median = lm(LAD_Median ~ PCA1 + FDis + log(SR) + MeanLeafT + SPEI  + 
                       PCA1*SPEI + FDis*SPEI + log(SR)*SPEI +
                       PCA1*MeanLeafT + FDis*MeanLeafT + log(SR)*MeanLeafT +
                      SPEI*MeanLeafT, 
                     na.action = na.omit , df)

summary(full.model.median)

bestfit.model.median = step(full.model.median, direction = "backward")

summary(bestfit.model.median)

final.model.median = lm(LAD_Median ~ PCA1 + FDis + log(SR) + MeanLeafT + SPEI + 
                        PCA1:SPEI  + FDis:MeanLeafT + MeanLeafT*SPEI, 
                      na.action = na.omit,  df)

summary(final.model.median)

anova(final.model.median, test="F")

anova(full.model.median, bestfit.model.median, final.model.median)
AIC(full.model.median, bestfit.model.median, final.model.median)


################################################################################
### Model selection LA_SD
################################################################################

full.model.sd = lm(LAD_SD ~ PCA1 + FDis + log(SR) + MeanLeafT + SPEI  + 
                       PCA1*SPEI + FDis*SPEI + log(SR)*SPEI +
                       PCA1*MeanLeafT + FDis*MeanLeafT + log(SR)*MeanLeafT +
                        MeanLeafT*SPEI, 
                     na.action = na.omit , df)

summary(full.model.sd)

bestfit.model.sd = step(full.model.sd, direction = "backward")

summary(bestfit.model.sd)

final.model.sd = lm(LAD_SD ~ PCA1 + FDis + log(SR) + MeanLeafT + SPEI +
                          PCA1:SPEI  + FDis:MeanLeafT,
                      na.action = na.omit , df)

summary(final.model.sd)

anova(final.model.sd, test="F")

anova(full.model.sd, bestfit.model.sd, final.model.sd)
AIC(full.model.sd, bestfit.model.sd, final.model.sd)



################################################################################
### Multicollinearity check
################################################################################


# Load the necessary package
library(car)


# Calculate VIF
vif_values <- car::vif(final.model.sd, type= "predictor")

# Display VIF values
print(vif_values)

df$MeanLeafT_Levels = cut(df$MeanLeafT, 2)

summary(df)

library(RColorBrewer)
library(ggplot2)
install.packages("viridis")
library(viridis)

ggplot(df,
      aes(x=FDis, y = LAD_Mean, colour = MeanLeafT_Levels)) +
  geom_point(size=3) +
  facet_wrap(~DATE, 
             labeller = as_labeller( 
               c("14_05" = "May 20014", "14_08" = "August 2014", 
                "15_05" = "May 2015", "15_08" = "August 2015"))) +
  #geom_smooth(method=lm, se = F) +
  labs(colour = "Mean Leaf\nTemperature\nRange") +
  #scale_color_viridis() +
  #ggtitle("PCA1 of CWM + LAD Mean") +
  xlab("FDis") +
  ylab("leaf angle distribution mean [°]")

ggplot(df, aes(x=PCA1, y = LAD_Median )) +
  geom_point(size=3) +
 facet_wrap(~ SPEI, labeller = as_labeller(   c("0.394" = "May 20014 SPEI = 0.394", "0.5" = "August 2014 SPEI = 0.5", "0.765" = "May 2015 SPEI = 0.765", "0.954" = "August 2015 SPEI = 0.954"))) +
  geom_smooth(method=lm, se=T) +
  scale_color_viridis() +
  #ggtitle("PCA1 of CWM + LAD Mean") +
  xlab("CWM (PCA1 Loading)") +
  ylab("leaf angle distribution median [°]")

facet_wrap

library(ggiraph)
library(ggiraphExtra)
require(plyr)


plot.model.sd = lm(LAD_Mean ~ PCA1 ,
                    na.action = na.omit , df)

ggPredict(plot.model.sd, se=T, colorAsFactor = F)



summary(df)
