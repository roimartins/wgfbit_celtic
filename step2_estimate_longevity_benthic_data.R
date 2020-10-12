
library(dplyr)
library(lme4)



##referenec station 

 

samples_sar <- dbGetQuery(con, 'select station_id, cum_prop_sar, cum_prop_biomass , c_square ,sar_avg, sar_sd,sample_gear_cat,
           msfd_bbht_merged, biomass as "Biomass", l1::numeric "L1", l1_3::numeric "L1_3", l3_10 "L3_10", l10 "L10" 
from wgfbit.benthic_data_combined_sar_msfd_bbht_cum_Sar_Step2 ')

table(samples_sar$sample_gear_cat)

str(sample_cov)

write.csv (samples_sar, "samples_sar.csv")
getwd()


sample_cov <-  dbGetQuery( con , "select * from  wgfbit.benthic_data_combined_sar_msfd_bbht_cum_Sar")

sample_cov <-  sample_cov[, 1:ncol(sample_cov)-1]

ggplot(samples_sar  ,  aes( y = cum_prop_sar , x =  cum_prop_biomass  , group = msfd_bbht_merged, color = msfd_bbht_merged)) + geom_point() + 
  facet_wrap( ~ sample_gear_cat, scales = "free_x") + theme_light()

# set folder directory
pathdir <- "C:/Users/rm12/OneDrive - CEFAS/Roi/projects/ices/wgfbit/2019/FBIT-dev/TAF - ICES tutorial/Step 2 Estimate relationships benthic data/"

# open benthic data  
setwd(paste(pathdir,"Benthic data",sep="/"))
datBen <- read.csv(file="Benthic_Data_tutorial.csv",header=T,sep=";")  
samples_sar[,10:13] <- samples_sar$Biomass *  samples_sar[,10:13] # multiply biomasss with fuzzy coded trait data
str(samples_sar )
which ( is.na( samples_sar[,10:13])  ) 

# summarize benthic data per sample ID and calculate the fraction
namesCol <- c("Biomass","L1","L1_3","L3_10","L10")
indexcol <- which(names(datBen) %in% namesCol)


datBen <- samples_sar
statdat <- datBen %>% 
  group_by(station_id) %>%
  summarise_each(funs(sum) , names( datBen) [indexcol]  )
  
 2/0.239

statdat <- as.data.frame(statdat)
statdat[,c(3:6)] <- statdat[,c(3:6)]/statdat$Biomass

# now link to environmental conditions ( already in my sql query ) 
  #statEnv <- read.csv(file="Env_Data_tutorial.csv",header=T,sep=";")  
  #statEnv <- cbind(statEnv,statdat[match(statEnv$ID,statdat$sample_ID), c(3:6)])

statEnv <- statdat %>% left_join(sample_cov, by = 'station_id')
dim(statEnv)
colnames(statEnv)

###get number of stations by habitat type
df_bio_st_hab <- as.data.frame( table(statEnv$msfd_bbht_merged) )
write.table(df_bio_st_hab,"clipboard",sep="\t",row.names=FALSE,col.names= colnames(df_bio_st_hab))


# select only MSFD habitats with multiple observations



statEnv <- subset(statEnv,statEnv$msfd_bbht_merged %in% c("Circalittoral sand", "Offshore circalittoral mixed sediment",  "Offshore circalittoral mud",
                                                          "Offshore circalittoral sand", "Upper bathyal sediment", "Offshore circalittoral coarse sediment"))

# now prepare for statistical analysis

# get longevity categories seperate for each station 
ID        <-rep(statEnv$station_id,3)
MSFD      <-rep(statEnv$msfd_bbht_merged,3)
Cumb      <-c(statEnv$L1,(statEnv$L1+statEnv$L1_3),(statEnv$L1+statEnv$L1_3+statEnv$L3_10)) ####why the biomas is cumulative summed ??!!
Longevity <-c(rep(1,nrow(statEnv)),rep(3,nrow(statEnv)),rep(10,nrow(statEnv)))  

fulldat   <-data.frame(ID,MSFD,Cumb,Longevity) 
fulldat$ll <-log(fulldat$Longevity)

fulldat %>% filter ( Longevity == 3)
table_id <- Id(schema = "wgfbit", table = "benthic_data_combined__fulldat_Step2")

dbWriteTable(conn = con ,   table_id, fulldat )

unique(fulldat[, c("ll", "MSFD")])

# add a small number to values very close to 0 and 1 
for (i in 1:(nrow(fulldat))){
  if (fulldat$Cumb[i] < 1e-3){ fulldat$Cumb[i] <- 1e-3}
  if (fulldat$Cumb[i] > 0.999){fulldat$Cumb[i] <- 0.999}
}   

# fit a linear mixed model with sampling station as random factor and MSFD habitats as exploratory variable
mod1   <-  glmer(Cumb ~ ll + MSFD*ll + (1 | ID), data=fulldat, family=binomial)
mod2   <-  glmer(Cumb ~ ll + MSFD + (1 | ID), data=fulldat, family=binomial)
mod3   <-  glmer(Cumb ~ ll + (1 | ID), data=fulldat, family=binomial)
AIC(mod1,mod2,mod3)
# models give a singular fit --> the random effect is very small (but you can argue that it, in principle, has to be included)

modcoeff  <-  fixef(mod1)

setwd(paste(pathdir))
save(modcoeff,file="Coefficients_Bdata.RData")  
