
  library(rje)
  library(ggplot2)
  library(RColorBrewer)
  #library(rworldmap)
  #library(rworldxtra)
  library(broom)
  library(latex2exp)
  library(sf)
  
  
  #####  conection to PostGIS
  
  # GDAL drivers
  st_drivers() %>% 
    filter(grepl("Post", name))
  
  

  #install.packages('rje')
  # set folder directory
  pathdir <- "C:/Users/rm12/OneDrive - CEFAS/Roi/projects/ices/wgfbit/2019/FBIT-dev/TAF - ICES tutorial/"
  
  # load output step 1 
  setwd(paste(pathdir,"Step 1 Assign region of interest/Data layers",sep="/"))
  load("region_grid.RData")
  
  bargrid <- sf::st_read(dsn = con, query  =  "select * from wgfbit.euseamap_msfd_bbht_2019_c_square  ")
  
  # only select region for which we had sampling data in step 2
  bargrid <- bargrid %>% filter(msfd_bbht_merged %in%  c("Circalittoral sand", "Offshore circalittoral mixed sediment",  "Offshore circalittoral mud","Offshore circalittoral sand", "Upper bathyal sediment", "Offshore circalittoral coarse sediment"))
  
  # load output step 2
  setwd(paste(pathdir,"Step 2 Estimate relationships benthic data",sep="/"))
  load("Coefficients_Bdata.RData")  

  coef_int <- modcoeff[1]
  coef_ll  <- modcoeff[2]
  coef_OCMIX <- modcoeff[3]
  coef_OCM  <- modcoeff[4]
  coef_OCS  <- modcoeff[5]
  coef_UBS <- modcoeff[6]
   coef_ll_OCMIX <- modcoeff[7]
  coef_ll_OCMUD <- modcoeff[8]
  coef_ll_OCSAND <- modcoeff[9]
  coef_ll_UBS <- modcoeff[10]
  
  
  
  
  
  
  
  
  # for each grid cell we can predict longevity at a certain cumulative biomass
  # for example show median longevity (50% of biomass is longer-living)
  # use outcome of the statistical model -> Cumb ~ ll + MSFD + (1 | ID)
  # longevity is log transformed in the model so backtransform --> exp(log(x)) = x
  # cumulative biomass 0.5 needs a logit as the statistical model is binomial; 
  # logit(p) == log(p/(1-p)) ; p = exp(A)/(1+exp(A))
  
 
  # prepare grid specific information to predict longevity at a certain location
  OCM <- ifelse(bargrid$msfd_bbht_merged == "Offshore circalittoral mud",1,0)
  OCS <- ifelse(bargrid$msfd_bbht_merged == "Offshore circalittoral sand",1,0)
  OCMIX <- ifelse(bargrid$msfd_bbht_merged == "Offshore circalittoral mixed sediment",1,0)
  UBS <- ifelse(bargrid$msfd_bbht_merged == "Upper bathyal sediment",1,0)
  medLong <- exp((logit(0.5)  - coef_int - coef_OCM*OCM - coef_OCS*OCS - coef_OCMIX*OCMIX - coef_UBS*UBS - coef_ll_OCMUD*OCM - coef_ll_OCSAND*OCS - coef_ll_OCMIX*OCMIX - coef_ll_UBS*UBS) / coef_ll)
  bargrid$medLong <- medLong
  
  
  summary(bargrid)
  # plot the median longevity to see which areas are more sensitive than others
  # load map code 
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R") # warnings are okay
  map_plot(bargrid,"medLong",bluegreen)  

  # to predict impact we will not use median longevity but the longevity distribution
  # hence estimate the slope and intercept for each gridcell
  slope <- rep(coef_ll,nrow(bargrid))  # slope of binomial model
  intercept <- coef_int + coef_OCM*OCM + coef_OCS*OCS  + coef_OCMIX*OCMIX + coef_UBS*UBS  + coef_ll_OCMUD*OCM + coef_ll_OCSAND*OCS + coef_ll_OCMIX*OCMIX + coef_ll_UBS*UBS# intercept of binomial model
  
    
  bargrid$intercept <- intercept
  bargrid$slope     <- slope
  
  setwd(paste(pathdir,"Step 3 Predict sensitivity",sep="/"))  
  save(bargrid,file = "region_grid_sensitivity.RData")  

  bargrid[ bargrid$medLong > 1 &  bargrid$medLong < 3 , ] 
  
  ##save in PostGIS final layer ####
  
  table_id <- Id(schema = "wgfbit", table = "prediction_longevity")
 
  st_write(bargrid, dsn = con, layer = table_id,
           overwrite = TRUE, append = FALSE)
  
