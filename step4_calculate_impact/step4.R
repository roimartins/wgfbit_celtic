
  library(rgdal)
  library(ggplot2)
  library(RColorBrewer)
  library(rworldmap)
  library(rworldxtra)
  library(broom)
  library(latex2exp)
  
  # set folder directory
  pathdir <-  "C:/Users/rm12/OneDrive - CEFAS/Roi/projects/ices/wgfbit/2019/FBIT-dev/TAF - ICES tutorial/"

  # load step 3 output
  setwd(paste(pathdir,"Step 3 Predict sensitivity",sep="/"))
  load("region_grid_sensitivity.RData")
  
  # load available fishing data: The shapefile datasets for the OSPAR region are available at: https://doi.org/10.17895/ices.pub.2861 ####
  workingdir<-paste(pathdir,"Step 4 Calculate impact/ICES.2017.OSPAR.Technical-Service-VMS-fishing-pressure",sep="/")
  
  
  # select a year and load swept area data
  Period <- 2015
  TBB   <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Beam_",Period[1],sep=""))
  OT    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Otter_",Period[1],sep=""))
  TD    <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Dredge_",Period[1],sep=""))
  Seine <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_Seine_",Period[1],sep=""))
  
  
  ###load using the PostGIS ICES later layers 
  Period <- 2015
  bargrid <- sf::st_read(dsn = con, query  =  "select * from wgfbit.prediction_longevity ")
  
  table_id <- Id(schema = "fishing_activity_ices", table = paste0 ( "celtic_beam_", Period)) 
  TBB   <- st_read(dsn = con ,layer=table_id)
  table_id <- Id(schema = "fishing_activity_ices", table = paste0 ( "celtic_otter_", Period)) 
  OT    <- st_read(dsn = con ,layer=table_id)
  table_id <- Id(schema = "fishing_activity_ices", table = paste0 ( "celtic_dredge_", Period)) 
  TD    <- st_read(dsn = con ,layer=table_id)
  table_id <- Id(schema = "fishing_activity_ices", table = paste0 ( "celtic_seine_", Period)) 
  Seine <- st_read(dsn = con ,layer=table_id)
  
  
  
    
  # link to grid via c-squares
  bargrid <- bargrid %>% left_join(as.data.frame(TBB)[,c( "surface_sar", "c_square") ]   , by = "c_square" )
  bargrid <- bargrid %>% left_join(as.data.frame(OT)[,c( "surface_sar", "c_square") ]   , by = "c_square" )
  bargrid <- bargrid %>% left_join(as.data.frame(TD)[,c( "surface_sar", "c_square") ]   , by = "c_square" )
  bargrid <- bargrid %>% left_join(as.data.frame(Seine)[,c( "surface_sar", "c_square") ]   , by = "c_square" )
  
  
 
  
  # add colnames 
  nb <- ncol(bargrid )
  colnames(bargrid )[(nb-3):nb-1] <- c("TBB_SurfSAR","OT_SurfSAR","TD_SurfSAR","Seine_SurfSAR")
  
  Depl_TBB  <- 0.14 * bargrid$TBB_SurfSAR   ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_OT   <- 0.06 * bargrid$OT_SurfSAR    ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_TD   <- 0.20 * bargrid$TD_SurfSAR    ### data from Hiddink et al. PNAS 2017 Table S4
  Depl_sein <- 0.06 * bargrid$Seine_SurfSAR ### unknown (now similar to otter trawling) 
  Depl <- cbind(Depl_TBB,Depl_OT,Depl_TD,Depl_sein)
  Depl_tot<-rowSums(Depl,na.rm=T)
  bargrid$Depl_tot <- Depl_tot
  
  # calculate state from RBS function per grid cell
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("RBS.R")
  
  state <- c()
  for(j in 1:nrow(bargrid)){
   state[j] <- RBS(a=bargrid$slope[j],b=bargrid$intercept[j],Fd=bargrid$Depl_tot[j])
    }
  
  bargrid$state <- state

  
  ###write in PostGIS ####
  table_id <- Id(schema = "wgfbit", table = "prediction_rbs_stef") 
  
  st_write(bargrid, dsn = con , layer =  table_id, overwrite = TRUE )
  
  # plot state as a function of total depletion
  plot(bargrid@data$state~bargrid@data$Depl_tot,ylab="State (PD model)",xlab="Depletion (SAR*d)",las=1)
  
  # make a map of benthic state
  setwd(paste(pathdir,"Utilities",sep="/"))
  source("map_plot.R") # warnings are okay
  map_plot(bargrid,"state",purples)  
  
  # estimate state per MSFD habitat
  aggregate(state ~ msfd_bbht_merged, data=bargrid, mean)
  
  # make a map of swept areas, weight and value
  Total  <- readOGR(dsn = workingdir ,layer=paste("OSPAR_intensity_total_",Period[1],sep=""))
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(5)])
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(6)])
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(7)])
  bargrid <- cbind(bargrid, Total@data[match(bargrid@data$csquares,Total@data$c_square), c(8)])
  
  # add colnames 
  nb <- ncol(bargrid@data)
  colnames(bargrid@data)[(nb-3):nb] <- c("SurfaceSAR","Subsurface","totweight","totvalue")

  map_plot(bargrid,"SurfaceSAR",bluegreen)  
  map_plot(bargrid,"Subsurface",bluegreen)  
  map_plot(bargrid,"totweight",yellowred)  
  map_plot(bargrid,"totvalue",yellowred)  
  
