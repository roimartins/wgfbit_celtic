# wgfbit_celtic
scripts trelated to wgfbit_celtic



## Step 0: Cleaning & format input data 
-	Survey data
o	Traits matrix
o	Longevity classes
o	Biomass
o	Habitats information
o	Additional explorations script ( e.g. biomass coverage  ) 

(Pascal script)

-	Habitats 
o	MSFD broad habitats Into c-square
o	 Calculated proportion of habitat by c-square ( external software , result table in csv  ) 
o	 Compare survey data habitat distribution by sample with c-square habitat  proportion presence  ( are the sampels taken from the most representative habitat ? ) 
( Paul habitats table) 
  
-	Pressures
o	Fisheries ( ICES WG SFD - ICES API ( Roi R script - gett_vms_library.R)  )
o	Fisherie aggregated at c-square and keep surface and subsurface sar's. 
o	Fisheries cumulative pressure indicators ( sd , average, cumulative  by c-square ) 
( Roi SQL -> R ) 

-	Additional information : 
o	Environmental additional paramater ( coverage issues ) . Not a priority
o	GEBCO bathymetry 

## Step 1: Assign region of Interest 
-	Celtic region: 
o	Split in part option 
o	Use bathymetry (contour lines) 

## Step 2 Estimating longevity benthic data.R




Plug our modifications into existing script 
## Step 3 Predict sensitivity

## Step 4 Calculate impact/

