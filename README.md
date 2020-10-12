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

-	Habitats & depth

o	MSFD broad habitats Into c-square

o	 Calculated proportion of habitat by c-square ( external software , result table in csv  ) 
o	 Compare survey data habitat distribution by sample with c-square habitat  proportion presence  ( are the sampels taken from the most representative habitat ? ) 
o	 GEBCO bathymetry values avg/sd by c_square 
( Paul habitats table) 

o	 Supply as well sf scripts to do it in the script instead of GIS desktop software 
(Roi ) 
  
-	Pressures
o	Fisheries ( ICES WG SFD - ICES API ( Roi R script - gett_vms_library.R)  )
o	Fisherie aggregated at c-square and keep surface and subsurface sar's. 
o	Fisheries cumulative pressure indicators ( sd , average, cumulative  by c-square ) 
( Roi SQL -> R ) 


- assign area of interest

o	 C-square grid for the Celtic Sea Region
o	 select ecoregion areas based on relevant data exploration
   -  Use bathymetry for non fisheable areas (contour lines) 


-	Additional enviromental parameter : 

o	Environmental additional paramater by c-square ( coverage issues ) . Not a priority


## Step 1:  Assign region of Interest / Define c-squares within management units

- C-square additional information: 

o	Assign EEZ
o	Assign OSPAR reporting units
o assign ICES ecoregions
 

## Step 2 Estimating longevity benthic data.R

Plug data from STEP 0 into the GENERAL FBIT STEP 2 to follow the process  ( Roi) 

## Step 3 Predict sensitivity

script from Roi 

## Step 4 Calculate impact/
script from Roi 

