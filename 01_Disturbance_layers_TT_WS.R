## 
## Caribou disturbance analysis 2018 for Tweedsmuir and Telkwa
## 
## November 14th 2018 
##
## written by genevieve perkins (genevieve.perkins@gov.bc.ca)
## 
## This requires initial preparation of layers within arcmap. 
## Step 1) 
## Assemble layers as per datasheet in arcmap mxd 
## Clip the layers to the range with the largest extent (for example range boundary for Boreal (not core))
## Create a filegeodatabdase and output these to the given data base. 
##
## Step 2)
## Run through the script below. You may need to adjust 
## - the names of files to match your arcmap exports 
## - the directory/folder sructure. 
##
## General notes: 
## For each disturbance layers the script will read in, intersect with range and core areas and calculate the area and or length. The peripery area will be calculated for each herd as well. 
## With each layer the compiled disturbance will also be calculated. 

## Associated help files for reference: 
##https://gis.stackexchange.com/questions/265863/how-to-read-a-feature-class-in-an-esri-personal-geodatabase-using-r
##http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
#http://www.rspatial.org/spatial/rst/7-vectmanip.html#spatial-queries
#https://r-spatial.github.io/sf/articles/sf1.html
#https://github.com/r-spatial/sf/issues/290

## Read in packages and libraries required: 

install.packages(c("rgdal","ggplot2","sp","dplyr","raster","rgeos","maptools","magrittr","tibble", 
			"tidyr","sf","lwgeom","mapview"),dep = T )

# for running on external GTS 
install.packages(c("rgdal","ggplot2","sp","dplyr","raster","rgeos","maptools","magrittr","tibble", 
                   "tidyr","sf","lwgeom","mapview"),dep = T , lib = "C:/Users/genperk/R_packages/")

library(ggplot2)
library(dplyr)
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(magrittr)
library(tibble)
library(tidyr)
library(sf)
library(lwgeom)
library(mapview)
#library(RQGIS)
#library(reticulate)


## set your output directory 

## To run analysis on restricted folder
#out.dir = "X:/projects/Desktop_Analysis/data/output1/"
#temp.dir = "X:/projects/Desktop_Analysis/data/temp/"

# to run analysis on W drive: 
#out.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Outputs/"
#temp.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Data"

# to run analysis on C drive: 
out.dir = "C:/Temp/TweedTelkwa/Temp/Perkins/Outputs/"
temp.dir = "C:/Temp/TweedTelkwa/Temp/Perkins/Data/"

temp.dir = "T:/temp/Perkins/Data/"
out.dir = "T:/temp/Perkins/Outputs/" 

## Set your input geodatabases (this will be where you saved your arcmap exports)
## edit these to your filepath and name of gdb

#Base= "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Data/Base_data.gdb" # contains 
#Base= "T:/temp/Perkins/Data/Base_data.gdb"
Base= "C:/temp/TweedTelkwa/Temp/Perkins/Data/Base_data.gdb" # contains 

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
base_list <- ogrListLayers(Base); print(base_list)

##############################################################################################
# Read in herd boundary layers 

b.range <- st_read(dsn=Base,layer="TT_boundary")
b.range <-st_cast(b.range,"MULTIPOLYGON")     # fix overlaps
  
# read in the whitesale boundary (simple dissolved range)

ws.range <- st_read(dsn=Base,layer="WS_bdry_simple") 
plot(st_geometry(ws.range),col = "red",add = TRUE)
plot(st_geometry(b.range), add = TRUE)

ws.range <-st_cast(ws.range,"POLYGON") 
ws.range$area = as.numeric(st_area(ws.range))

# calculate the areas of each of the Herd habitat types (HWSR,LWR,LSR,MR,ALL)
Herd_key<- data.frame(ws.range) %>% 
  mutate(R_area_ha = area/10000) %>%
  mutate(AOI = "whitesale")%>%
  group_by(AOI)%>%
  summarise(R_area_ha = sum(R_area_ha))

##############################################################################################
# Read in individual Disturbance layers:

# 1) pipeline (length)
r.pipe <- st_read(dsn=Base,layer="Pipeline_clip") # multistring 
          r.pipe.int = st_intersection(ws.range,r.pipe)   # intersect with single all ranges
          # RANGE: intersect with range = Zero
          
          # no rail 
        
# 2) transmission (length and area) 
r.tran.sf <- st_read(dsn=Base,layer="Trans_clip") # multistring  
          
          # 1) RANGE: calculate the range extent to use range extent 
          r.tran <- st_intersection(ws.range,r.tran.sf)# intersect with ranges
          
          # no transmission 
         
# 3) mine
r.mine.sf <- st_read(dsn=Base,layer="Mining_clip") # multipoly
r.mine <- st_zm(r.mine.sf,drop = TRUE) # drop the z portion of the shapefile. 

          # 1) RANGE: calculate the range extent to use range extent 
          r.mine <- st_intersection(ws.range,r.mine)
          
          # no mine


# 4) agriculture 
r.agr.sf <- st_read(dsn=Base,layer="Agri_clip") # multipoly
r.agr.sf <- st_zm(r.agr.sf,drop = TRUE)

        # 1) RANGE: calculate the range extent to use range extent 
        r.agr <- st_intersection(ws.range,r.agr.sf)# intersect with ranges
        
        # no agriculture
        
# 5) air strips
r.air.sf <- st_read(dsn=Base,layer="Airstrip_clip") # multipoly
r.air.sf <- st_zm(r.air.sf,drop = TRUE)
r.air.sf <- st_cast(r.air.sf,"POLYGON")

          # 1) RANGE: calculate the range extent to use range extent 
          r.air <- st_intersection(ws.range,r.air.sf)# intersect with ranges
          
          # no air
          
# 6) dams 
r.dam.sf <- st_read(dsn=Base,layer="dams_clip" ) # multipoly
r.dam.sf  <- st_zm(r.dam.sf ,drop = TRUE)
        ## RANGE: intersect with range and calculate length per range
        r.dams = st_intersection(ws.range,r.dam.sf)   # intersect with ranges
        
        
        #no dams
        
    
# 7) reservoirs 
r.res.sf <- st_read(dsn=Base,layer="Resevoir_clip") ;  plot(st_geometry(r.res.sf))         
              # only one feature found (Nechako Reservoir) This is not included as it is not calculated as part of the area of the herds.
          ## RANGE: intersect with range and calculate length per range
          r.res = st_intersection(ws.range,r.res.sf)   # intersect with ranges
          r.res <- st_cast(r.res,"POLYGON")
          r.res$area <- as.numeric(st_area(r.res))
          plot(st_geometry(r.res))
          plot(st_geometry(ws.range),add = T, col = "red")
          # ignore the resevoir layers as not included in area cals
        
# 9) wells 
r.wells.sf <- st_read(dsn=Base,layer="Wells_clip" ) # multipoly
# no wells present in these herd

          #no wells

# 10) urban
r.urban.sf <- st_read(dsn=Base,layer="urban_clip" ) # multipoly
r.urban.sf <- st_zm(r.urban.sf ,drop = TRUE)
r.urban.sf <- st_union(r.urban.sf);
r.urban.sf <- st_cast(r.urban.sf,"POLYGON")
#all.urban = sum(st_area(r.urban.sf)) ; plot(st_geometry(r.urban.sf))  
    
            ## Intersect with RANGE: intersect with range and calculate length per range
            r.urban = st_intersection(ws.range,r.urban.sf)   # intersect with range
            
            # no urban
           
# 11) Rail  # no rail in the herds
r.rail.sf <- st_read(dsn=Base,layer="Rail_clip") # multipoly
st_is_empty(r.rail.sf)
r.rail.sf <- st_intersection(ws.range,r.rail.sf)
      
            # no rail         

# 12)  Recreation sites 
r.rec.sf <- st_read(dsn=Base,layer="Rec_clip") # multipoly
r.rec.sf <- st_zm(r.rec.sf ,drop = TRUE)
      
        # 1) RANGE: calculate the range extent to use range extent 
        r.rec <- st_intersection(ws.range,r.rec.sf)# intersect with range
        r.rec <- st_cast(r.rec,"POLYGON")
        r.rec$area = as.numeric(st_area(r.rec))
        
        #plot(st_geometry(r.rec),add = T,col = "blue") 

        r.rec.df = data.frame(r.rec)        # calculate the length per range 
        r.rec.df.out  = r.rec.df%>% 
          group_by(PROJECT_TYPE) %>% 
          summarise(R_Rec_m2 = sum(area))
        
        ##plot(st_geometry(r.rec))
        #st_write(r.rec,"Dist_R_Rec.shp")       #write out individual dist_layer for Range
        
        # combine into disturbance by layer 
        all.range.out <- merge(Herd_key,r.rec.df.out)  
        all.range.out[is.na(all.range.out)]<-0      
        
# 13) Seismic Lines
b.s1 = sf::st_read(dsn = Base , layer ="Seismic_clip")
b.s1 <- st_zm(b.s1,drop = TRUE)
b.s1 <- st_make_valid(b.s1)
b.s1<- st_cast(b.s1, "POLYGON")
b.s1 <- st_union(b.s1)

        b.s1 <- st_intersection(ws.range,b.s1)# intersect with range
        
        # no seismic
      
## 14) Roads # buffer to 15m width as per blairs analysis

## Tweedsmuir herd 
      b.r2.sf = sf::st_read(dsn = Base , layer ="Tw_roads" ) 
      b.r2.sf <- st_zm( b.r2.sf ,drop = TRUE)
      b.r2 = st_intersection(ws.range,b.r2.sf )
      #plot(st_geometry(b.r2),add = TRUE)
      b.r2 <- st_buffer(b.r2,7.5)  # buffer   
      
      st_is_valid(b.r2)
      #st_make_valid(b.r2)
      b.r2.sf <- st_cast(b.r2,"POLYGON")
      b.r2.sf$Area.m <- as.numeric(st_area(b.r2.sf))
      #plot(st_geometry(b.r2.sf))
      #st_write(r.pipe.int,"Dist_R_pipe.shp")       #write out individual dist_layer for Range
      b.r2.df = data.frame(b.r2.sf)        # calcaulte area
      b.r2.df.out  = b.r2.df %>% 
        summarise(R_Road_area_m = sum(Area.m))
    
      # combine into disturbance by layer 
      all.range.out <- cbind(all.range.out, b.r2.df.out)  
     
      ## ALL DISTURBANCE: add rec to roads
      out1 = st_union(r.rec,b.r2.sf ) ;
      plot(st_geometry(out1))
      ##x.area = sum(st_area(out8))     ; x.area  # 420881436 [m^2]
      
#######################################################################

# end of part 1: write out all the static disturbance types;
        
# write out all the layers combined into a single disturbance layer ( note this excludes Roads and Seismic )
st_write(out1,paste(temp.dir,"WS_static_distubance.shp",sep = "")) # this writes out as single layer   
        
# Write out the datasheet onto a temp file 
write.csv(all.range.out,paste(temp.dir,"WS_static_dist.csv",sep = "") )        
  
################################################################################    
################################################################################

## 8) Cutblock ## this is all years of consolidated cutblock layer 
# Split out age classes

b.r.c = st_read(dsn = Base , layer = "cutblock_union_Te")
b.r.c <- st_zm(b.r.c ,drop = TRUE)
b.r.c = st_cast(b.r.c,"POLYGON")


b.r.c$HARVEST_YEAR2 = b.r.c$HARVEST_YEAR + b.r.c$HARVEST_YEAR_1

b.r.c$TimeSinceCut = 2018 - b.r.c$HARVEST_YEAR;


b.r.c <- mutate(b.r.c,TimeSinceCut = ifelse(HARVEST_YEAR == 0,2018-b.r.c$HARVEST_YEAR_1,0))


b.r.c %>% filter(TimeSinceCut == 0)


# create new column with age since cut 
#sort(unique(b.r.c$TimeSinceCut)) # check the range in years since cut #note in the boreal the oldest age cut is 78 years 
#sort(unique(b.r.c$HARVEST_YEAR2))

# cutblocks 0-80 years
b.r.c0.80 = b.r.c[b.r.c$TimeSinceCut < 81,] ; unique(b.r.c0.80$TimeSinceCut); #plot(b.r.c0.80$Shape)
b.r.c0.80 = st_cast(b.r.c0.80,"POLYGON"); #
b.r.c0.80 = st_make_valid(b.r.c0.80)
b.r.c0.80.0 <- b.r.c0.80  # set this asside to use later in the code
#all.cut = sum(st_area(b.r.c0.80)) #386611203 [m^2]
b.r.c0.80.u = st_union(b.r.c0.80) # all cutblocks 0-80 years old 

# cutblocks 0-40 years
b.r.c0.40 = b.r.c[b.r.c$TimeSinceCut < 41,] ; unique(b.r.c0.40$TimeSinceCut); #plot(st_geometry(b.r.c0.40))
b.r.c0.40 = st_cast(b.r.c0.40,"POLYGON")
b.r.c0.40 = st_make_valid(b.r.c0.40) #; st_is_valid(b.r.c0.40)
#all.cut = sum(st_area(b.r.c0.40)) #260124543  [m^2]
b.r.c0.40.u = st_union(b.r.c0.40) # all cutblocks 0-40 years old 

## ALL DISTURBANCE UNION 5 # may need to run this in stand alone R rather than R -studio
#out5 = st_union(b.r.c0.40.u,out4) ; plot(st_geometry(out5))
#out5 = st_union(out5); plot(st_geometry(out5))
#x.area = sum(st_area(out5)) ; x.area #399025272 [m^2]

## RANGE: intersect with range
r.cut = st_intersection(all.range,b.r.c0.80.0)     # intersect with ranges # this may take some time
#st_is_valid(r.cut)
r.cut <- st_cast(r.cut,"POLYGON")
r.cut$area <- as.numeric(st_area(r.cut)) 
##plot(st_geometry(r.cut))
#st_write(r.cut,"Dist_R_cutblock0.80.shp")       #write out individual dist_layer for Range
r.cut.df = data.frame(r.cut)        # calculate the length per range 
sort(unique(b.r.c0.80.0$HARVEST_YEAR)) # check the range in years when cutblocks were logged

# add a column to differentiate the age brackets of cutblocks 
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1940 & HARVEST_YEAR <= 1949,1940,0))
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1950 & HARVEST_YEAR <= 1959,1950,dec.period))
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1960 & HARVEST_YEAR <= 1969,1960,dec.period))
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1970 & HARVEST_YEAR <= 1979,1970,dec.period))   
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1980 & HARVEST_YEAR <= 1989,1980,dec.period))   
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 1990 & HARVEST_YEAR <= 1999,1990,dec.period)) 
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 2000 & HARVEST_YEAR <= 2009,2000,dec.period))   
r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR >= 2010 & HARVEST_YEAR <= 2019,2010,dec.period)) 

#r.cut.df[r.cut.df$dec.period == 0,]
#unique(r.cut.df$dec.period)

# output the amount of cutblock by range (all years (0-80))  
r.cut.df.out  = r.cut.df %>% 
  group_by(SiteName,V17_CH ) %>% 
  summarise(R_cut0_80_m2 = sum(area))

# output the amount of cutblock by range (all years (0-40))  
r.cut.df.out.0.40 <- r.cut.df %>%
  filter(HARVEST_YEAR >= 1978) %>% 
  group_by(SiteName,V17_CH ) %>% 
  summarise(R_cut0_40_m2 = sum(area))

r.cut.out = merge(r.cut.df.out,r.cut.df.out.0.40 )

# combine into disturbance by layer 
all.range.out <- left_join(all.range.out,r.cut.out)  
all.range.out[is.na(all.range.out)]<-0

# output cut blocks by decade # we will use this later in the temporal data aggregation (with pest and burns)
cut.decade = r.cut.df %>% 
  group_by(SiteName,V17_CH,dec.period ) %>% 
  summarise(R_cut0_80_m2 = sum(area))
## we will use this later with burn data 







#### NATURAL DISTURBANCE   

## BURN:
# Add to Burn information Break down of burns into 0-40 and 0-80 years (and total) 
# Break down burns into fire years (same as cutblocks) 

b.r.0 = st_read(dsn = Base, layer ="fire_clip")
b.r.0<- st_zm(b.r.0 ,drop = TRUE) # this is a linear feature so need to buffer to estimate area calcs
#st_is_valid(b.r.0)
b.r.0$TimeSinceBurn = 2018-b.r.0$FIRE_YEAR #; plot(b.r.0$Shape)
b.r.0 <- st_intersection(all.range,b.r.0) #; st_is_valid(b.r.0)
b.r.0 <- st_cast(b.r.0,"POLYGON")
b.r.0$area.m = as.numeric(st_area(b.r.0))
#head(b.r.0)

    # burns 0-40 years 
    b.r.0.40 = b.r.0[b.r.0$TimeSinceBurn <41,]; #sort(unique(b.r.0$TimeSinceBurn)) 
    b.r.0.40 <- st_cast(b.r.0.40,"POLYGON")
    b.r.0.40.df = data.frame(b.r.0.40)        # calculate the length per range 
    b.r.0.40.out  =  b.r.0.40.df%>% 
      group_by(SiteName,V17_CH) %>% 
      summarise(R_Burn040_m2 = sum(area.m))
    
    # burns 0-80 years 
    b.r.0.80 =b.r.0[b.r.0$TimeSinceBurn <81,];
    b.r.0.80 <- st_cast(b.r.0.80,"POLYGON")
    b.r.0.80.df = data.frame(b.r.0.80)        # calculate the length per range 
    b.r.0.80.out  =  b.r.0.80.df%>% 
      group_by(SiteName,V17_CH) %>% 
      summarise(R_Burn080_m2 = sum(area.m))
    
    r.burn.out = merge(b.r.0.40.out,b.r.0.80.out )
    
    # combine into disturbance by layer 
    all.range.out <- left_join(all.range.out,r.burn.out)  
    all.range.out[is.na(all.range.out)]<-0
    
    # split burns into decades (using all the entire data set)
    b.r.00.df <-   b.r.0.80.df 
  
          # add a column to differentiate the age brackets of cutblocks 
          b.r.00.df <- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1940 & FIRE_YEAR <= 1949,1940,0))
          b.r.00.df <- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1950 & FIRE_YEAR <= 1959,1950,dec.period))
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1960 & FIRE_YEAR <= 1969,1960,dec.period))
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1970 & FIRE_YEAR <= 1979,1970,dec.period))   
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1980 & FIRE_YEAR <= 1989,1980,dec.period))   
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 1990 & FIRE_YEAR <= 1999,1990,dec.period)) 
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 2000 & FIRE_YEAR <= 2009,2000,dec.period))   
          b.r.00.df<- mutate(b.r.00.df,dec.period = ifelse(FIRE_YEAR >= 2010 & FIRE_YEAR <= 2019,2010,dec.period)) 
        
          b.r.00.df[b.r.00.df$dec.period == 0,]
          b.r.00.df = b.r.00.df %>% filter(dec.period>0)
          unique(b.r.00.df$dec.period)
          
          #head(b.r.00.df$dec.period)
          # need to adjust this to cols with dec period rather than Rows (?Spread?)
          Burn.dec =b.r.00.df %>% group_by(SiteName,V17_CH,dec.period ) %>% summarise(R_burn0_80_m2 = sum(area.m))
          

###########################################

# join the cutlock with burn decade data. 
all.temp.data = merge(cut.decade,Burn.dec,by = c("SiteName", "V17_CH", "dec.period"), all = TRUE)
all.temp.data[is.na(all.temp.data)]<- 0 

############################################
### PEST 



## Still to do - in arcmap interesect and clip and export 





r.pest <-  st_read(dsn = Base, layer ="Pest_clip")
r.pest<- st_zm(r.pest ,drop = TRUE) # this is a linear feature so need to buffer to estimate area calcs
r.pest = st_cast(r.pest,"POLYGON")
st_make_valid(r.pest)
st_make_valid(all.range)
st_is_valid(all.range)
        # Range 
        r.pest <-  st_intersection(all.range,r.pest) #; st_is_valid(r.pest)
        
        ## UP TO HERE 
        # subset 1) pest 0 - 40 years 
        
        # work with the data frames for 0-40 years ## RANGE 
        r.pest$area.m = as.numeric(st_area(r.pest)) 
        r.pest.df = data.frame(r.pest)        # calculate the length per range 
        
        sort(unique(r.pest$CAPTURE_YEAR)) #check the unique years of 
        
        
        # subset 2) pest (all years)
        
        
        
        # Pest by decade 
        
        
        # Pest by type
        
        
        
        ## UP TO HERE 
        
        
        
        # add a column to differentiate the age brackets of pest capture
        r.pest.df<- r.pest.df %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1988 & CAPTURE_YEAR <= 1997,1988,0))   
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1998 & CAPTURE_YEAR <= 2007,1998,dec.period))   
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2008 & CAPTURE_YEAR <= 2018,2008,dec.period)) 
        #r.pest.df[r.pest.df$dec.period == 0,]
        #head(r.pest.df)
        #unique(r.pest.df$dec.period)
        
        # output the amount of burns by range (all years (0-80))  
        r.pest.df.out  = r.pest.df %>% 
            group_by(Range) %>% summarise(R_pest_m2 = sum(area.m))
        
        #output the amount of cutblock per decade (all years) 
        r.pest.df.out.temp  =   r.pest.df %>% group_by(Range,dec.period) %>% summarise(R_pest_dec_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        r.pest.df.out.type  = r.pest.df %>% group_by(Range,PEST_SPECIES_CODE) %>% summarise(R_pest_type_m2 = sum(area.m))
          
        # output the amount of pests by range (all years (0-80))  
        c.pest.df.out  = c.pest.df %>% group_by(Range) %>% summarise(C_pest_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        c.pest.df.out.temp  = c.pest.df %>% group_by(Range,dec.period) %>% summarise(C_pest_dec_m2 = sum(area.m))
        #output the amount of cutblock per decade (all years) 
        c.pest.df.out.type  = c.pest.df %>% group_by(Range,PEST_SPECIES_CODE) %>% summarise(C_pest_type_m2 = sum(area.m))
        
    
        
   
############################################################################
# Aggregate natural disturbance    
        
# Pests + Burns (0-40 years combined)
        
############################################################################       
    
# Step 1 aggregate table and output 
        
out.temp = left_join(burn.all, pest.range)   
write.csv(out.temp,paste(out.dir,"Nat_dist_type_RPC.csv",sep =""))    

# write out temporal datasets: 
all.temp.data <-left_join(all.temp.data,pest.temp)           
write.csv(all.temp.data,paste(out.dir,"Temporal_decade_dist_RPC.csv",sep ="")) 

       
# Add disturbance combined: UNION Natural Dist 
r.pest.0 = st_union(r.pest.0) 
out.nat = st_union(all.dis.burn, r.pest.0) ; plot(st_geometry(out.nat))

# write out shape file 
out.nat2 = st_cast(out.nat,"POLYGON")
xnat.area2 = sum(st_area(out.nat2)) 
x = st_simplify(out.nat2)    ;  plot(st_geometry(x)) 
st_write(x, "All_natural_dissolve.shp") # this writes out as single layer   

# calculate the range and core overlaps for "all disturbance" 
out.nat1 = st_union(out.nat);
st_make_valid(out.nat1)

out.nat1 <- st_cast(out.nat1,"POLYGON")

# calcualte the totals for all nat. disturbacne 
out.nat.r = st_intersection(b.range,out.nat1)
out.nat.c = st_intersection(b.core.r,out.nat1)
# add areas
out.nat.r$area.m = st_area(out.nat.r)
out.nat.c$area.m = st_area(out.nat.c)
# conver to DF
out.nat.r.df <-data.frame(out.nat.r)
out.nat.c.df <-data.frame(out.nat.c)
# summarise data 
range.out  <- out.nat.r.df%>%  group_by(Range) %>% summarise(R_allnatdis_m = sum(area.m))
core.out <- out.nat.c.df %>%  group_by(Range) %>% summarise(C_allnatdis_m = sum(area.m))
# 

all.dis<- left_join(range.out,core.out)
all.dis$P_allnatdis_m  = all.dis$R_allnatdis_m  - all.dis$C_allnatdis_m 
write.csv(all.dis,paste(out.dir,"Combines_Nat_dist_RPC.csv",sep ="")) 


###################################################################################

# once this script is run - then use the 04_Disturbance_Data_summary.R script 
# to collate all the csv's generated as part of this script and oputput the main summary table for reporting


