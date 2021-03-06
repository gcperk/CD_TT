## 
## Caribou disturbance analysis 2018 for Whitesale region (part of the Tweedsmuir Area )
## 
## November 26th  2018 
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

#install.packages(c("rgdal","ggplot2","sp","dplyr","raster","rgeos","maptools","magrittr","tibble", 
#			"tidyr","sf","lwgeom","mapview"),dep = T )

# for running on external GTS 
#install.packages(c("rgdal","ggplot2","sp","dplyr","raster","rgeos","maptools","magrittr","tibble", 
#                   "tidyr","sf","lwgeom","mapview"),dep = T , lib = "C:/Users/genperk/R_packages/")

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

# to run analysis on W drive: 
#out.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Outputs/"
#temp.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Data"


# to run on the I drive
out.dir = "I:/ES/General/Wildlife/Wildlife Species/Caribou/Tweedsmuir caribou/2015-2018/Disturbance Analysis/Whitesale_AOI/Outputs/"
temp.dir = "I:/ES/General/Wildlife/Wildlife Species/Caribou/Tweedsmuir caribou/2015-2018/Disturbance Analysis/Whitesale_AOI/Data/"
final.dir = "I:/ES/General/Wildlife/Wildlife Species/Caribou/Tweedsmuir caribou/2015-2018/Disturbance Analysis/Whitesale_AOI/Final/"

# to run analysis on C drive: 
out.dir = "C:/Temp/TweedTelkwa/Temp/Perkins/Outputs/"
temp.dir = "C:/Temp/TweedTelkwa/Temp/Perkins/Data/"
final.dir = "C:/Temp/TweedTelkwa/Temp/Perkins/Outputs/whitesale/"

## Set your input geodatabases (this will be where you saved your arcmap exports)
## edit these to your filepath and name of gdb

#Base= "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Data/Base_data.gdb" # contains 
#Base= "T:/temp/Perkins/Data/Base_data.gdb"
Base= "C:/temp/TweedTelkwa/Temp/Perkins/Data/Base_data.gdb" # contains 

Base= "I:/ES/General/Wildlife/Wildlife Species/Caribou/Tweedsmuir caribou/2015-2018/Disturbance Analysis/Whitesale_AOI/Data/Base_data.gdb"


## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
base_list <- ogrListLayers(Base); print(base_list)

##############################################################################################
# Read in herd boundary layers 

b.range <- st_read(dsn=Base,layer="TT_boundary")
b.range <-st_cast(b.range,"MULTIPOLYGON")     # fix overlaps
  
# read in the whitesale boundary (simple dissolved range)

ws.range <- st_read(dsn=Base,layer="WS_bdry_simple") 
#plot(st_geometry(ws.range),col = "red")
#plot(st_geometry(b.range), add = TRUE)

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

          # r.pipe.int  =  Zero #.. no pipeline
       
# 2) no rail 
        
# 3) no transmission (length and area) 

          # no transmission 
         
# 3) no mine

# 4) no agriculture 

# 5) no air strips

# 6) no dams 

# 7) reservoirs (not included in AOI  buffer) 

# 9) no  wells 

# 10) no urban
# 11) no Rail  # no rail in the herds

# 12)  Recreation sites  (no buffer )
r.rec.sf <- st_read(dsn=Base,layer="Rec_clip") # multipoly
r.rec.sf <- st_zm(r.rec.sf ,drop = TRUE)
      
        # 1) RANGE: calculate the range extent to use range extent 
        r.rec <- st_intersection(ws.range,r.rec.sf)# intersect with range
        r.rec <- st_cast(r.rec,"POLYGON")
        r.rec <- st_buffer(r.rec,250)
        r.rec <- st_union(r.rec)
        r.rec <- st_cast(r.rec,"POLYGON")
        r.rec.area = sum(as.numeric(st_area(r.rec))) 
        
       # plot(st_geometry(r.rec),add = T,col = "blue") 
        ##plot(st_geometry(r.rec))
        st_write(r.rec,paste(final.dir,"WS_d_rec_buf.shp",sep = ""))      #write out individual dist_layer for Range
        
        # combine into disturbance by layer 
        all.range.out <- cbind(Herd_key,r.rec.area)  
        
# 13) no Seismic Lines
      
## 14) Roads # buffer to 15m width as per blairs analysis

## Tweedsmuir herd 
        #HWSR
        b.r2.sf = sf::st_read(dsn = Base , layer ="Tw_HWSR_Rd_bu" ) 
        b.r2.sf = st_make_valid(b.r2.sf)
        b.r2.sf <- st_cast(b.r2.sf,"POLYGON")
        b.r2.sf <- st_buffer(b.r2.sf,250)
        b.r2.sf = st_intersection(ws.range,b.r2.sf)
        
        #LWR
        b.r1.sf = sf::st_read(dsn = Base , layer ="Tw_LSR_Rd_bd" ) 
        b.r1.sf = st_make_valid(b.r1.sf)
        b.r1.sf = st_intersection(ws.range,b.r1.sf)
        b.r1.sf <- st_cast(b.r1.sf,"POLYGON")
        b.r1.sf <- st_buffer(b.r1.sf,250)
        # join together 
        b.r <- st_union(b.r2.sf,b.r1.sf) #; plot(st_geometry(b.r2.sf),add = TRUE)
        b.r <- st_union(b.r)
        b.road.Area.m <- sum(as.numeric(st_area(b.r)))
        st_write(b.r,paste(final.dir,"WS_d_road_buf.shp", sep ="/"))  
      # combine into disturbance by layer 
      all.range.out <- cbind(all.range.out,b.road.Area.m)  
     
      ## ALL DISTURBANCE: add rec to roads
      outbuf = st_union(r.rec,b.r ) 
      plot(st_geometry(outbuf ))
      ##x.area = sum(st_area(out8))     ; x.area  # 420881436 [m^2]
      
#######################################################################

# end of part 1: write out all the static disturbance types;
        
# write out all the layers combined into a single disturbance layer ( note this excludes Roads and Seismic )
st_write(outbuf,paste(temp.dir,"WS_static_disturbance_buf.shp",sep = "")) # this writes out as single layer   
# Write out the datasheet onto a temp file 
write.csv(all.range.out,paste(temp.dir,"WS_static_dist_buf.csv",sep = "") )        
  
################################################################################

# read in pre-processed data (if script has previously been run through)

all.range.out = read.csv(paste(temp.dir,"WS_static_dist_buf.csv",sep = ""))

################################################################################    
################################################################################

## 8) Cutblock ## this is all years of consolidated cutblock layer 

# Cutblock data was suplemented with canfor recent cutblock data from 2016-2018 

# cutblock data from Canfor 2016 - 2018 data set 
#b.r.c1 = st_read(paste("C:\\Temp\\TweedTelkwa\\Temp\\Perkins\\Data\\whitesail_shapefiles_2018_12_10","Whitesail_harvested_blocks_cfp_2018_12_11.shp",sep = "\\"))
b.r.c1 = st_read(paste(temp.dir,"\\whitesail_shapefiles_2018_12_10\\Whitesail_harvested_blocks_cfp_2018_12_11.shp",sep = ""))

b.r.c1  = st_transform(b.r.c1 ,3005) 
#unique(b.r.c1$HS_DATE)
b.r.c1  = st_union(b.r.c1)
b.r.c1  = st_buffer(b.r.c1 ,250)

#plot(st_geometry(b.r.c1))
#plot(st_geometry(b.r.c),add = T, col="red")

b.r.c1<- st_intersection(ws.range,b.r.c1) ; plot(st_geometry(b.r.c1 ))
b.r.c1 = st_cast(b.r.c1,"POLYGON")
b.r.c1  = st_union(b.r.c1)
########################

# Split out age classes

b.r.c = st_read(dsn = Base , layer = "cutblock_union_Tw")
b.r.c <- st_zm(b.r.c ,drop = TRUE)
b.r.c<- st_intersection(ws.range,b.r.c) #; st_is_valid(b.r.0)
b.r.c = st_cast(b.r.c,"POLYGON")
b.r.c$HARVEST_YEAR_ALL = ifelse(b.r.c$HARVEST_YEAR > 0,b.r.c$HARVEST_YEAR,b.r.c$HARVEST_YEAR_1 ) #sort(unique(b.r.c$HARVEST_YEAR_ALL)) # error check 
b.r.c$TimeSinceCut = 2018-b.r.c$HARVEST_YEAR_ALL; # create new column with age since cut 
#sort(unique(b.r.c$TimeSinceCut)) # check the range in years since cut #note in the boreal the oldest age cut is 78 years 
b.r.c = st_make_valid(b.r.c)

    # cutblocks 0-80 years
    b.r.c0.80 = b.r.c[b.r.c$TimeSinceCut < 81,] ; unique(b.r.c0.80$TimeSinceCut); #plot(b.r.c0.80$Shape)
    #b.r.c0.80 = st_cast(b.r.c0.80,"POLYGON"); #
    b.r.c0.80 = st_make_valid(b.r.c0.80)
    b.r.c0.80 = st_buffer(b.r.c0.80,250)
    b.r.c0.80 = st_union(b.r.c0.80,b.r.c1 ) # add the latest data (as processed above from canfore )
    b.r.c0.80 = st_union(b.r.c0.80)
    b.r.c0.80.area = sum(st_area(b.r.c0.80)) 
      
    # cutblocks 0-40 years
    b.r.c0.40 = b.r.c[b.r.c$TimeSinceCut < 41,] ; unique(b.r.c0.40$TimeSinceCut); #plot(st_geometry(b.r.c0.40))
    b.r.c0.40 = st_cast(b.r.c0.40,"POLYGON")
    b.r.c0.40 = st_make_valid(b.r.c0.40) #; st_is_valid(b.r.c0.40)
    b.r.c0.40 = st_buffer(b.r.c0.40,250)
    b.r.c0.40 = st_union(b.r.c0.40,b.r.c1 ) # add the additional Canfor data (2016-2018)
    b.r.c0.40 = st_union(b.r.c0.40)
    b.r.c0.40.area = sum(st_area(b.r.c0.40)) #260124543  [m^2]
 
    r.cut.out = cbind(b.r.c0.80.area,b.r.c0.40.area)
    
    r.cut.df = b.r.c
    r.cut.df  = st_buffer(r.cut.df ,250) # buffer the results 
    #r.cut.df  = st_union( r.cut.df )
    
    # add a column to differentiate the age brackets of cutblocks 
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1940 & HARVEST_YEAR_ALL <= 1949,1940,0))
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1950 & HARVEST_YEAR_ALL <= 1959,1950,dec.period))
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1960 & HARVEST_YEAR_ALL <= 1969,1960,dec.period))
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1970 & HARVEST_YEAR_ALL <= 1979,1970,dec.period))   
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1980 & HARVEST_YEAR_ALL <= 1989,1980,dec.period))   
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1990 & HARVEST_YEAR_ALL <= 1999,1990,dec.period)) 
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 2000 & HARVEST_YEAR_ALL <= 2009,2000,dec.period))   
    r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 2010 & HARVEST_YEAR_ALL <= 2019,2010,dec.period)) 
  
  #r.cut.df[r.cut.df$dec.period == 0,] ; unique(r.cut.df$dec.period)  # error check
    # generate cumulative burn disturbance shapefiles to be added sequentially to "static disturbance" 
    head(r.cut.df) ; unique(r.cut.df$dec.period)
    Cut.dec.1970 <-r.cut.df %>% filter(dec.period < 1971 )
    Cut.dec.1980 <-r.cut.df %>% filter(dec.period < 1981 )
    Cut.dec.1990 <- r.cut.df %>% filter(dec.period < 1991 )
    Cut.dec.2000 <- r.cut.df%>% filter(dec.period < 2001 )
    Cut.dec.2010 <- r.cut.df %>% filter(dec.period < 2011 )
    Cut.dec.2010 <- st_union(Cut.dec.2010 ,b.r.c1)
    #st_is_valid(Cut.dec.2010 )
    
    ## write out the shapefiles to Data.drive
    st_write(Cut.dec.1970,paste(temp.dir,"Cut.ws.dec.1970.buf.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.1980,paste(temp.dir,"Cut.ws.dec.1980.buf.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.1990,paste(temp.dir,"Cut.ws.dec.1990.buf.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.2000,paste(temp.dir,"Cut.ws.dec.2000.buf.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.2010,paste(temp.dir,"Cut.ws.dec.2010.buf.shp",sep = "")) # this writes out as single layer
    
    # generate consolidated output summary tables for each decade (same as the 0-40 and 0-80 as above) overtime per decage 
    c.1970 <- st_union(Cut.dec.1970)
    c.1970 <- st_cast(c.1970,"POLYGON") #; st_is_valid(c.1960)
    c.1970 = st_intersection(ws.range, c.1970) ; c.1970 <- st_make_valid(c.1970)
    c.1970 <- st_cast(c.1970,"POLYGON")
    c.1970$area.m = as.numeric(st_area(c.1970))
    c.1970.df <- as.data.frame(c.1970) 
    c.1970.df.out <-  c.1970.df %>% 
      mutate(AOI = "whitesale")%>%
      group_by(AOI)%>%
      summarise(R_cut_1970_area_m2 = sum(area.m))
    r.cut.out = merge(r.cut.out,c.1970.df.out) # add to the data summary
    
    c.1980 <- st_union(Cut.dec.1980)
    c.1980 <- st_cast(c.1980,"POLYGON") #; st_is_valid(c.1960)
    c.1980 = st_intersection(ws.range, c.1980) ; c.1980 <- st_make_valid(c.1980)
    c.1980 <- st_cast(c.1980,"POLYGON")
    c.1980$area.m = as.numeric(st_area(c.1980))
    c.1980.df <- as.data.frame(c.1980) 
    c.1980.df.out <-  c.1980.df %>% 
      mutate(AOI = "whitesale")%>%
      group_by(AOI)%>%
      summarise(R_cut_1980_m2 = sum(area.m))
    r.cut.out = merge(r.cut.out,c.1980.df.out) # add to the data summary
    
    c.1990 <- st_union(Cut.dec.1990)
    c.1990 <- st_cast(c.1990,"POLYGON") #; st_is_valid(c.1960)
    c.1990 = st_intersection(ws.range, c.1990) ; c.1990 <- st_make_valid(c.1990)
    c.1990 <- st_cast(c.1990,"POLYGON")
    c.1990$area.m = as.numeric(st_area(c.1990))
    c.1990.df <- as.data.frame(c.1990) 
    c.1990.df.out <-  c.1990.df %>% 
      mutate(AOI = "whitesale")%>%
      group_by(AOI)%>% 
      summarise(R_cut_1990_m2 = sum(area.m))
    r.cut.out = merge(r.cut.out,c.1990.df.out) # add to the data summary
    
    c.2000 <- st_union(Cut.dec.2000)
    c.2000 <- st_cast(c.2000,"POLYGON") #; st_is_valid(c.2000)
    c.2000 = st_intersection(ws.range, c.2000) ; c.2000 <- st_make_valid(c.2000)
    c.2000 <- st_cast(c.2000,"POLYGON")
    c.2000$area.m = as.numeric(st_area(c.2000))
    st_write(c.2000,paste(temp.dir,"cut.2000.WS.buf.shp",sep ="")) # write these out so we can join them in arcmap to the pest data 
    c.2000.df <- as.data.frame(c.2000) 
    c.2000.df.out <-  c.2000.df %>% 
      mutate(AOI = "whitesale")%>%
      group_by(AOI)%>% 
      summarise(R_cut_2000_m2 = sum(area.m))
    r.cut.out = merge(r.cut.out,c.2000.df.out) # add to the data summary
    
    c.2010 <- st_union(Cut.dec.2010)
    c.2010 <- st_cast(c.2010,"POLYGON") ; st_is_valid(c.2010); c.2010 <- st_make_valid(c.2010)
    c.2010 = st_intersection(ws.range, c.2010) 
    c.2010 <- st_cast(c.2010,"POLYGON")
    st_write(c.2010,paste(temp.dir,"cut.2010.WS.buf.shp",sep ="")) 
    c.2010$area.m = as.numeric(st_area(c.2010))
    c.2010.df <- as.data.frame(c.2010) 
    c.2010.df.out <-  c.2010.df %>% 
      mutate(AOI = "whitesale")%>%
      group_by(AOI)%>%
      summarise(R_cut_2010_m2 = sum(area.m))
    r.cut.out = merge(r.cut.out,c.2010.df.out) # add to the data summary
   
     # outputs   = data table with all 80-0 and 40-0 + each decade (cumulative)  = r.cut.out
     #            = spatial data at each decade. 
    
    ##########################################
    
    all.temp.out <- r.cut.out  # combine into disturbance by layer

########################################################
## BURN:
# Add to Burn information Break down of burns into 0-40 and 0-80 years (and total) 
# Break down burns into fire years (same as cutblocks) 

b.r.0 = st_read(dsn = Base, layer ="fire_clip")
b.r.0<- st_zm(b.r.0 ,drop = TRUE) # this is a linear feature so need to buffer to estimate area calcs
b.r.0 <- st_intersection(ws.range,b.r.0) #; st_is_valid(b.r.0)
#st_is_valid(b.r.0)
b.r.0$TimeSinceBurn = 2018-b.r.0$FIRE_YEAR #; plot(b.r.0$Shape)
b.r.0 <- st_cast(b.r.0,"POLYGON")
b.r.0 <-st_buffer(b.r.0,250)

    # burns 0-40 years 
    b.r.0.40 = b.r.0[b.r.0$TimeSinceBurn <41,]; #sort(unique(b.r.0$TimeSinceBurn)) 
    b.r.0.40.area  <- sum(st_area(b.r.0.40)) # note no burns in lat 40 years 
    
    # burns 0-80 years 
    b.r.0.80 =b.r.0[b.r.0$TimeSinceBurn <81,];
    b.r.0.80 <- st_cast(b.r.0.80,"POLYGON")
    b.r.0.80 = st_union(b.r.0.80)
    b.r.0.80.area  <- sum(st_area(b.r.0.80)) # note no burns in lat 80 years 

    r.burn.out = cbind(b.r.0.40.area,b.r.0.80.area )

    ##############################################
    
    all.temp.out = cbind(all.temp.out,r.burn.out)
    
############################################
### PEST 

r.pest <-  st_read(dsn = Base, layer ="Pest_clip_Tw_IBMIBS") #; plot(st_geometry(r.pest.tw))
r.pest<- st_zm(r.pest ,drop = TRUE) # this is a linear feature so need to buffer to estimate area calcs
r.pest = st_cast(r.pest,"POLYGON")
r.pest <-  st_intersection(ws.range,r.pest) #; st_is_valid(r.pest)
r.pest$TimeSincePest = 2018-r.pest$CAPTURE_YEAR  #sort(unique(r.pest$CAPTURE_YEAR)) # 1985 - 2015 
r.pest  <-st_buffer(r.pest,0.0)
r.pest = st_cast(r.pest,"POLYGON")
#st_is_valid(r.pest)
r.pest  <-st_buffer(r.pest,250)
r.pest <-  st_intersection(ws.range,r.pest) 
   
      r.pest.df = r.pest
      
      # add decade data   # add a column to differentiate the age brackets of pest capture
      r.pest.df<- r.pest.df %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1980 & CAPTURE_YEAR <= 1989,1980,0))
      #r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1980 & CAPTURE_YEAR <= 1989,1980,dec.period))
      r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1990 & CAPTURE_YEAR <= 1999,1990,dec.period))
      r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2000 & CAPTURE_YEAR <= 2009,2000,dec.period))
      r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2010 & CAPTURE_YEAR <= 2018,2010,dec.period))
 
      r.pest.df <- r.pest.df %>% dplyr::select(PEST_SPECIES_CODE,CAPTURE_YEAR,dec.period,Shape)     
      #r.pest.df <- st_cast(r.pest.df,"POLYGON")
      
      # split into decades 
      pest.dec.1980 <- r.pest.df %>% filter(dec.period < 1981 )
      pest.dec.1990 <- r.pest.df %>% filter(dec.period < 1991 )
      pest.dec.2000 <- r.pest.df%>% filter(dec.period < 2001 )
              #pest.dec.2000 = st_make_valid( pest.dec.2000); 
              #pest.dec.2000.u = st_union(pest.dec.2000)
              pest.dec.2000 = st_cast(pest.dec.2000,"POLYGON")
     
      pest.dec.2010 <- r.pest.df %>% filter(dec.period < 2011 )
              pest.dec.2010 <- st_buffer(pest.dec.2010, 0.0) # there is a point in this file so if we buffer by zero we can convert to polygon 
             # pest.dec.2010 = st_make_valid(pest.dec.2010); 
             pest.dec.2010 = st_cast(pest.dec.2010,"POLYGON")
      
      st_write(pest.dec.1980,paste(temp.dir,"pest.ws.1980.buf.shp",sep = "")) # this writes out as single layer
      st_write(pest.dec.1990,paste(temp.dir,"pest.ws.1990.buf.shp",sep = "")) # this writes out as single layer
      st_write(pest.dec.2000,paste(temp.dir,"pest.ws.2000.buf.shp",sep = "")) # this writes out as single layer
      st_write(pest.dec.2010,paste(temp.dir,"pest.ws.2010.buf.shp",sep = "")) # this writes out as single layer
      
      #r.pest.df[r.pest.df$dec.period == 0,]
      #head(r.pest.df)
      #unique(r.pest.df$dec.period)
      
      p.1980 <- st_union(pest.dec.1980)
      p.1980 <- st_cast(p.1980,"POLYGON") #; st_is_valid(c.1960)
      p.1980 = st_intersection(ws.range, p.1980) #; b.1980 <- st_make_valid(b.1980)
      #b.1980 <- st_cast(b.1980,"POLYGON") #; st_is_valid(b.1980)
      p.1980$area.m = as.numeric(st_area(p.1980))
      p.1980.df <- as.data.frame(p.1980)
      p.1980.df.out <-  p.1980.df %>% 
        mutate(AOI = "whitesale")%>%
        group_by(AOI)%>%
        summarise(R_pest_1980_m2 = sum(area.m))
      r.pest.out.total = p.1980.df.out # add to the data summary
      
      p.1990 <- st_union(pest.dec.1990)
      p.1990 <- st_cast(p.1990,"POLYGON") #; st_is_valid(c.1960)
      p.1990 = st_intersection(ws.range, p.1990) #; b.1990 <- st_make_valid(c.1990)
      #p.1990 <- st_cast(b.1990,"POLYGON")
      p.1990$area.m = as.numeric(st_area(p.1990))
      p.1990.df <- as.data.frame(p.1990) 
      p.1990.df.out <-  p.1990.df %>% 
        mutate(AOI = "whitesale")%>%
        group_by(AOI)%>%
        summarise(R_pest_1990_m2 = sum(area.m))
      r.pest.out.total = left_join(r.pest.out.total,p.1990.df.out) # add to the data summary
      
      # for the 2000 and 2010 data sets need to preproces in Arcmap to be able to run (possibly this is a computer issue)
      p.2000 <- st_read(paste(temp.dir,"p2000ws_buf_d.shp",sep = "")) #; plot(st_geometry(r.pest.tw))
      p.2000 <- st_set_crs(p.2000,3005)
      p.2000 <- st_cast(p.2000,"POLYGON")
      p.2000 = st_intersection(ws.range, p.2000) ; p.2000 <- st_make_valid(p.2000)
      p.2000$area.m = as.numeric(st_area(p.2000))
      p.2000.df <- as.data.frame(p.2000) 
      p.2000.df.out <-  p.2000.df %>% 
        mutate(AOI = "whitesale")%>%
        group_by(AOI)%>%
        summarise(R_pest_2000_m2 = sum(area.m))
      r.pest.out.total = left_join(r.pest.out.total,p.2000.df.out) # add to the data summary
    
      # 2010 
      # read in pre-dissolved layers created in ArcMap 
      p.2010 <-  st_read(paste(temp.dir,"p2010ws_buf_d.shp",sep = "")) #; plot(st_geometry(r.pest.tw))
      p.2010 <- st_set_crs(p.2010, 3005)
      p.2010 <- st_cast(p.2010,"POLYGON")
      p.2010 <- st_union(p.2010 )
      p.2010 <- st_make_valid(p.2010) ; plot(st_geometry(p.2010))
      p.2010 = st_intersection(ws.range, p.2010) 
      p.2010$area.m = as.numeric(st_area(p.2010))
      p.2010.df <- as.data.frame(p.2010) 
      p.2010.df.out <-  p.2010.df %>% 
        mutate(AOI = "whitesale")%>%
        group_by(AOI)%>%
        summarise(R_pest_2010_m2 = sum(area.m))
      r.pest.out.total = left_join(r.pest.out.total,p.2010.df.out) # add to the data summary
      
   
      ##########################################
  
      all.temp.out = cbind(all.temp.out,r.pest.out.total)
      
      
      write.csv(all.temp.out,paste(temp.dir,"WS_temp_dist_buf.csv",sep = ""))

############################################################################
## Consolidate all the disturbance into a single oytput per decade
      
# read in pre-processed data (if script has previously been run through)

all.range.out = read.csv(paste(temp.dir,"WS_static_dist_buf.csv",sep = ""))
out1 = outbuf
rm(outbuf)

# only cut & pest layers (no burns)
# add the temporal disturbance and calculate total disturbance per decade

#1970 
    dist.1970 <- st_union(out1, c.1970) # join together the disturbance layers (temporal + static)
    dist.1970 <- st_union(dist.1970)
    head(dist.1970); plot(dist.1970)
    dist.1970 <-st_cast(dist.1970,"POLYGON")
    dist.1970.area.m <- sum(st_area(dist.1970))
    st_write(dist.1970,paste(temp.dir,"Dist_1970_WS_buf.shp",sep = "")) 
    #st_write(r.rec,"Dist_R_Rec.shp")  

    all.dist.tally_buf = dist.1970.area.m

#1980 
    dist.1980 <- st_union(p.1980, c.1980)  # join cut and pest areas. 
    dist.1980 <- st_union(out1, dist.1980) # join together the disturbance layers (temporal + static)
    dist.1980 <- st_union(dist.1980)
    dist.1980.area.m <- sum(st_area(dist.1980)) ; #head(dist.1980)
    st_write(dist.1980,paste(temp.dir,"Dist_1980_WS_buf1.shp",sep = "")) 
    
    all.dist.tally_buf = cbind(all.dist.tally_buf, dist.1980.area.m) 
    
#1990 
    dist.1990 <- st_union(p.1990, c.1990)  # join cut and pest areas. 
    dist.1990 <- st_union(dist.1990) 
    dist.1990 <- st_union(out1, dist.1990) # join together the disturbance layers (temporal + static)
    dist.1990 <- st_union(dist.1990)  ; head(dist.1990)
    dist.1990 <-st_cast(dist.1990,"POLYGON")
    dist.1990.area.m <- sum(st_area(dist.1990))
    st_write(dist.1990,paste(temp.dir,"Dist_1990_WS_buf1.shp",sep = "")) 
    all.dist.tally_buf = cbind(all.dist.tally_buf, dist.1990.area.m) 
    
#2000
    dist.2000 <- st_read(paste(temp.dir,"Dist_2000_WS_buf_d.shp",sep = ""))
    dist.2000 <- st_transform(dist.2000, 3005)
    dist.2000 <-st_cast(dist.2000,"POLYGON")
    dist.2000 <- st_union(dist.2000)
    dist.2000.area.m <- sum(st_area(dist.2000))
    all.dist.tally_buf = cbind(all.dist.tally_buf, dist.2000.area.m) 
    
#2010
    dist.2010 <- st_read(paste(temp.dir,"Dist_2010_WS_buf_d.shp",sep = ""))
    dist.2010 <-st_transform(dist.2010, 3005)
    dist.2010 <-st_cast(dist.2010,"POLYGON")
    dist.2010 <- st_union(dist.2010)
    dist.2010.area.m <- sum(st_area(dist.2010))
   
    all.dist.tally_buf = cbind(all.dist.tally_buf, dist.2010.area.m) 
  
    write.csv(all.dist.tally_buf,paste(temp.dir,"Final_WS_data_summary_buf_cumulate.csv",sep = ""))
    
    
##############################################################
     
     # Step 2 aggregate tables  static data with the temporal data (all.temp.out)
     all.range.out = read.csv(paste(temp.dir,"WS_static_dist_buf.csv",sep = "")) 
  
     all.range.out = all.range.out[,c(2,3,4,5)] 
     all.range.out = cbind(all.range.out,all.temp.out)   # add the temporal data areas  
     all.range.out = cbind(all.range.out, all.dist.tally_buf) # add the combined totals per year. 
     
     ## adjust this section based on the line above
     
     all.range.out = all.range.out[,-c(5,15)]     # check this after re-running roads layer 
    # all.range.out = all.range.out[,-c(18)]  
     x = as.tibble(all.range.out)
     x = x %>% tidyr::gather(attribute, value)
     
     x <- x[-1,]
     x <- x %>% mutate(Area_ha = as.numeric(value)/10000) %>% mutate(Area_pc = (Area_ha/45041) * 100 )
     
     write.csv(x,paste(out.dir,"Final_WS_data_summary_ha_pc_buf_all.csv",sep = ""))
     
################################################################

# Generate plots per decade of disturbance

## Cutblocks 
# Figure 1: make a plot of temporal cutblocks disturbance

p1 = ggplot() + 
  geom_sf(data = ws.range) +
  geom_sf(data = r.cut.df, col = "red") + facet_wrap(~dec.period) #+       
#theme_()
p1
ggsave(paste(out.dir,"WS_buf_cutblock_decades.png",sep = ""))              

############################################################################

### generate a plot to show location

b.range <- st_read(dsn=Base,layer="TT_bdry_diss") 
b.range <-st_cast(b.range,"MULTIPOLYGON")     # check and fix geometry 
t.range <- b.range %>% dplyr::filter(SiteName== "Tweedsmuir")
t.range <- 

plot(st_geometry(t.range), col = "grey") # plot the herds to see if it looks correct
plot(st_geometry(ws.range),add = T, col = "red")
ggsave(paste(out.dir,"WS_AOI_Tweeds_bdry.png",sep = ""))



