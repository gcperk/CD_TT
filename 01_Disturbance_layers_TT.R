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

## To run analysis on restricted folder
#out.dir = "X:/projects/Desktop_Analysis/data/output1/"
#temp.dir = "X:/projects/Desktop_Analysis/data/temp/"

# to run analysis on W drive: 
#out.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Outputs/"
#temp.dir = "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Data"

# to run analysis on C drive: 
#out.dir = "C:/Temp/TweedTelkwa/Temp/Perkins/Outputs/"
#temp.dir = "C:/Temp/TweedTelkwa/Temp/Perkins/Data/"

temp.dir = "T:/temp/Perkins/Data/"
out.dir = "T:/temp/Perkins/Outputs/" 

## Set your input geodatabases (this will be where you saved your arcmap exports)
## edit these to your filepath and name of gdb

#Base= "Z:/01.Projects/Wildlife/Caribou/02.Disturbance/TweedTelkwa/Temp/Perkins/Data/Base_data.gdb" # contains 
Base= "T:/temp/Perkins/Data/Base_data.gdb"
#Base= "C:/temp/TweedTelkwa/Temp/Perkins/Data/Base_data.gdb" # contains 

## List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
base_list <- ogrListLayers(Base); print(base_list)

##############################################################################################
# Read in herd boundary layers 

b.range <- st_read(dsn=Base,layer="TT_boundary")
b.range <-st_cast(b.range,"MULTIPOLYGON")     # fix overlaps

# calculate the areas of each of the Herd habitat types (HWSR,LWR,LSR,MR,ALL)
Herd_key<- data.frame(b.range) %>% 
  dplyr::select(SiteName,V17_CH,SHAPE_Area) %>%  # note SHAPE_AREA is calculated in square meters
  mutate(R_area_ha = SHAPE_Area/10000) %>%
  dplyr::select(c(SiteName,V17_CH,R_area_ha)) %>%
  group_by(SiteName)%>%
  summarise(R_area_ha = sum(R_area_ha))

Herd_key_detail <-  data.frame(b.range) %>% 
  dplyr::select(SiteName,V17_CH,SHAPE_Area) %>%  # note SHAPE_AREA is calculated in square meters
  mutate(H_area_ha = SHAPE_Area/10000) %>%
  group_by(SiteName,V17_CH)%>%
  summarise(R_area_ha = H_area_ha) %>%
  spread(V17_CH,R_area_ha) %>%
  rename(HWSR_ha = 'High Elevation Winter/Summer Range') %>%
  rename(LSR_ha = 'Low Elevation Summer Range')%>%
  rename(LWR_ha = 'Low Elevation Winter Range')%>%
  rename(MR_ha = 'Matrix Range')

Herd_key = left_join(Herd_key,Herd_key_detail)
Herd_key[is.na(Herd_key)] <- 0

# calculate the area for all habitats and 
  write.csv(Herd_key, paste(out.dir,"Herd_key.csv",sep = ""))

Herd_key_detail <-  data.frame(b.range) %>% 
    dplyr::select(SiteName,V17_CH,SHAPE_Area) %>%  # note SHAPE_AREA is calculated in square meters
    mutate(H_area_ha = SHAPE_Area/10000) %>%
    group_by(SiteName,V17_CH)%>%
    summarise(R_area_ha = H_area_ha)  

# calculate the area for all habitats and 
write.csv(Herd_key_detail, paste(out.dir,"Herd_key_detail.csv",sep = ""))

  
#############################################################
# Read in the boundary data  

all.range <- st_read(dsn=Base,layer="TT_boundary"); plot(st_geometry(all.range))
all.range <- st_zm(all.range,drop = TRUE)
all.range <- st_cast(all.range,"MULTIPOLYGON")
all.range <- st_make_valid(all.range)
all.range.out <- data.frame(all.range)%>%
  dplyr::select(SiteName,V17_CH )
 
gc()
#b.range <- st_read(dsn=Base,layer="TT_bdry_diss") ; plot(st_geometry(all.range))
#b.HWSR <- st_read(dsn=Base,layer="B_HWSR") ; plot(st_geometry(b.HWSR))
##b.LSR <- st_read(dsn=Base,layer="B_LSR"); plot(st_geometry(b.LSR)) ## this is not working correctly 
#b.LWR <- st_read(dsn = Base,layer="B_LWR"); plot(st_geometry(b.LWR ))
#b.MR <- st_read(dsn = Base,layer="B_MR"); plot(st_geometry(b.MR ))
  
##############################################################################################
# Read in individual Disturbance layers:

# 1) pipeline (length)
r.pipe <- st_read(dsn=Base,layer="Pipeline_clip") # multistring 
          r.pipe.int = st_intersection(all.range,r.pipe)   # intersect with single all ranges
          # RANGE: intersect with range 
          r.pipe.int <- st_buffer(r.pipe.int,1)  ; all.pipe = sum(st_area(r.pipe)) ; plot(st_geometry(r.pipe))
          st_is_valid(r.pipe.int)                       # check valid geometry
          r.pipe.int = st_cast(r.pipe.int,"POLYGON")     # fix overlaps
          r.pipe.int$Area.m <- as.numeric(st_area(r.pipe.int))
          plot(st_geometry(r.pipe.int))
          #st_write(r.pipe.int,"Dist_R_pipe.shp")       #write out individual dist_layer for Range
          r.pipe.int.df = data.frame(r.pipe.int)        # calcaulte area
          r.pipe.int.df.out  = r.pipe.int.df %>% 
                   group_by(SiteName,V17_CH) %>% 
                  summarise(R_Pipe_area_m = sum(Area.m))
      
          #out [is.na(out )] <- 0
          #out$P_Pipe_area_m = out$R_Pipe_area_m - out$C_Pipe_area_m
          out.pipe = r.pipe.int
          all.range.out<- left_join(all.range.out, r.pipe.int.df.out)     
          all.range.out[is.na(all.range.out)]<-0
          
# 2) transmission (length and area) 
r.tran.sf <- st_read(dsn=Base,layer="Trans_clip") # multistring  
          
          # 1) RANGE: calculate the range extent to use range extent 
          r.tran <- st_intersection(all.range,r.tran.sf)# intersect with ranges
          r.tran <- st_buffer(r.tran,1) #;all.tran = sum(st_area(r.tran)) 
          r.tran <- st_cast(r.tran,"POLYGON")
          r.tran$area_m = as.numeric(st_area(r.tran))
          #st_is_valid(r.tran)                   # check valid geometr
          r.tran.df = data.frame(r.tran)        # calculate the length per range 
          r.tran.df.out  = r.tran.df%>% 
            group_by(SiteName,V17_CH) %>% 
            summarise(R_Trans_area_m = sum(area_m))
        
          ##plot(st_geometry(r.tran))
          #st_write(r.tran,"Dist_R_tran.shp")       #write out individual dist_layer for Range
          out.trans <- r.tran.df.out 
          all.range.out<- left_join(all.range.out, r.tran.df.out)     
          all.range.out[is.na(all.range.out)]<-0
          
          ## 3) ALL DISTURBANCE UNION 1
          out = st_union(r.pipe.int,r.tran) ; plot(st_geometry(out))
          out = st_union(out); plot(st_geometry(out),add = T)
          out = st_cast(out,"POLYGON")
          ##x.area = sum(st_area(out)) ; x.area #298446.6 
          
          
# 3) mine
r.mine.sf <- st_read(dsn=Base,layer="Mining_clip") # multipoly
r.mine <- st_zm(r.mine.sf,drop = TRUE) # drop the z portion of the shapefile. 

          # 1) RANGE: calculate the range extent to use range extent 
          r.mine <- st_intersection(all.range,r.mine)
          r.mine <- st_cast(r.mine,"POLYGON")
          #st_is_valid(r.mine)                   # check valid geometr
          r.mine$Area.m <- as.numeric(st_area(r.mine))
          r.mine.df = data.frame(r.mine)        # calculate the length per range 
          r.mine.df.out  = r.mine.df %>% 
            group_by(SiteName,V17_CH) %>% 
            summarise(R_mine_m2 = sum(Area.m))

                   # combine into disturbance by layer 
          all.range.out <- left_join(all.range.out,r.mine.df.out) ; 
          all.range.out[is.na(all.range.out)]<-0

        ## 3) ALL DISTURBANCE UNION 2 
          out = st_make_valid(out)
          r.mine = st_make_valid(r.mine)
          out1 = st_union(out,r.mine)  ; plot(st_geometry(out1)) ; rm(out)
        #  #x.area = sum(st_area(out1)) ; x.area #20570863 m2 # error check 

# 4) agriculture 
r.agr.sf <- st_read(dsn=Base,layer="Agri_clip") # multipoly
r.agr.sf <- st_zm(r.agr.sf,drop = TRUE)

        # 1) RANGE: calculate the range extent to use range extent 
        r.agr <- st_intersection(all.range,r.agr.sf)# intersect with ranges
        r.agr <- st_cast( r.agr,"POLYGON")
        #st_is_valid(r.agr)                   # check valid geometr
        r.agr$Area.m <- as.numeric(st_area(r.agr))
        r.agr.df = data.frame(r.agr)        # calculate the length per range 
        r.agr.df.out  = r.agr.df %>% 
          group_by(SiteName,V17_CH) %>% 
          summarise(R_agr_m2 = sum(Area.m))
        #plot(st_geometry(r.agr))
        
        # combine into disturbance by layer 
        all.range.out <- left_join(all.range.out,r.agr.df.out) ; 
        all.range.out[is.na(all.range.out)]<-0
      
        ## ALL DISTURBANCE COMBINED: UNION 3
        out2 = st_union(out1,r.agr.sf) ; plot(st_geometry(out2))
        out2 = st_union(out2); plot(st_geometry(out2)) ; rm (out1)
        #x.area = sum(st_area(out2)); x.area # 144954516  #out2 = st_cast(out2,"POLYGON")
        
# 5) air strips
r.air.sf <- st_read(dsn=Base,layer="Airstrip_clip") # multipoly
r.air.sf <- st_zm(r.air.sf,drop = TRUE)
r.air.sf <- st_cast(r.air.sf,"POLYGON")

          # 1) RANGE: calculate the range extent to use range extent 
          r.air <- st_intersection(all.range,r.air.sf)# intersect with ranges
          r.air <- st_cast( r.air,"POLYGON")
          #st_is_valid(r.air)                   # check valid geometr
          r.air$Area.m <- as.numeric(st_area(r.air))
          r.air.df = data.frame(r.air)        # calculate the length per range 
          r.air.df.out  = r.air.df %>% 
            group_by(SiteName,V17_CH) %>% 
            summarise(R_air_m2 = sum(Area.m))
          #plot(st_geometry(r.air))
          
          # combine into disturbance by layer 
          all.range.out <- left_join(all.range.out,r.air.df.out) ; 
          all.range.out[is.na(all.range.out)]<-0
        
        ## UNION 4
        out3 = st_union(out2,r.air.sf) #; plot(st_geometry(out3))
        out3 = st_union(out3); plot(st_geometry(out3)) ; rm(out2)
        ##x.area = sum(st_area(out3)) ;x.area #145235941 [m^2]

# 6) dams 
r.dam.sf <- st_read(dsn=Base,layer="dams_clip" ) # multipoly
r.dam.sf  <- st_zm(r.dam.sf ,drop = TRUE)
        ## RANGE: intersect with range and calculate length per range
        r.dams = st_intersection(all.range,r.dam.sf)   # intersect with ranges
        r.dams <- st_buffer(r.dams,1)
        r.dams <- st_cast(r.dams,"POLYGON")
        r.dams$Area.m <- as.numeric(st_area(r.dams))
        ##plot(st_geometry(r.dams))
        #st_write(r.dams,"Dist_R_dams.shp")       #write out individual dist_layer for Range
        r.dams.df = data.frame(r.dams)        # calculate the length per range 
        r.dams.df.out  = r.dams.df %>% 
          group_by(SiteName,V17_CH) %>% 
          summarise(R_Dams_m2 = sum(Area.m))
     
        # combine into disturbance by layer 
        all.range.out <- left_join(all.range.out,r.dams.df.out)  
        all.range.out[is.na(all.range.out)]<-0
        
        # # UNION 5
        r.dam.sf<- st_buffer(r.dam.sf,1) #;all.dam = sum(st_area(r.dam.sf )) ; plot(st_geometry(r.dam.sf))  
        out4 = st_union(out3,r.dam.sf) ; plot(st_geometry(out4))
        out4 = st_union(out4); plot(st_geometry(out4)) ; rm(out3)
        #x.area = sum(st_area(out4))  ;x.area    #145240406 [m^2]
        
# 7) reservoirs 
##r.res.sf <- st_read(dsn=Base,layer="Resevoir_clip") ;  plot(st_geometry(r.res.sf))         
 # only one feature found (Nechako Reservoir) This is not included as it is not calculated as part of the area of the herds.

        
# 9) wells 
r.wells.sf <- st_read(dsn=Base,layer="Wells_clip" ) # multipoly
# no wells present in these herd

#r.wells.sf <- st_union(r.wells.sf)
#all.wells = sum(st_area(r.wells.sf)) ; plot(st_geometry(r.wells.sf))  

        ## All Disturbance : UNION 5 # may need to run this in stand alone R rather than R -studio
        #out6 = st_union(out4,r.wells.sf) ; plot(st_geometry(out6))
        ##out6 = st_union(out6); plot(st_geometry(out6))
        ##x.area = sum(st_area(out6)) ; x.area #392383839 
        
        #out6 = out5
        
        ## Intersect with RANGE: intersect with range and calculate length per range
              r.wells = st_intersection(all.range,r.wells.sf)   # intersect with range
              r.wells <- st_cast(r.wells,"POLYGON")
              r.wells$area <- as.numeric(st_area(r.wells))
              
              ##plot(st_geometry(r.wells))
              #st_write(r.dams,"Dist_R_wells.shp")       #write out individual dist_layer for Range
              r.wells.df = data.frame(r.wells)        # calculate the length per range 
              r.wells.df.out  = r.wells.df %>% 
                group_by( SiteName,V17_CH ) %>% 
                summarise(R_Wells_m2 = sum(area))
              
        # combine into disturbance by layer 
        all.range.out <- left_join(all.range.out,r.wells.df.out)  
        all.range.out[is.na(all.range.out)]<-0
            
# 10) urban
r.urban.sf <- st_read(dsn=Base,layer="urban_clip" ) # multipoly
r.urban.sf <- st_zm(r.urban.sf ,drop = TRUE)
r.urban.sf <- st_union(r.urban.sf);
r.urban.sf <- st_cast(r.urban.sf,"POLYGON")
#all.urban = sum(st_area(r.urban.sf)) ; plot(st_geometry(r.urban.sf))  
      
      ## All disturbance: UNION 6 # may need to run this in stand alone R rather than R -studio
      out7 = st_union(out4,r.urban.sf)  # or line below if there is wells present ; rm(out4)
      #out7 = st_union(out6,r.urban.sf) 
      out7 = st_union(out7);
      #x.area = sum(st_area(out7)); x.area #396359128 
 
            ## Intersect with RANGE: intersect with range and calculate length per range
            r.urban = st_intersection(all.range,r.urban.sf)   # intersect with range
            r.urban = st_cast(r.urban,"POLYGON")
            r.urban$area <- as.numeric(st_area(r.urban))
            ##plot(st_geometry(r.urban))
            #st_write(r.dams,"Dist_R_urban.shp")       #write out individual dist_layer for Range
            r.urban.df = data.frame(r.urban)        # calculate the length per range 
            r.urban.df.out  = r.urban.df %>% 
              group_by( SiteName,V17_CH) %>% 
              summarise(R_Urban_m2 = sum(area))
            # combine into disturbance by layer 
            all.range.out <- left_join(all.range.out,r.urban.df.out)  
            all.range.out[is.na(all.range.out)]<-0
            
# 11) Rail  # no rail in the herds
r.rail.sf <- st_read(dsn=Base,layer="Rail_clip") # multipoly
st_is_empty(r.rail.sf)
r.rail.sf <- st_intersection(all.range,r.rail.sf)
#r.rail.sf <- st_cast(r.rail.sf,"POLYGON") 
#st_is_valid(r.rail.sf)
#r.rail.sf <- st_make_valid((r.rail.sf))
r.rail <- st_buffer(r.rail.sf,1)
r.rail <- st_cast(r.rail,"POLYGON") ; all.rail= sum(st_area(r.rail)) ; plot(st_geometry(r.rail))  
        
        # This union is not tested 
        ## UNION 7 # may need to run this in stand alone R rather than R -studio
        #out8 = st_union(out7,r.rail) ;# plot(st_geometry(out7))
        #  out8 = st_union(out8);# plot(st_geometry(out8))
        #  #x.area = sum(st_area(out7)) 
        
        #out8 = out7 # need to comment out this line if you are running the union about (union7)        

        r.rail$area.m <- as.numeric(st_area(r.rail))
        r.rail.df = data.frame(r.rail)        # calculate the length per range 
        r.rail.df.out  = r.rail.df%>% 
          group_by(SiteName,V17_CH) %>% 
          summarise(R_Rail_m2 = sum(area.m))
        ##plot(st_geometry(r.rail))
        #st_write(r.rail,"Dist_R_rail.shp")       #write out individual dist_layer for Range
        
        # combine into disturbance by layer 
        all.range.out <- left_join(all.range.out,r.rail.df.out)  
        all.range.out[is.na(all.range.out)]<-0      
    
# 12)  Recreation sites 
r.rec.sf <- st_read(dsn=Base,layer="Rec_clip") # multipoly
r.rec.sf <- st_zm(r.rec.sf ,drop = TRUE)
r.rec.sf <- st_intersection(all.range,r.rec.sf)
r.rec.sf <- st_union(r.rec.sf)
#all.rec = sum(st_area(r.rec.sf)) ; plot(st_geometry(r.rec.sf))  

        ## ALL DISTURBANCE: UNION 8 # may need to run this in stand alone R rather than R -studio
       out8 = out7; rm(out7)
       out9 = st_union(out8,r.rec.sf) ; plot(st_geometry(out9)); rm(out8)
       out9 = st_union(out9); plot(st_geometry(out9))
        #x.area = sum(st_area(out8))     ; x.area  # 396359128 [m^2]
       
        # 1) RANGE: calculate the range extent to use range extent 
        r.rec <- st_intersection(all.range,r.rec.sf)# intersect with range
        r.rec <- st_cast(r.rec,"POLYGON")
        r.rec$area = as.numeric(st_area(r.rec))
        #all.rec = sum(st_area(r.rec))
        #st_is_valid(r.tran)                   # check valid geometr
        r.rec.df = data.frame(r.rec)        # calculate the length per range 
        r.rec.df.out  = r.rec.df%>% 
          group_by(SiteName,V17_CH) %>% 
          summarise(R_Rec_m2 = sum(area))
        
        ##plot(st_geometry(r.rec))
        #st_write(r.rec,"Dist_R_Rec.shp")       #write out individual dist_layer for Range
        
        # combine into disturbance by layer 
        all.range.out <- left_join(all.range.out,r.rec.df.out)  
        all.range.out[is.na(all.range.out)]<-0      
        
        
# 13) Seismic Lines
b.s1 = sf::st_read(dsn = Base , layer ="Seismic_clip")
b.s1 <- st_zm(b.s1,drop = TRUE)
b.s1 <- st_make_valid(b.s1)
b.s1<- st_cast(b.s1, "POLYGON")
b.s1 <- st_union(b.s1)

      ## ALL DISTURBANCE: UNION 10 # may need to run this in stand alone R rather than R -studio
      out10 = st_union(out9,b.s1 ) ; plot(st_geometry(out10)) ; rm(out9)
      out10 = st_union(out10); plot(st_geometry(out10))
      ##x.area = sum(st_area(out8))     ; x.area  # 420881436 [m^2]

        b.s1 <- st_intersection(all.range,b.s1)# intersect with range
        b.s1 <- st_cast(b.s1 , "POLYGON")
        #st_is_valid(b.s1)

        b.s1$area.m <- as.numeric(st_area(b.s1))
        b.s1.df = data.frame(b.s1)        # calculate the length per range 
        b.s1.df.out  =  b.s1.df%>% 
          group_by(SiteName,V17_CH) %>% 
          summarise(R_Seis_m2 = sum(area.m))
        
        # combine into disturbance by layer 
        all.range.out <- left_join(all.range.out,b.s1.df.out)  
        all.range.out[is.na(all.range.out)]<-0      
   
             
## 14) Roads # buffer to 6m width (3m either side) 
# due to the size - roads are split into herds
# Preparation includes split into herds and buffer to 15m width (Telkwa) - Tweeds still to complete) 

# 15 m buffer needs to be added
        
## Roads divided into two shapefiles #Roads1_clip ; #Roads2_clip , #Roads_merge, Roads_diss
        
## Telkwa herd        
      b.r1.sf = sf::st_read(dsn = Base , layer ="Te_road_buf_diss" ) 
      st_is_valid(b.r1.sf )
      b.r1.sf = st_make_valid(b.r1.sf)
      b.r1.sf <- st_cast(b.r1.sf,"POLYGON")
      b.r1 = st_intersection(all.range,b.r1.sf )  # intersect with single all ranges
      st_is_valid(b.r1.int)  
      b.r1$Area.m <- as.numeric(st_area(b.r1 ))
      plot(st_geometry(b.r1))
      #st_write(r.pipe.int,"Dist_R_pipe.shp")       #write out individual dist_layer for Range
      b.r1.df = data.frame(b.r1)        # calcaulte area
      b.r1.df.out  = b.r1.df %>% 
        group_by(SiteName,V17_CH) %>% 
        summarise(R_Roade_area_m = sum(Area.m))


      
      
      ## STILL NEED TO FIX THIS ONE..
      
      
## Tweedsmuir herd 
      b.r2.sf = sf::st_read(dsn = Base , layer ="Tw_roads" ) 
      b.r2.sf <- st_zm( b.r2.sf ,drop = TRUE)
      #b.r2.sf =st_is_valid( b.r2.sf)
      b.r2.sf <- st_buffer(b.r2.sf,7.5)  # buffer    ## STILL NEED TO FIX THIS ONE..
    
      st_is_valid(b.r2.sf )
      b.r2.sf = st_make_valid(b.r2.sf)
      b.r2.sf <- st_cast(b.r2.sf,"POLYGON")
      b.r2 = st_intersection(all.range,b.r2.sf )  # intersect with single all ranges

      b.r2$Area.m <- as.numeric(st_area(b.r2))
      plot(st_geometry(b.r2))
      #st_write(r.pipe.int,"Dist_R_pipe.shp")       #write out individual dist_layer for Range
      b.r2.df = data.frame(b.r2)        # calcaulte area
      b.r2.df.out  = b.r2.df %>% 
        group_by(SiteName,V17_CH) %>% 
        summarise(R_Road_area_m = sum(Area.m))
      
     # join the two roads outputs together 
      
      
      
      
      
      ### NEED TO CHECK THESE ARE WORKING CORRECTLY 
      
      # still neeed to check these are working correctly 
     
      roads.out = b.r1.df.out
      #roads.out = join(b.r1.df.out, b.r2.df.out) 
      
      # combine into disturbance by layer 
      all.range.out <- left_join(all.range.out,roads.out)  
      #all.range.out <- left_join(all.range.out,b.r1.df.out)  
      all.range.out[is.na(all.range.out)]<-0      
      
      
      ## ALL DISTURBANCE: UNION 11 # may need to run this in stand alone R rather than R -studio
      out11 = st_union(out10,b.r1 ) ; plot(st_geometry(out11)) ; rm(out10); rm(b.r1)
      out11 = st_union(out11); plot(st_geometry(out11))
     
      
      
      
      ## STILL TO TEST THIS BIT 
      out12 = st_union(out11,b.r2 ) ; plot(st_geometry(out12)) ; rm(out11); rm(b.r.2)
      out12 = st_union(out12); plot(st_geometry(out12))
      ##x.area = sum(st_area(out8))     ; x.area  # 420881436 [m^2]
      
#######################################################################

# end of part 1: write out all the static disturbance types;
        
# write out all the layers combined into a single disturbance layer ( note this excludes Roads and Seismic )
st_write(out11,paste(temp.dir,"Static_disturb_TT.shp",sep = "")) # this writes out as single layer   
        
# Write out the datasheet onto a temp file 
write.csv(all.range.out,paste(temp.dir,"Static_dist_TT.csv",sep = "") )        
        
# make memory room!
rm('out1','out2','out3','out4','out5','out6','out7','r.agr.sf','r.air.sf','r.dam.sf','r.mine.sf')
rm('r.pipe','r.rail.sf','r.rec.sf','r.tran','r.tran.sf','r.urban.sf','r.wells.sf')
#rm(c('b.r.c0.80','b.r.c0.80.u'))

for (obj in ls()) { message(obj); print(object.size(get(obj)), units='auto') }


################################################################################    
################################################################################

## PART TWO : TEMPORAL DISTURBANCE 

# Cutblocks, 
# Burns
# Pests



#out9 = st_read(dsn = Base, layer = "Disturb_poly_draft1" )
#out9 = st_read(paste(out.dir,"Disturb_poly_draft1.shp",sep = ""), package="sf")
#st_transform(out9,3005)
#out9 <- st_cast(out9, "POLYGON")
#st_is_valid(out9)
#st_make_valid(out9) ; plot(st_geometry(out9))
#out9 <- st_union(out9)

##################################################################################
##################################################################################

## 8) Cutblock ## this is all years of consolidated cutblock layer and Blairs error checked cutblocks

# HERD 1)  ## Telkwa 
b.r.c= st_read(dsn = Base , layer = "cutblock_union_Te")
b.r.c <- st_zm(b.r.c ,drop = TRUE)
# get both values of HARVEST YEAR 
b.r.c$HARVEST_YEAR_ALL = ifelse(b.r.c$HARVEST_YEAR > 0,b.r.c$HARVEST_YEAR,b.r.c$HARVEST_YEAR_1 ) #sort(unique(b.r.c$HARVEST_YEAR_ALL)) # error check 
b.r.c$TimeSinceCut = 2018-b.r.c$HARVEST_YEAR_ALL; # create new column with age since cut 
#sort(unique(b.r.c$TimeSinceCut)) # check the range in years since cut #note in the boreal the oldest age cut is 78 years 
b.r.c<- st_intersection(all.range,b.r.c) #; st_is_valid(b.r.0)
b.r.c <- st_cast(b.r.c,"POLYGON")
b.r.c$area.m = as.numeric(st_area(b.r.c))

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

      r.cut.df = b.r.c
      
      # add a column to differentiate the age brackets of cutblocks 
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1940 & HARVEST_YEAR_ALL <= 1949,1940,0))
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1950 & HARVEST_YEAR_ALL <= 1959,1950,dec.period))
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1960 & HARVEST_YEAR_ALL <= 1969,1960,dec.period))
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1970 & HARVEST_YEAR_ALL <= 1979,1970,dec.period))   
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1980 & HARVEST_YEAR_ALL <= 1989,1980,dec.period))   
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 1990 & HARVEST_YEAR_ALL <= 1999,1990,dec.period)) 
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 2000 & HARVEST_YEAR_ALL <= 2009,2000,dec.period))   
      r.cut.df <- mutate(r.cut.df,dec.period = ifelse(HARVEST_YEAR_ALL >= 2010 & HARVEST_YEAR_ALL <= 2019,2010,dec.period)) 
gc()
      #r.cut.df[r.cut.df$dec.period == 0,] ; unique(r.cut.df$dec.period)  # error check

      # generate table output the amount of cutblock by range (all years (0-80))  
      r.cut.df.df = as.data.frame(r.cut.df)
      
      # all years  
      r.cut.df.out  = r.cut.df.df %>% 
        group_by(SiteName,V17_CH ) %>% 
        summarise(R_cut0_80_m2 = sum(area.m))

      # output the amount of cutblock by range (all years (0-40))  
      r.cut.df.out.0.40 <- r.cut.df.df %>%
        filter(HARVEST_YEAR_ALL >= 1978) %>% 
        group_by(SiteName,V17_CH ) %>% 
        summarise(R_cut0_40_m2 = sum(area.m))

    r.cut.out = merge(r.cut.df.out,r.cut.df.out.0.40 )

    # output cut blocks by decade # we will use this later in the temporal data aggregation (with pest and burns)
    cut.decade = r.cut.df.df %>% 
      group_by(SiteName,V17_CH,dec.period ) %>% 
      summarise(R_cut0_80_m2 = sum(area.m))
    
    # generate cumulative burn disturbance shapefiles to be added sequentially to "static Disturbance" 
    head(r.cut.df) ; unique(r.cut.df$dec.period)
    Cut.dec.1950 <- r.cut.df %>% filter(dec.period == 1950)
    Cut.dec.1960 <- r.cut.df %>% filter(dec.period < 1961 )
    Cut.dec.1970 <-r.cut.df %>% filter(dec.period < 1971 )
    Cut.dec.1980 <-r.cut.df %>% filter(dec.period < 1981 )
    Cut.dec.1990 <- r.cut.df %>% filter(dec.period < 1991 )
    Cut.dec.2000 <- r.cut.df%>% filter(dec.period < 2001 )
    Cut.dec.2010 <- r.cut.df %>% filter(dec.period < 2011 )
    
    # write out the shapefiles to Data.drive
    st_write(Cut.dec.1950,paste(temp.dir,"Cut.te.dec.1950.shp",sep = "")) # this writes out as single layer   
    st_write(Cut.dec.1960,paste(temp.dir,"Cut.te.dec.1960.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.1970,paste(temp.dir,"Cut.te.dec.1970.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.1980,paste(temp.dir,"Cut.te.dec.1980.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.1990,paste(temp.dir,"Cut.te.dec.1990.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.2000,paste(temp.dir,"Cut.te.dec.2000.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.2010,paste(temp.dir,"Cut.te.dec.2010.shp",sep = "")) # this writes out as single layer
    
    
# HERD 2)  ## Tweedsmuir
    b.r.c2= st_read(dsn = Base , layer = "cutblock_union_Tw")
    b.r.c2 <- st_zm(b.r.c2 ,drop = TRUE)
    # get both values of HARVEST YEAR 
    b.r.c2$HARVEST_YEAR_ALL = ifelse(b.r.c2$HARVEST_YEAR > 0,b.r.c2$HARVEST_YEAR,b.r.c2$HARVEST_YEAR_1 ) #sort(unique(b.r.c$HARVEST_YEAR_ALL)) # error check 
    b.r.c2$TimeSinceCut = 2018-b.r.c2$HARVEST_YEAR_ALL; # create new column with age since cut 
    #sort(unique(b.r.c$TimeSinceCut)) # check the range in years since cut #note in the boreal the oldest age cut is 78 years 
    b.r.c2<- st_intersection(all.range,b.r.c2) #; st_is_valid(b.r.0)
    b.r.c2 <- st_cast(b.r.c2,"POLYGON")
    b.r.c2$area.m = as.numeric(st_area(b.r.c2))
    
    # cutblocks 0-80 years
    b.r.c0.802 = b.r.c2[b.r.c2$TimeSinceCut < 81,] ; unique(b.r.c0.802$TimeSinceCut); #plot(b.r.c0.80$Shape)
    b.r.c0.802 = st_cast(b.r.c0.802,"POLYGON"); #
    b.r.c0.802 = st_make_valid(b.r.c0.802)
    b.r.c0.80.02 <- b.r.c0.802  # set this asside to use later in the code
    #all.cut = sum(st_area(b.r.c0.80)) #386611203 [m^2]
    b.r.c0.80.u2 = st_union(b.r.c0.802) # all cutblocks 0-80 years old 
    
    # cutblocks 0-40 years
    b.r.c0.402 = b.r.c2[b.r.c2$TimeSinceCut < 41,] ; unique(b.r.c0.40$TimeSinceCut); #plot(st_geometry(b.r.c0.40))
    b.r.c0.402 = st_cast(b.r.c0.402,"POLYGON")
    b.r.c0.402 = st_make_valid(b.r.c0.402) #; st_is_valid(b.r.c0.40)
    #all.cut = sum(st_area(b.r.c0.40)) #260124543  [m^2]
    b.r.c0.40.u2 = st_union(b.r.c0.402) # all cutblocks 0-40 years old 
    
    r.cut.df2 = b.r.c2
    
    # add a column to differentiate the age brackets of cutblocks 
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 1940 & HARVEST_YEAR_ALL <= 1949,1940,0))
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 1950 & HARVEST_YEAR_ALL <= 1959,1950,dec.period))
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 1960 & HARVEST_YEAR_ALL <= 1969,1960,dec.period))
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 1970 & HARVEST_YEAR_ALL <= 1979,1970,dec.period))   
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 1980 & HARVEST_YEAR_ALL <= 1989,1980,dec.period))   
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 1990 & HARVEST_YEAR_ALL <= 1999,1990,dec.period)) 
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 2000 & HARVEST_YEAR_ALL <= 2009,2000,dec.period))   
    r.cut.df2 <- mutate(r.cut.df2,dec.period = ifelse(HARVEST_YEAR_ALL >= 2010 & HARVEST_YEAR_ALL <= 2019,2010,dec.period)) 
 
    #r.cut.df[r.cut.df$dec.period == 0,] ; unique(r.cut.df$dec.period)  # error check
    
    # generate table output the amount of cutblock by range (all years (0-80))  
    r.cut.df.df2 = as.data.frame(r.cut.df2)
    
    # all years  
    r.cut.df.out2  = r.cut.df.df2 %>% 
      group_by(SiteName,V17_CH ) %>% 
      summarise(R_cut0_80_m2 = sum(area.m))
    
    # output the amount of cutblock by range (all years (0-40))  
    r.cut.df.out.0.402 <- r.cut.df.df2 %>%
      filter(HARVEST_YEAR_ALL >= 1978) %>% 
      group_by(SiteName,V17_CH ) %>% 
      summarise(R_cut0_40_m2 = sum(area.m))
    
    r.cut.out2 = merge(r.cut.df.out2,r.cut.df.out.0.402 )
    
    # output cut blocks by decade # we will use this later in the temporal data aggregation (with pest and burns)
    cut.decade2 = r.cut.df.df2 %>% 
      group_by(SiteName,V17_CH,dec.period ) %>% 
      summarise(R_cut0_80_m2 = sum(area.m))
    
    # generate cumulative burn disturbance shapefiles to be added sequentially to "static Disturbance" 
    head(r.cut.df2) ; unique(r.cut.df2$dec.period)
    Cut.dec.19502 <- r.cut.df2 %>% filter(dec.period == 1950)
    Cut.dec.19602 <- r.cut.df2 %>% filter(dec.period < 1961 )
    Cut.dec.19702 <-r.cut.df2 %>% filter(dec.period < 1971 )
    Cut.dec.19802 <-r.cut.df2 %>% filter(dec.period < 1981 )
    Cut.dec.19902 <- r.cut.df2 %>% filter(dec.period < 1991 )
    Cut.dec.20002 <- r.cut.df2%>% filter(dec.period < 2001 )
    Cut.dec.20102 <- r.cut.df2 %>% filter(dec.period < 2011 )
    
    # write out the shapefiles to Data.drive
    st_write(Cut.dec.19502,paste(temp.dir,"Cut.tw.dec.1950.shp",sep = "")) # this writes out as single layer   
    st_write(Cut.dec.19602,paste(temp.dir,"Cut.tw.dec.1960.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.19702,paste(temp.dir,"Cut.tw.dec.1970.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.19802,paste(temp.dir,"Cut.tw.dec.1980.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.19902,paste(temp.dir,"Cut.tw.dec.1990.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.20002,paste(temp.dir,"Cut.tw.dec.2000.shp",sep = "")) # this writes out as single layer
    st_write(Cut.dec.20102,paste(temp.dir,"Cut.tw.dec.2010.shp",sep = "")) # this writes out as single layer
    
   
    ###############
  
     # Join the telkwa and Tweedsmuir herd info together   
    r.cut.out.all = rbind(r.cut.out,r.cut.out2) 
    r.cut.decade.all = rbind(cut.decade,cut.decade2)  # keep this for the other decadenal outputs
    
   # write.csv(r.cut.decade.all,paste(temp.dir,"Temp_cut_dec_outputs.csv",sep =""))  
    
    # combine into disturbance by layer 
    all.range.out <- left_join(all.range.out,r.cut.out.all)  
    all.range.out[is.na(all.range.out)]<-0
    
  #  write.csv(all.range.out,paste(temp.dir,"Temp_output_summary.csv",sep =""))  
    
    
#################################################################### 

## BURN:
# Add to Burn information Break down of burns into 0-40 and 0-80 years (and total) 
# Break down burns into fire years (same as cutblocks) 
# this generates the outputs for the 0-80 years, 0-40 years and for cumulative decades (1950 - 2010)

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
    
    # combine into disturbance by layer (from 0 - 80 years)
    all.range.out <- left_join(all.range.out,r.burn.out)  
    all.range.out[is.na(all.range.out)]<-0
    #write.csv(all.range.out,paste(temp.dir,"Temp_output_summary.csv",sep =""))  
    
    # split burns into decades (using all the entire data set)
    b.r.00.df <-   b.r.0.80
  
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
          # write out the decade data to table. 
          Burn.dec = b.r.00.df %>% group_by(SiteName,V17_CH,dec.period ) %>% summarise(R_burn0_80_m2 = sum(area.m))
          Burn.dec.df = as.data.frame(Burn.dec)
          Burn.dec.df <- Burn.dec.df %>% dplyr::select(c(SiteName,V17_CH, dec.period, R_burn0_80_m2))
          
          # generate cumulative burn disturbance shapefiles to be added sequentially to "static Disturbance" 
          head( b.r.0.80)
          Burn.dec.1950 <- Burn.dec %>% filter(dec.period == 1950)
          Burn.dec.1960 <- Burn.dec %>% filter(dec.period < 1961 )
          Burn.dec.1970 <- Burn.dec %>% filter(dec.period < 1971 )
          Burn.dec.1980 <- Burn.dec %>% filter(dec.period < 1981 )
          Burn.dec.1990 <- Burn.dec %>% filter(dec.period < 1991 )
          Burn.dec.2000 <- Burn.dec %>% filter(dec.period < 2001 )
          Burn.dec.2010 <- Burn.dec %>% filter(dec.period < 2011 )
         # Burn.dec.2010 <- Burn.dec %>% filter(dec.period < 2011 )
          
          # write out the shapefiles to Data.drive
          st_write(Burn.dec.1950,paste(temp.dir,"Burn.dec.1950.shp",sep = "")) # this writes out as single layer   
          st_write(Burn.dec.1960,paste(temp.dir,"Burn.dec.1960.shp",sep = "")) # this writes out as single layer
          st_write(Burn.dec.1970,paste(temp.dir,"Burn.dec.1970.shp",sep = "")) # this writes out as single layer
          st_write(Burn.dec.1980,paste(temp.dir,"Burn.dec.1980.shp",sep = "")) # this writes out as single layer
          st_write(Burn.dec.1990,paste(temp.dir,"Burn.dec.1990.shp",sep = "")) # this writes out as single layer
          st_write(Burn.dec.2000,paste(temp.dir,"Burn.dec.2000.shp",sep = "")) # this writes out as single layer
          st_write(Burn.dec.2010,paste(temp.dir,"Burn.dec.2010.shp",sep = "")) # this writes out as single layer
          
###########################################

# join the cutlock with burn decade data. 
# write out the data table: 
          
all.temp.data = left_join(r.cut.decade.all,Burn.dec.df ,by = c("SiteName", "V17_CH", "dec.period"))
all.temp.data[is.na(all.temp.data)]<- 0 

# write.csv(all.temp.data,paste(temp.dir,"Temp_cut_burn_dec_outputs.csv",sep =""))  

############################################
### PEST 
# this needs pre-processing in arcmap - select ibm and ibs species - split into two sections as this is 
r.pest.te <-  st_read(dsn = Base, layer ="Pest_clip_Te_IBMIBS") #; plot(st_geometry(r.pest.te))
r.pest.tw <-  st_read(dsn = Base, layer ="Pest_clip_Tw_IBMIBS") #; plot(st_geometry(r.pest.tw))

# Telkwa 
r.pest <- r.pest.te
r.pest<- st_zm(r.pest ,drop = TRUE) # this is a linear feature so need to buffer to estimate area calcs
r.pest = st_cast(r.pest,"POLYGON")
st_make_valid(r.pest)

        # Range 
        r.pest <-  st_intersection(all.range,r.pest) #; st_is_valid(r.pest)
        r.pest$area.m = as.numeric(st_area(r.pest)) 
        st_is_valid(r.pest)
        st_make_valid(r.pest)
        
        r.pest.df = r.pest
        
        # add decade data   # add a column to differentiate the age brackets of pest capture
        r.pest.df<- r.pest.df %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1960 & CAPTURE_YEAR <= 1969,1960,0))
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1970 & CAPTURE_YEAR <= 1979,1970,dec.period))
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1980 & CAPTURE_YEAR <= 1989,1980,dec.period))
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 1990 & CAPTURE_YEAR <= 1999,1990,dec.period))
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2000 & CAPTURE_YEAR <= 2009,2000,dec.period))
        r.pest.df<- mutate(r.pest.df,dec.period = ifelse(CAPTURE_YEAR >= 2010 & CAPTURE_YEAR <= 2018,2010,dec.period))
   
        #sort(unique(r.pest$CAPTURE_YEAR)) 

        # output tables: 
        # work with the data frames
        r.pest.df.df =  as.data.frame(r.pest.df)
        
        # output the amount of burns by range (all years (0-80))  
        r.pest.df.out  = r.pest.df.df %>% 
          group_by(SiteName, V17_CH) %>% summarise(R_pest_m2 = sum(area.m))
        
        #output the amount of burns per decade (all years) 
        r.pest.df.out.temp  =   r.pest.df.df %>% group_by(SiteName, V17_CH,dec.period) %>% summarise(R_pest_dec_m2 = sum(area.m))
       
         #output the amount of burn per decade per species (all years) 
        r.pest.df.out.type  = r.pest.df.df %>% group_by(SiteName,PEST_SPECIES_CODE,dec.period) %>% summarise(R_pest_type_m2 = sum(area.m))
        
        # Generate the cumulative pest damage into decades to add to "static Disturbance" 
        #Pest.dec.1950 <- r.pest.df %>% filter(dec.period == 1950)
        Pest.dec.1960 <- r.pest.df %>% filter(dec.period < 1961 )
        Pest.dec.1970 <- r.pest.df %>% filter(dec.period < 1971 )
        Pest.dec.1980 <- r.pest.df %>% filter(dec.period < 1981 )
        Pest.dec.1990 <- r.pest.df %>% filter(dec.period < 1991 )
        Pest.dec.2000 <- r.pest.df %>% filter(dec.period < 2001 )
        Pest.dec.2010 <- r.pest.df %>% filter(dec.period < 2011 )
        # Burn.dec.2010 <- Burn.dec %>% filter(dec.period < 2011 )
        
         # output shapefiles. 
        # write out the shapefiles to Data.drive
        st_write(Pest.dec.1950 ,paste(temp.dir,"Pest.te.dec.1950.shp",sep = "")) # this writes out as single layer   
        st_write(Pest.dec.1960 ,paste(temp.dir,"Pest.te.dec.1960.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.1970 ,paste(temp.dir,"Pest.te.dec.1970.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.1980 ,paste(temp.dir,"Pest.te.dec.1980.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.1990,paste(temp.dir,"Pest.te.dec.1990.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.2000,paste(temp.dir,"Pest.te.dec.2000.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.2010,paste(temp.dir,"Pest.te.dec.2010.shp",sep = "")) # this writes out as single layer
        
        

# Tweedsmuir 
r.pest2 <- r.pest.tw
r.pest2<- st_zm(r.pest2 ,drop = TRUE) # this is a linear feature so need to buffer to estimate area calcs
r.pest2 = st_cast(r.pest2,"POLYGON")
st_make_valid(r.pest2)
        
        # Range 
        r.pest2 <-  st_intersection(all.range,r.pest2) #; st_is_valid(r.pest)
        r.pest2$area.m = as.numeric(st_area(r.pest2)) 
        st_is_valid(r.pest2)
        st_make_valid(r.pest2)
        
        r.pest.df2 = r.pest2
        
        # add decade data   # add a column to differentiate the age brackets of pest capture
        r.pest.df2<- r.pest.df2 %>% mutate(dec.period = ifelse(CAPTURE_YEAR >= 1960 & CAPTURE_YEAR <= 1969,1960,0))
        r.pest.df2<- mutate(r.pest.df2,dec.period = ifelse(CAPTURE_YEAR >= 1970 & CAPTURE_YEAR <= 1979,1970,dec.period))
        r.pest.df2<- mutate(r.pest.df2,dec.period = ifelse(CAPTURE_YEAR >= 1980 & CAPTURE_YEAR <= 1989,1980,dec.period))
        r.pest.df2<- mutate(r.pest.df2,dec.period = ifelse(CAPTURE_YEAR >= 1990 & CAPTURE_YEAR <= 1999,1990,dec.period))
        r.pest.df2<- mutate(r.pest.df2,dec.period = ifelse(CAPTURE_YEAR >= 2000 & CAPTURE_YEAR <= 2009,2000,dec.period))
        r.pest.df2<- mutate(r.pest.df2,dec.period = ifelse(CAPTURE_YEAR >= 2010 & CAPTURE_YEAR <= 2018,2010,dec.period))
        
        #sort(unique(r.pest$CAPTURE_YEAR)) 
        
        # output tables: 
        # work with the data frames
        r.pest.df.df2 =  as.data.frame(r.pest.df2)
        
        # output the amount of burns by range (all years (0-80))  
        r.pest.df.out2  = r.pest.df.df2 %>% 
          group_by(SiteName, V17_CH) %>% summarise(R_pest_m2 = sum(area.m))
        
        #output the amount of burns per decade (all years) 
        r.pest.df.out.temp2  =   r.pest.df.df2 %>% group_by(SiteName, V17_CH,dec.period) %>% summarise(R_pest_dec_m2 = sum(area.m))
        
        #output the amount of burn per decade per species (all years) 
        r.pest.df.out.type2  = r.pest.df.df2 %>% group_by(SiteName,PEST_SPECIES_CODE,dec.period) %>% summarise(R_pest_type_m2 = sum(area.m))
        
        # Generate the cumulative pest damage into decades to add to "static Disturbance" 
        Pest.dec.19502 <- r.pest.df2 %>% filter(dec.period == 1950)
        Pest.dec.19602 <- r.pest.df2 %>% filter(dec.period < 1961 )
        Pest.dec.19702 <- r.pest.df2 %>% filter(dec.period < 1971 )
        Pest.dec.19802 <- r.pest.df2 %>% filter(dec.period < 1981 )
        Pest.dec.19902 <- r.pest.df2 %>% filter(dec.period < 1991 )
        Pest.dec.20002 <- r.pest.df2 %>% filter(dec.period < 2001 )
        Pest.dec.20102 <- r.pest.df2 %>% filter(dec.period < 2011 )
        # Burn.dec.2010 <- Burn.dec %>% filter(dec.period < 2011 )
        
        # output shapefiles. 
        # write out the shapefiles to Data.drive
        st_write(Pest.dec.19502 ,paste(temp.dir,"Pest.tw.dec.1950.shp",sep = "")) # this writes out as single layer   
        st_write(Pest.dec.19602 ,paste(temp.dir,"Pest.tw.dec.1960.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.19702 ,paste(temp.dir,"Pest.tw.dec.1970.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.19802 ,paste(temp.dir,"Pest.tw.dec.1980.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.19902,paste(temp.dir,"Pest.tw.dec.1990.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.20002,paste(temp.dir,"Pest.tw.dec.2000.shp",sep = "")) # this writes out as single layer
        st_write(Pest.dec.20102,paste(temp.dir,"Pest.tw.dec.2010.shp",sep = "")) # this writes out as single layer
        
        
        
 ###############
        
        # Join the telkwa and Tweedsmuir herd info together   
        r.pest.out.all = rbind(r.pest.out,r.pest.out2) 
        r.pest.decade.all = rbind(r.pest.df.out.temp,r.pest.df.out.temp2)  # keep this for the other decadenal outputs
        
        write.csv(r.pest.decade.all,paste(temp.dir,"Temp_pest_dec_outputs.csv",sep =""))  
        
        # combine into disturbance by layer 
        all.range.out <- left_join(all.range.out,r.pest.out.all)  
        all.range.out[is.na(all.range.out)]<-0
        
        write.csv(all.range.out,paste(temp.dir,"Temp_output_summary.csv",sep =""))  
        
   
############################################################################

# Aggregate temporal data sets 
  
        
        
        
# STILL TO DO>         
        
        
              
# Pests + Burns 
        
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


