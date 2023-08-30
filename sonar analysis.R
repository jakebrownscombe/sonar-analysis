#Aquatic habitat analysis from sonar data associated with:

#Jacob W. Brownscombe, Kurtis Smith, Rick Elsner, Karen Smokorowski. 2023.
#Guidance and Tools for Mapping Fish Habitat with Side-scan Sonar. Can. Tech. Rep. Fish. Aquat. Sci. 

#packages (may need to be installed on your computer)
library(ggplot2)
library(dplyr)
library(sf)
library(raster)
library(tmap)
library(tmaptools)
library(sfheaders)
library(tidyr)
library(terra)
library(interp)
library(patchwork)
library(ggridges)

#load files

#In the eg from the above document, we bring in an .mbtiles file from ReefMaster. 
#This file is 8cm resolution, which is too large >4GB to upload to GitHub. 
#This is some example code to load and downsample that file type:

#library(raster)
#sidescan.mosaic <- stack("~/file.mbtiles")
#sidescan.mosaic <- stack(aggregate(sidescan.mosaic, fact=50, fun="mean")) 
#sidescan.mosaic <- projectRaster(sidescan.mosaic, crs = "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")

#starting here with a down-sampled side scan sonar mosaic from git repo:
sidescan.mosaic <- readRDS("~/github/sonar-analysis/data/sonar.mosaic.rds")
plot(sidescan.mosaic)


#Habitat shapefiles drawn in QGIS: 
hab.poly <- read_sf("~/github/sonar-analysis/data/habitat shapefiles/habitats.shp")
hab.poly <- hab.poly %>%  st_transform(crs="+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")
plot(hab.poly)



#arrange the habitat polygons to the same scale as the image mosaic 

#mosaic to points 
mosaic.spdf <- as(sidescan.mosaic, "SpatialPixelsDataFrame")
mosaic.df <- as.data.frame(mosaic.spdf)
ggplot(mosaic.df %>% filter(channel.eg_1!=0), aes(x,y,fill=channel.eg_1))+geom_tile()

#to sf data format
mosaic.sf <- mosaic.df %>% st_as_sf(coords=c("x","y"), crs="+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")
mosaic.sf


#convert habitat polys to point data at the scale of side scan
habitats <- st_intersection(hab.poly, mosaic.sf)

#joing with full grid
habitats.sf <- st_join(mosaic.sf %>% filter(channel.eg_1!=0), habitats)  %>%  dplyr::select(id)
habitats.sf

tmap_mode('view')
tm_shape(habitats.sf)+tm_dots(col="id")+
  tmap_options(basemaps = 'Esri.WorldImagery')





#fill in blanks with k-nearest neighbour algorithm
habitats.df <- sf_to_df(habitats.sf) %>% dplyr::select(point_id,x,y)
habitats.df$id <- habitats.sf$id

#format train/test cases
train <- habitats.df %>% filter(!is.na(id)) %>% dplyr::select(x,y)
test <- habitats.df %>% filter(is.na(id)) %>% dplyr::select(x,y)
test.output <- habitats.df %>% filter(is.na(id)) %>% dplyr::select(point_id,x,y)
cl <- habitats.df %>% filter(!is.na(id)) %>% dplyr::select(id) %>% mutate(id=as.factor(id))
cl <- as.factor(cl$id)


#use model to predict
test.output$pred <- class::knn(train=train, test=test, cl=cl, k = 10, prob=F)
head(test.output)

#assign to data and format
habitats.df$pred <- test.output$pred[match(habitats.df$point_id, test.output$point_id)]
habitats.df$hab.n <- ifelse(is.na(habitats.df$id), habitats.df$pred, habitats.df$id)

#assign character classes
hab.classes <- data.frame(hab.n=c(1,2,3,4,5), habitat.class=c("vegetation","bedrock","wood","fines","boulders"))
habitats.df$habitat.class <- hab.classes$habitat.class[match(habitats.df$hab.n, hab.classes$hab.n)]
habitats.df <- habitats.df %>% dplyr::select(point_id, x,y,id, hab.n, habitat.class)
habitats.output.sf <- habitats.df %>% st_as_sf(coords=c("x","y"), crs="+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")

#maps
tmap_mode('view')
tm_shape(habitats.output.sf)+tm_dots(col="habitat.class")+
  tmap_options(basemaps = 'Esri.WorldImagery')


#assign habitats back to grid raster scale
hab.grd  <- habitats.df %>% pivot_wider(names_from=habitat.class, values_from = hab.n)
hab.grd$habitat <- ifelse(!is.na(hab.grd$wood), hab.grd$wood,
                          ifelse(!is.na(hab.grd$vegetation), hab.grd$vegetation,
                                 ifelse(!is.na(hab.grd$fines), hab.grd$fines,
                                        ifelse(!is.na(hab.grd$bedrock), hab.grd$bedrock, hab.grd$boulders))))

hab.raster <- rasterize(hab.grd %>% dplyr::select(x,y), y=sidescan.mosaic, field=hab.grd$habitat)
hab.raster
plot(hab.raster)
table(hab.raster@data@values)


#back to sf for plotting
hab.output.spdf <- as(hab.raster, "SpatialPixelsDataFrame")
hab.output.df <- as.data.frame(hab.output.spdf)
head(hab.output.df)

#using sf
hab.output.sf <- hab.output.df %>% st_as_sf(coords=c("x","y"), crs="+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")
hab.output.sf$habitat <- hab.classes$habitat.class[match(hab.output.df$layer, hab.classes$hab.n)]
hab.output.sf 


#plot
bbox <- bb(hab.raster, ext=1.3)
osm_stoney <- read_osm(bbox, type='bing') 
values(sidescan.mosaic)[values(sidescan.mosaic) == 0] = NA

#1. just side scan
tm_shape(osm_stoney)+tm_rgb()+tm_shape(sidescan.mosaic) + tm_rgb(r=1, g=2, b=3, colorNA=NULL)+
  tm_layout(legend.bg.color="white", legend.text.size=1, legend.title.size = 2, legend.frame="grey60", 
            legend.frame.lwd = 2, frame="grey60", frame.lwd=3, fontfamily = "Arial")+
  tm_scale_bar(text.color = "white", text.size = 1)


#2. ss+hab polys
tm_shape(osm_stoney)+tm_rgb()+tm_shape(sidescan.mosaic) + tm_rgb(r=1, g=2, b=3, colorNA=NULL)+
  tm_shape(hab.poly)+tm_polygons(col="id", n=4, alpha=0.6, palette=c("forestgreen","darkslategray","sienna","bisque3"), style="fixed", 
                                 breaks=c(0.5,1.5,2.5,4.5,5.5), labels=c("Vegetation","Bedrock","Fines","Boulders"), title="Habitat")+
  tm_layout(legend.bg.color="white", legend.text.size=1, legend.title.size = 2, legend.frame="grey60", 
            legend.frame.lwd = 2, frame="grey60", frame.lwd=3, fontfamily = "Arial")



#3. habitats
tm_shape(osm_stoney)+tm_rgb()+tm_shape(sidescan.mosaic) + tm_rgb(r=1, g=2, b=3, colorNA=NULL)+
  tm_shape(hab.raster)+
  tm_raster(n=4, alpha=0.6, palette=c("forestgreen","darkslategray","sienna","bisque3"), style="fixed", 
            breaks=c(0.5,1.5,2.5,4.5,5.5), labels=c("Vegetation","Bedrock","Fines","Boulders"), title="Habitat")+
  tm_layout(legend.bg.color="white", legend.text.size=1, legend.title.size = 2, legend.frame="grey60", 
            legend.frame.lwd = 2, frame="grey60", frame.lwd=3, fontfamily = "Arial")







#calculate area of each hab type 
hab.raster
2.66*2.65 #is area of each cell 

#calc
hab.area <- data.frame(raster::zonal(hab.raster, hab.raster, "count"))
hab.area$km2 <- hab.area$count*(2.66*2.65)/1000000
hab.area$habitat <- hab.classes$habitat.class[match(hab.area$zone, hab.classes$hab.n)]
hab.area

ggplot(hab.area, aes(habitat, km2, fill=habitat))+geom_bar(stat = 'identity', col="black", alpha=0.8)+
  xlab("Habitat Type")+ylab(bquote("Area km"^2))+
  scale_fill_manual(values=c("darkslategray","bisque3","sienna","forestgreen"), name="Habitat")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "none", text=element_text(size=20))













#Down view sonar data

#Biobase outputs:
bio.depth <- read.csv("~/github/sonar-analysis/data/depth.csv")
bio.hard <- read.csv("~/github/sonar-analysis/data/hardness.csv")
bio.veg <- read.csv("~/github/sonar-analysis/data/veg.csv")
head(bio.depth)

bio.depth$depth.m <- bio.depth$DepthInFeet*0.3048

bio.all <- merge(bio.depth %>% dplyr::select(y=Latitude, x=Longitude, depth.m),
                 bio.hard %>% dplyr::select(y=Latitude, x=Longitude, Hardness), by=c("x","y"))
bio.all <- merge(bio.all, bio.veg %>% dplyr::select(y=Latitude, x=Longitude, BioVolume), by=c("x","y"))
head(bio.all)

#need to filter to just burleigh area, use that to build a raster with higher res.
bio.all.burleigh <- bio.all %>% filter(x< -78.17289)


#rasterize outputs
bio.sf <- st_as_sf(bio.all.burleigh, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  st_transform(crs="+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")
bio.sf

templateRaster <- raster(xmn = 722386,   # set minimum x coordinate
                         xmx = 724296,    # set maximum x coordinate
                         ymn = 4936471,     # set minimum y coordinate
                         ymx = 4937761,     # set maximum y coordinate
                         res = c(10,10), crs="+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs")

bio.raster <- rasterize(bio.sf, templateRaster)
bio.raster

#calculate 2d contours for depths:
depth.contour <- rasterToContour(bio.raster[[2]], nlevels=34, maxpixels = 2110500)


#plot
tmap_mode("view")
tm_shape(bio.raster[[2]]) +
  tm_raster(palette = map.pal("water"), n=100)+
  tm_shape(depth.contour)+
  tm_lines(lwd=0.5)+
  tm_shape(bio.raster[[3]]) +
  tm_raster(alpha=1)+
  tm_shape(bio.raster[[4]]) +
  tm_raster(alpha=1, palette = "viridis")+
  tmap_options(basemaps = 'Esri.WorldImagery', max.raster = c(plot = 5000000, view = 500000000))+
  tm_facets(as.layers=TRUE)




#compare outputs with SSS 

#combine outputs
hab.sf.processed <- st_as_sf(hab.grd %>% dplyr::select(x,y,habitat), coords=c("x", "y"))
hab.bio.raster <- rasterize(hab.sf.processed, bio.raster)
plot(hab.bio.raster)
hab.bio.raster

hab.bio.df <- as.data.frame(hab.bio.raster, xy=TRUE)
bio.raster.df <- as.data.frame(bio.raster, xy=TRUE)
hab.bio.df.comb <- merge(hab.bio.df, bio.raster.df, by=c("x","y"), all.x=TRUE)
hab.bio.df.comb <- hab.bio.df.comb %>% dplyr::select(-ID.x, -ID.y) %>% filter(!is.na(habitat))
hab.bio.df.comb$habitat.class <- hab.classes$habitat.class[match(hab.bio.df.comb$habitat, hab.classes$hab.n)]
head(hab.bio.df.comb) #all data in same object for plotting


#all three variables combined - need to interpolate 
hab.bio.df.comb <- hab.bio.df.comb %>% filter(!is.na(BioVolume))
bio.interp <- interp(hab.bio.df.comb$depth.m, hab.bio.df.comb$Hardness, hab.bio.df.comb$BioVolume)
bio.interp.df <- bio.interp[[3]]
colnames(bio.interp.df)<- bio.interp[[2]]
bio.interp.df <- cbind(bio.interp.df, data.frame(depth.m=bio.interp[[1]]))
bio.interp.df <- bio.interp.df %>% pivot_longer(cols=1:40, names_to="Hardness", values_to="BioVolume")
bio.interp.df$Hardness <- as.numeric(bio.interp.df$Hardness)
head(bio.interp.df)



#plots
ggplot(hab.bio.df.comb, aes(depth.m, BioVolume))+geom_point()+geom_smooth(col="red")+
  theme_bw()+xlab("Water Depth (m)")+ylab("Vegetation Bio-volume (proportion)")+
  
  ggplot(hab.bio.df.comb, aes(depth.m, habitat.class, fill=habitat.class))+geom_density_ridges(alpha=0.6)+
  xlab("Depth (meters, down scan sonar)")+ylab("Habitat Class (side scan sonar)")+
  scale_fill_manual(values=c("darkslategray","bisque3","sienna","forestgreen"))+theme_bw()+theme(legend.position = "none")+
  
  ggplot(hab.bio.df.comb, aes(Hardness, BioVolume))+geom_point()+geom_smooth(col="red")+
  theme_bw()+xlab("Hardness (proportion)")+ylab("Vegetation Bio-volume (proportion)")+
  
  ggplot(hab.bio.df.comb, aes(BioVolume, habitat.class, fill=habitat.class))+geom_density_ridges(alpha=0.6)+
  xlab("BioVolume (proportion, down scan sonar)")+ylab("Habitat Class (side scan sonar)")+
  scale_fill_manual(values=c("darkslategray","bisque3","sienna","forestgreen"))+theme_bw()+theme(legend.position = "none")+
  
  ggplot(bio.interp.df, aes(depth.m, Hardness, fill=BioVolume))+
  geom_tile()+scale_fill_viridis_c(na.value="NA")+
  theme_bw()+xlab("Depth (m)")+ylab("Hardness (proportion)")+
  
  ggplot(hab.bio.df.comb, aes(Hardness, habitat.class, fill=habitat.class))+geom_density_ridges(alpha=0.6)+
  xlab("Hardness (proportion, down scan sonar)")+ylab("Habitat Class (side scan sonar)")+
  scale_fill_manual(values=c("darkslategray","bisque3","sienna","forestgreen"))+theme_bw()+theme(legend.position = "none")+
  
  plot_layout(ncol=2)


