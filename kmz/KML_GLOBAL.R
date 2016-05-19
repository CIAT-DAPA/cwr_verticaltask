
require(plotKML);require(raster)
out_dir<-""
src.dir <- paste("",sep="") # !!! change accordingly !!!

SP_RICH_DIR<-""
GAP_RICH_DIR<-""


sp_rich<-raster(paste0(SP_RICH_DIR,"/","global_species_richness.asc"))
sp_rich[which(sp_rich[]==0)]<-NA
sp_rich[which(sp_rich[]<0)]<-NA
sp_rich<-setMinMax(sp_rich)
proj4string(sp_rich) <-CRS("+proj=longlat +datum=WGS84") 

gap_rich<-raster(paste0(GAP_RICH_DIR,"/","global_gap_richness.asc"))
gap_rich[which(gap_rich[]==0)]<-NA
gap_rich[which(gap_rich[]<0)]<-NA
gap_rich<-setMinMax(gap_rich)
proj4string(gap_rich) <-CRS("+proj=longlat +datum=WGS84") 


SP_RICH_OUT_PATH<-paste0(out_dir,"/","global_species_richness");if (!file.exists(SP_RICH_OUT_PATH)) {dir.create(SP_RICH_OUT_PATH)}
GAP_RICH_OUT_PATH<-paste0(out_dir,"/","global_gap_richness");if (!file.exists(GAP_RICH_OUT_PATH)) {dir.create(GAP_RICH_OUT_PATH)} 



F1<-c("#ED801D","#5AED1D","#1DED80","#1D5AED","#B01DED","#ED1D5A") #6
G1<-c("#ED801D","#77ED1D","#1DED75","#1D83ED","#391DED","#EC1DED","#ED1D3C") #7


setwd(SP_RICH_OUT_PATH)
kml(sp_rich,file.name="global_species_richness.kml",raster_name="global_species_richness.png",colour_scale=G1,plot.legend =T, kmz = F,layer.name="Collecting hotspots map",png.width=7640,png.height = 1600)
zip("global_species_richness.kmz",files=c("global_species_richness.png","global_species_richness.kml","global_species_richness_legend.png"))
file.remove(c("global_species_richness.png","global_species_richness.kml","global_species_richness_legend.png"))

setwd(GAP_RICH_OUT_PATH)
kml(gap_rich,file.name="global_gap_richness.kml",raster_name="global_gap_richness.png",colour_scale=F1,plot.legend =T, kmz = F,layer.name="Collecting hotspots map",png.width=7640,png.height = 1600)
zip("global_gap_richness.kmz",files=c("global_gap_richness.png","global_gap_richness.kml","global_gap_richness_legend.png"))
file.remove(c("global_gap_richness.png","global_gap_richness.kml","global_gap_richness_legend.png"))

rm(list=ls(all=T));quit("yes")