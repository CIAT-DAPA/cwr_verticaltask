# Organize models to publish on website
# N. Castaneda, 2013

####################
require(raster)

src.dir <- paste("/curie_data2/ncastaneda/gap-analysis/gap_",crop,"/_scripts",sep="") # !!! change accordingly !!!
source(paste(src.dir,"/000.zipRead.R",sep=""))
source(paste(src.dir,"/000.zipWrite.R",sep=""))

####################
# Project priority crops
cropList= c("avena", "bambara", "bean", "cajanus", "cicer", "cowpea", "daucus", "eggplant", "eleusine", "faba_bean", "helianthus", "hordeum", 
            "ipomoea", "lathyrus", "lens", "lima_bean", "malus","medicago","musa","pennisetum", "pisum", "potato", "rice", "secale", "sorghum", 
            "triticum", "vetch")

maindir = "/curie_data2/ncastaneda/gap-analysis"
outdir = paste(maindir, "/_models4web", sep=""); if(!file.exists(outdir)){dir.create(outdir)}

########################################
# Preparing distribution models
########################################
reclassNAs <- function(crop){
  crop_dir = paste("/curie_data2/ncastaneda/gap-analysis/gap_", crop, sep="")
  outCrop = paste(outdir, "/gap_", crop, sep=""); if(!file.exists(outCrop)) {dir.create(outCrop)}
  out = paste(outCrop, "/models", sep=""); if(!file.exists(out)){dir.create(out)}
  priorities = paste(crop_dir, "/priorities/priorities.csv", sep="")
  pri = read.csv(priorities)
  
  #Preparing files for species with valid models
  priList = pri[pri$IS_VALID==1,]
  for(spp in priList$TAXON){
    modelDir <- paste(crop_dir, "/maxent_modeling/models/", spp, "/projections", sep="")
    sppFile <- paste(spp, "_worldclim2_5_EMN_PA.asc.gz", sep="")
    sppModel <- zipRead(modelDir, sppFile)
    sppModel[which(sppModel[]==0)] <- NA
    writeRaster(sppModel, paste(out, "/", spp, ".asc", sep=""), overwrite=T)
    #     zipWrite(sppModel, out, paste(spp,".asc.gz",sep=""))
  }
  
  #Preparing files for species with non-valid models
  priList = pri[pri$IS_VALID==0,]
  for(spp in priList$TAXON){
    modelDir <- paste(crop_dir, "/samples_calculations/", spp, sep="")
    sppFile <- "samples-buffer-na.asc.gz"
    if(!file.exists(modelDir)){
      cat(spp, "does not have georeferenciated data \n")
    }else{
      sppModel <- zipRead(modelDir, sppFile)
      sppModel[which(sppModel[]==0)] <- NA
      if(sum(!is.na(sppModel[]))==0){
        cat("No distribution for ",spp, "within native area \n")
      }else{
        writeRaster(sppModel, paste(out, "/", spp, ".asc", sep=""), overwrite=T)
        #     zipWrite(sppModel, out, paste(spp,".asc.gz",sep="") ) 
      }
    }
  }
}

#################################

for(crop in cropList){
  reclassNAs(crop)
}

########################################
# Preparing richness and gap files
########################################
require(raster)

src.dir <- paste("/curie_data2/ncastaneda/gap-analysis/gap_",crop,"/_scripts",sep="") # !!! change accordingly !!!
source(paste(src.dir,"/000.zipRead.R",sep=""))
source(paste(src.dir,"/000.zipWrite.R",sep=""))

# Project priority crops
# cropList= c("avena", "bambara", "bean", "cajanus", "cicer", "cowpea", "daucus", "eggplant", "eleusine", "faba_bean", "helianthus", "hordeum", 
#             "ipomoea", "lathyrus", "lens", "lima_bean", "malus","medicago","musa","pennisetum", "pisum", "potato", "rice", "secale", "sorghum", 
#             "triticum", "vetch")

maindir = "/curie_data2/ncastaneda/gap-analysis"
outdir = paste(maindir, "/_models4web", sep=""); if(!file.exists(outdir)){dir.create(outdir)}

# Preparing files
reclassNAs <- function(crop){
  crop_dir = paste("/curie_data2/ncastaneda/gap-analysis/gap_", crop, sep="")
  outCrop = paste(outdir, "/gap_", crop, sep=""); if(!file.exists(outCrop)) {dir.create(outCrop)}
  # Creating directories
  outgrDir = paste(outCrop, "/gap_richness", sep=""); if(!file.exists(outgrDir)){dir.create(outgrDir)} #gap richness folder
  outgsDir = paste(outCrop, "/gap_spp", sep=""); if(!file.exists(outgsDir)){dir.create(outgsDir)} #gap species folder
  outsrDir = paste(outCrop, "/species_richness", sep=""); if(!file.exists(outsrDir)){dir.create(outsrDir)} #species richness folder
  
  # Preparing gap related files
  priList <- c("HPS", "MPS", "LPS", "NFCR")
  for (p in priList){
    # Editing gap richness files
    gaprichDir <- paste(crop_dir, "/gap_richness", sep="")
    pDir <- paste(gaprichDir, "/", p, sep="")
    outpDir <- paste(outgrDir, "/", p, sep=""); if(!file.exists(outpDir)){dir.create(outpDir)}
    if(!file.exists(paste(pDir, "/gap-richness.asc.gz", sep=""))){
      cat("No richness file available for", p, "\n")
    }else{
      grich <- zipRead(pDir, "gap-richness.asc.gz")
      grich[which(grich[]==0)] <- NA
      writeRaster(grich, paste(outpDir, "/gap-richness.asc", sep=""), overwrite=T)
    }
    
    # Editing gap species files
    gapsppDir <- paste(crop_dir, "/gap_spp", sep="")
    psppDir <- paste(gapsppDir, "/", p, sep="")
    outpsppDir <- paste(outgsDir, "/", p, sep=""); if(!file.exists(outpsppDir)){dir.create(outpsppDir)}
    sppList <- list.files(psppDir)
    if(length(sppList)==0){
      cat("No gap files available for category", p, "\n")
    }else{
      for(i in 1:length(sppList)){
        gapspp = zipRead(psppDir, sppList[i])
        gapspp[which(gapspp[]==0)] <- NA
        spp <- unlist(strsplit(sppList[i], ".", fixed=T))[1]
        writeRaster(gapspp, paste(outpsppDir, "/", spp, ".asc", sep=""), overwrite = T)
      }
    } 
  }
  
  # Preparing species richness files
  spprichDir <- paste(crop_dir, "/species_richness", sep="")
  spprich <- zipRead(spprichDir, "species-richness.asc.gz")
  spprich[which(spprich[]==0)] <- NA
  writeRaster(spprich, paste(outsrDir, "/species-richness.asc", sep=""), overwrite=T)
}

#################################

for(crop in cropList){
  reclassNAs(crop)
}