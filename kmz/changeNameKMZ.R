# CSOSA Modified of a H. Achicanoy script
# CIAT, 2016





data<-read.csv("",header=T)


### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
### Rename folders and files in KML_FOLDER
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

#main_dir = ""
main_dir = ""

files_ch = data
files_ch = files_ch[order(files_ch$crop),]
rownames(files_ch) = 1:nrow(files_ch)

crops = as.character(files_ch$crop)
crops = unique(crops) # crops = "ipomoea"

#crops<-"avena"
# Loop for crops
for(i in 1:length(crops)){
  
  #options(warn=-1)
  
  # Define taxa to change
  taxa  = files_ch$taxon_id[files_ch$crop==crops[i]] # old ID
  taxa  = as.character(taxa)
  #taxa  = as.numeric(taxa)
  ntaxa = files_ch$new_taxon_id[files_ch$crop==crops[i]] # new ID
  
  ntaxa = as.character(ntaxa)
 # ntaxa = as.numeric(ntaxa)
  
  if(!any(is.na(ntaxa))){ # Verify if new_taxon_id exists
    
    fpcat = files_ch$FPCAT[files_ch$crop==crops[i]] #  FPS category
    fpcat = as.character(fpcat)
    
    # Define occurrence, models and gap spp directories for each crop
    mod_dir = paste(main_dir,"/gap_",crops[i],"/models",sep="")
    gsp_dir = paste(main_dir,"/gap_",crops[i],"/gap_spp",sep="")
    
    # Loop for taxa within each crop
    
    for(j in 1:length(taxa)){
      
    
      # Models
      
      models_im = paste(mod_dir,"/",taxa[j],".KMZ",sep="")
      if(file.exists(models_im)){
        cat("Change models  name for", crops[i], "changing the taxon number:",taxa[j],"\n")
        file.rename(from=paste(mod_dir,"/",taxa[j],".KMZ",sep=""),to=paste(mod_dir,"/",ntaxa[j],".KMZ",sep=""))
        cat("Done! \n")
      } else {
        cat("The model name for", crops[i], "taxon number:",taxa[j],"was changed manually. \n")
      }
      rm(models_im)
      
      
      
      # Gap spp test image
      gpspp_im = paste(gsp_dir,"/",fpcat[j],"/",taxa[j],".KMZ",sep="")
      if(file.exists(gpspp_im)){
        cat("Change gap_spp KMZ file for", fpcat[j], "category in", crops[i], "changing the taxon number:",taxa[j],"\n")
        file.rename(from=paste(gsp_dir,"/",fpcat[j],"/",taxa[j],".KMZ",sep=""), to=paste(gsp_dir,"/",fpcat[j],"/",ntaxa[j],".KMZ",sep=""))
        cat("Done! \n")
      } else {
        cat("The gap_spp KMZ file for", fpcat[j], "category in", crops[i], "taxon number:",taxa[j],"was changed manually. \n")
      }
      rm(gpspp_im)
      
      
    }
      # Rename models folder
     
  } else {
    
    cat("For the crop:",crops[i],", exists taxa without new taxon ID assigned \n")
    cat("Please check again, before to run the code. \n")
    
  }
 } 

