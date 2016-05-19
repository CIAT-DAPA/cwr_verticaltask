###################### Dependencies #######################

library(parallel)

###################### Configuration #######################

work_home <- ""
work_destination <- ""
work_force <- FALSE
work_cores <- 10

###################### Internal Variables #######################


root_dirs <- NULL
file_extension <- ".KMZ"
folder_richness_gap <- "gap_richness"
folder_richness_species <-"species_richness"
folder_taxon_distribution <-"models"
folder_taxon_priorities <-"gap_spp"

###################### Functions #######################

force_folder <- function(path){
  if(work_force && !file.exists(file.path(c(work_destination,path)))){
    dir.create(file.path(work_destination,path))
  }
}

file_copy_ext <- function(from, to, extension,crop){    
  files <- list.files(path=from,pattern=paste0("*",extension))  
  
  copy <- lapply(files,function(file){        
    from_full_path <- file.path(from,file)
    temp_dir <- gsub(extension, "", file)
    if(temp_dir == folder_richness_species || temp_dir == folder_richness_gap){
      temp_dir <- gsub("_", "-", temp_dir)
    }
    to_full_path <- file.path(to,temp_dir,file)
    file.copy(from=from_full_path, to=to_full_path, overwrite = work_force, recursive = FALSE, copy.mode = TRUE) 
    if(!file.exists(file.path(to,temp_dir))){
      x<-to_full_path
      fpath<-paste0(work_destination,"/",crop)
      sname<-gsub(paste0(fpath,"/"),"",to)
      write.csv(x,paste0(fpath,"/",sname,"_",file,"_","NOT_COPIED_KMZ.RUN"),row.names = F,quote=F)
      
      cat(file.path(to,temp_dir),"|NOT MATCH|", "\n")
    }else{
#       x<-to_full_path
#       write.csv(x,paste0(to,"/",temp_dir,"/","COPIED_KMZ.RUN"))
      cat(file.path(to,temp_dir),"|MATCH|", "\n")
      
    }
  })
  
}

process_crop <- function(crop){    
  print(paste0(crop," starts to move in destination folder"))
  force_folder(crop)
  if(file.exists(file.path(work_destination,crop))){
    
    # species richness  
    
    print(paste0(crop," SPECIES_RICHNESS processing"))
    file_from <- file.path(work_home,crop,folder_richness_species)
    file_to <- file.path(work_destination,crop,folder_richness_species)        
    file_copy_ext(file_from,file_to,file_extension,crop)
    
    # gap richness
    
    print(paste0(crop," GAP_RICHNESS HPS processing"))
    file_from <- file.path(work_home,crop,folder_richness_gap,"HPS")
    file_to <- file.path(work_destination,crop,folder_richness_gap,"HPS")
    file_copy_ext(file_from,file_to,file_extension,crop)
    
    # models
    print(paste0(crop," MODELS processing"))
    file_from <- file.path(work_home,crop,folder_taxon_distribution)
    file_to <- file.path(work_destination,crop,folder_taxon_distribution)
    file_copy_ext(file_from,file_to,file_extension,crop)
    
    # gap spp
    
    print(paste0(crop," GAP_SPP HPS processing"))
    file_from <- file.path(work_home,crop,folder_taxon_priorities,"HPS")
    file_to <- file.path(work_destination,crop,folder_taxon_priorities,"HPS")
    file_copy_ext(file_from,file_to,file_extension,crop)
    
    print(paste0(crop," GAP_SPP MPS processing"))
    file_from <- file.path(work_home,crop,folder_taxon_priorities,"MPS")
    file_to <- file.path(work_destination,crop,folder_taxon_priorities,"MPS")
    file_copy_ext(file_from,file_to,file_extension,crop)
    
    print(paste0(crop," GAP_SPP LPS processing"))
    file_from <- file.path(work_home,crop,folder_taxon_priorities,"LPS")
    file_to <- file.path(work_destination,crop,folder_taxon_priorities,"LPS")
    file_copy_ext(file_from,file_to,file_extension,crop)
    
    print(paste0(crop," GAP_SPP NFCR processing"))
    file_from <- file.path(work_home,crop,folder_taxon_priorities,"NFCR")
    file_to <- file.path(work_destination,crop,folder_taxon_priorities,"NFCR")
    file_copy_ext(file_from,file_to,file_extension,crop)
    
    print(paste0(crop," have moved"))
    
  } else {
    print(paste0(crop," have not moved (CHECK IT)"))
  }
}

###################### Process #######################

crops_dirs<-dir(work_home)

#crops_processed <- mclapply(root_dirs, process_crop, mc.cores=work_cores)
crops_processed <- lapply(crops_dirs, process_crop)



################ Removing old .RUN files###############



# work_destination <- ""
# work_home <- ""
# crops_dirs<-dir(work_home)

# lapply(crops_dirs,function(crop){
# 
#   W_PATH<-paste0(work_destination,"/",crop);gc()
#  l_files<-list.files(W_PATH,pattern = ".RUN",recursive = T);gc()
#  if(length(l_files)>0){
#    cat("Processing ",as.character(crop),"\n")
#   cat("removing .RUN files for  ",as.character(crop),"\n")
#    cat("removing ",l_files,"\n")
#    file.remove(l_files);gc()  
#    
#  }else{
#    cat("Skipping ",as.character(crop),"\n")
#  }
# })
work_destination <- ""
l<-list.files(work_destination,pattern = ".RUN",recursive = T);gc()
