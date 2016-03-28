###################### Dependencies #######################

library(parallel)

###################### Configuration #######################

work_home <- ""
work_destination <- ""
work_force <- FALSE
work_cores <- 10

###################### Internal Variables #######################

root_dirs <- NULL
file_extension <- ".kmz"
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

file_copy_ext <- function(from, to, extension){    
    files <- list.files(path=from,pattern=paste0("*",extension))    
    copy <- lapply(files,function(file){        
        from_full_path <- file.path(from,file)
        temp_dir <- gsub(extension, "", file)
        if(temp_dir == folder_richness_species || temp_dir == folder_richness_gap){
            temp_dir <- gsub("_", "-", temp_dir)
        }
        to_full_path <- file.path(to,temp_dir,file)
        file.copy(from=from_full_path, to=to_full_path, overwrite = work_force, recursive = FALSE, copy.mode = TRUE)    
    })
    
}

process_crop <- function(crop){    
    print(paste0(crop," starts"))
    force_folder(crop)
    if(file.exists(file.path(work_destination,crop))){
        # species richness        
        file_from <- file.path(work_home,crop,folder_richness_species)
        file_to <- file.path(work_destination,crop,folder_richness_species)        
        file_copy_ext(file_from,file_to,file_extension)
        # gap richness
        file_from <- file.path(work_home,crop,folder_richness_gap,"HPS")
        file_to <- file.path(work_destination,crop,folder_richness_gap,"HPS")
        file_copy_ext(file_from,file_to,file_extension)
        # models
        file_from <- file.path(work_home,crop,folder_taxon_distribution,"HPS")
        file_to <- file.path(work_destination,crop,folder_taxon_distribution,"HPS")
        file_copy_ext(file_from,file_to,file_extension)
        # gap spp
        file_from <- file.path(work_home,crop,folder_taxon_priorities,"HPS")
        file_to <- file.path(work_destination,crop,folder_taxon_priorities,"HPS")
        file_copy_ext(file_from,file_to,file_extension)
        
        print(paste0(crop," moved"))
       
    } else {
        print(paste0(crop," didn't move"))
    }
}

###################### Process #######################

crops_dirs<-dir(work_home)

#crops_processed <- mclapply(root_dirs, process_crop, mc.cores=work_cores)
crops_processed <- lapply(crops_dirs, process_crop)

###################### Export #######################

