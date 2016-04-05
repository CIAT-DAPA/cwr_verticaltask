###################### Dependencies #######################

library(RMySQL)
library(parallel)

###################### Configuration #######################

# The source file must have one column called taxon

# Varibles Global
work_file_taxon <- ""
work_folder <- ""
work_destination <- ""

# Variables Database Connection
db_host <- ""
db_user <- ""
db_password <- ""
db_name <- ""

# Parameters
work_cores <- 10
work_force <- TRUE
work_sep_field <- ","

###################### Internal Variables #######################

# Constants
crop_prefix <- "gap_"

# Database
db_cnn <- NULL
db_base_query <- NULL
db_rs_init <- NULL
db_data_init <- NULL
db_query <- NULL

# Export
taxonomy <- NULL
taxonomy_values <- NULL
taxonomy_query_1 <- "Select distinct s1.taxon_id as id, s1.scientific_name as taxon,c.crop_code as crop,FPCAT as fpcat FROM species as s1 inner join cwr_occurrences_species as c on s1.valid_taxon_id = c.valid_taxon_id inner join species as s2 on s1.taxon_id = s2.valid_taxon_id where s2.scientific_name in ("
taxonomy_query_2 <- ") and (!isnull(fpcat) and fpcat !='') and (!isnull(crop_code) and crop_code!='') order by crop"

# Work
tx <- NULL
cp <- NULL
fp <- NULL
id <- NULL
model_folder <- NULL
model_file <- NULL
to_path <- NULL
spp_file <- NULL
richness_folder <- NULL
species_file <- NULL
###################### Functions #######################
force_folder <- function(path){
    if(work_force && !file.exists(file.path(work_destination,path))){
        dir.create(file.path(work_destination,path))
        dir.create(file.path(work_destination,path,"models"))
        dir.create(file.path(work_destination,path, "gap_spp"))
        dir.create(file.path(work_destination,path, "gap_richness"))
        dir.create(file.path(work_destination,path, "species_richness"))
    }
}

process_taxonomy <- function(index){  
    tx <- gsub("'","",gsub(" ","_",db_data_init$taxon[index]))
    cp <- db_data_init$crop[index]
    fp <- db_data_init$fpcat[index]
    id <- db_data_init$id[index]
    
    print(paste0(tx," starts"))
    force_folder(cp)
    if(file.exists(file.path(work_destination,cp))){
        
        # Read file priorities
        priorities_file <- file.path(work_folder, paste0(crop_prefix,cp), "priorities", "priorities.csv")
        priorities <- read.csv(file=priorities_file,head=TRUE,sep=work_sep_field)
        
        # Get taxon information
        priorities_information <- priorities[priorities$TAXON==id,]
        if(nrow(priorities_information) < 1){
            priorities_information <- priorities[priorities$TAXON==tx,]
        }
        
        # Get model
        if(priorities_information$IS_VALID == 1){
            model_folder <-  file.path(work_folder, paste0(crop_prefix,cp), "maxent_modeling", "models", id, "projections")
            if(file.exists(model_folder)){
                
                model_file <- paste0(id,"_worldclim2_5_EMN_PA.asc.gz")          
                # change name of the file to taxon
                to_path <- file.path(work_destination, cp, "models", paste0(tx,"_worldclim2_5_EMN_PA.asc.gz")) 
                
            }else if(file.exists(file.path(work_folder, paste0(crop_prefix,cp), "maxent_modeling", "models", tx, "projections"))){
                
                # change the directory
                model_folder <-  file.path(work_folder, paste0(crop_prefix,cp), "maxent_modeling", "models", tx, "projections")
                model_file <- paste0(tx,"_worldclim2_5_EMN_PA.asc.gz")                
                to_path <- file.path(work_destination, cp, "models", model_file)
            }
        } else if(priorities_information$IS_VALID == 0){
            
            model_folder <-  file.path(work_folder, paste0(crop_prefix,cp), "samples_calculations", id)
            model_file <-"samples-buffer-na.asc.gz"
            
            if(file.exists(model_folder)){
                # change name of the file to taxon
                to_path <- file.path(work_destination, cp, "models", paste0(tx,"_samples-buffer-na.asc.gz"))
                
            }else if(file.exists(file.path(work_folder, paste0(crop_prefix,cp), "samples_calculations", tx))){
                # change the directory
                model_folder <-  file.path(work_folder, paste0(crop_prefix,cp), "samples_calculations", tx)
                to_path <- file.path(work_destination, cp, "models", paste0(tx,"_samples-buffer-na.asc.gz"))               
            }
        }
        # Copy model
        file.copy(from= paste0(model_folder,"/", model_file), to=to_path, overwrite = work_force, recursive = FALSE, copy.mode = TRUE)
        print("model copied")
        
        # Get gap_spp
        spp_file <-  file.path(work_folder, paste0(crop_prefix,cp), "gap_spp", priorities_information$FPCAT, paste0(id,".asc.gz"))
        if(file.exists(spp_file)){          
            # change name of the file to taxon
            to_path <- file.path(work_destination, cp, "gap_spp", paste0(tx,".asc.gz")) 
            
        }else if(file.exists(file.path(work_folder, paste0(crop_prefix,cp), "gap_spp", priorities_information$FPCAT, paste0(tx,".asc.gz")))){
            spp_file <- file.path(work_folder, paste0(crop_prefix,cp), "gap_spp", priorities_information$FPCAT, paste0(tx,".asc.gz"))                          
            to_path <- file.path(work_destination, cp, "gap_spp", paste0(tx,".asc.gz")) 
        }
        # Copy Spp
        file.copy(from= spp_file, to=to_path, overwrite = work_force, recursive = FALSE, copy.mode = TRUE)
        print("spp copied")
        
        # Get gap richness
        richness_folder <-  file.path(work_folder, paste0(crop_prefix,cp), "gap_richness")
        to_path <- file.path(work_destination, cp)
        file.copy(from=richness_folder, to=to_path, overwrite = work_force, recursive = TRUE, copy.mode = TRUE)
        print("gap copied")
        
        # Get species richness
        species_file <-  file.path(work_folder, paste0(crop_prefix,cp), "species_richness", "species-richness.asc.gz")
        to_path <- file.path(work_destination, cp, "species_richness","species-richness.asc.gz")
        file.copy(from=species_file, to=to_path, overwrite = work_force, recursive = FALSE, copy.mode = TRUE)
        print("species copied")
        
    } else {
        print(paste0(tx," didn't copy"))
    }
}

###################### Process #######################

print("Start process")

# Read taxonomy request
taxonomy <- read.csv(file=work_file_taxon,head=TRUE,sep=work_sep_field)
taxonomy$taxon <- paste("'",taxonomy$taxon,"'", sep="")
taxonomy_values <- paste(taxonomy$taxon, collapse=",")
print("Read file")

# Connection Database
db_cnn <- dbConnect(MySQL(),user = db_user,password = db_password,host = db_host,dbname=db_name)
print("Connected to database")
db_query <- paste0(taxonomy_query_1,taxonomy_values,taxonomy_query_2)
#print (db_query)
db_rs_init <- dbSendQuery(db_cnn, db_query)  
db_data_init <- fetch(db_rs_init, n=-1) 
print("Attached rows")

###################### Export #######################

#taxonomy_processed <- mclapply(db_data_init, process_taxonomy, mc.cores=work_cores)
taxonomy_processed <- lapply(seq(1,nrow(db_data_init)), process_taxonomy)

print("End process")
