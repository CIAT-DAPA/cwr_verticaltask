###################### Dependencies #######################
library(raster)
library(shapefiles)

###################### Parameters #######################
# Full path where are located the original raster
src.source <- ""
# Full path where will located the crop raster files
src.destination <- ""
# Full path of the shape file 
src.shape <- ""
# Name of the files that are going to crop
src.rasters <- c("", "")
# Name of country to crop
# If you need check names of country try with work.shp@data$NAME_0 after load the shape file
src.country <- "South Sudan"

###################### Funtions #######################

# Function to write a raster object into a raster file
zipWrite <- function(rs, path, fname) {
    infile <- paste(path, "/", fname, sep="")
    
    #automatically detect the file type (zip or gzip)
    
    splt <- unlist(strsplit(fname, ".", fixed=T))
    ext <- splt[length(splt)]
    
    infname <- substring(fname, 1, nchar(fname)-nchar(ext)-1)
    
    if (ext == "gz") {
        zz <- gzfile(infile, "w")
    } else {
        stop("Not supported file type")
    }
    
    #Getting the properties to write
    nc <- ncol(rs)
    nr <- nrow(rs)
    xll <- rs@extent@xmin
    yll <- rs@extent@ymin
    cz <- (rs@extent@xmax - rs@extent@xmin) / nc
    nas <- -9999
    
    #Writing header
    cat("ncols         ", as.character(nc), '\n', sep="", file=zz)
    cat("nrows         ", as.character(nr), '\n', sep="", file=zz)
    cat("xllcorner     ", as.character(xll), '\n', sep="", file=zz)
    cat("yllcorner     ", as.character(yll), '\n', sep="", file=zz)
    cat("cellsize      ", as.character(cz), '\n', sep="", file=zz)
    cat("NODATA_value  ", as.character(nas), '\n', sep="", file=zz)
    
    #Change NA to nas string
    rs[is.na(rs)] <- -9999
    
    #Get the values
    printValues <- matrix(rs[], ncol=nc, nrow=nr, byrow=F)
    lastRow <- rep("\n", nc)
    printValues <- rbind(printValues, lastRow)
    
    #Print the values
    cat(printValues, file=zz)
    
    #Close the connection
    close(zz)
    
    return(infile)
}

# FUnction to crop a raster file in other
work.crop <- function(item){
    print("loading raster")
    work.file <- raster(paste0(src.source,src.rasters[item]))
    print("crop raster")
    work.obj <- crop(work.file,work.shp)
    work.obj <- mask(work.file,work.shp)
    print("write raster")
    x <- zipWrite(work.obj , src.destination, paste0(src.rasters[item],".gz"))
    
}

###################### Process #######################

work.shp <- shapefile(src.shape)
work.shp<-work.shp[which(work.shp@data$NAME_0==src.country ),]

lapply(1:length(src.rasters),work.crop)