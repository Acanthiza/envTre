
  ## function to get new data
  get_GBIF <- function(out_file, data_map) {
    
    library(rgbif)
    
    aoiWKT <- st_as_sfc(st_bbox(sa)) %>%
      st_buffer(100000) %>%
      st_geometry() %>%
      st_transform(crs = 4326) %>%
      st_as_text()
    
    temp <- occ_download(pred("taxonKey",6) # 6 = Plantae
                      , pred("hasCoordinate",TRUE)
                      , pred("geometry",aoiWKT)
                      , user = Sys.getenv("GBIF_user")
                      , pwd = Sys.getenv("GBIF_pwd")
                      , email = Sys.getenv("GBIF_email")
                      )
    
    occ_download_wait(temp)
    
    meta <- occ_download_meta(temp)
    
    getDownload <- occ_download_get(temp
                                    , path = path("out","gbif")
                                    , overwrite = TRUE
                                    )
  
    #------get gbif meta data---------
    
    metaGBIF <- list()
      
    info <- occ_download_list(Sys.getenv("GBIF_user"),Sys.getenv("GBIF_pwd"))$results %>%
      dplyr::mutate(created = ymd_hms(created,tz=Sys.timezone())) %>%
      dplyr::filter(created == max(created))
    
    metaGBIF$key <- info %>%
      dplyr::pull(key)
    
    metaGBIF$doi <- info %>%
      dplyr::pull(doi)
    
    metaGBIF$licence <- info %>%
      dplyr::pull(license)
    
    metaGBIF$date <- info %>%
      dplyr::pull(created)
    
    metaGBIF$ref <- get_bib(metaGBIF$doi,outfile = "gbifDataRef.bib")
    
    # Make a reference for the download
    ref <- read_lines("gbifDataRef.bib")
    ref[1] <- paste0("@misc{GBIFRef,")
    write_lines(ref,"gbifDataRef.bib")
  
    #-------unzip gbif data--------
    
    unzip(path("out","gbif",paste0(metaGBIF$key,".zip"))
          , exdir = path("out","gbif",metaGBIF$key)
          )
      
    rawGBIF <- data.table::fread(path("out","ds","GBIF",metaGBIF$key,"occurrence.txt")) %>%
      dtplyr::lazy_dt() %>%
      dplyr::filter(is.na(occurrenceStatus) | occurrenceStatus != "ABSENT") %>%
      dplyr::filter(is.na(individualCount) | individualCount > 0) %>%
      dplyr::filter(is.na(organismQuantity) | organismQuantity > 0) %>%
      tibble::as_tibble()
    
    # What names to grab before writing results?
    selectNames <- data_map %>%
      dplyr::filter(data_name == "GBIF") %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit()
      
    rio::export(rawGBIF %>%
                  dplyr::select(tidyselect::any_of(selectNames))
                  ,out_file
                  )
    
    return(rawGBIF)
    
  }
  
