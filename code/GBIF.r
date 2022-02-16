
  ## function to get new data
  get_GBIF <- function(out_file, data_map, taxon_key) {

    library(rgbif)

    aoiWKT <- sf::st_as_sfc(sf::st_bbox(sa)) %>%
      sf::st_buffer(100000) %>%
      sf::st_geometry() %>%
      sf::st_transform(crs = 4326) %>%
      sf::st_as_text()

    temp <- rgbif::occ_download(pred("taxonKey", taxon_key)
                                , pred("hasCoordinate",TRUE)
                                , pred("geometry",aoiWKT)
                                , user = Sys.getenv("GBIF_user")
                                , pwd = Sys.getenv("GBIF_pwd")
                                , email = Sys.getenv("GBIF_email")
                                )

    rgbif::occ_download_wait(temp)

    meta <- rgbif::occ_download_meta(temp)

    save_loc <- fs::path(dirname(out_file)
                         , "GBIF"
                         )

    getDownload <- rgbif::occ_download_get(temp
                                           , path = save_loc
                                           , overwrite = TRUE
                                           )

    #------get gbif meta data---------

    metaGBIF <- list()

    info <- rgbif::occ_download_list(Sys.getenv("GBIF_user")
                                     , Sys.getenv("GBIF_pwd")
                                     )$results %>%
      dplyr::mutate(created = lubridate::ymd_hms(created
                                                 , tz = Sys.timezone()
                                                 )
                    ) %>%
      dplyr::filter(key == meta$key)

    metaGBIF$key <- info %>%
      dplyr::pull(key)

    metaGBIF$doi <- info %>%
      dplyr::pull(doi)

    metaGBIF$licence <- info %>%
      dplyr::pull(license)

    metaGBIF$date <- info %>%
      dplyr::pull(created)

    bib_file <- fs::path(save_loc, "gbif_data_ref.bib")

    metaGBIF$ref <- RefManageR::GetBibEntryWithDOI(metaGBIF$doi
                                                   , temp.file = bib_file
                                                   , delete.file = FALSE
                                                   )

    # Make a reference for the download
    ref <- readr::read_lines(bib_file)
    ref[1] <- paste0("@misc{GBIFRef,")
    readr::write_lines(ref, bib_file)

    #-------unzip gbif data--------

    utils::unzip(fs::path(save_loc
                          , paste0(metaGBIF$key
                                   ,".zip")
                          )
                 , exdir = fs::path(save_loc
                                    , metaGBIF$key
                                    )
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
                  dplyr::select(tidyselect::any_of(selectNames)) %>%
                  dplyr::filter(!is.na(eventDate)
                                , !is.na(decimalLatitude)
                                , !is.na(decimalLongitude)
                                , !is.na(species)
                                , species != ""
                                )
                , out_file
                )

    return(rawGBIF)

  }

