
  #---------Packages---------

  packages <- sort(unique(c("dplyr"
                            , "tidyr"
                            , "purrr"
                            , "forcats"
                            , "ggplot2"
                            , "tibble"
                            , "stringr"
                            , "readr"
                            , "fs"
                            , "sf"
                            , "dbplyr"
                            , "DBI"
                            , "lubridate"
                            , "raster"
                            , "rgbif"
                            , "rasterVis"
                            , "rstan"
                            , "rstanarm"

                            , "furrr"
                            , "snowfall"

                            # env
                            , "envFunc"
                            , "envImport"
                            , "envClean"
                            , "envTrend"
                            )
                          )
                   )

  purrr::walk(packages,library,character.only=TRUE)


  # Ecosystem specific functions
  codes <- fs::dir_ls("R")

  walk(codes
       , source
       )

  #----------Project------------

  # no scientific notation for numbers
  options(scipen = 999)

  # Out directories
  out_aoi <- path("out"
                  ,paste0(aoi_name
                          , "_"
                          , poly_buf/1000
                          , "_"
                          , toi
                          )
                  )

  out_runs <- path(out_aoi,"runs")
  out_dir <- path(out_runs,paste0(format(Sys.time(),"%Y-%m-%d-%H%M")))


  # Necessary folders
  folders <- c("out"
               , "data"
               , "out/ds"
               , "out/shp"
               , out_aoi
               , out_runs
               , out_dir
               )

  walk(folders,dir.create)


  #----------Lookups--------

  # LSAs
  lulsa <- tribble(
    ~LSA, ~REGION, ~LSARegion, ~Zone, ~System, ~R, ~G, ~B, ~A
    ,"HF","Hills and Fleurieu","Hills and Fleurieu",	"Agricultural",	"High-rainfall", 89, 23, 138, 255
    ,"AW","Alinytjara Wilurara","Alinytjara Wilurara",	"Arid",	"Arid",	191,	54,	44,	255
    ,"EP","Eyre Peninsula","Eyre Peninsula",	"Agricultural",	"Low-rainfall",	86,	156,	190,	255
    ,"KI","Kangaroo Island","Kangaroo Island",	"Agricultural",	"High-rainfall",	0,	137,	152,	255
    ,"NY","Northern and Yorke","Northern and Yorke",	"Agricultural",	"Low-rainfall",	248,	185,	44,	255
    ,"SAAL","South Australian Arid Lands","South Australian Arid Lands",	"Arid",	"Arid",	213,	94,	0,	255
    ,"MR","Murraylands and Riverland","Murraylands and Riverland",	"Agricultural",	"Low-rainfall",	0,	132,	197,	255
    ,"LC","Limestone Coast","Limestone Coast","Agricultural",	"High-rainfall",	102,	181,	98,	255
    ,"GA","Green Adelaide","Green Adelaide","Urban","High-rainfall",	34,	139,34,	255
    ) %>%
    dplyr::mutate(across(where(is.character),~gsub("Wilur|Wilu\\?",paste0("Wilu","\u1E5F"),.)))


  lulikelihood <- tribble(
    ~likelihood, ~maxVal
    , "Exceptionally unlikely", 0.01
    , "Extremely unlikely", 0.05
    , "Very unlikely", 0.1
    , "Unlikely", 1/3
    , "About as likely as not", 2/3
    , "Likely", 0.9
    , "Very likely", 0.95
    , "Extremely likely", 0.99
    , "Virtually certain", 1
    ) %>%
    dplyr::mutate(likelihood = fct_inorder(likelihood)
                  , range = cut(maxVal
                                , breaks = c(0,.$maxVal)
                                )
                  )


  #----------Maps-----------

  # State map
  out_file <- path("out","shp","sa.shp")

  if(!file.exists(out_file)) {

    st_read(path("data","shp","aust_cd66states.shp")
            , crs = 4326
            , quiet = TRUE
            ) %>%
      dplyr::filter(STE == 4) %>%
      dplyr::mutate(State = "SA") %>%
      st_transform(crs = 3577) %>%
      st_write(out_file
               , append = FALSE
               )

  }

  sa <- sf::st_read(out_file)


  # LSA
  out_file <- path("out","shp","LSA.shp")

  if(!file.exists(out_file)) {

    st_read(path("data","shp","LSA.shp")
                 , quiet = TRUE
                 ) %>%
      as_tibble() %>%
      dplyr::mutate(across(where(is.character),~gsub("Wilur|Wilu\\?",paste0("Wilu","\u1E5F"),.))) %>%
      st_as_sf() %>%
      st_transform(crs = 3577) %>%
      dplyr::left_join(lulsa) %>%
      sf::st_write(out_file
                   , append = FALSE
                   )

  }

  lsa <- sf::st_read(out_file) %>%
    dplyr::mutate(across(where(is.character),~gsub("Wilur|Wilu\\?",paste0("Wilu","\u1E5F"),.)))


  lsa_palette <- lulsa %>%
    dplyr::mutate(colour = rgb(R,G,B,A,maxColorValue = 255)) %>%
    dplyr::pull(colour, name = "REGION")

  # IBRA Sub
  out_file <- path("out","shp","ibra_sub.shp")

  if(!file.exists(out_file)) {

    st_read(path("data","shp","LANDSCAPE_IbraSubregionAust70.shp")) %>%
      sf::st_transform(crs = 3577) %>%
      sf::st_filter(sa) %>%
      sf::st_make_valid() %>%
      sf::st_write(out_file)

  }

  ibra_sub <- sf::st_read(out_file)


  #---------AOI---------

  aoi <- make_aoi(polygons = get(aoi_polys)
                  , filterpolys = aoi_name
                  , filterpolyscol = aoi_col
                  , buffer = poly_buf
                  )

  # base grid
  aoi_grid_s <- raster(ext = round(extent(aoi), -3)
                      , resolution = grid_s_x
                      , crs = CRS(paste0("+init=epsg:",use_epsg))
                      )

  aoi_grid_l <- raster(ext = extent(aoi_grid_s)
                      , resolution = grid_l_x
                      , crs = CRS(paste0("+init=epsg:",use_epsg))
                      )


  #--------Import--------

  # Flora import data map
  data_map_bio <- tibble::tibble(
    data_name = c("ALIS", "BCM", "BDBSA", "EGIS", "NVB", "PTP", "TERN", "GBIF"),
    order = c(4,5,1,2,7,6,3,8),
    days = c(60, 60, 21, 21, 1000, 365, 60, 90),
    site = c("SITENUMBER", "SITE_ID", "PATCHID", "FLORACODE", "path", "PlantDataID", "site_unique", "gbifID"),
    date = c("SurveyDate", "ASSESSMENT_DATE", "VISITDATE", "SIGHTINGDATE", "date", "date", "visit_start_date", "eventDate"),
    lat = c("LATITUDE", "LATITUDE", "LATITUDE", "LATITUDE", "lat", "LATITUDE", "latitude", "decimalLatitude"),
    long = c("LONGITUDE", "LONGITUDE", "LONGITUDE", "LONGITUDE", "lon", "LONGITUDE", "longitude", "decimalLongitude"),
    original_name = c("SPECIES", "SPECIES", "SPECIES", "SPECIES", "Spp", "SPECIES", "herbarium_determination", "scientificName"),
    nsx = c("NSXCODE", "NSXCODE", "NSXCODE", "NSXCODE", NA, "NSXCODE", NA, "organismID"),
    number = c(NA, NA, "NUMOBSERVED","NUMOBSERVED", NA, NA, NA, NA),
    survey_nr = c(NA, NA, "SURVEYNR", "SURVEYNR", NA, NA, NA, NA),
    survey = c(NA, NA, "SURVEYNAME", "SURVEYNAME", NA, NA, NA, NA),
    ind = c("ISINDIGENOUS", "ISINDIGENOUS", "ISINDIGENOUSFLAG", "ISINDIGENOUSFLAG", NA, "Native_Introduced_original", NA, NA),
    rel_dist = c(NA, NA, "rel_dist", "rel_dist", NA, NA, NA, "coordinateUncertaintyInMeters"),
    desc = c("Arid lands information systems"
             , "Bushland condition monitoring"
             , "Biological databases of South Australia"
             , "Flora 'Supertable' from the environmental databases of South Australia"
             , "DEW Native Vegetation Branch"
             , "Paddock tree project"
             , "Terrestrial ecosystem network"
             , "Global biodiversity information facility"
             )
    ) %>%
    dplyr::mutate(data_name = fct_reorder(data_name, order)) %>%
    dplyr::filter(data_name == "BDBSA")


  #----------Clean----------

  # SPECIES filter
  species_filt <- c("lichen", "moss", "another species", "not naturalised in SA"
                     , "unidentified", "unverified", "dead", "annual forb"
                     , "annual grass", "annual tussock grass", "no id"
                    , "bold:"
                     )

  num_filt <- c("0"
                , "Not seen"
                )


  # stan settings
  good_chains <- 5
  good_iter <- 2000


  #------Model------

  quant_probs <- c(0.05, 0.5, 0.95)


#----------Parallel----------

  # Cores to use for any parallel processing
  use_cores <- if(parallel::detectCores() > max_cores) max_cores else parallel::detectCores()-1

  # Plan for any furrr functions
  future::plan(multisession
               , workers = use_cores
               )

  # stan options
  options(mc.cores = good_chains)
  rstan_options(auto_write = TRUE)

  # Set limit per core for furrr functions
  options(future.globals.maxSize = (memory.limit()*(use_cores/parallel::detectCores())*1000000)/use_cores)

  # raster processing options
  mem_lim <- memory.limit()*(use_cores/parallel::detectCores()) * 1000000

  rasterOptions(maxmemory = mem_lim)
  rasterOptions(chunksize = mem_lim/use_cores)
