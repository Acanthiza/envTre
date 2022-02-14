#------UNSAVED SETTINGS-------

  # Ecosystem specific functions
  codes <- fs::dir_ls("code") %>%
    grep("\\/_", ., value = TRUE, invert = TRUE)

  walk(codes
       , source
       )

#----------Maps-----------

  tmap::tmap_mode("view")

  tmap::tmap_options(basemaps = c("OpenStreetMap.Mapnik"
                                  , "Esri.WorldImagery"
                                  )
                     , limits = c(facets.plot = 100)
                     )


  # State map
  outfile <- path("out","shp","sa.shp")

  if(!file.exists(outfile)) {

    st_read(path("data","shp","aust_cd66states.shp")
            , crs = 4326
            , quiet = TRUE
            ) %>%
      dplyr::filter(STE == 4) %>%
      dplyr::mutate(State = "SA") %>%
      st_transform(crs = use_epsg) %>%
      st_write(outfile)

  }

  sa <- sf::st_read(outfile)


  # LSA
  outfile <- fs::path("out","shp","LSA.shp")

  if(!file.exists(outfile)) {

    st_read(path("data","shp","LSA.shp")
            , quiet = TRUE
            ) %>%
      as_tibble() %>%
      dplyr::mutate(across(where(is.character),~gsub("Wilur|Wilu\\?",paste0("Wilu","\u1E5F"),.))) %>%
      st_as_sf() %>%
      st_transform(crs = use_epsg) %>%
      dplyr::left_join(lulsa) %>%
      sf::st_write(outfile, append = FALSE)

  }

  lsa <- sf::st_read(outfile) %>%
    dplyr::mutate(across(where(is.character),~gsub("Wilur|Wilu\\?",paste0("Wilu","\u1E5F"),.)))


  lsa_palette <- lulsa %>%
    dplyr::mutate(colour = rgb(R,G,B,A,maxColorValue = 255)) %>%
    dplyr::pull(colour, name = "REGION")

  # IBRA Sub
  outfile <- fs::path("out","shp","ibra_sub.shp")

  if(!file.exists(outfile)) {

    sf::st_read(path("data","shp","LANDSCAPE_IbraSubregionAust70.shp")) %>%
      sf::st_transform(crs = use_epsg) %>%
      sf::st_filter(sa) %>%
      sf::st_make_valid() %>%
      sf::st_write(outfile)

  }

  ibra_sub <- sf::st_read(outfile)

  lu_geo <- ibra_sub %>%
    st_set_geometry(NULL) %>%
    dplyr::distinct(IBRA_SUB_N,IBRA_SUB_C,IBRA_SUB_1,IBRA_REG_N,IBRA_REG_C,IBRA_REG_1)


  # Ag
  out_file <- fs::path("out", "shp", "zone.shp")

  if(!file.exists(out_file)) {

    sf::st_read(fs::path("data", "shp", "TOPO_AgZone2020.shp")) %>%
      sf::st_transform(crs = use_epsg) %>%
      sf::st_make_valid() %>%
      sf::st_write(out_file
                   , append = FALSE
                   )

  }

  zone <- sf::st_read(out_file)


  #---------AOI---------

  aoi <- make_aoi(polygons = get(aoi_polys)
                  , filterpolys = aoi_name
                  , filterpolyscol = aoi_col
                  , buffer = poly_buf
                  )


  #-------- grids---------

  # base grid
  aoi_grid_s <- terra::rast(ext = round(extent(aoi), -3)
                            , resolution = grid_s_x
                            , crs = CRS(paste0("+init=epsg:",use_epsg))
                            )

  aoi_grid_l <- terra::rast(ext = round(extent(aoi), -3)
                            , resolution = grid_l_x
                            , crs = CRS(paste0("+init=epsg:",use_epsg))
                            )


  #----------Parallel----------

  # Cores to use for any parallel processing
  use_cores <- if(parallel::detectCores() > max_cores) max_cores else parallel::detectCores()-1

  # Plan for any furrr functions
  future::plan(sequential)

  future::plan(multisession
               , workers = use_cores
               )

  # Set limit per core for furrr functions
  options(future.globals.maxSize = (memory.limit()*(use_cores/parallel::detectCores())*1000000)/use_cores)

  # stan options
  options(mc.cores = good_chains)
  rstan_options(auto_write = TRUE)
