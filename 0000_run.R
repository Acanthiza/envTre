

  new_run <- TRUE
  out_dir <- "out/sa_50_class/runs/2021-10-22-1607" # overwritten if newRun == TRUE

  library(magrittr)

  #---------Overall settings---------

  # testing?

    testing <- TRUE


  # What is the area of interest (AOI) for this analysis?
    aoi_polys <- "ibra_sub"
    aoi_name <- c("KAN", "FLB", "EYB", "MDD", "SVP", "NCP", "RIV") # used in output paths
    use_aoi_name <- "Ag"
    aoi_fullname <- "Flinders-Lofty Block IBRA Region"
    aoi_col <- "IBRA_REG_C"
    poly_buf <- 0


  # What is the taxonomy of interest (TOI) for this analysis?
    toi <- "class"


  # Contexts

    # Geo
    grid_s_x <- 1000
    grid_l_x <- 10000

    location_cols <- c("lat", "long")

    geo1 <- "IBRA_REG_N"
    geo2 <- "IBRA_SUB_N"
    geo3 <- "grid_l" # large cell
    geo4 <- "grid_s" # small cell

    geo_cols <- unlist(mget(ls(pattern = "geo\\d{1}")))

    # Time
    time_cols <- c("year")

    # Visit
    visit_cols <- c(time_cols, toi, unname(geo_cols))

    # Taxa
    taxa_cols <- c("original_name", "taxa", toi)



  # Maximum number of cores
    max_cores <- 14

  # Maximum allowed spatial reliability
    use_rel_dist <- 1000


  #------Import---------

  # What EPSG to use?
    use_epsg <- 3577


  #--------Clean---------

    # Effort (species richness) threshold
    extreme_sr_lo <- 0.025
    extreme_sr_hi <- 0.025

    # Absolute minimum number of sites for a taxa
    min_abs_sites <- 5

    # Earliest date
    min_year <- 1990


  #--------Report------------

    do_data_summary <- TRUE

    do_dend <- TRUE

    do_include_effort <- TRUE


    make_book <- TRUE

    make_slides <- FALSE

    make_shiny <- FALSE


  #-------Export--------

    export_results <- TRUE

    export_latest <- TRUE


  #--------Model---------

    test_chains <- 3
    test_iter <- 1000

    use_chains <- 5
    use_iter <- 3000

    # Years at which to predict (and compare change)
    test_years <- tibble::tribble(~type, ~year
                                 , "reference", 2000
                                 , "recent", 2015
                                 ) %>%
      tidyr::unnest(cols = c(year))

    reference <- test_years$year[test_years$type == "reference"]
    recent <- test_years$year[test_years$type == "recent"]


  #----------RUN---------

    run_from <- 0
    run_to <- 100

    dir() %>%
      grep("^\\d{4}_.*\\.R$",.,value=TRUE) %>%
      setNames(stringr::str_extract(.,"\\d{4}")) %>%
      `[` (names(.)[as.numeric(names(.)) <= run_to & as.numeric(names(.)) >= if(run_from == 0) 1 else run_from]) %>%
      purrr::walk(source, verbose = TRUE)

