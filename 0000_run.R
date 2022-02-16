

  library(magrittr)

  #---------Directories--------

  # Clean empty folders
  files <- 1

  while(files > 0) {

    empty <- fs::dir_info("out"
                          , recurse = TRUE
                          ) %>%
      dplyr::filter(type == "directory") %>%
      dplyr::select(path) %>%
      dplyr::mutate(files = purrr::map_int(path, ~length(fs::dir_ls(.)))) %>%
      dplyr::filter(files < 1) %>%
      dplyr::mutate(nchar = nchar(path)) %>%
      dplyr::arrange(desc(nchar)) %>%
      dplyr::pull(path)

    unlink(empty, recursive = TRUE)

    files <- length(empty)

  }

  # Out directories
  out_aoi <- fs::path("out"
                      ,paste0(aoi_name
                              , "_"
                              , poly_buf/1000
                              , "_"
                              , toi
                              )
                      )

  out_runs <- fs::path(out_aoi,"runs")

  # Reload a specific run
  if(is.character(new_run)) {

    out_dir <- fs::path("out"
                        ,paste0(aoi_name
                                , "_"
                                , poly_buf/1000
                                , "_"
                                , toi
                                )
                        , "runs"
                        , new_run
                        )

  }

  # Get the last run
  if(isFALSE(new_run)) {

    run_tib <- tibble::tibble(runs = fs::dir_ls(out_runs)) %>%
      dplyr::mutate(run = basename(runs)) %>%
      dplyr::filter(run != "latest")

    if(nrow(run_tib) < 1) {

      out_dir <- fs::path(out_runs
                          , paste0(format(Sys.time(),"%Y-%m-%d-%H%M"))
                          )


    } else {

      out_dir <- run_tib %>%
        dplyr::arrange(desc(run)) %>%
        dplyr::slice(1) %>%
        dplyr::pull(run) %>%
        fs::path(out_runs, .)

    }

  }

  # New run
  if(isTRUE(new_run)) {

    out_dir <- fs::path(out_runs
                        , format(Sys.time(),"%Y-%m-%d-%H%M")
                        )

  }

  #------Settings file---------

  settings_file <- fs::path(out_dir, paste0(basename(out_dir), "_setup.rda"))


  #---------Overall settings---------

  # Contexts

  # What EPSG to use?
  use_epsg <- 3577
  latlon_epsg <- 4283

  # Geo
  geo1 <- "IBRA_REG_N"
  geo2 <- "IBRA_SUB_N"
  geo3 <- "grid_l" # large cell
  geo4 <- "grid_s" # small cell

  geo_cols <- c(unlist(mget(ls(pattern = "geo\\d{1}"))))

  grid_s_x <- 1000
  grid_l_x <- 10000

  location_cols <- c("lat", "long")

  # Time
  time_cols <- c("year")


  # Visit
  visit_cols <- c(time_cols, toi, unname(geo_cols), location_cols)


  # Taxa
  taxa_cols <- c("original_name", "taxa", "common", toi)


  # Cooccur

    # within
    cooccur_within <- c(toi, geo1, geo2)

    # at
    cooccur_at <- c(toi, geo3)


  # Context cols
  context_rr <- c(time_cols, toi, unname(geo_cols[-length(geo_cols)]))

  context_ll <- c(time_cols, toi, unname(geo_cols[-length(geo_cols)]))

  context_occ <- c(time_cols, toi, unname(geo_cols))


  # All
  all_contexts <- sort(unique(c(visit_cols, taxa_cols)))


  # Maximum number of cores
  max_cores <- 14


  #----------RUN---------

  if(!exists("run_from")) run_from <- 0
  if(!exists("run_to")) run_to <- 100

  dir() %>%
    grep("^\\d{4}_.*\\.R$",.,value=TRUE) %>%
    setNames(stringr::str_extract(.,"\\d{4}")) %>%
    `[` (names(.)[as.numeric(names(.)) <= run_to & as.numeric(names(.)) >= if(run_from == 0) 1 else run_from]) %>%
    purrr::walk(source
                , verbose = TRUE
                )

