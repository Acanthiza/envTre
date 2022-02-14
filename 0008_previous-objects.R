
  # load any saved objects (any .rds files)

  stuff <- fs::dir_info(out_dir
                        , recurse = TRUE
                        ) %>%
    dplyr::filter(grepl("\\.rds$", path)) %>%
    dplyr::filter(!grepl("list-length|occupancy|reporting-rate", path)) %>%
    dplyr::select(path) %>%
    dplyr::mutate(obj = map(path, rio::import)
                  , name = gsub("\\..*$", "", basename(path))
                  )

  walk2(stuff$name
        , stuff$obj
        , assign
        , envir = globalenv()
        )

  if(exists("filt_summary")) {

    walk2(filt_summary$name
          , filt_summary$obj
          , assign
          , envir = globalenv()
          )

    }

