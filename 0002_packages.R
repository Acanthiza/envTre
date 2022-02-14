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
                            , "envRaster"
                            )
                          )
                   )

  purrr::walk(packages,library,character.only=TRUE)
