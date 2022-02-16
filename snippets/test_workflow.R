

  # Create sample data to test envTre

  # Assumes, say, `SA_50_class.R` has been run up to, but not including, import.R

  n_sites <- 1000
  n_visits <- 3000
  n_taxas <- 100

  taxas <- paste0("sp-", 1:n_taxas)

  bio_start <- tibble::tibble(site = paste0("site-", 1:n_sites)) %>%
    dplyr::bind_cols(sf::st_coordinates(sf::st_sample(aoi
                                                      , n_sites
                                                      )
                                        ) %>%
                       tibble::as_tibble()
                     ) %>%
    add_raster_cell(aoi_grid_s
                    , .
                    , x = "X"
                    , y = "Y"
                    , crs_df = use_epsg
                    , add_xy = TRUE
                    ) %>%
    dplyr::rename(grid_s = cell) %>%
    dplyr::mutate(x_grid_s = X
                  , y_grid_s = Y
                  ) %>%
    add_raster_cell(aoi_grid_l
                    , .
                    , x = "X"
                    , y =  "Y"
                    , crs_df = use_epsg
                    ) %>%
    dplyr::rename(grid_l = cell) %>%
    dplyr::filter(!is.na(across(contains("grid")))) %>%
    dplyr::slice_sample(n = n_visits
                        , replace = TRUE
                        ) %>%
    dplyr::mutate(year = sample(reference:recent, n_visits, replace = TRUE)
                  , sr = rnbinom(n_visits
                                 , 5
                                 , mu = 3
                                 ) + 1
                  , taxa = purrr::map(sr, ~sample(taxas, .))
                  ) %>%
    tidyr:::unnest(cols = c(taxa))

  cells_geo <- bio_start %>%
    dplyr::distinct(across(contains("grid"))) %>%
    sf::st_as_sf(coords = c("x_grid_s", "y_grid_s")
                 , crs = use_epsg
                 ) %>%
    st_transform(crs = st_crs(ibra_sub)) %>%
    sf::st_intersection(ibra_sub) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(matches("grid"), matches("IBRA.*N")) %>%
    na.omit()

  bio_tidy <- bio_start %>%
    dplyr::inner_join(cells_geo) %>%
    add_time_stamp()




