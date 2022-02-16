

  #--------Polygons-----------

  poly_aois <- ls() %>%
    tibble::enframe(name = NULL, value = "obj_name") %>%
    dplyr::mutate(obj = map(obj_name,get)
                  , cls = map(obj,class)
                  ) %>%
    tidyr::unnest(cols = c(cls)) %>%
    dplyr::filter(cls == "sf"
                  , !obj_name %in% c("aoi","sa")
                  ) %>%
    dplyr::mutate(poly_aoi = map(obj,st_filter,aoi)
                  , poly_aoi_name = paste0(obj_name,"_aoi")
                  )

  walk2(poly_aois$poly_aoi_name
        , poly_aois$poly_aoi
        , assign,envir = globalenv()
        )


  #---------make taxonomy--------

  # generates two dataframes: lutaxa and taxa_taxonomy

  make_taxa_taxonomy(df = bio_all
                     , taxa_col = "original_name"
                     , ind_col = "ind"
                     , save_luGBIF = "out/luGBIF.feather"
                     , poor_filt = species_filt
                     , king = "Animalia"
                     , get_common = TRUE
                     )


  out_file <- fs::path(out_dir, "filt_summary.rds")

  if(!file.exists(out_file)) {

    #--------Filter AOI-------

    bio_aoi <- filter_aoi(bio_all
                           , use_aoi = aoi
                           , crs_aoi = st_crs(aoi)
                           ) %>%
      add_time_stamp()


    bio_time <- bio_aoi %>%
      dplyr::filter(year > min_year) %>%
      add_time_stamp()


    #---------Get cell numbers----------

    # This has the side effect of filtering sites not on the aoi_raster

    bio_cell <- add_raster_cell(aoi_grid_s
                                , bio_time
                                , add_xy = TRUE
                                ) %>%
      dplyr::rename(grid_s = cell) %>%
      dplyr::mutate(long_s = long
                    , lat_s = lat
                    ) %>%
      add_raster_cell(aoi_grid_l
                      , .
                      , x = "long"
                      , y =  "lat"
                      ) %>%
      dplyr::rename(grid_l = cell
                    , long = long_s
                    , lat = lat_s
                    ) %>%
      dplyr::filter(!is.na(across(contains("grid")))) %>%
      add_time_stamp()


    #---------Spatial reliability--------

    bio_rel <- filter_spat_rel(bio_cell
                                , dist_col = "rel_dist"
                                , context = visit_cols
                                , dist = use_rel_dist
                                , over_ride = list(survey_nr = include_survey_nr)
                                ) %>%
      dplyr::filter(is.na(rel_dist) | rel_dist < use_extra_rel_dist) %>%
      add_time_stamp()


    #-------Taxonomy---------

    # This generates a single row per taxa*Visit*any extracols
    # If lutaxa does not exist, make_taxa_taxonomy is run.

    bio_taxa <- filter_taxa(bio_rel
                            , taxa_col = "original_name"
                            , context = visit_cols
                            , extra_cols = NULL
                            , target_rank = rank_to_target
                            , poor_filt = species_filt
                            , save_luGBIF = fs::path("out","luGBIF.feather")
                            , king = "Animalia"
                            , get_common = TRUE
                            ) %>%
      dplyr::filter(grepl(paste0(filt_toi, collapse = "|"), !!ensym(toi))) %>%
      add_time_stamp()


    #---------Singletons---------

    bio_single <- filter_counts(bio_taxa
                                , context = visit_cols
                                , thresh = 1
                                ) %>%
      add_time_stamp()


    #---------Geo context---------

    cells_geo <- bio_single %>%
      dplyr::distinct(lat, long, across(contains("grid"))) %>%
      sf::st_as_sf(coords = c("long", "lat")
                   , crs = 4283
                   ) %>%
      st_transform(crs = st_crs(ibra_sub_aoi)) %>%
      sf::st_intersection(ibra_sub_aoi) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::select(matches("grid"), matches("IBRA.*N")) %>%
      na.omit()

    bio_geo <- bio_single %>%
      dplyr::inner_join(cells_geo) %>%
      add_time_stamp()


    #-------Clean contexts--------

    bio_NA <- bio_geo %>%
      dplyr::filter(across(any_of(all_contexts)
                           , ~ !is.na(.x)
                           )
                    ) %>%
      add_time_stamp()


    #----------Effort---------

    out_file <- fs::path(out_dir, "effort_mod.rds")

    if(!file.exists(out_file)) {

      effort_mod <- make_effort_mod(bio_NA
                                    , context = visit_cols
                                    , cat_cols = c(geo2, toi)
                                    , iter = good_iter
                                    , chains = good_chains
                                    , threshold_lo = extreme_sr_lo
                                    , threshold_hi = extreme_sr_hi
                                    )

      rio::export(effort_mod
                  , out_file
                  )

    }

    bio_effort <- bio_geo %>%
      dplyr::inner_join(effort_mod$mod_cell_result) %>%
      dplyr::filter(keep_hi
                    , keep_lo
                    ) %>%
      add_time_stamp()


    #---------Summary----------

    bio_tidy <- bio_effort %>%
      filter_counts(context = visit_cols) %>%
      dplyr::distinct(taxa
                      , across(any_of(visit_cols))
                      ) %>%
      add_time_stamp()


    out_file <- fs::path(out_dir, "cells_tidy.rds")

    if(!file.exists(out_file)) {

      cells_tidy <- bio_tidy %>%
        dplyr::count(across(all_of(visit_cols))
                     , name = "sr"
                     )

      rio::export(cells_tidy
                  , out_file
                  )

      data_name_tidy <- bio_cell %>%
        dplyr::distinct(across(any_of(visit_cols))
                        , data_name
                        , survey_nr
                        , survey
                        ) %>%
        dplyr::left_join(cells_tidy %>%
                           dplyr::mutate(used = TRUE)
                         ) %>%
        dplyr::left_join(data_map_bio %>%
                           dplyr::select(data_name,order)
                         ) %>%
        dplyr::group_by(across(contains("grid"))) %>%
        dplyr::filter(order == min(order)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(used = if_else(is.na(used),FALSE,TRUE)) %>%
        dplyr::count(data_name,survey_nr,survey,used) %>%
        tidyr::pivot_wider(names_from = "used",values_from = "n")

      rio::export(data_name_tidy
                  , fs::path(out_dir, "data_name_tidy.rds")
                  )

    }

    #--------Filtering summary--------

    out_file <- fs::path(out_dir, "filt_summary.rds")

    filt_summary <- ls(pattern = "bio_") %>%
      grep("combine|all|wide", ., value = TRUE, invert = TRUE) %>%
      tibble::enframe(name = NULL, value = "name") %>%
      dplyr::mutate(obj = map(name,get)
                    , has_stamp = map_lgl(obj,~"ctime" %in% names(attributes(.)))
                    ) %>%
      dplyr::filter(has_stamp) %>%
      dplyr::mutate(nrow = map_dbl(obj, nrow)) %>%
      dplyr::mutate(ctime = map(obj,attr,"ctime")) %>%
      tidyr::unnest(cols = c(ctime)) %>%
      dplyr::arrange(desc(nrow),ctime)

    rio::export(filt_summary
                , out_file
                )

  }


  #-------Cooccur---------

  out_file <- fs::path(out_dir, "bio_cooccur.rds")

  if(!file.exists(out_file)) {

    bio_cooccur <- bio_tidy %>%
      tidyr::nest(data = -c(any_of(cooccur_within))) %>%

      #dplyr::sample_n(5) %>% # TESTING

      dplyr::mutate(n_lists = map_dbl(data
                                      , . %>%
                                        dplyr::distinct(across(any_of(cooccur_at))) %>%
                                        nrow()
                                       )
                    , n_taxa = map_dbl(data,~n_distinct(.$taxa))
                    ) %>%
      # more than x*min(min_abs_sites) lists within a context
      dplyr::filter(n_lists > 2*min(min_abs_sites)) %>%

      # at least x*min(min_abs_sites) distinct Taxa
      dplyr::filter(n_taxa > 2*min(min_abs_sites)) %>%

      # at least x taxa with more than three records
      dplyr::filter(map_dbl(data,~sum(table(.$taxa) < 3)) > 3) %>%

      dplyr::mutate(cooccur = furrr::future_map(data
                                                , make_cooccur
                                                , context = cooccur_at
                                                , spp_names = TRUE
                                                , thresh = FALSE
                                                )
                    )

    rio::export(bio_cooccur
                , out_file
                )

  }

  taxa_cooccur <- bio_cooccur %>%
    dplyr::mutate(pair = map(cooccur, "results")) %>%
    dplyr::select(where(negate(is.list)), pair) %>%
    tidyr::unnest(cols = c(pair)) %>%
    dplyr::filter(p_gt < 0.05
                  , exp_cooccur > 1
                  ) %>%
    dplyr::select(any_of(visit_cols), sp1_name, sp2_name, sp1_inc, sp2_inc, exp_cooccur, obs_cooccur, p_gt)
