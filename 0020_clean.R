

  #--------Polygons-----------

  poly_aois <- ls() %>%
    tibble::enframe(name = NULL, value = "objName") %>%
    dplyr::mutate(obj = map(objName,get)
                  , cls = map(obj,class)
                  ) %>%
    tidyr::unnest(cols = c(cls)) %>%
    dplyr::filter(cls == "sf"
                  , !objName %in% c("aoi","sa")
                  ) %>%
    dplyr::mutate(poly_aoi = map(obj,st_filter,aoi)
                  , poly_aoi_name = paste0(objName,"_aoi")
                  )

  walk2(poly_aois$poly_aoi_name,poly_aois$poly_aoi,assign,envir = globalenv())


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



  #--------Filter AOI-------

  bio_aoi <- filter_aoi(bio_all
                         , use_aoi = aoi
                         , crs_aoi = st_crs(aoi)
                         ) %>%
    add_time_stamp()


  bio_time <- bio_aoi %>%
    dplyr::filter(year > min_year)


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
                    ) %>%
    dplyr::rename(grid_l = cell) %>%
    add_time_stamp()


  #---------Spatial reliability--------

  include_survey_nr <- c(15 # KI
                       , 23 # APY
                       , 24 # South Olary Plains
                       , 29 # SOUTH EAST
                       , 31 # MIDNORTH AND WEST MURRAY FLATS
                       , 48 # Rare rodents
                       #, 72 # SOOTY DUNNART KANGAROO IS
                       #, 82 # Coastal Dune and Clifftop
                       #, 83 # LOFTY BLOCK GRASSLANDS
                       , 84 # SE BOX & BULOKE WOODLANDS
                       , 85 #	DEEP SWAMP
                       , 90 # TILLEY SWAMP
                       , 94 # Sandy Desert
                       , 104 # Flinders Ranges
                       , 117 # Southern MLR
                       #, 119 # LAKE HAWDON
                       #, 408 # EP Th Fl monitoring
                       , 562 # NMM
                       #, 676 # KI Th Plants on roadsides
                       , 714 # Western Murray Mallee LAF
                       , 886 # CLMMM Bird and Veg
                       , 934 # Lowan mallee remnant veg
                       #, 958 # 	NORTHERN AMLR GRASSY ECOSYSTEM ASSESSMEN
                       #, 1067 # Alexandrina Council Th Acacias
                       , 1078 # CLMMM emergent veg
                       #, 1103 # BUFFEL GRASS CONTROL IN ARID RANGELANDS
                       #, 1089 # MARGINAL TO MAINSTREAM
                       #, 1239 # ADELAIDE PLAINS REMNANT VEGETATION
                       )

  include_data_name <- c("ALIS","BCM","NVB","TERN")

  bio_rel <- filter_spat_rel(bio_cell
                              , dist_col = "rel_dist"
                              , context = visit_cols
                              , dist = use_rel_dist
                              , over_ride = list(data_name = include_data_name
                                                 , survey_nr = include_survey_nr
                                                 )
                              ) %>%
    add_time_stamp()


  #-------Taxonomy---------

  # This generates a single row per taxa*Visit*any extracols
  # If lutaxa does not exist, make_taxa_taxonomy is run.

  bio_taxa <- filter_taxa(bio_rel
                          , taxa_col = "original_name"
                          , context = visit_cols
                          , extra_cols = NULL
                          , target_rank = "species"
                          , poor = species_filt
                          , save_gbif_file = fs::path("out","luGBIF.feather")
                          , king_for_taxa = "Animalia"
                          ) %>%
    add_time_stamp()


  #---------Singletons---------

  bio_single <- filter_counts(bio_taxa
                              , context = visit_cols
                              , thresh = 1
                              ) %>%
    add_time_stamp()


  #---------Geo context---------

  cells_geo <- bio_single %>%
    dplyr::distinct(across(contains("grid"))) %>%
    dplyr::bind_cols(as_tibble(raster::xyFromCell(aoi_grid_s
                                                  , .$grid_s
                                                  )
                               )
                     ) %>%
    sf::st_as_sf(coords = c("x", "y")
                 , crs = st_crs(aoi_grid_s)
                 ) %>%
    st_transform(crs = st_crs(ibra_sub_aoi)) %>%
    sf::st_intersection(ibra_sub_aoi) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(matches("grid"), matches("IBRA.*N")) %>%
    na.omit()

  bio_geo <- bio_single %>%
    dplyr::inner_join(cells_geo) %>%
    add_time_stamp()


  #----------Effort---------

  effort_mod <- make_effort_mod_cat(bio_geo
                                    , context = visit_cols
                                    , cat_cols = c(geo2, toi)
                                    )

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

  cells_tidy <- bio_tidy %>%
    dplyr::count(across(all_of(visit_cols))
                 , name = "sr"
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


  #--------Filtering summary--------

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

  #-------Co-occurrence

  bio_cooccur <- bio_tidy %>%
    tidyr::nest(data = -c(toi, geo1, geo2)) %>%
    dplyr::mutate(n_lists = map_dbl(data
                                    , . %>%
                                      dplyr::distinct(across(any_of(visit_cols))) %>%
                                      nrow()
                                     )
                  , n_taxa = map_dbl(data,~n_distinct(.$taxa))
                  ) %>%
    # more than 10*min(min_abs_sites) lists within a context
    dplyr::filter(n_lists > 5*min(min_abs_sites)) %>%
    # at least 2*min(min_abs_sites) distinct Taxa
    dplyr::filter(n_taxa > 2*min(min_abs_sites)) %>%
    # at least three taxa with more than three records
    dplyr::filter(map_dbl(data,~sum(table(.$taxa) < 3)) > 3) %>%

    #dplyr::sample_n(5) %>% # TESTING

    dplyr::mutate(cooccur = future_map(data
                                       , make_cooccur
                                       , context = visit_cols
                                       , spp_names = TRUE
                                       , thresh = FALSE
                                       )
                  )

  taxa_cooccur <- bio_cooccur %>%
    dplyr::mutate(pair = map(cooccur,"results")) %>%
    dplyr::select(where(negate(is.list)), pair) %>%
    tidyr::unnest(cols = c(pair)) %>%
    dplyr::filter(p_gt < 0.05) %>%
    dplyr::select(any_of(visit_cols), sp1_name, sp2_name)

