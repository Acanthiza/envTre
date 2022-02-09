

  #-------Filter for analysis---------

  occ_lists <- bio_tidy %>%
    add_list_length(context = visit_cols
                    , create_list_col = TRUE
                    )

  occ_filt <- occ_lists %>%
    filter_list_df(taxa_col = "taxa"
                   , geo_levels = c(geo1, geo2)
                   , tax_levels = c("taxa", toi)
                   , time_levels = "year"
                   , min_length = 2
                   , min_years = 0
                   , min_year = min(test_years$year)
                   , max_year = max(test_years$year)
                   )


  #------Absences from cooccurs-------

  occ_filt_absences <- occ_filt %>%
    # sp2_name are taxa that indicate absence
    dplyr::inner_join(taxa_cooccur %>%
                        dplyr::rename(taxa = sp2_name)
                      ) %>%
    dplyr::select(-taxa) %>%
    dplyr::distinct() %>%
    # Now join all the sp2_name
    dplyr::rename(taxa = sp1_name) %>%
    dplyr::filter(!is.na(taxa)) %>%
    dplyr::left_join(occ_filt %>%
                       dplyr::mutate(p = 1)
                     ) %>%
    dplyr::mutate(p = if_else(is.na(p), 0, 1))


  #-------Data prep---------

  dat <- occ_filt_absences %>%
    dplyr::group_by(taxa
                    , across(any_of(c(toi, "year", geo1, geo2, geo3, geo4)))
                    ) %>%
    dplyr::summarise(success = sum(p, na.rm = TRUE)
                     , trials = n()
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(taxa
                    , across(any_of(c(toi, geo1, geo2)))
                    ) %>%
    dplyr::mutate(tot_records = sum(success, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(tot_records >= min_abs_sites) %>%
    dplyr::mutate(prop = success/trials) %>%
    tidyr::nest(data = -any_of(taxa_cols)) %>%
    # dplyr::mutate(successes = map_dbl(data, ~sum(.$success))) %>%
    # dplyr::filter(successes > min_abs_sites) %>%
    dplyr::left_join(taxa_taxonomy %>%
                       dplyr::select(taxa
                                     , common
                                     )
                     )


  #------Run models-------

  todo <- dat %>%
    {if(testing) (.) %>% dplyr::filter(taxa %in% test_taxa$taxa) else (.)} %>%
    dplyr::mutate(out_file = fs::path(out_dir,paste0("occupancy_mod_",taxa,".rds"))
                  , done = map_lgl(out_file,file.exists)
                  )


  if(sum(!todo$done) > 0) {

    if(sum(!todo$done) > use_cores/(if(testing) test_chains else use_chains)) {

      # Note each stan analysis is sequential (not 1 core per chain), as the
      # spawned analysis defaults to options(mc.cores = 1)

      future_pwalk(list(todo$taxa[!todo$done]
                        , todo$data[!todo$done]
                        , todo$out_file[!todo$done]
                        )
                   , make_occ_model
                   , geo_cols = c(geo1, geo2)
                   , chains = if(testing) test_chains else use_chains
                   , iter = if(testing) test_iter else use_iter
                   )

    } else {

      pwalk(list(todo$taxa[!todo$done]
                 , todo$data[!todo$done]
                 , todo$out_file[!todo$done]
                 )
            , make_occ_model
            , geo_cols = c(geo1, geo2)
            , chains = if(testing) test_chains else use_chains
            , iter = if(testing) test_iter else use_iter
            )

    }

  }


  #--------Explore models-----------

 mods_occ <- dat %>%
    dplyr::mutate(data = fs::path(out_dir,paste0("occupancy_dat_",taxa,".rds"))
                  , mod_path = fs::path(out_dir,paste0("occupancy_mod_",taxa,".rds"))
                  ) %>%
    dplyr::filter(file.exists(mod_path)) %>%
    dplyr::mutate(data = map(data, rio::import)
                  , type = "Occupancy"
                  )

  doof <- mods_occ

  future_pwalk(list(doof$taxa
                    , doof$common
                    , doof$data
                    , doof$mod_path
                    , doof$type
                    )
               , explore_mod
               , resp_var = "occ"
               , exp_var = c(toi, geo2)
               , max_levels = 30
               , draws = 200
               , post_groups = c("year", geo1, geo2)
               , re_run = TRUE
               )


