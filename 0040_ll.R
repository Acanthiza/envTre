

  #-------Filter for analysis---------

  ll_lists <- bio_tidy %>%
    add_list_length(context = visit_cols
                    , create_list_col = TRUE
                    )

  ll_filt <- ll_lists %>%
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

  ll_filt_absences <- ll_filt %>%
    # sp2_name are taxa that indicate absence
    dplyr::inner_join(taxa_cooccur %>%
                        dplyr::rename(taxa = sp2_name)
                      ) %>%
    dplyr::select(-taxa) %>%
    dplyr::distinct() %>%
    # Now join all the sp2_name
    dplyr::rename(taxa = sp1_name) %>%
    dplyr::filter(!is.na(taxa)) %>%
    dplyr::left_join(ll_filt %>%
                       dplyr::mutate(p = 1)
                     ) %>%
    dplyr::mutate(p = if_else(is.na(p), 0, 1))


  #-------Data prep---------

  dat <- ll_filt_absences %>%
    dplyr::mutate(success = p
                  , trials = 1
                  ) %>%
    dplyr::group_by(taxa
                    , across(any_of(c(toi, geo1, geo2)))
                    ) %>%
    dplyr::mutate(tot_records = sum(success)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(tot_records >= min_abs_sites) %>%
    dplyr::mutate(prop = success/trials
                  , log_list_length = log(list_length)
                  ) %>%
    tidyr::nest(data = -any_of(taxa_cols)) %>%
    # dplyr::mutate(successes = map_dbl(data, ~sum(.$success))) %>%
    # dplyr::filter(successes > min_abs_sites) %>%
    dplyr::left_join(taxa_taxonomy %>%
                       dplyr::select(taxa
                                     , common
                                     )
                     )


  #------Run models-------

  # Check if ll models have been run - run if not
  todo <- dat %>%
    {if(length(use_taxa) > 0) (.) %>% dplyr::filter(taxa %in% use_taxa) else (.)} %>%
    dplyr::mutate(out_file = fs::path(out_dir,paste0("list-length_mod_",taxa,".rds"))
                  , done = map_lgl(out_file
                                   , file.exists
                                   )
                  )

  if(sum(!todo$done) > 0) {

    if(sum(!todo$done) > use_cores / use_chains) {

      # Note each stan analysis is sequential (not 1 core per chain), as the
      # spawned analysis defaults to options(mc.cores = 1)

      furrr::future_pwalk(list(todo$taxa[!todo$done]
                               , todo$data[!todo$done]
                               , todo$out_file[!todo$done]
                               )
                          , make_ll_model
                          , geo_cols = c(geo1, geo2)
                          , chains = use_chains
                          , iter = use_iter
                          , .options = furrr_options(seed = TRUE)
                          )

    } else {

      purrr::pwalk(list(todo$taxa[!todo$done]
                        , todo$data[!todo$done]
                        , todo$out_file[!todo$done]
                        )
                   , make_ll_model
                   , geo_cols = c(geo1, geo2)
                   , chains = use_chains
                   , iter = use_iter
                   )

    }


  }


  #--------Explore models-----------

  mods_ll <- dat %>%
    dplyr::mutate(mod_path = fs::path(out_dir,paste0("list-length_mod_",taxa,".rds"))) %>%
    dplyr::filter(file.exists(mod_path)) %>%
    dplyr::mutate(type = "List length")

  doof <- mods_ll

  future_pwalk(list(doof$taxa
                    , doof$common
                    , doof$data
                    , doof$mod_path
                    , doof$type
                    )
               , explore_mod
               , resp_var = "prop"
               , exp_var = c(toi, geo2)
               , max_levels = 30
               , draws = 200
               , post_groups = c("year", geo1, geo2)
               , re_run = TRUE
               , tests = test_years
               , .options = furrr_options(seed = TRUE)
               )
