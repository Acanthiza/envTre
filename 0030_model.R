

  #-------Filter for analysis---------

  rr_lists <- bio_tidy %>%
    add_list_length(context = context_cols
                    , create_list_col = TRUE
                    )

  rr_filt <- rr_lists %>%
    filter_list_df(taxa_col = "taxa"
                   , geo_levels = context_rr_filt[context_rr_filt %in% geo_cols]
                   , tax_levels = context_rr_filt[context_rr_filt %in% taxa_cols]
                   , time_levels = context_rr_filt[context_rr_filt %in% time_cols]
                   , analysis_levels = context_rr_analysis
                   , min_years = 0
                   , min_span = 5
                   )


  #------Absences from cooccurs-------

  rr_filt_absences <- rr_filt %>%
    # sp2_name are taxa that indicate absence
    dplyr::inner_join(taxa_cooccur %>%
                        dplyr::rename(taxa = sp2_name)
                      ) %>%
    # sometimes there is no taxa indicating absence. This excludes those (not optimal...)
    #dplyr::filter(!is.na(sp1_name)) %>%
    dplyr::select(-taxa) %>%
    dplyr::distinct() %>%
    # Now join all the sp2_name
    dplyr::rename(taxa = sp1_name) %>%
    dplyr::left_join(rr_filt %>%
                       dplyr::mutate(p = 1)
                     ) %>%
    dplyr::mutate(p = if_else(is.na(p), 0, 1))


  #-------Data prep---------

  dat <- rr_filt_absences %>%
    dplyr::group_by(taxa
                    , across(any_of(context_rr_analysis))
                    ) %>%
    dplyr::summarise(success = sum(p)
                     , trials = n()
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prop = success/trials) %>%
    tidyr::nest(data = -any_of(taxa_cols)) %>%
    dplyr::mutate(successes = map_dbl(data, ~sum(.$success))) %>%
    dplyr::filter(successes > min_abs_sites) %>%
    dplyr::left_join(taxa_taxonomy %>%
                       dplyr::select(taxa
                                     , common
                                     )
                     )

  #------Model function--------

  rr <- function(taxa
                 , data
                 ) {

    print(taxa)

    out_file <- fs::path(out_dir,paste0("reporting-rate_Mod_",taxa,".rds"))

    geos <- data %>%
      dplyr::distinct(across(any_of(c(geo1,geo2)))) %>%
      nrow()

    grid_cells <- data %>%
      dplyr::distinct(across(contains("grid"))) %>%
      nrow()

    # GAM
    if(geos > 1) {

      mod <- stan_gamm4(as.formula(paste0("cbind(success,trials - success) ~ "
                                          , "s(year, k = 4, bs = 'ts') +"
                                          , geo2
                                          , " + s(year, k = 4, by = "
                                          , geo2
                                          , ", bs = 'ts')"
                                          )
                                   )
                        , data = data
                        , family = binomial()
                        , random = ~(1|grid_l)
                        , chains = if(testing) test_chains else use_chains
                        , iter = if(testing) test_iter else use_iter
                        )

    } else {

      mod <- stan_gamm4(cbind(success,trials-success) ~ s(year, k = 4, bs = "ts")
                            , data = data
                            , random = if(grid_cells > 1) formula(~ (1|grid_l)) else NULL
                            , family = binomial()
                            , chains = if(testing) test_chains else use_chains
                            , iter = if(testing) test_iter else use_iter
                            )

    }

    write_rds(mod,out_file)

  }


 #------Run models-------


  # Check if rr models have been run - run if not
  todo <- dat %>%
    dplyr::mutate(out_file = fs::path(out_dir,paste0("reporting-rate_Mod_",taxa,".rds"))
                  , done = map_lgl(out_file
                                   , file.exists
                                   )
                  )

  if(sum(!todo$done) > 0) {

    if(sum(!todo$done) > use_cores/(if(testing) test_chains else use_chains)) {

      # Note each stan analysis is sequential (not 1 core per chain), as the
      # spawned analysis defaults to options(mc.cores = 1)

      future_pwalk(list(todo$taxa[!todo$done]
                        , todo$data[!todo$done]
                        )
                   , rr
                   )

    } else {

      pwalk(list(todo$taxa,todo$data),rr)

    }


  }


  #--------Explore models-----------

  mods_rr <- dat %>%
    dplyr::mutate(mod_path = fs::path(out_dir,paste0("reporting-rate_Mod_",taxa,".rds"))) %>%
    dplyr::filter(file.exists(mod_path)) %>%
    dplyr::mutate(type = "Reporting rate")

  doof <- mods_rr

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
               , post_groups = c("taxa"
                                 , "common"
                                 , "list_length"
                                 , "year"
                                 , geo2
                                 )
               )

  timer$stop("rr", comment = paste0("Reporting rate models run for "
                                    ,nrow(taxaModsRR)
                                    ," taxa, of which "
                                    ,nrow(todo)
                                    ," were new"
                                    )
             )
