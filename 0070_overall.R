
  calc_quants <- c(0.005
                   , 0.025
                   , 0.05
                   , 0.25
                   , 0.5
                   , 0.75
                   , 0.95
                   , 0.975
                   , 0.995
                   )

  taxa_mods <- ls(pattern = "^mods_") %>%
    tibble::enframe(name = NULL, value = "obj") %>%
    dplyr::mutate(data = map(obj,get)) %>%
    tidyr::unnest(cols = c(data)) %>%
    dplyr::mutate(res = map(mod_path
                                , ~rio::import(gsub("\\.rds"
                                                    , "_res.rds"
                                                    , .
                                                    )
                                               )
                                )
                  , year_diff_df = map(res
                                       , "year_diff_df"
                                       )
                  , type = fct_relevel(type
                                       , "Reporting rate","List length"
                                       )
                  ) %>%
    dplyr::arrange(!!ensym(toi)
                   , taxa
                   , type
                   )

  taxa_mods_overall <- taxa_mods %>%
    dplyr::select(type, !!ensym(toi), year_diff_df) %>%
    tidyr::unnest(cols = c(year_diff_df)) %>%
    tidyr::nest(data = -c(taxa, common, !!ensym(toi))) %>%
    dplyr::mutate(overall = pmap(list(taxa
                                      , common
                                      , data
                                      )
                                 , function(a,b,c) make_overall(a
                                                                , b
                                                                , c
                                                                )
                                 )
                  )
