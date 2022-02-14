

  # override_days = override get_new (based on days since update). Null default.
        # TRUE - get new data
        # FALSE - do not get new data


  need_to_update <- map(data_map_bio$data_name
                       , days_since_update
                       ) %>%
    map2(., data_map_bio$days, `>`) %>%
    unlist() %>%
    any()

  bio_all_file <- fs::path("out", "bio_all.csv")

  out_file <- if(need_to_update) tempfile() else bio_all_file

  if(!file.exists(out_file)) {

    bio_combine <- unite_data_sources(data_map = data_map_bio
                          , override_days = NULL

                          # get_GBIF args
                          , taxon_key = 1
                          ) %>%
      as_tibble() %>%
      dplyr::filter(!is.na(original_name))

    dates <- bio_combine %>%
      dplyr::distinct(date) %>%
      dplyr::mutate(month = month(date)
                    , year = year(date)
                    )

    bio_all <- bio_combine %>%
      dplyr::left_join(dates)

    rio::export(bio_all
                , bio_all_file
                )

  } else {

    bio_all <- rio::import(bio_all_file) %>%
      tibble::as_tibble()

  }





