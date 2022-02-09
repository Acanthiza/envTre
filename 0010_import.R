

  # override_days = override get_new (based on days since update). Null default.
        # TRUE - get new data
        # FALSE - do not get new data

  bio_combine <- unite_data_sources(data_map = data_map_bio
                        , override_days = FALSE
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
              , fs::path("out", "bio_all.csv")
              )





