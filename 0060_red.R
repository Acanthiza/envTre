
  red <- bio_tidy %>%
    tidyr::nest(data = -c(taxa,!!ensym(toi))) %>%
    dplyr::mutate(dat_mat = map(data, . %>% dplyr::select(long, lat) %>% as.matrix(ncol = 2))
                  , aoo = map_dbl(dat_mat, red::aoo)
                  , eoo = map_dbl(dat_mat, red::eoo)
                  )


  prep_epochs <- function(min_year = NULL
                          , max_year = NULL
                          , epoch_step = 10
                          , epoch_overlap = FALSE
                          , include_now = FALSE
                          , filt_which = 2
                          ) {

    now <- as.numeric(format(Sys.Date(), "%Y"))

    n_epochs <- include_now + filt_which - 1

    if(!is.null(min_year)) {

      starts <- min_year + epoch_step*0:n_epochs
      ends <- starts + (epoch_step - 1 + epoch_overlap)

    }

    if(!is.null(max_year)) {

      ends <- max_year - epoch_step*0:n_epochs
      starts <- ends - (epoch_step - 1 + epoch_overlap)

    }

    tibble::tibble(start = starts
                   , end = ends
                   ) %>%
      dplyr::arrange(start) %>%
      dplyr::mutate(year = purrr::map2(start, end, ~.x:.y)
                    , epoch = paste0(substr(start,3,4), "-", substr(end,3,4))
                    , epoch = forcats::fct_inorder(epoch, ordered = TRUE)
                    , epoch_min = purrr::map_int(year, min)
                    , epoch_max = purrr::map_int(year, max)
                    , epoch_now = purrr::map_lgl(year, ~ now %in% .)
                    , epoch_now = as.logical(cumsum(epoch_now))
                    ) %>%
      {if(include_now) (.) else (.) %>% dplyr::filter(!epoch_now)} %>%
      dplyr::select(Negate(is.logical)) %>%
      dplyr::filter(start <= now)

  }

  eps <- prep_epochs(min_year = 1990
                     , filt_which = 10
                     , include_now = FALSE
                     ) %>%
    tidyr::unnest(cols = c(year))

  ref_ep <- as.character(max(eps$epoch))

  ep_aoo <- bio_tidy %>%
    dplyr::inner_join(eps) %>%
    tidyr::nest(data = -c(epoch, taxa,!!ensym(toi))) %>%
    dplyr::mutate(dat_mat = map(data, . %>% dplyr::select(long, lat) %>% as.matrix(ncol = 2))
                  , aoo = map_dbl(dat_mat, red::aoo)
                  #, eoo = map_dbl(dat_mat, red::eoo)
                  ) %>%
    dplyr::select(Negate(is.list)) %>%
    dplyr::arrange(epoch) %>%
    tidyr::pivot_wider(names_from = epoch, values_from = aoo) %>%
    dplyr::mutate(across(contains("-")
                         , ~ .x / !!ensym(ref_ep)
                         , .names = "delta_{.col}"
                         )
                  )


  ep_eoo <- bio_tidy %>%
    dplyr::inner_join(eps) %>%
    tidyr::nest(data = -c(epoch, taxa,!!ensym(toi))) %>%
    dplyr::mutate(dat_mat =map(data, . %>% dplyr::select(long, lat) %>% as.matrix(ncol = 2))
                  , eoo = map_dbl(dat_mat, red::eoo)
                  ) %>%
    dplyr::select(Negate(is.list)) %>%
    dplyr::arrange(epoch) %>%
    tidyr::pivot_wider(names_from = epoch, values_from = eoo) %>%
    dplyr::mutate(across(contains("-")
                         , ~ .x / !!ensym(ref_ep)
                         , .names = "delta_{.col}"
                         )
                  )
