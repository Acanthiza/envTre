
  filtering_text <- function(df, lag_df, desc, step, context) {

    # previous data set
    if(step != 1) {

      whichCol <- grep("original_name|taxa",names(lag_df),value = TRUE)

      prevTaxa <- lag_df %>%
        dplyr::pull(!!ensym(whichCol)) %>%
        n_distinct() %>%
        format(big.mark = ",")

      prevCells <- lag_df %>%
        dplyr::distinct(across(any_of(context))) %>%
        nrow() %>%
        format(big.mark = ",")

      prevRecords <- format(nrow(lag_df), big.mark = ",")

      # current data set
      whichCol <- grep("original_name|taxa",names(df),value = TRUE)

      currentTaxa <- df %>%
        dplyr::pull(!!ensym(whichCol)) %>%
        n_distinct() %>%
        format(big.mark = ",")

      currentCells <- df %>%
        dplyr::distinct(across(any_of(context))) %>%
        nrow() %>%
        format(big.mark = ",")

      currentRecords <- format(nrow(df), big.mark = ",")

      # make text result
      paste0("Filtering "
             , desc
             , " took the number of taxa from "
             , prevTaxa
             , " to "
             , currentTaxa
             , ", the number of contexts from "
             ,  prevCells
             , " to "
             , currentCells
             , ", and the number of records from "
             , prevRecords
             , " to "
             , currentRecords
             ,"."
             )

    } else {

      # current data set
      whichCol <- grep("original_name|taxa",names(df),value = TRUE)

      currentTaxa <- df %>%
        dplyr::pull(!!ensym(whichCol)) %>%
        n_distinct() %>%
        format(big.mark = ",")

      currentCells <- df %>%
        dplyr::distinct(across(any_of(context))) %>%
        nrow() %>%
        format(big.mark = ",")

      currentRecords <- format(nrow(df), big.mark = ",")

      # make text result
      paste0("At the start of the filtering process there were "
             , currentTaxa
             , " taxa, "
             , currentCells
             , " contexts, and "
             , currentRecords
             ," records."
             )


    }

  }

  data_name_time_plot <- function(df) {

    dfYear <- df %>%
      dplyr::mutate(Year = year(date)
                    , Month = month(date)
                    , yearmon = zoo::as.yearmon(date)
                    )

    df_data_name <- unique(dfYear$data_name)

    dfTime <- dfYear %>%
      dplyr::count(Year)

    ggplot(dfTime, aes(Year,n)) +
      geom_col() +
      labs(y = "Number of records"
           , subtitle = df_data_name
           ) +
      scale_x_continuous(breaks = scales::breaks_extended())

  }



  summarise_data_name <- function(df
                                  , site_cols
                                  , visit_cols
                                  ) {

    # This has been hardly touched since original 'ecosystems' project. Could do with a considerable re-write.
    # Possibly genericise to:
      # `summarise_df` with arguments `df` and `col(s)`
      # `summarise_time_span` with arguments `df`, `time_col`, `interest_col` (= taxa)
      # `summarise_singletons` with arguments `df`, `context`, `interest_col`

    df <- df %>%
      dplyr::mutate(year = year(date)
                    , month = month(date)
                    )

    if("quad_x" %in% names(df)) {

      df <- df %>%
        dplyr::mutate(qsize = quad_x*quad_y)

    }

    taxa <- dplyr::n_distinct(df["original_name"])
    sites <- df %>% dplyr::distinct(across(any_of(site_cols))) %>% nrow()
    visits <- df %>% dplyr::distinct(across(any_of(visit_cols))) %>% nrow()
    records <- nrow(df)

    df_data_name <- unique(df$data_name)

    minYear <- df %>%
      dplyr::pull(year) %>%
      min()

    maxYear <- df %>%
      dplyr::pull(year) %>%
      max()

    maxSpp <- df %>%
      dplyr::count(original_name) %>%
      dplyr::arrange(desc(n)) %>%
      dplyr::slice(1)

    maxSppMinYear <- df %>%
      dplyr::add_count(original_name) %>%
      dplyr::filter(n == max(n)) %>%
      dplyr::filter(year == min(year)) %>%
      dplyr::pull(year) %>%
      unique()

    lastSppDf <- df %>%
      dplyr::group_by(original_name) %>%
      dplyr::filter(year == min(year)) %>%
      dplyr::filter(month == min(month)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(year == max(year)) %>%
      dplyr::filter(month == max(month)) %>%
      dplyr::add_count(original_name) %>%
      dplyr::slice(1)

    lastSpp <- lastSppDf %>%
      dplyr::distinct(original_name) %>%
      dplyr::pull(original_name)

    lastSppYear <- lastSppDf %>%
      dplyr::distinct(year) %>%
      dplyr::pull(year)

    lastSppMonth <- lastSppDf %>%
      dplyr::distinct(month) %>%
      dplyr::pull(month)

    checkCols <- tribble(
      ~col, ~colName
      , "cover", "cover estimate"
      , "lifeform", "lifeform (shrub, grass, tree etc.)"
      , "lifespan", "lifespan (annual or perennial)"
    )

    summaryYear <- list()

    if(n_distinct(df$year) > 1) {

      summaryYear$range <- paste0("The earliest "
                                  , df_data_name
                                  , " records were collected in "
                                  , minYear
                                  , ". The latest "
                                  , df_data_name
                                  , " records were collected in "
                                  , maxYear
                                  , "."
                                  )

      summaryYear$spp <- paste0("_"
                                , maxSpp$original_name
                                , "_ was first recorded in "
                                , maxSppMinYear
                                , ". The last taxa to be added to the data set was _"
                                , lastSpp
                                , "_ which was first recored in "
                                , month.name[lastSppMonth]
                                , " "
                                , lastSppYear
                                , "."
                                )

    } else {

      summaryYear$range <- paste0("All ",df_data_name," records were dated ",unique(df$year),".")

      summaryYear$spp <- NULL

    }


    summaryCols <- if(any(c(checkCols$col) %in% names(df))) {

      paste0(" Of the records, "
             , df %>%
               {if(!c("COVER") %in% names(.)) (.) else if("COVCODE" %in% names(.)) (.) %>%
                   dplyr::mutate(COVCODE = if_else(is.na(COVCODE),as.character(COVER),COVCODE)) else (.) %>%
                   dplyr::mutate(COVCODE = COVER)
               } %>%
               dplyr::summarise(records = n()
                                , across(any_of(checkCols$col),~sum(!is.na(.)))
               ) %>%
               tidyr::pivot_longer(2:ncol(.),names_to = "col", values_to = "value") %>%
               dplyr::mutate(per = round(100*value/records,1)) %>%
               dplyr::left_join(checkCols) %>%
               dplyr::select(col,colName,per) %>%
               dplyr::mutate(text = paste0(per
                                           , "% had a "
                                           , colName
                                           , " recorded"
               )
               ) %>%
               dplyr::pull(text) %>%
               vec_to_sentence()
             , "."
      )

    }

    # Patches
    cellsCols <- tribble(
      ~col, ~colName
      , "qsize", "quadrat size recorded"
      , "rel_dist", paste0("spatial accuracy estimate recorded")
    )

    summaryColsPatches <- if(any(cellsCols$col %in% names(df))) {

      cellsYear <- df %>%
        dplyr::count(across(all_of(c("year","month")))
                     , across(any_of(cellsCols$col))
                     , lat
                     , long
                     , name = "richness"
                     ) %>%
        add_raster_cell(aoi_grid_s
                        , .
                        , x = "long"
                        , y = "lat"
                        ) %>%
        dplyr::filter(!is.na(cell))

      cellsRel <- cellsYear %>%
        dplyr::summarise(visits = n()
                         , cells = n_distinct(cell)
                         , setDistBelow = sum(rel_dist <= use_rel_dist, na.rm = TRUE)
                         , setDistAbove = sum(rel_dist > use_rel_dist, na.rm = TRUE)
                         , nas = sum(is.na(rel_dist))
                         )

      paste0(" Of the visits, "
             , cellsYear %>%
                 dplyr::summarise(records = n()
                                  , across(any_of(cellsCols$col),~sum(!is.na(.)))
                                  ) %>%
                 tidyr::pivot_longer(2:ncol(.),names_to = "col", values_to = "value") %>%
                 dplyr::mutate(per = round(100*value/records,1)) %>%
                 dplyr::left_join(cellsCols) %>%
                 dplyr::select(col,colName,per) %>%
                 dplyr::mutate(text = paste0(per
                                             , "% had a "
                                             , colName
                                             )
                               ) %>%
                 dplyr::pull(text) %>%
                 vec_to_sentence()
             , "."
      )

    }

    singletons <- df %>%
      dplyr::count(across(any_of(visit_cols)), name = "sr") %>%
      dplyr::filter(sr == 1) %>%
      dplyr::inner_join(df) %>%
      dplyr::count(original_name, name = "records") %>%
      dplyr::mutate(text = paste0("_"
                                  ,original_name
                                  ,"_ ("
                                  ,format(records,big.mark=",",trim = TRUE)
                                  ,")"
                                  )
                    )


    text <- paste0("The initial retrieval of plant data from "
                   , df_data_name
                   , " gave a data set of "
                   , format(records,big.mark=",")
                   , " records (from "
                   , format(visits, big.mark = ",")
                   , " visits to "
                   , format(sites, big.mark = ",")
                   , " sites) of "
                   , taxa
                   , " different taxa."
                   , if(!is.null(summaryYear$range)) paste0(" ",summaryYear$range)
                   , " The taxa with the most records was _"
                   , maxSpp$original_name
                   , "_ with "
                   , format(maxSpp$n,big.mark=",")
                   , " records."
                   , if(!is.null(summaryYear$spp)) paste0(" ",summaryYear$spp)
                   , if(!is.null(summaryCols)) summaryCols
                   , summaryColsPatches
                   , " There were "
                   , format(sum(singletons$records),big.mark=",")
                   , " sites with only one taxa recorded (singleton sites)."
                   , if(nrow(singletons) > 0) paste0(" The most common taxa at singleton sites "
                                                     , if(n_distinct(singletons$original_name) > 1) "were" else "was"
                                                     , " [taxa (sites)] "
                                                     , singletons %>%
                                                       dplyr::arrange(desc(records)) %>%
                                                       dplyr::pull(text) %>%
                                                       head(5) %>%
                                                       vec_to_sentence()
                                                     , "."
                   )
                   , " The visit (cell within a month in a year) with the most taxa had "
                   , max(cellsYear$richness,na.rm = TRUE)
                   , " taxa recorded."
    )

    return(text)

  }
