

  get_EGIS <- function(out_file, data_map) {
    
    lurelBDBSA <- tribble(
      ~RELIABNR,~max_dist,
      0,5,
      1,50,
      2,100,
      3,250,
      4,500,
      5,1000,
      6,10000,
      7,25000,
      8,100000,
      9,1,
      12,0.5,
      13,0.02,
      14,0.001,
      15,100000,
      16,0.01,
      17,0.1,
      18,100000,
      20,150,
      21,30000,
      22,125000,
      23,625000,
      24,2000000,
      26,12345678901234568e8,
      28,5000,
      29,10000,
      30,10
    )
    
    con <- dbConnect(odbc::odbc()
                 , "EGIS Production"
                 , database = "EGIS_PRD"
                 , uid = Sys.getenv("EGIS_user")
                 , pwd = Sys.getenv("EGIS_pwd")
                 )
    
    selectNames <- data_map %>%
      dplyr::filter(data_name == "EGIS") %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit()

    temp <- dplyr::tbl(con, dbplyr::in_schema("FLORA","SUPERTABLEUNFILTERED_TAB"))  %>%
      dplyr::filter(SPECIESTYPE == "P"
                    , DATEACCURACY != "C"
                    , DATEACCURACY != "T"
                    , ISCERTAIN == "Y"
                    , !NUMOBSERVED %in% c("0","none detected","None detected")
                    )
      collect() %>%
      dplyr::left_join(lurelBDBSA) %>%
      dplyr::select(any_of(selectNames))
    
    dbDisconnect(con)
    
    rio::export(temp
                , out_file
                )
    
    return(temp)
    
  }

