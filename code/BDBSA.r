

  ## function to get new data
  get_BDBSA <- function(out_file, data_map) {
    
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
    
    # connect to BDBSA
    con <- dbConnect(odbc::odbc()
                     , "BDBSA Production"
                     , database = "BDBSA Production’"
                     , uid = Sys.getenv("BDBSA_PRD_user")
                     , pwd = Sys.getenv("BDBSA_PRD_pwd")
                     )
    
    # Link to each table
    excludeVars <- c("CREATED_DATE", "CREATED_USER", "MODIFIED_DATE", "MODIFIED_USER")
    
    # Survey
    sur <- dplyr::tbl(con,"LUSURVEYNAME") %>%
      dplyr::select(!any_of(excludeVars))
    
    # Patch
    pat <- dplyr::tbl(con,"SUPATCH") %>%
      dplyr::select(!any_of(excludeVars))
    
    # Visit
    vis <- dplyr::tbl(con,"SUVISIT") %>%
      dplyr::select(!any_of(excludeVars))
    
    # Species
    spp <- tbl(con,"SUSPECIES") %>%
      dplyr::filter(SPECIESTYPE == "P"
                    , DATEACCURACY != "C"
                    , DATEACCURACY != "T"
                    , ISCERTAIN == "Y"
                    , !NUMOBSERVED %in% c("0","none detected","None detected")
                    ) %>%
      dplyr::select(!any_of(excludeVars))
    
    # Veg assoc
    # desc <- tbl(con,"SUVVEGASSOC") %>%
    #   dplyr::select(!any_of(excludeVars))
    
    # Flora lookup from 'not synonymous and not renamed' linked to to FL_FLSP
    luFlor <- tbl(con,"FLVNONSYNNOTREN") %>%
      dplyr::left_join(tbl(con,"FLSP") %>%
                         dplyr::select(SPECIESNR,NSXCODE,LIFESPAN,ISINDIGENOUS)
                       ,by=c("SPECIESNR","NSXCODE")
                       ) %>%
      dplyr::select(!any_of(excludeVars))
    
    # What names to grab before collect()?
    selectNames <- data_map %>%
      dplyr::filter(data_name == "BDBSA") %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit()
    
    # Get all records
    temp <- sur %>%
      dplyr::left_join(pat, by = "SURVEYNR") %>%
      dplyr::left_join(vis, by = "PATCHID") %>%
      #dplyr::left_join(desc) %>%
      dplyr::left_join(spp, by = "VISITNR") %>%
      dplyr::left_join(luFlor, by = "NSXCODE") %>%
      dplyr::collect() %>%
      dplyr::left_join(lurelBDBSA) %>%
      dplyr::select(tidyselect::any_of(selectNames)) %>%
      dplyr::filter(!is.na(SPECIES))
    
    dbDisconnect(con)
      
    rio::export(temp
                , out_file
                )
    
    return(temp)
    
  }
  
