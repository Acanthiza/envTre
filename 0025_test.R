
if(testing) {
  
  #--------Test taxa---------
  
  # These are only used when testing = TRUE
  
  orders <- c(NULL
              # , "Anseriformes"
              # , "Charadriiformes"
               , "Accipitriformes"
              # , "Diprotodontia"
              )
  
  genera <- c(NULL
               , "Melithreptus"
              # , "Pandion"
              # , "Haliaeetus"
              # , "Cacatua"
              # , "Myiagra"
              # , "Pseudonaja"
              # , "Pogona"
              # , "Vespadelus"
              # , "Macropus"
              # , "Pachycephala"
              # , "Phylidonyris"
              # , "Climacteris"
              )
  
  
  ki_taxa <- read_csv(path("data","ki_taxa.csv")
                    , col_names = FALSE
                    ) %>%
    dplyr::mutate(X1 = gsub("\\s"," ",X1)) %>%
    tidyr::separate(X1,into = c("genus","species")) %>%
    dplyr::mutate(spp = paste0(genus," ",species)) %>%
    dplyr::distinct(spp) %>%
    dplyr::arrange(spp) %>%
    dplyr::pull(spp)
  
  
  spp <- c(NULL
          , "Melithreptus gularis"
          , "Melithreptus lunatus"
          , "Amytornis striatus"
          )
  
  
  spp <- c(spp
           ,ki_taxa
           )
  
    
  test_order <- taxa_taxonomy %>%
    dplyr::filter(order %in% orders)
  
  test_genus <- taxa_taxonomy %>%
    dplyr::filter(genus %in% genera)
  
  test_taxa <- taxa_taxonomy %>%
    dplyr::filter(taxa %in% spp)
  
  tests <- sort(unique(c(test_order$taxa,test_genus$taxa,test_taxa$taxa)))
  
  test_toi <- taxa_taxonomy %>%
    dplyr::filter(taxa %in% tests) %>%
    dplyr::pull(!!ensym(toi)) %>%
    unique()
  
}

  
  
