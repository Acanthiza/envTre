
  orders <- c(NULL
              # , "Anseriformes"
              # , "Charadriiformes"
               #, "Accipitriformes"
              # , "Diprotodontia"
              )

  genera <- c(NULL
               #, "Melithreptus"
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
              # , "Stipiturus"
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
          #, "Melithreptus gularis"
          #, "Melithreptus lunatus"
          #, "Amytornis striatus"
          #, "Neophema chrysostoma"
          , "Stipiturus malachurus"
          , "Stagonopleura guttata"
          , "Stagonopleura bella"
          #, "Melanodryas cucullata"
          #, "Climacteris picumnus"
          #, "Lophochroa leadbeateri"
          )


  spp <- c(spp
           #, ki_taxa
           )


  use_order <- taxa_taxonomy %>%
    dplyr::filter(order %in% orders)

  use_genus <- taxa_taxonomy %>%
    dplyr::filter(genus %in% genera)

  use_taxa <- taxa_taxonomy %>%
    dplyr::filter(taxa %in% spp)

  use_taxa <- sort(unique(c(use_order$taxa,use_genus$taxa,use_taxa$taxa)))

  use_toi <- taxa_taxonomy %>%
    dplyr::filter(taxa %in% use_taxa) %>%
    dplyr::pull(!!ensym(toi)) %>%
    unique()


