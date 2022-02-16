
  #----------Project------------

  # no scientific notation for numbers
  options(scipen = 999)

  # Necessary folders
  folders <- c("out"
               , "data"
               , "out/ds"
               , "out/shp"
               , out_aoi
               , out_runs
               , out_dir
               )

  walk(folders, dir.create)


  #----------Lookups--------

  # LSAs
  lulsa <- tribble(
    ~LSA, ~REGION, ~LSARegion, ~Zone, ~System, ~R, ~G, ~B, ~A
    ,"HF","Hills and Fleurieu","Hills and Fleurieu",	"Agricultural",	"High-rainfall", 89, 23, 138, 255
    ,"AW","Alinytjara Wilurara","Alinytjara Wilurara",	"Arid",	"Arid",	191,	54,	44,	255
    ,"EP","Eyre Peninsula","Eyre Peninsula",	"Agricultural",	"Low-rainfall",	86,	156,	190,	255
    ,"KI","Kangaroo Island","Kangaroo Island",	"Agricultural",	"High-rainfall",	0,	137,	152,	255
    ,"NY","Northern and Yorke","Northern and Yorke",	"Agricultural",	"Low-rainfall",	248,	185,	44,	255
    ,"SAAL","South Australian Arid Lands","South Australian Arid Lands",	"Arid",	"Arid",	213,	94,	0,	255
    ,"MR","Murraylands and Riverland","Murraylands and Riverland",	"Agricultural",	"Low-rainfall",	0,	132,	197,	255
    ,"LC","Limestone Coast","Limestone Coast","Agricultural",	"High-rainfall",	102,	181,	98,	255
    ,"GA","Green Adelaide","Green Adelaide","Urban","High-rainfall",	34,	139,34,	255
    ) %>%
    dplyr::mutate(across(where(is.character),~gsub("Wilur|Wilu\\?",paste0("Wilu","\u1E5F"),.)))


  lulikelihood <- tribble(
    ~likelihood, ~maxVal
    , "Exceptionally unlikely", 0.01
    , "Extremely unlikely", 0.05
    , "Very unlikely", 0.1
    , "Unlikely", 1/3
    , "About as likely as not", 2/3
    , "Likely", 0.9
    , "Very likely", 0.95
    , "Extremely likely", 0.99
    , "Virtually certain", 1
    ) %>%
    dplyr::mutate(likelihood = fct_inorder(likelihood)
                  , range = cut(maxVal
                                , breaks = c(0,.$maxVal)
                                )
                  )




  #--------Import--------

  # What EPSG to use?
  use_epsg <- 3577

  # import data map
  data_map_bio <- tibble::tibble(
    data_name = c("ALIS", "BCM", "BDBSA", "EGIS", "NVB", "PTP", "TERN", "GBIF"),
    order = c(4,5,1,2,7,6,3,8),
    days = c(60, 60, 700, 700, 1000, 700, 60, 90),
    site = c("SITENUMBER", "SITE_ID", "PATCHID", "FLORACODE", "path", "PlantDataID", "site_unique", "gbifID"),
    date = c("SurveyDate", "ASSESSMENT_DATE", "VISITDATE", "SIGHTINGDATE", "date", "date", "visit_start_date", "eventDate"),
    lat = c("LATITUDE", "LATITUDE", "LATITUDE", "LATITUDE", "lat", "LATITUDE", "latitude", "decimalLatitude"),
    long = c("LONGITUDE", "LONGITUDE", "LONGITUDE", "LONGITUDE", "lon", "LONGITUDE", "longitude", "decimalLongitude"),
    original_name = c("SPECIES", "SPECIES", "SPECIES", "SPECIES", "Spp", "SPECIES", "herbarium_determination", "species"),
    nsx = c("NSXCODE", "NSXCODE", "NSXCODE", "NSXCODE", NA, "NSXCODE", NA, "organismID"),
    number = c(NA, NA, "NUMOBSERVED","NUMOBSERVED", NA, NA, NA, NA),
    survey_nr = c(NA, NA, "SURVEYNR", "SURVEYNR", NA, NA, NA, NA),
    survey = c(NA, NA, "SURVEYNAME", "SURVEYNAME", NA, NA, NA, NA),
    ind = c("ISINDIGENOUS", "ISINDIGENOUS", "ISINDIGENOUSFLAG", "ISINDIGENOUSFLAG", NA, "Native_Introduced_original", NA, NA),
    rel_dist = c(NA, NA, "rel_dist", "rel_dist", NA, NA, NA, "coordinateUncertaintyInMeters"),
    sens = c(NA, NA, NA, "DISTRIBNDESC", NA, NA, NA, NA),
    desc = c("Arid lands information systems"
             , "Bushland condition monitoring"
             , "Biological databases of South Australia"
             , "Flora 'Supertable' from the environmental databases of South Australia"
             , "DEW Native Vegetation Branch"
             , "Paddock tree project"
             , "Terrestrial ecosystem network"
             , "Global biodiversity information facility"
             )
    ) %>%
    dplyr::mutate(data_name = forcats::fct_reorder(data_name, order)) %>%
    dplyr::filter(data_name %in% gsub("\\.r|code\\/","",fs::dir_ls("code")))


  #----------Clean----------

  # Maximum allowed spatial reliability
  use_rel_dist <- 1000
  use_extra_rel_dist <- 5000

  # Effort (species richness) threshold
  extreme_sr_lo <- 0.025
  extreme_sr_hi <- 0.025

  # Absolute minimum number of sites for a taxa
  min_abs_sites <- 5

  # Earliest date
  min_year <- 1990

  # SPECIES filter
  species_filt <- c("lichen", "moss", "another species", "not naturalised in SA"
                     , "unidentified", "unverified", "dead", "annual forb"
                     , "annual grass", "annual tussock grass", "no id"
                    , "bold:"
                     )

  # Target rank
  rank_to_target <- "species"

  num_filt <- c("0"
                , "Not seen"
                )


  # stan settings
  good_chains <- 5
  good_iter <- 2000

  # 'include' where .... use_rel_dist < reliability < use_extra_rel_dist
  include_survey_nr <- c(15 # KI
                         , 23 # APY
                         , 24 # South Olary Plains
                         , 29 # SOUTH EAST
                         , 31 # MIDNORTH AND WEST MURRAY FLATS
                         , 48 # Rare rodents
                         #, 72 # SOOTY DUNNART KANGAROO IS
                         #, 82 # Coastal Dune and Clifftop
                         #, 83 # LOFTY BLOCK GRASSLANDS
                         , 84 # SE BOX & BULOKE WOODLANDS
                         , 85 #	DEEP SWAMP
                         , 90 # TILLEY SWAMP
                         , 94 # Sandy Desert
                         , 104 # Flinders Ranges
                         , 117 # Southern MLR
                         #, 119 # LAKE HAWDON
                         #, 408 # EP Th Fl monitoring
                         , 562 # NMM
                         #, 676 # KI Th Plants on roadsides
                         , 714 # Western Murray Mallee LAF
                         , 886 # CLMMM Bird and Veg
                         , 934 # Lowan mallee remnant veg
                         #, 958 # 	NORTHERN AMLR GRASSY ECOSYSTEM ASSESSMEN
                         #, 1067 # Alexandrina Council Th Acacias
                         , 1078 # CLMMM emergent veg
                         #, 1103 # BUFFEL GRASS CONTROL IN ARID RANGELANDS
                         #, 1089 # MARGINAL TO MAINSTREAM
                         #, 1239 # ADELAIDE PLAINS REMNANT VEGETATION
                         )


  #------Model------

  quant_probs <- c(0.05, 0.5, 0.95)

  test_years <- tibble::tribble(~type, ~year
                                , "reference", reference
                                , "recent", recent
                                ) %>%
    tidyr::unnest(cols = c(year))


  test_chains <- 3
  test_iter <- 1000

  use_chains <- 5
  use_iter <- 3000


  #--------Report-------

  do_data_summary <- T
  do_clean <- T
  do_include_effort <- T
  do_cooccur <- T

  make_report <- TRUE
  test_rmd <- FALSE

  make_short <- FALSE

  make_slides <- FALSE


  #-------Export--------

  export_results <- TRUE

  export_latest <- TRUE

  #--------Save settings-------

  save.image(settings_file)

