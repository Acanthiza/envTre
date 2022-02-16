

  # Totally new run?
  # Can be T (new), F (use last) or provide out_dir (e.g. "2021-11-22-1543")
  new_run <- T

  # What is the area of interest (AOI) for this analysis?

  aoi_polys <- "sa"
  aoi_name <- "SA" # used in output paths
  aoi_fullname <- "South Australia"
  aoi_col <- "State"
  aoi_type <- "state boundary" # used in text description of aoi boundaries

  poly_buf <- 50000

  toi <- "class"

  filt_toi <- c("Aves", "Mammalia")

  # Years at which to predict (and compare change)
  recent <- as.numeric(format(Sys.time(), "%Y")) - 5
  reference <- recent - 10



  # Which files to source?
  run_from <- 0
  run_to <- 100

  source("0000_run.R")
