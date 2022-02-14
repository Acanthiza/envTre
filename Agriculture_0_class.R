

  # Totally new run?
  # Can be T (new), F (use last) or provide out_dir (e.g. "2021-11-22-1543")
  new_run <- T

  # What is the area of interest (AOI) for this analysis?

  aoi_polys <- "zone"
  aoi_name <- "Agriculture" # used in output paths
  aoi_fullname <- "agricultural zone"
  aoi_col <- "PRODUCTION"
  aoi_type <- "agricultural zone (roughly south of Goyder's line)" # used in text description of aoi boundaries

  poly_buf <- 0

  toi <- "class"

  filt_toi <- "Aves"

  run_from <- 0
  run_to <- 100

  source("0000_run.R")
