 
 #---------To github--------
  
  if(nrow(gert::git_status() > 1)) {
    
    git_commit_env(paste0(gsub("out\\/","",out_aoi),"_",basename(out_dir)))
    
  }
  
  
  #------To network-------

  base_network_path <- path("//env.sa.gov.au/dfsroot/IST/DEHProjects/Landscapes/envTre")
  to_network_path <- path(base_network_path,out_dir)
  
  latest_network <- path(dirname(to_network_path),paste0("latest"))
  latest_local <- path_wd(out_runs,paste0("latest"))

  filesRegex <- c("report.docx"
                  , "slides.html"
                  , "short.docx"
                  )

  files_to_copy <- tibble(path = fs::dir_ls(out_dir
                                            , regexp = paste0(filesRegex,collapse = "|")
                                            )
                          ) %>%
    dplyr::filter(!grepl("~",path)) %>%
    dplyr::mutate(to_network_path = path(to_network_path,basename(path))
                  , latest_network = if_else(grepl("tif", path)
                                             , path(latest_network, gsub("\\/.*tif" ,"eco.tif", to_network_path))
                                             , path(latest_network, basename(to_network_path))
                                             )
                  , latest_local = if_else(grepl("tif", path)
                                          , path(latest_local, gsub("\\/.*tif", "eco.tif", to_network_path))
                                          , path(latest_local, basename(to_network_path))
                                          )
                  )

  
  if(export_results) {
    
    if(file.exists(base_network_path)) {
      
      dir_create(to_network_path)
      
      walk2(files_to_copy$path
            , files_to_copy$to_network_path
            , file.copy
            , overwrite = FALSE
            )
      
    }
    
  }


  #-------To latest-------
  
  if(export_latest) {
    
    if(file.exists(dirname(latest_network))) {
      
      if(file.exists(latest_network)) {
        
        files_to_delete <- dir_ls(latest_network
                                , regexp = paste0(filesRegex,collapse = "|")
                                )
        
        walk(files_to_delete
             , file_delete
             )
        
        } else dir_create(latest_network)
      
      walk2(files_to_copy$path
            , files_to_copy$latest_network
            , file_copy
            , overwrite = TRUE
            )
      
    }
  
    if(file.exists(dirname(latest_local))) {
      
      if(file.exists(latest_local)) {
        
        files_to_delete <- dir_ls(latest_local
                                  , regexp = paste0(filesRegex,collapse = "|")
                                  )
        
        walk(files_to_delete
             , file_delete
             )
        
      } else dir_create(latest_local)
      
      walk2(files_to_copy$path
            , files_to_copy$latest_local
            , file_copy,overwrite = TRUE
            )
      
    }
  
}

  