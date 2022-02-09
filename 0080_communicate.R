

#-------make book-------

  if(make_report) {
    
    unlink(c("report/_main.Rmd"
             , "packages.bib"
             )
           )
    
    xfun::in_dir("report"
                 , bookdown::render_book("000_setup.Rmd")
                 )
    
    file_copy(path("report", "_book", "_main.docx")
              , path(out_dir, "report.docx")
              , overwrite = TRUE
              )
    
  }
  

#-------make short-------
  
  if(make_short) {
    
    unlink(c("short/_main.Rmd"
             , "packages.bib"
             )
           )
    
    xfun::in_dir("short"
                 , bookdown::render_book("short.Rmd")
                 )
    
    file_copy(path("short", "_book", "_main.docx")
              , path(out_dir, "short.docx")
              , overwrite = TRUE
              )
    
  }


#-------make slides---------

  if(make_slides) {
    
    unlink(c("slides/_main.Rmd"
             , "packages.bib"
             )
           )
    
    xfun::in_dir("slides"
                 , bookdown::render_book("slides.Rmd")
                 )
    
    file_copy(path("slides", "_main.html")
              , path(out_dir, "slides.html")
              , overwrite = TRUE
              )
    
  }


  

  
  