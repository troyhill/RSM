

getDSSdata <- function(station,  # one or more stations - as named in DSS files
                       dss,      # one or more DSS files - full addresses, with alternatives in named parent folders
                       alternatives = NULL, # a vector of length(dss) with alternative names. if NULL, names extracted from dss file addresses assuming they are in labeled folders
                       RSM_file = 'RSMBN', # everything in the filename before '_output.dss'; could be 'RSMGL' (LOSOM) or 'RSMGL_SD' (COP)
                       type = "FLOW", # type of measurement. TODO: allow vectors of length(stations)
                       category = "SIMULATED", # can also be 'INPUT' (for reg schedules)
                       endYear = 2016 # final year in model - to allow use on model periods ending in 2005
) {
  ### function pulls data from .dss files
  ### to use: 
  ### install 32-bit and 64-bit versions of Java SE Development Kit 8u301, as described here: https://stackoverflow.com/a/7604469/3723870
  ### switch to 32-bit R in RStudio
  ### install rJava
  ### get dssrip to install, either trying github or devtools::install_github("dss-rip","eheisman",args="--no-multiarch")
  
  
  ### get alternative names from dss addresses (should be parent folder for dss file)
  if (is.null(alternatives)) {
    alt.names <- sapply(strsplit(dss, "/"), "[", 2)
  } else {
    alt.names <- alternatives
    if (!(length(alternatives) == length(dss))) {
      stop("The number of dss files provided differs from the number of stated alternatives. These quantities must be the same length and in the same order; each dss file must have a corresponding alternative name (or be in a parent folder labeled with the alternative name).\n")
    }
  }
  
  q.dat <- data.frame()
  for(j in 1:length(dss)){
    tryCatch( # attempt to handle failure to open the dss file
      {
        dss_out <- dssrip::opendss(dss[j])},
      error=function(cond) {
        message("error message:")
        message(cond)
        # # Choose a return value in case of error
        dss_out$close()
      },
      warning=function(cond) {
        message("original warning message:")
        message(cond)
        # # Choose a return value in case of warning
        dss_out$close()
      }, 
      finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you 
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>' 
        # next
      }
    )
    
    
    for(i in 1:length(station)){
      paths.tmp <- paste0("/", RSM_file, "/", station[i],"/", toupper(type), "/01JAN1965 - 01JAN", endYear, "/1DAY/", toupper(category), "/")  
      tryCatch(
        {
          tmp       <- data.frame(dssrip::getFullTSC(dss_out, paths.tmp))
          tmp$date  <- as.POSIXct(rownames(tmp), format = "%Y-%m-%d")
          rownames(tmp) <- NULL
          tmp$stn   <- station[i]
          tmp$alt   <- alt.names[j]
        },
        error=function(cond) {
          message("error message:") # 'non-numeric argument to binary operator' = no data for station
          message(cond)
          # next
        },
        warning=function(cond) {
          message("original warning message:")
          message(cond)
          # next
        }, 
        finally={
          # NOTE:
          # Here goes everything that should be executed at the end,
          # regardless of success or error.
          # If you want more than one expression to be executed, then you 
          # need to wrap them in curly brackets ({...}); otherwise you could
          # just have written 'finally=<expression>' 
          # next
        }
      )
      if (exists("tmp")) {
        q.dat     <- rbind(tmp,q.dat)
        rm(tmp) # clean up
      } 
      cat(alt.names[j], "progress:", round(i/length(station) * 100), "%\n")
    }
  }
  return(q.dat)
}



