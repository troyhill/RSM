### script pulls dss data using 32-bit R
### and saves it as an .RData file in the parent folder
### Usage: 
# system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", 
#               shQuote("C:\\RDATA\\EVER_misc\\script_pullDSS_20211006.R"), 
#               " --args G:\\data\\models\\LOSOM\\Iteration_3\\sensitivity STAGE LOK")) #, wait = FALSE, invisible = FALSE)




### to install dssrip R package:
### install 32-bit and 64-bit versions of Java SE Development Kit 8u301, as described here: https://stackoverflow.com/a/7604469/3723870
### switch to 32-bit R in RStudio
### install rJava
### get dssrip to install, either trying github or devtools::install_github("dss-rip","eheisman",args="--no-multiarch")


# get command line arguments ----------------------------------------------

vargs <- commandArgs(trailingOnly = TRUE)

### args should be:
### 
# vargs <- strsplit("--args G:\\data\\models\\LOSOM\\Iteration_3\\sensitivity STAGE LOK,WCA1", " ")[[1]]
# vargs <- strsplit("--args G:\\data\\models\\LOSOM\\Iteration_3\\sensitivity STAGE LOK,WCA1 INPUT", " ")[[1]]

print(vargs)

parentFolder <- vargs[2]
dataType     <- vargs[3] # "FLOW" or "STAGE" # would use toupper() but maybe some datatypes are case sensitive
stations     <- strsplit(vargs[4], ",")[[1]] # remove all whitespace
categoryType <- ifelse(length(vargs) == 5, vargs[5], 'SIMULATED')



### this is important - set to DSSVue location before loading dssrip
options(dss_override_location = "C:\\Program Files (x86)\\HEC\\HEC-DSSVue-v3.0.00.212\\")

library(dssrip)

# myFile <- opendss("G:\\data\\models\\LOSOM\\Iteration_3\\NA25\\RSMBN_output.dss")
# dat$lok_flow_south <- dat$S351 + dat$S354


# source("C:/RDATA/EVER_misc/script_functions_20210913.R") # loads getDSSdata()
getDSSdata <- function(station,  # one or more stations - as named in DSS files
                       dss,      # one or more DSS files - full addresses, with alternatives in named parent folders
                       alternatives = NULL, # a vector of length(dss) with alternative names. if NULL, names extracted from dss file addresses assuming they are in labeled folders
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
    dss_out <- dssrip::opendss(dss[j])  
    
    for(i in 1:length(station)){
      paths.tmp <- paste0("/RSMBN/", station[i],"/", toupper(type), "/01JAN1965 - 01JAN", endYear, "/1DAY/", toupper(category), "/")  
      tmp       <- data.frame(dssrip::getFullTSC(dss_out, paths.tmp))
      tmp$date  <- as.POSIXct(rownames(tmp), format = "%Y-%m-%d")
      rownames(tmp) <- NULL
      tmp$stn   <- station[i]
      tmp$alt   <- alt.names[j]
      q.dat     <- rbind(tmp,q.dat)
      cat(alt.names[j], "progress:", round(i/length(station) * 100), "%\n")
    }
  }
  return(q.dat)
}



# get alternative names ----------------------------------------------
alts      <- list.files(parentFolder, pattern = 'RSMBN_output\\.dss', recursive = TRUE, full.names = TRUE)
n.alts    <- length(alts)
alt.names <- sapply(strsplit(alts, "/"), "[", 2)


# pull data ---------------------------------------------------------------

dat <- getDSSdata(station = stations, type = dataType, dss = alts, category = categoryType) # output format? a long dataframe (results rbinded)
names(dat) <- tolower(names(dat))


# save data: note type, date ----------------------------------------------

save(dat, file = paste0(parentFolder, "\\", format(Sys.Date(), format = "%Y%m%d"), "_", dataType, ".RData")) #"C:/RDATA/LOSOM/iter3/iter3_dss.RData")
### user can rename the .RData file.
