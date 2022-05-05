### dssrip workflow for 32-bit R command line
### dssrip example works in R i386 v.4.0.5
### this stopped working from 64-bit R (libraries not loading)


options(dss_override_location = "C:\\Program Files (x86)\\HEC\\HEC-DSSVue-v3.0.00.212\\")

library(dssrip)
library(RSM)
# myFile <- opendss("G:\\data\\models\\COP\\ALTQ\\RSMGL_SD_output.dss")

parentFolder <- "G:\\data\\models\\COP\\"
RSM_type     <- "RSMGL"
dataType     <- "FLOW"
stns         <- 'S12A,S12A_WEIR,S12B,S12B_WEIR,S12C,S12D,S333,S333N,S355A,S355B,S356,S334,S11A,S11B,S11C,S9,S9A,S9XN,S9XS,S344,S343A,S343B'
stations     <- strsplit(stns, ",")[[1]] # remove all whitespace
endYear      <- 2005 # 2005 or 2016, whatever the last year in the time series is
categoryType <- 'SIMULATED' # can also be 'INPUT' (for reg schedules)


source(system.file("extdata/scripts/getDSSdata.R", package = "RSM"))

# extractDSS(parentFolder = 'G:\\data\\models\\COP\\ALTQ\\',
#            RSM_type     = 'RSMGL',
#            dataType     = 'FLOW',
#            stations     = 'S12A,S12A_WEIR,S12B,S12B_WEIR,S12C,S12D,S333,S333N,S355A,S355B,S356,S334,S11A,S11B,S11C,S9,S9A,S9XN,S9XS,S344,S343A,S343B',
#            category     = 'SIMULATED',
#            script       = system.file("extdata\\scripts","script_extractDSS.R",package="RSM"),
#            rVersion     = "C:/PROGRA~1/R/R-40~1.5",
#            libraryLoc = "C:\\Users\\tdh\\Documents\\R\\win-library\\4.0")

### equivalent from r 32 bit command line

# get alternative names ----------------------------------------------
alts      <- list.files(parentFolder, pattern = paste0(RSM_type, '.*_output\\.dss'), recursive = TRUE, full.names = TRUE)
n.alts    <- length(alts)
alt.names <- sapply(strsplit(alts, "/|\\\\"), "[", length(strsplit(alts, "/|\\\\")[[1]])-1) # base directory must have alternative name

### get text in the filename after before '_output.dss'; could be 'RSMGL' (LOSOM) or 'RSMGL_SD' (COP)
### use lapply if this naming convention doesn't stay the same for all dss files
RSM_type_mod <- tail(strsplit(alts, split = "/|\\\\|_output.dss")[[1]], 1) # COP .dss files have data labeled with 'RSMGL_SD'

# pull data ---------------------------------------------------------------
dat <- getDSSdata(station = stations, RSM_file = RSM_type_mod, type = dataType, dss = alts, 
                  endYear = endYear, category = categoryType) # output format? a long dataframe (results rbinded)
names(dat) <- tolower(names(dat))


