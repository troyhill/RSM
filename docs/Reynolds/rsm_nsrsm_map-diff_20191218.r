rm(list=ls(all=TRUE))

# Code to produce Everglades differences maps comparing natural system (nsrsm) and rsm depths, stages, hydroperiods or dry-down duration. Code also may be used to compare two rsm runs.
# Written by Gregg Reynolds, Hydrologist, Everglades National Park
# 11 December 2019

# Notes: This code was modified at request of Everglades Foundation to produce difference maps (stage/depth/hydroperiods, etc.) from models using different meshes and cellIDs.
#        Request was to compare natural system model (nsrsm) depths with existing conditions (ECB19RR). Expectation is that difference will be computed as nsrsm minus rsm. 
#        There is no area weighting of topo or depths from a polygon overlay. Results represent a simple cellID to cellID overlay, based on the location of cell centroids (rsm cell IDs mapped to nsrsm cell IDs).
#        This code has not undergone rigorous testing for use with all available rsm output. Code is not guaranteed to work for all nsrsm/rsm model output. All maps produced using this code should be evaluated for reasonableness.
 

pkgs <- c("plyr", "dplyr", "data.table", "quantreg", "timeSeries", "sig", "assertive", "stringr", 
          "ncdf4", "sp", "maptools", "rgdal", "maps", "shapefiles", "mapdata", "rgeos", "PBSmapping", 
          "RColorBrewer")
install.packages(pkgs[!pkgs %in% installed.packages()])
lapply(pkgs, require, character.only = TRUE)


#library(lattice)  # not used by this script
#library(survival) # contributes the date/time functions date.mdy and mdy.date; dates are handled in sourced scripts/functions.
#library(RNetCDF)  # commented out; NetCDF reading and processing is done in sourced functions.
#library(date)     # dates are handled in sourced scripts/functions.
#library(ggplot2)  # ggplot2 plotting package is not used for difference maps
#library(reshape2) # used in scripts needing "melt" function
#library(scales)   # used for log, frequency etc. axes scales, not used for maps
#library(arrayhelpers) # used to simplify conversion of multidimensional arrays to dataframes (w/array2df) or vectors
###################################


source( "R/DataExtract.R" )              # Author: J. Park
source( "R/ProcessDSS.R" )               # Author: J. Park
source( "R/ProcessNetCDF.R" )            # Author: J. Park
source( "R/unitConversion.R" )           # Author: G. Reynolds
source( "R/netCDF_Extract.R" )           # Author: J. Park
source( "R/NetCDF_IR_Cell_Data.R" )      # Author: J. Park
source( "R/ExtractCellData.R" )          # Author: J. Park
source( "R/unitConversion.R" )           # Author: J. Park

# pass as arguments: "stn_dt" file, base directory, max value in color ramp (may be "null"; hard coded not to exceed 3.3 ft)  #
###############-- DATA ENTRY by passing arguments for command line execution --########################################################
#Note: execute with Rscript, e.g. Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_ALTN2_ECB19RR_depth_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_ALTN2_ECB19RR_depth_average_1965.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_IORBL_depth_max.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_H1115_discont-hp_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_WECBR_discont-hp_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_IORBL_stage_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_WECBR_depth_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_WECBR_apr_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_WECBR_oct_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_WECBR_cont-hp_average.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_ALTQ_ALTO_discont-drydown_-1.5-ftOffset_annual.csv /opt/physical null
# or                              Rscript rsm_nsrsm_map-diff_fork_11Dec2019_fixedRamp.r ./CTRLfiles/mapdiff_WALTH_WECBR_depth_0-ftOffset_2005-5-15-2005-5-16.csv /opt/physical 1.5

# skip following lines if passing arguments from IDE (e.g. RStudio)
args <- commandArgs(trailingOnly = TRUE)

if (length(args)<3) {
  stop("3 arguments must be supplied.n", call.=FALSE)
}
cat(args, sep = "\n")

# pass arguments from IDE (e.g. RStudio):
#args <- c('./CTRLfiles/mapdiff_WALTH_WECBR_depth_0-ftOffset_2005-5-15-2005-5-16.csv', '/opt/physical', 1.5)
#args <- c('./CTRLfiles/mapdff_IORBL_ECB19RR_depth_0-ftOffset_1966-4-1-1966-4-30.csv', '/opt/physical', 'null')
#args <- c('./CTRLfiles/mapdff_ALTO_ECB19RR_depth_0-ftOffset_1966-4-1-1966-4-30.csv', '/opt/physical', 'null')
#args <- c('./CTRLfiles/mapdiff_nsrsm_ECB19RR_depth_0-ftOffset_annual.csv', '/opt/physical', 'null')
#args <- c('./CTRLfiles/mapdiff_nsrsm_ECB19RR_stage_0-ftOffset_annual.csv', '/opt/physical', 'null')
#args <- c('./CTRLfiles/mapdiff_ALTQ_ECB19RR_cont-hp_0-ftOffset_annual.csv', '/opt/physical', 'null')
#args <- c('./CTRLfiles/mapdiff_nsrsm_ECB19RR_cont-hp_0-ftOffset_annual.csv', '/opt/physical', 'null')

stnDTfile  <- paste0(getwd(), "/inst/extdata/CTRLfiles/mapdiff_nsrsm_ECB19RR_cont-hp_0-ftOffset_annual.csv")
baseDir    <- tempdir()
maxRamp    <- args[3]                 # use 'null' if unrestricted color ramp range

if(maxRamp != 'null') {

   maxRamp <- as.numeric(maxRamp) - 0.1  # 0.1 or 0.2 will be added back later depending on if odd or even
}

graph_type <- 'mapdiff'

# read list of alts, start date and end date of seasons, stations, stat #
stnDT <- read.csv(stnDTfile, header = TRUE, sep = ",")

modelList       <- as.factor(stnDT$model)
modelVec        <- as.vector(modelList)
verList         <- as.factor(stnDT$ver)
altList         <- as.factor(stnDT$alt)
stationList     <- as.factor(stnDT$station)
stationtypeList <- as.factor(stnDT$stationtype)
datatypeList    <- as.factor(stnDT$datatype)
offsetList      <- stnDT$offset
seasonList      <- as.factor(stnDT$season)
statList        <- as.factor(stnDT$stat)
unitsList       <- as.factor(stnDT$units)
dataSourceList  <- as.factor(stnDT$dataSource)

POR      <- 0
i        <- 0
pptUnits <- 'in'  # may be changed later in code

dssPath <- paste0(getwd(), "inst/extdata/DSSVue_python") # normalizePath('../DSSvue_python/')
outPath <- tempdir() # normalizePath('../../temp/')

# note: input will already include offset
FUNhp      <- function(x) {
  test     <- is_positive(max(x))
  if(test == TRUE) {
    rles   <- rle((x) > 0)
    result <- max(rles$lengths[rles$values], na.rm = FALSE)
  } else {
    result <- 0
  }
  return(result)
}

FUNdrydown <- function(x) {
  offset   <- 0  # input already includes offset
  test     <- is_positive(offset - min(x))
  if(test == TRUE) {
    rles   <- rle((offset-x) > 0)
    result <- max(rles$lengths[rles$values], na.rm = FALSE)

  } else {
    result <- 0 # never below threshold/ground, wet all year
  }
  return(result)
}

##-- for every row of stn_dt list --##
for (j in 1:length(verList)) {
  
  dataDir <- paste(baseDir,'/models/',modelList[j],'/',verList[j],'/',altList[j],'/output', sep='')

  #- following only used for dss output (dataSourceList[j] == "dss") -#
  if(tolower(stationtypeList[j]) == "transect") {
    dssIn <- 'transect_flows.dss'

  } else if(tolower(modelList[j]) == 'rsmbn') {
     dssIn <- 'RSMBN_output.dss'

  } else {

    destfile <- paste(dataDir,'/RSMGL_CEPP_output.dss', sep='')
    print(paste('destfile = ',destfile, sep=''))

    if(file.exists(destfile)) {
      dssIn  <- 'RSMGL_CEPP_output.dss'  # note: this could change
    } else {
      dssIn  <- 'RSMGL_SD_output.dss'    # note: this could change
    }
  }
  
  dataType <- toupper(datatypeList[j])  # for dss
  stn      <- stationList[j]
  statFlag <- statList[j]

################################################################################################
  # start and end dates from control file e.g. 1965-5-1-2005-4-30
  startEndDefine <- unlist(strsplit(as.character(seasonList[j]),"-"))

  start.date     <- as.Date(paste(startEndDefine[1],'-',startEndDefine[2],'-',startEndDefine[3],sep=''))
  end.date       <- as.Date(paste(startEndDefine[4],'-',startEndDefine[5],'-',startEndDefine[6],sep=''))
  print('in master script start & end dates')
  print(start.date)
  print(end.date)
#################################################################################################
  
  dataDir      <- paste(baseDir,'/models/',modelList[j],'/',verList[j],'/',altList[j],'/output', sep='')
  cellListFile <- paste(modelList[j],verList[j],'indicatorRegion.csv', sep='')

  if(dataSourceList[j] == "dss") {

    system(paste("cat ",dssPath,'/ExtractDssTemplate.ini ','| sed "s:dataDir:',dataDir,':g" | sed "s:tmpDir:',outPath,':g" | sed "s:inFile:',dssIn,':g" | sed "s:dataType:',dataType,':g" | sed "s:stn:',stn,':g" > ',dssPath,'/ExtractDss.ini', sep=""))
    
  }
  
  if(datatypeList[j] == "depth" | datatypeList[j] == 'cont-hp' | datatypeList[j] == 'discont-hp' | datatypeList[j] == 'cont-drydown' | datatypeList[j] == 'discont-drydown' ) {
    land.surface.datum <- TRUE
    variableDT         <- 'ComputedHead'

  } else if (datatypeList[j] == "stage" & dataSourceList[j] != "csv") {
    land.surface.datum <- FALSE

    variableDT         <- 'ComputedHead'
    if(dataSourceList[j] == "dss") {  # note: currently datatype is case sensitive
      variableDT       <- toupper(datatypeList[j])

    }

  } else {
    land.surface.datum <- FALSE
    variableDT         <- datatypeList[j]
  }

  rainFall <- FALSE
  
  DF <- DataExtract(
    data.source        = dataSourceList[j],  # or 'DSS'
    data.type          = datatypeList[j],   # stage, flow, vector
    station.type       = stationtypeList[j],      # cell, gauge, structure, IR, transect
    path.dss.ini       = dssPath,
    file.dss.ini       = '/ExtractDss.ini',
    path.dss.py        = dssPath,
    file.dss.py        = '/ExtractDss.py',
    #data.path          = paste(baseDir,'/models/rsm',sep=''),
    data.path          = paste(baseDir,'/models/',modelVec[j],sep=''),
    model.version      = verList[j],
    model.alternative  = altList[j],
    indicator.region   = as.character(stationList[j]),
    file.in            = "Y:/troy/RDATA/RSM_supplemental/rsm_COP_ECB19RR/globalmonitors.nc", # paste0(getwd(), '/inst/extdata/globalmonitors.nc'), # RSMGL_CEPP_output.dss, transect_flow.dss, etc.
    variable           = variableDT,
    land.surface.datum = land.surface.datum,
    rainFall           = rainFall,
    datumOffset        = offsetList[j],
    file.out           = 'dataVals.csv',
    out.path           = '',
    #start.date         = NULL, # '1965-01-01',
    #end.date           = NULL, # '2005-12-31',
    start.date         = start.date,
    end.date           = end.date,
    IR.cell.list.path  = '../../listsDB',
    IR.cell.list.file  = cellListFile,
    cell.list          = cell.list,
    indexOffset        = TRUE, # Add +1 to NetCDF cell indices?
    digits.precision   = 3, 
    DEBUG              = TRUE
  )
  
  endYR <- as.integer(unlist(strsplit(as.character(tail(DF$Date, 1)),'-'))[1])
  
  seasonDefine      <- unlist(strsplit(as.character(seasonList[j]),"-"))
  seasonBegin       <- as.Date(paste(seasonDefine[1],'-',seasonDefine[2],'-',seasonDefine[3], sep=''))
  seasonbeginDate   <- c(as.integer(seasonDefine[1]),as.integer(seasonDefine[2]),as.integer(seasonDefine[3]))

  # yyyy-mm-dd to yyyy-mm-dd
  if(as.integer(seasonDefine[2]) > as.integer(seasonDefine[5])) {
     # season ends in following year #
     print('test:')
     seasonEnd      <- as.Date(paste(as.integer(seasonDefine[1])+1,'-',seasonDefine[5],'-',seasonDefine[6], sep=''))
       print('end test:')
     seasonendDate  <- c((as.integer(seasonDefine[1]))+1,as.integer(seasonDefine[5]),as.integer(seasonDefine[6])) 
  } else {
     # season ends in same year #
     seasonEnd      <- as.Date(paste(seasonDefine[1],'-',seasonDefine[5],'-',seasonDefine[6], sep=''))
     seasonendDate     <- c(as.integer(seasonDefine[1]),as.integer(seasonDefine[5]),as.integer(seasonDefine[6]))
  }

  #print('')
  #print(seasonBegin)
  print(seasonEnd)
  print('') 

  if(as.integer(seasonDefine[2]) > as.integer(seasonDefine[5])) {
    endYR <- endYR - 1 
  }

###############################################################################################################
if(paste(seasonDefine[2],'/',seasonDefine[3]) == paste(seasonDefine[5],'/',seasonDefine[6]) & seasonDefine[1] == seasonDefine[4])      {

   seasonLegend  <- paste(seasonDefine[2],'/',seasonDefine[3],' (',seasonDefine[1],')',sep='')

}  else if(paste(seasonDefine[2],'/',seasonDefine[3]) == paste(seasonDefine[5],'/',seasonDefine[6])) {

      seasonLegend  <- paste(seasonDefine[2],'/',seasonDefine[3],' (',seasonDefine[1],'-',seasonDefine[4],')',sep='')

}  else if(seasonDefine[1] == seasonDefine[4]) {

      seasonLegend  <- paste(seasonDefine[2],'/',seasonDefine[3],'-',seasonDefine[5],'/',seasonDefine[6],' (',seasonDefine[1],')',sep='')

}  else {

      seasonLegend  <- paste(seasonDefine[2],'/',seasonDefine[3],'-',seasonDefine[5],'/',seasonDefine[6],' (',seasonDefine[1],'-',seasonDefine[4],')',sep='')
}

###############################################################################################################

  POR  <- endYR - as.integer(seasonDefine[1]) +1
  
  YRstart <- as.integer(seasonDefine[1]) -1   # will be incremented in loop
  YRend   <- seasonendDate[1] -1   # will be incremented in loop
  
  for (i in 1:POR) {
      
    YRstart <- YRstart + 1
    YRend   <- YRend + 1

    seasonbeginDate[1] <- c(YRstart)
    seasonendDate[1]   <- c(YRend)
    seasonBegin        <- as.Date(paste(YRstart,'-',seasonDefine[2],'-',seasonDefine[3], sep=''))
    seasonEnd          <- as.Date(paste(YRend,'-',seasonDefine[5],'-',seasonDefine[6], sep=''))

    if ( as.integer(as.character(seasonDefine[2])) >= 6 ) {
      climYR <- YRend
    } else {
      climYR <- YRstart
    }
    
    print('')
    print(seasonBegin)
    print(seasonEnd)
    print('')

    test <- DF[DF$Date %in% seasonBegin:seasonEnd, ]  # dates should be incremented 1 year at a time

    # now compute metric for every column #
    
    dataVals <- test[,2:length(colnames(test))]

    ##############################################
    if ( tolower(datatypeList[j]) == "cont-hp" ) {
      valueDF <- as.data.frame(sapply(dataVals, function(x) FUNhp(as.vector(x)) ))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')      
      
    } else if ( tolower(datatypeList[j]) == "discont-hp" ) {
      valueDF <- as.data.frame(sapply(dataVals, function(x) length(which(x > 0)) ))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')
      
    } else if ( tolower(datatypeList[j]) == "cont-drydown" ) {
      valueDF <- as.data.frame(sapply(dataVals, function(x) FUNdrydown(as.vector(x)) ))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')   
      
    } else if ( tolower(datatypeList[j]) == "discont-drydown" ) {
      valueDF <- as.data.frame(sapply(dataVals, function(x) length(which(x <= 0)) ))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')
      
    } else if ( tolower(statList[j]) == "max" ) {
      valueDF <- as.data.frame(apply(dataVals, 2, max))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')
      
    } else if ( tolower(statList[j]) == "min" ) {
      valueDF <- as.data.frame(apply(dataVals, 2, min))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')
      
    } else if ( tolower(statList[j]) == "sum" ) {
      valueDF <- as.data.frame(apply(dataVals, 2, sum))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')
      
    } else {
      valueDF <- as.data.frame(apply(dataVals, 2, mean))
      setDT(valueDF, keep.rownames = TRUE)[]
      colnames(valueDF) <- c('station','value')
      
    }   

############################################################################################
    #valueDF$year         <- YRstart
    valueDF$year         <- climYR
    valueDF$model        <- modelList[j]
    valueDF$version      <- verList[j]
    valueDF$alt          <- altList[j]
    #valueDF$station      <- stationList[j]
    valueDF$stationtype  <- stationtypeList[j]
    valueDF$datatype     <- datatypeList[j]
    valueDF$season       <- seasonList[j]
    valueDF$stat         <- statList[j]    
    valueDF$seasonLegend <- as.factor(seasonLegend)
    valueDF$pp           <- -99  # not used for maps

    ######################################################
    cf <- unitConversion(
      #data.type          = datatypeList[j],   # stage, flow, vector
      data.source        = dataSourceList[j],
      #variable           = datatypeList[j],
      station.type       = stationtypeList[j],
      units              = unitsList[j],
      graph_type         = graph_type,
      #aggTEST            = aggTEST,
      digits.precision   = 3, 
      DEBUG              = FALSE
    )
    valueDF$value <- valueDF$value * cf
    #####################################################
    
    if(i == 1) {
      valueDFfullalt <- valueDF                        # valueDFfullalt resets for each curve 
    } else {
      valueDFfullalt <- rbind(valueDFfullalt,valueDF)  # add each year as it is computed
    }

  }
  
  if(j == 1) {
    valueDFfull <- valueDFfullalt                                          # create valueDFfull for 1st curve 
  } else {
    valueDFfull <- rbind(valueDFfull,valueDFfullalt)
  }

}

valueDFfull$station <- as.factor(valueDFfull$station)

plotValuesTMP <- aggregate(valueDFfull$value, list(station=valueDFfull$station,valueDFfull$alt), mean) #mean of annual values for each alt
plotValues1   <- subset(plotValuesTMP, plotValuesTMP$Group.2 == altList[1])
plotValues2   <- subset(plotValuesTMP, plotValuesTMP$Group.2 == altList[2])                            #sort order should be same for each alt
plotValues    <- plotValues1

plotValues1$CELLID <- str_split_fixed(plotValues1$station, "_", 2)[,2]
plotValues1 <- data.frame(CELLID=plotValues1$CELLID, value1=plotValues$x)

plotValues2$CELLID <- str_split_fixed(plotValues2$station, "_", 2)[,2]
plotValues2 <- data.frame(CELLID=plotValues2$CELLID, value2=plotValues2$x)

plotValues$CELLID <- str_split_fixed(plotValues$station, "_", 2)[,2]
plotValues <- data.frame(CELLID=plotValues$CELLID, value=plotValues$x)

if(tolower(modelList[1]) == "nsrsm" ) {
  grid <- readOGR(paste(baseDir,'/gis/models/',modelList[1],'/',verList[1], sep=''),"nsrsm_mesh_landuse")
  
} else {      
  grid <- readOGR(paste(baseDir,'/gis/models/',modelList[1],'/',verList[1], sep=''),"mesh")
}

grid <- spTransform(grid, CRS("+proj=utm +zone=17 +datum=NAD83"))

#####################################################################################################################
grid1 <- grid    # grid1 should be nsrsm if not comparing two rsm runs

if(tolower(modelList[2]) == "nsrsm" ) {
  grid2 <- readOGR(paste(baseDir,'/gis/models/',modelList[2],'/',verList[2], sep=''),"nsrsm_mesh_landuse")
  
} else {      
  grid2 <- readOGR(paste(baseDir,'/gis/models/',modelList[2],'/',verList[2], sep=''),"mesh")
}

grid2 <- spTransform(grid2, CRS("+proj=utm +zone=17 +datum=NAD83"))

#   note: if using other shapefiles, some shapefiles column headings will be full caps, in other shapefiles column headings have a mix of upper and lower case  #

grid1col <- match("cellid", tolower(colnames(as.data.frame(grid1))))
grid2col <- match("cellid", tolower(colnames(as.data.frame(grid2))))

#grid1colName <- colnames(as.data.frame(grid1))[grid1col]
#grid2colName <- colnames(as.data.frame(grid2))[grid2col]

colnames(grid1@data)[grid1col] = "CELLID"
colnames(grid2@data)[grid2col] = "CELLID"

# "@data" is not needed in following lines; i.e. "grid1$CELLID" and "grid1@data$CELLID" are identical #
grid1$CELLID <- as.numeric(as.character(grid1$CELLID))  # grid1 should be nsrsm if map depicts difference from nsrsm
grid2$CELLID <- as.numeric(as.character(grid2$CELLID))

# if capitalization of "CellId" is known, above code can be simplified to:
#grid1$CELLID <- as.numeric(as.character(grid1$CellId))  # grid1 should be nsrsm if map depicts difference from nsrsm
#grid2$CELLID <- as.numeric(as.character(grid2$CellId))

PMgridpointstmp <- merge(grid1, plotValues1, duplicateGeoms = T) # "duplicateGeoms" may not be necessary

PMgridpoints1 <- PMgridpointstmp  # grid1 should be nsrsm if not comparing two rsm runs

#   note: grid1 and grid2 column headings may not match  #
PMgridpointstmp <- merge(grid2, plotValues2, duplicateGeoms = T)


PMgridpoints2 <- PMgridpointstmp

# 1) find centroids of rsm cells (2nd model mesh)
# 2) overlay rsm cell centroids on nsrsm mesh (mesh of 1st model; could also be rsm mesh)
# 3) map rsm cell IDs (2nd mesh) to nsrsm cell (1st mesh)

cellCentroid <- gCentroid(grid2,byid=TRUE) # SpatialPoints centroids for each rsm cell
grid1ID      <- over(cellCentroid, grid1)  # extract grid1 (nsrsm) attributes at location of rsm cell centroids 
                                           # joins attributes of latter (nsrsm) to former (rsm) based on spatial location (like a transparency overlay)
grid1ID      <- over(cellCentroid, grid1)  # extract grid1 (nsrsm) attributes at location of rsm cell centroids 

grid2IDdf      <- data.frame(CELLID1=grid1ID$CELLID) # nsrsm cellIDs @ rsm cell centroids; this step is not necessary, just done to remove unneeded data/clean up

row.names(grid2IDdf) <- row.names(grid1ID)
gridBind2x    <- spCbind(grid2,grid2IDdf)     # combine grid2 (rsm) and grid1 attributes (nsrsm cellIDs) at location of grid2 (rsm) cell centroids

cellCents <- subset(gridBind2x, CELLID != "NA")   # CELLID is nsrsm cell; CELLID.1 & Cellid are corresponding rsm; Topo_avg is nsrsm, Pst_Topo is RSM
cellCents <- subset(cellCents, CELLID1 != "NA")

rsm2nsrsmTMP  <- as.data.frame(cbind(CELLID2=cellCents$CELLID, CELLID1=cellCents$CELLID1)) # CELLID is grid2 rsm mesh; CELLID2 is grid1 nsrsm or rsm mesh

rsm2nsrsm     <- rsm2nsrsmTMP[rsm2nsrsmTMP$CELLID2 %in% grid2$CELLID, ]  # remove grid2 cells assigned numbers !=cellID from overlay
rsm2nsrsmUniq <- rsm2nsrsm[ !duplicated(rsm2nsrsm[ , 1] ) , ]        # only one nsrsm cell can be mapped to each rsm cell; this does not necessarily find best overlay of 1st mesh and 2nd

crossWalkNumeric  <- rsm2nsrsmUniq
crossWalk         <- rsm2nsrsmUniq
crossWalk$CELLID1 <- as.factor(crossWalk$CELLID1)
crossWalk$CELLID2 <- as.factor(crossWalk$CELLID2)

# clear memory #
rm(rsm2nsrsm)
rm(rsm2nsrsmUniq)
rm(plotValues1)

tmpDF <- data.frame(PMgridpoints1$CELLID,PMgridpoints1$value1)
colnames(tmpDF) <- c("CELLID1", "value1")

testxx <- merge(crossWalk, tmpDF)  # CELLID2 is nsrsm (PMgridpoints1); CELLID is rsm (PMgridpoints2)
PMgridpoints2$CELLID2 <- as.factor(PMgridpoints2$CELLID)

PMgridpoints <- merge(PMgridpoints2, testxx)

PMgridpoints$value <- PMgridpoints$value1 - PMgridpoints$value2
PMgridpoints <- subset(PMgridpoints, value != "NA")


if(datatypeList[j] == "depth" | datatypeList[j] == "stage") {
    
   if(maxRamp == 'null') {

      absMin <- -2.7
      if(statList[j] == 'max'| as.numeric(startEndDefine[2]) >= 6 ) {
         absMax <- 3.3
      } else {      
         absMax <- 3.1
      }
      
   } else {      
         absMax <- as.numeric(maxRamp)
         absMin <- as.numeric(maxRamp) * -1
   }

} else {

   absMax <- max(PMgridpoints$value)
   absMin <- min(PMgridpoints$value)
}

# restrict depths in plotValues; consider restricting max also
PMgridpoints$value <- pmax(PMgridpoints$value, absMin)  # restrict below ground depths to absMin (-2.7) ft to reduce range of color ramp !! Be careful selecting maxRamp !!
PMgridpoints$value <- pmin(PMgridpoints$value, absMax)  # restrict above ground depths to absMax (3.1) ft to reduce range of color ramp !! Be careful selecting maxRamp !!

##################################################################################


gridMap  <- list("sp.polygons",grid2, col="grey", lwd=0.1, first=FALSE) # to overlay, use sp.lines for SpatialLines object, sp.points for SpatialPoints object, & sp.text for text
rm(grid)

CSSS <- readOGR(paste(baseDir,'/gis/ever', sep=''),"CSSS_habitat")
CSSSMap  <- list("sp.polygons",CSSS, col="green", lwd=0.8, first=FALSE) # to overlay, use sp.lines for SpatialLines object, sp.points for SpatialPoints object, & sp.text for text
rm(CSSS)

CSSSext <- readOGR(paste(baseDir,'/gis/ever', sep=''),"SubpopAExt")
CSSSextMap  <- list("sp.polygons",CSSSext, col="green", lwd=0.8, first=FALSE) # to overlay, use sp.lines for SpatialLines object, sp.points for SpatialPoints object, & sp.text for text
rm(CSSSext)

coastlineMap <- readOGR(paste(baseDir,'/gis/ever', sep=''),"ParkCoastlines")
baseMap  <- list("sp.lines",coastlineMap, col="grey") # to overlay, use sp.lines for SpatialLines object, sp.points for SpatialPoints object, & sp.text for text
rm(coastlineMap)

canals <- readOGR(paste(baseDir,'/gis/ever', sep=''),"MajorCanals")

canalMap <- list("sp.lines",canals, col="blue") # to overlay, use sp.lines for SpatialLines object, sp.points for SpatialPoints object, & sp.text for text
rm(canals)

roads <- readOGR(paste(baseDir,'/gis/ever', sep=''),"MajorRoadsSouthFlorida")

roadMap <- list("sp.lines",roads, col="brown") # to overlay, use sp.lines for SpatialLines object, sp.points for SpatialPoints object, & sp.text for text
rm(roads)

#uniqAlt <- length(unique(valuesSort$alt))
altVec       <- as.vector(unique(altList))
uniqSeason   <- length(unique(seasonList))
uniqStation  <- length(unique(stationList))
stationVec   <- as.vector(unique(stationList))
stations     <- paste(as.character(stationVec), collapse = ", ")
stationsfn   <- paste(as.character(stationVec), collapse = "_")
alts         <- paste(as.character(altVec), collapse = ", ")
altsfn       <- paste(as.character(altVec), collapse = "_")
datatypeVec  <- as.vector(unique(datatypeList))
datatypes    <- paste(as.character(datatypeVec), collapse = ", ")
datatypesfn  <- paste(as.character(datatypeVec), collapse = "_")

if(datatypeList[j] == "stage" | datatypeList[j] == "depth") {
  stats      <- paste(as.character(unique(statList)), collapse = ", ")
  statsfn    <- paste(as.character(unique(statList)), collapse = "_")
} else {
  stats      <- "average"
  statsfn    <- "average"
}

datumOffsets <- paste(as.character(unique(offsetList)), collapse = ", ")
offsetsfn    <- paste(as.character(unique(offsetList)), collapse = "_")

if(datatypeList[j] == "stage" | datatypeList[j] == "depth") {
  units      <- paste(as.character(unique(unitsList)), collapse = ", ")
} else {
  units      <- "days"
}

##-- maps  --##

seasonVec    <- as.vector(unique(valueDFfull$seasonLegend))
seasonTitle  <- paste(as.character(seasonVec), collapse = ", ")

altDiff <- paste(altList[1],'-',altList[2], sep='')

if(datatypeList[j] == "stage" | datatypeList[j] == "depth") {
  title <- paste(altDiff,' ',stats,' ',seasonTitle,' ',datatypes," difference (",units,")", sep="")
} else {
  title <- paste(altDiff,' ',stats,' ',seasonTitle,' ',datatypes,'_',offsetsfn,"'-offset diff (",units,")", sep="")

}

################################################################################################################################
if(datatypeList[j] == "stage" | datatypeList[j] == "depth") {
  print(datatypeList[j])

  if(maxRamp == 'null') {
     keyTest1    <- abs(max(PMgridpoints$value))
     keyTest2    <- abs(min(PMgridpoints$value))
  } else {
    num1 <- as.numeric(format(round(as.numeric(maxRamp), 2), nsmall = 1))
    num2 <- round_any(as.numeric(maxRamp), 1, floor)
    
    if((((num1-num2)*10) %% 2) == 0) {
      #print("test is Even")
      keyTest1 <- as.numeric(maxRamp) + 0.1
      keyTest2 <- as.numeric(maxRamp) + 0.1
      
    } else {
      #print("test is Odd")
      keyTest1 <- as.numeric(maxRamp)
      keyTest2 <- as.numeric(maxRamp)      
    }
  }
  
  print('plot values max and min:')
  print(keyTest1)
  print(keyTest2)

##########################################################################
   if( tolower(unitsList[j]) == "mm" ) {
      #cf=304.8
      binSize <- 100
      toAdd    <- 50
      limit   <- 1800 + toAdd
      dig     <- 0

    } else if ( tolower(unitsList[j]) == "cm" ) {
      #cf <- 30.48
      binSize <- 10
      toAdd    <- 5
      limit   <- 180 + toAdd
      dig     <- 0

   } else if ( tolower(unitsList[j]) == 'm' | tolower(unitsList[j]) == 'meters' ) {
      #cf <- 0.3048
      binSize <- .2
      toAdd    <- .1
      limit   <- 2 + toAdd
      dig     <- 1

   } else if ( tolower(unitsList[j]) == 'inch' | tolower(unitsList[j]) == 'in' | tolower(unitsList[j]) == 'inches' ) {
      #cf <- 12.0
      binSize <- 2
      toAdd    <- 1
      limit   <- 72 + toAdd
      dig     <- 0

   } else {
      print("reported units are same as model output ")
      binSize <- .2
      toAdd    <- .1
      limit   <- 3.4 + toAdd  # result should be odd number;  binSize and toAdd together used to a bin with zero point at center
      dig     <- 1

   }

###################XXXXXXXXXXXXXXXXXXXXXX##################################
  keySeqRange <- round_any(max(keyTest1,keyTest2), binSize, ceiling)
  keySeqRange <- keySeqRange + toAdd
  keySeqRange <- min(limit, keySeqRange)

  at=seq(keySeqRange*-1,keySeqRange,by=binSize)  #THIS IS IMPORTANT TO DEFINE THE RANGE in the colkey (spplot)!
  colorKey <- as.character(round(at, digits=dig))
##########################################################################

} else {

  binSize <- 20
  keyTest1    <- abs(max(PMgridpoints$value))
  keyTest2    <- abs(min(PMgridpoints$value))

  keySeqRange <- round_any(max(keyTest1,keyTest2), 10, ceiling)
  test <- (keySeqRange/10) %% 2    # remainder; keySeqRange should be odd so to place zero in correct place on color ramp

  if(test != 1) {
    keySeqRange <- keySeqRange + 10
  }
  at=seq(keySeqRange*-1,keySeqRange,by=binSize)  #THIS IS IMPORTANT TO DEFINE THE RANGE in the colkey (spplot)!
  if(at[1] < -365) {
   at[1] <- -366
  }
  atLength <- length(at)
  if(at[atLength] > 365) {
   at[atLength] <- 366
  }
 
  colorKey <- as.character(round(at, digits=0))

}

colorKeyCuts <- length(colorKey) -1

if(datatypeList[j] == "discont-drydown" | datatypeList[j] == "cont-drydown") {
  colors <- brewer.pal(9,"YlOrBr") # ... or try "YlOrRd"
 
} else {
   colors <- brewer.pal(10,"RdYlBu") 
}

pal <- colorRampPalette(colors)

################################################################################################################################

# following from: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS3_MakingMaps_part1_mappingVectorData.html
scale.parameter = 0.77  # scaling paramter. less than 1 is zooming in, more than 1 zooming out. 
xshift = -15000                     # Positive number is Shift to East in map units. 
yshift = -17000                     # Positive number is Shift to South in map units. 

original.bbox = PMgridpoints@bbox  # Pass bbox of your Spatial* Object. 

edges = original.bbox

edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1, ]) + xshift
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2, ]) + yshift

seasonName <- gsub('-','~',seasonTitle)

seasonName <- gsub('/','-',seasonName)
seasonName <- gsub(' ','',seasonName)

plotdir <- '../maps/png/'
mapName <- paste(plotdir,'map-diff_',altsfn,'_',seasonName,'_',statsfn,'_',datatypesfn,'_',offsetsfn,"'-offset.png", sep="")

png( file = mapName, width = 8.5, height = 11, units = "in", pointsize = 12, bg = "white",  res = 300 )

a <- spplot(PMgridpoints,"value", col=NA, sp.layout=list(gridMap,baseMap,canalMap,roadMap,CSSSMap,CSSSextMap), xlim = edges[1, ], ylim = edges[2, ], main=title, at=at, colorkey=list(labels=colorKey), col.regions=pal(colorKeyCuts)) 

print(a)
dev.off()

plotdir <- '../maps/pdf/'
mapName <- paste(plotdir,'map-diff_',altsfn,'_',seasonName,'_',statsfn,'_',datatypesfn,'_',offsetsfn,"'-offset.pdf", sep="")
pdf( file = mapName)
a <- spplot(PMgridpoints,"value", col=NA, sp.layout=list(gridMap,baseMap,canalMap,roadMap,CSSSMap,CSSSextMap), xlim = edges[1, ], ylim = edges[2, ], main=list(title, cex=0.9), at=at, colorkey=list(labels=colorKey), col.regions=pal(colorKeyCuts)) 

print(a)
dev.off()

#########################################################################################################################
print('warnings: ')
print(warnings())

rm(list=ls(all=TRUE))



