#' getModelData
#'
#' @description An updated version of DataExtract() to work with streamlined . Extracts data. requires python. 
#' 
#' Wrapper to extract NetCDF or DSS data. The data.source controls whether ProcessNetCDF() or ProcessDSS() are called to retrive data. 
#' ProcessNetCDF() is governed by function input parameters, writes a .csv
#' file to out.path/file.out, and returns a data.frame.
#' ProcessDSS() is governed by a .ini file (path.dss.ini/file.dss.ini) writes a .csv file, returns NULL. 
#' 
#' @param  data.source        can be "dss" or "netcdf"
#' @param  data.type          can be one of the following: stage, flow, vector
#' @param  station.type       station type. options: cell, gauge, structure, IR, transect
#' @param  path.dss.ini       '../DSSvue_python/',
#' @param  file.dss.ini       'ExtractDss.ini',
#' @param  path.dss.py        '../DSSvue_python/',
#' @param  file.dss.py        'ExtractDss.py',
#' @param  data.path          paste(baseDir,'/models/rsm',sep=''),
#' @param  model.version      = 'vWERP',
#' @param  model.alternative  = 'WALT3RNL',
#' @param  indicator.region   = 'IR129',
#' @param  file.in            = 'globalmonitors.nc', # RSMGL_CEPP_output.dss, transect_flow.dss, etc.
#' @param  variable           = 'ComputedHead',
#' @param  land.surface.datum = FALSE,
#' @param  rainFall           = FALSE,
#' @param  datumOffset        = 0.0,
#' @param file.out            'ComputedHead.csv'
#' @param out.path            tempdir()
#' @param outFile             tempfile(fileext = ".csv")
#' @param start.date         '1965-01-01'
#' @param end.date           '2005-12-31'
#' @param IR.cell.list.file  File address of model mesh cells in each indicator region (e.g., '../../listsDB/rsmWERPindicatorRegion.csv')
#' @param indexOffset        Whether or not to add 1 to cell indices. Default =  TRUE
#' @param  cell.list          c( 1317, 1533, 1892, 2333, 2508, 3117, 3329, 2942 )
#' @param digits.precision   decimal places reported
#' @param DEBUG              not sure what this is for
#'
#' @return not sure.
#'
#' @importFrom utils read.csv
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom utils str
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncatt_get
#' 
#' @export
#' 

### add RTools directory to PATH 
# Sys.getenv("PATH")
# old_path <- Sys.getenv("PATH") 
# Sys.setenv(PATH = paste(old_path, "C:\\Rtools\\bin;C:\\Rtools\\gcc-4.6.3\\bin;C:\\Rtools\\perl\\bin;C:\\Rtools\\MinGW\\bin", sep = ";"))

#source( "../Extract/NetCDF4_MapData12Oct2018.R" )
#source( "/opt/physical/ML/graphics_new/fork/src/Extract/NetCDF4_MapData.R" )
#source( "/opt/physical/postProcessing/fork/src/Extract/NetCDF4_MapData.R" )
#source( "/home/jpark/NPS/PostProcessing/Extract/NetCDF4_MapData.R" )


getModelData = function(
  data.source        = 'NetCDF',  # or 'DSS'
  data.type          = 'stage',   # stage, flow, vector
  station.type       = 'IR',      # cell, gauge, structure, IR, transect
  path.dss.ini       = '../DSSvue_python/',
  file.dss.ini       = 'ExtractDss.ini',
  path.dss.py        = '../DSSvue_python/',
  file.dss.py        = 'ExtractDss.py',
  data.path          = 'Y:/troy/RDATA/RSM_supplemental/inputFiles', # paste(baseDir,'/models/rsm',sep=''),
  model.version      = 'rsm',
  model.alternative  = 'COP/ECB19RR',
  indicator.region   = 'IR129',
  file.in            = 'globalmonitors.nc', # RSMGL_CEPP_output.dss, transect_flow.dss, etc.
  variable           = 'ComputedHead',
  land.surface.datum = FALSE,
  rainFall           = FALSE,
  datumOffset        = 0.0,
  file.out           = 'ComputedHead.csv',
  out.path           = '../../temp',
  outFile            = tempfile(fileext = ".csv"),
  #start.date         = NULL, # '1965-01-01',
  #end.date           = NULL, # '2005-12-31',
  start.date         = '1965-01-01',
  end.date           = '2005-12-31',
  # IR.cell.list.path  = '../../listsDB',
  IR.cell.list.file  = '../../listsDB/rsmWERPindicatorRegion.csv',
  cell.list          = c( 1317, 1533, 1892, 2333, 2508, 3117, 3329, 2942 ),
  indexOffset        = TRUE, # Add +1 to NetCDF cell indices?
  digits.precision   = 3, 
  DEBUG              = FALSE
) {

  ##############################
  ## Inserted from NetCDF_Extract
  # Resolve NetCDF path/file and open the NetCDF file
  ncdf.file = paste( data.path, model.version, model.alternative,
                     'output', file.in, sep = '/' )
  if ( DEBUG ) {
    print( paste( 'NetCDF_Extract: Open NetCDF file:', ncdf.file ) )
  }
  
  if ( file.access( ncdf.file, mode = 4 ) == -1 ) {
    stop( 'NetCDF_Extract: Cannot access file ', ncdf.file )
  }
  
  # Open NetCDF file
  ncdf = ncdf4::nc_open( ncdf.file )
  
  #--------------------------------------------------------------
  # Read variables from NetCDF file
  #--------------------------------------------------------------
  # Get starting date from "timestamps" variable
  if ( ! ( 'timestamps' %in% names( ncdf $ var ) ) ) {
    ncdf4::nc_close( ncdf )
    stop( 'NetCDF_Extract: timestamps not found in ', ncdf.file )
  }
  # start.ncdf : "days since 1965-1-1 24:0:00"
  start.ncdf = ncdf4::ncatt_get( ncdf, "timestamps" ) $ units
  
  if ( ! ( "days" %in% strsplit( start.ncdf, ' ' )[[1]][1] ) ) {
    ncdf4::nc_close( ncdf )
    stop( 'NetCDF_Extract: timestamps must be in days for ', ncdf.file )
  }
  start.ncdf = strsplit( start.ncdf, ' ' )[[1]][3] # "1965-1-1"
  start.ncdf.Date = as.Date( start.ncdf )
  
  if ( ! ( 'time' %in% names( ncdf $ dim ) ) ) {
    ncdf4::nc_close( ncdf )
    stop( 'NetCDF_Extract: time not found in dim of ', ncdf.file )
  }
  #time.steps  = ncdf $ dim[["time"]] $ vals # sequence of values
  N.time.steps = ncdf $ dim[["time"]] $ len  # 14975
  
  #--------------------------------------------------------------
  # Get cellmap info
  # globalmonitors.nc cellmap:
  #    int cellmap(cells=5794, axis=2);
  #      :description = "Mapping of Cell id to Cell index";
  #      :attributes  = "ref, positions";
  # cellmap is an R matrix with dim( cellmap ) = 2, 5794
  # Transpose the returned cellmap into 5794 rows x 2 cols
  if ( ! ( 'cellmap' %in% names( ncdf $ var ) ) ) {
    ncdf4::nc_close( ncdf )
    stop( 'NetCDF_Extract: cellmap variable not found in ', ncdf.file )
  }
  ##############################
  
  df = NULL
  
  station.type <- tolower(station.type)
  #graph_type   <- tolower(graph_type)
  data.source  <- tolower(data.source)
  
  if ( 'netcdf' %in% data.source ) {
    df = ProcessNetCDF_new(
      data.type          = data.type,
      station.type       = station.type,
      # data.path          = data.path,
      # model.version      = model.version,
      # model.alternative  = model.alternative,
      ncdfObj            = ncdf,
      indicator.region   = indicator.region,
      file.in            = file.in,
      variable           = variable,
      land.surface.datum = land.surface.datum,
      rainFall           = rainFall,
      datumOffset        = datumOffset,
      file.out           = file.out,
      outFile            = outFile,
      out.path           = out.path,
      start.date         = start.date,
      end.date           = end.date,
      IR.cell.list.file  = IR.cell.list.file,
      cell.list          = cell.list,
      indexOffset        = indexOffset,
      digits.precision   = digits.precision,
      DEBUG              = DEBUG )

      # format is: Date value_cell1 value_cell2 value_cell3
      # e.g. Date       Cell_2556 Cell_2557 Cell_2558 Cell_2559 Cell_2972 Cell_2203
      #     1965-01-01    0.664     0.559     0.700     0.722    -0.769     0.794
      #print('df =')
      #print(head(df))

###############################################################################
     print('start & end dates')
     print(start.date)
     print(end.date)
#     print('df test before: ')
#     print(head(df))
#     print(str(df))
     df <- df[ which(df$Date >= start.date), ]
     df <- df[ which(df$Date <= end.date),   ]
#     print('df test after: ')
#     print(head(df))
#     print(tail(df))
###############################################################################

  } else if ( 'csv' %in% tolower( data.source ) ) {

#       if ( tolower(indicator.region) == 'redline' | tolower(indicator.region) == 'blueline' | tolower(indicator.region) == 'srs' | tolower(indicator.region) == 'flbay' ) {
          #fn       <- paste('summedDailyOut_',tolower(indicator.region),'_',model.alternative,'.csv',sep='')
          #####df       <- read.csv(paste(outPath,'/summedDailyOut_redline_ECB19RR.csv', sep=''), header = TRUE, sep = ",")
          #df       <- read.csv(paste(outPath,'/',fn, sep=''), header = TRUE, sep = ",")

          fn       <- paste(tolower(indicator.region),'_',model.alternative,'_',variable,'.csv',sep='')
          df       <- read.csv(paste('../csv/',fn, sep=''), header = TRUE, sep = ",")

          df$value <- as.numeric(as.character(df$value))
          #df$value <- df$value/0.00001157   # units are in cfs
          df$date  <- as.Date(df$date)
          print(indicator.region)
          print('start & end dates')
          print(start.date)
          print(end.date)
          print('df test before: ')
          print(utils::head(df))
          print(str(df))
          df      <- df[ which(df$date >= start.date), ]
          df      <- df[ which(df$date <= end.date),   ]
          print('df test: ')
          print(utils::head(df))
          print(utils::tail(df))

  } else if ( 'dss' %in% tolower( data.source ) ) {

    print(paste( 'executing: hec-dssvue "', path.dss.py, file.dss.py, ' -i ', path.dss.ini, file.dss.ini, '"', sep = '' ))
    #print(paste( ' "', path.dss.py, file.dss.py, ' -i ',path.dss.ini, file.dss.ini, '"', sep = '' ))

    #df <- ProcessDSS(
    ProcessDSS(
      path.dss.py  = path.dss.py,
      file.dss.py  = file.dss.py,
      path.dss.ini = path.dss.ini,
      file.dss.ini = file.dss.ini,
      start.date   = start.date,
      end.date     = end.date,
      DEBUG        = DEBUG )

    if (file.exists(paste(out.path,'/extracted_dss.csv', sep=''))) {

       print('foundID')
#       print(foundID)
       #print(paste(outPath,'/extracted_dss.csv', sep=''))
       df           <- read.csv(paste(out.path,'/extracted_dss.csv', sep=''), header = FALSE, sep = ",")
       df           <- df[5:length(df[ ,1]), ]
       df           <- df[ ,c(2,4)]
       colnames(df) <- c('date','value')

       # split date and convert month mmm to number #
       dateDefine           <- data.frame(do.call('rbind', strsplit(as.character(df$date),' ',fixed=TRUE)))
       colnames(dateDefine) <- c('day','month','year')
       dateDefine$month     <- match(dateDefine$month,month.abb)
       df$date              <- as.Date(paste(dateDefine[ ,3],'-',dateDefine[ ,2],'-',dateDefine[ ,1], sep=''))
       #df$date              <- paste(dateDefine[ ,3],'-',dateDefine[ ,2],'-',dateDefine[ ,1], sep='')
       df$value             <- as.numeric(as.character(df$value))
       #head(df)

    ###############################################################################
        print('start & end dates')
        print(start.date)
        print(end.date)
        print('df test before: ')
        print(utils::head(df))
        print(utils::str(df))
        df <- df[ which(df$date >= start.date), ]
        df <- df[ which(df$date <= end.date),   ]
        print('df test: ')
        print(utils::head(df))
        print(utils::tail(df))
        #write.table(format(df, scientific=FALSE),file="/opt/physical/ML/graphics_new/fork/src/R_utils/df2.csv",sep=",",row.names=FALSE,col.names=T)
    ###############################################################################

    } else {

       print('station ID not found')
       # CHANGE THIS TO RETURN DATAFRAME with null values #

       df <- utils::read.csv(paste(out.path,'/extracted_dss_nul.csv', sep=''), header = FALSE, sep = ",")
       #df <- read.csv(paste(outPath,'/extracted_dss_nil.csv', sep=''), header = FALSE, sep = ",")
       #df <- df[FALSE,]  # blank df for missing station
       #df <- data.frame(date = character(0), value = numeric(0))

       df           <- df[5:length(df[ ,1]), ]
       df           <- df[ ,c(2,4)]
       colnames(df) <- c('date','value')
       print('head df')
       print(utils::head(df))
       # split date and convert month mmm to number #
       dateDefine           <- data.frame(do.call('rbind', strsplit(as.character(df$date),' ',fixed=TRUE)))
       colnames(dateDefine) <- c('day','month','year')
       dateDefine$month     <- match(dateDefine$month,month.abb)
       df$date              <- as.Date(paste(dateDefine[ ,3],'-',dateDefine[ ,2],'-',dateDefine[ ,1], sep=''))
       #df$date              <- paste(dateDefine[ ,3],'-',dateDefine[ ,2],'-',dateDefine[ ,1], sep='')
       df$value             <- as.numeric(as.character(df$value))
       # if this station is not in alt set value = 0 #
#       df$value  <- 0
       print(indicator.region)
       print('start & end dates')
       print(start.date)
       print(end.date)
       print('df test before: ')
       print(utils::head(df))
       print(utils::str(df))
       df <- df[ which(df$date >= start.date), ]
       df <- df[ which(df$date <= end.date),   ]
       print('df test: ')
       print(utils::head(df))
       print(utils::tail(df))
       #write.table(format(df, scientific=FALSE),file="/opt/physical/ML/graphics_new/fork/src/R_utils/df.csv",sep=",",row.names=FALSE,col.names=T)

    }

  } else {
    stop( 'DataExtract: Invalid data.source: ', data.source )
  }

  invisible( df )
}
