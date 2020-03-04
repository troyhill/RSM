#' NetCDF_IR_Cell_Data_new
#'
#' @description Read an RSM generated NetCDF file (globalmonitors.nc).
#' Read the indicator.region list of RSM cell IDs for the model.version 
#' and model.alternative from a binary R dictionary in IR.cell.list.file
#'  (see R_utils/ReadIRList.R).
#' Subset the variable data from the indicator.region cells.
#' Write the data.frame to file.out as a .csv file and returns the data.frame. 
#' 
# @param data.path            paste0(baseDir,'/models/rsm')
# @param model.version        'vWERP'
# @param model.alternative    'WALT3RNL'
#' @param ncdfObj            ncdf object loaded by getModelData (or elsewhere)
#' @param indicator.region     'IR129'
#' @param station.type         station type. options: cell, gauge, structure, IR, transect
#' @param file.in              'globalmonitors.nc'
#' @param variable             'ComputedHead'
#' @param file.out             'ComputedHead.csv'
#' @param out.path             tempdir()
#' @param outFile              tempfile(fileext = ".csv")
#' @param start.date           '1965-01-01'
#' @param end.date             '2005-12-31'
#' @param IR.cell.list.file    Full address of cell list file (e.g., 'rsmWERPindicatorRegion.csv')
#' @param indexOffset          Whether or not to add 1 to cell indices. Default =  TRUE
#' @param digits.precision     decimal places reported
#' @param DEBUG                not sure what this is for
#'
#' @return a dataframe. The 2-D data frame will have columns of: Date, Cell_ID, Cell_ID,... 
#' Date       Cell_4516 Cell_4517 Cell_4518 Cell_4512
#' 1965-01-01 17.85     17.85     17.85     17.84
#' 1965-01-02 17.77     17.77     17.77     17.76
#' 
#' The 3-D data frame: 
#' Date       Cell_4516_X Cell_4517_X ... Cell_4516_Y Cell_4517_Y
#' 1965-01-01 -0.00522    -0.00515    ... -0.008      -0.00604
#' 1965-01-02 -0.00075     0.00176    ... -0.00485    -0.00456
#' 
#' Notes: See Notes in NetCDF_Extract()
#'
#' @importFrom utils write.csv
#' @importFrom utils head
#' @importFrom utils str
#' 
#' @export
#' 

NetCDF_IR_Cell_Data_new  = function(
  # data.path          =  'Y:/troy/RDATA/RSM_supplemental/inputFiles',
  # model.version      = 'rsm',
  # model.alternative  = 'COP/ECB19RR',
  ncdfObj            = NULL,
  indicator.region   = 'IR129',
  station.type       = 'IR',
  file.in            = 'globalmonitors.nc',
  variable           = 'ComputedHead',
  file.out           = 'ComputedHead.csv',
  out.path           = tempdir(),
  outFile            = tempfile(fileext = ".csv"),
  start.date         = '1965-01-01',
  end.date           = '2005-12-31',
  # IR.cell.list.path  = dirname(system.file("extdata", "rsmWERPindicatorRegion.lst", package = "RSM")), # '../../listsDB',
  IR.cell.list.file  = NULL, # system.file("extdata", "rsmWERPindicatorRegion.lst", package = "RSM"),          # 'rsmWERPindicatorRegion.csv',
  indexOffset        = TRUE, # Add +1 to cell indices?
  digits.precision   = 3, 
  DEBUG              = TRUE
) {
  
  #--------------------------------------------------------------
  # Get list of [ dimensions, cellmap, varData ] from NetCDF_Extract
  #--------------------------------------------------------------
  NetCDF.data = NetCDF_Extract_new(
    # data.path         = data.path,
    # model.version     = model.version,
    # model.alternative = model.alternative,
    ncdfObj            = ncdfObj,
    indicator.region  = indicator.region,
    file.in           = file.in,
    out.file          =  outFile, # list of indicator cell IDs created in NetCDF_Extract(). ELIMINATE THIS.
    variable          = variable,
    start.date        = start.date,
    end.date          = end.date,
    indexOffset       = indexOffset,
    DEBUG             = DEBUG )
  
  Dates       = NetCDF.data[[ 'Dates'      ]]

  dimensions  = NetCDF.data[[ 'dimensions' ]]
  cellmap     = NetCDF.data[[ 'cellmap'    ]]
  varData     = NetCDF.data[[ 'varData'    ]]
  cellIDData  = NetCDF.data[[ 'cellIDs'    ]]
  #--------------------------------------------------------------
  # Retreive list of RSM cell IDs for model.version & indicator.region
  # Example: L[['V235']][['IR129']] returns [ 4516 4517 4518 ]
  #L = get( load( paste( IR.cell.list.file, sep = '/' ) ) )
  ##  cell.list = L[[ toupper( model.version ) ]] [[ indicator.region ]]
  
  ##  IR.cell.list.path = '/opt/physical/ML/graphics_new/fork/listsDB'
  
  #  BELOW COMMENTED OUT 16APR2019; IR.cell.list.file filename now passed to function #
  #  IR.cell.list.file = paste('rsm',model.version,'indicatorRegion.csv',sep='')
  
  #  if ( tolower(model.alternative) == 'nsrsm' ) {
  #  IR.cell.list.file = paste('nsrsm',model.version,'indicatorRegion.csv',sep='')
  #  }
  
  if ( DEBUG ) {
    print("")
    print(paste('IR.cell.list.file = ', IR.cell.list.file, sep=''))  # this can be an IR list or a transect list
    print(paste('IR = ', indicator.region, sep=''))
    print("")
  }
  
  if ( tolower(indicator.region) == 'rsmgl_cellids' | tolower(indicator.region) == 'nsrsm_cellids') {
    IRList <- read.csv(outFile, header=T, sep=",") # cellIDData # outFile = "RSM/inst/extdata/tempCellIDs.csv"
    cell.list  <- as.vector(t(IRList$cellID))
  } else if ( tolower(station.type) == 'cell' ) {
    cell.list <- as.vector(as.integer(as.character(indicator.region)))
  }  else {
    IRList    <- read.csv(IR.cell.list.file, header=T, sep=",")
    cell.list <- as.vector(t(subset(IRList, IR == indicator.region, select=c(cell))))                       ### possible source of a note: IR and cell are not defined
    print('head cell list: ')
    print(utils::head(cell.list))
  }
  
  if ( length( cell.list ) < 1 ) {
    stop( 'NetCDF_IR_Cell_Data: Cell list for model version', ' : ', 
          indicator.region, ' is empty.  File is: ', IR.cell.list.file )
  }
  
  if ( DEBUG ) {
    print('')
  }
  
  # Find the requested version & IR RSM cell.list IDs in the NetCDF data
  if ( ! all( cell.list %in% cellmap[ , 'cellID' ] ) ) {
    stop( 'NetCDF_IR_Cell_Data: ',
          'Failed to find IR cell.list in cellmap from ', file.in )
  }
  
  # Indices of matching cell.list RSM Cell IDs in the NetCDF cellmap
  cell.list.i  = match( cell.list, cellmap[ , 'cellID' ] )
  # Get the corresponding NetCDF cellmap indices (to extract data at)
  cell.indices = cellmap[ cell.list.i, 'index' ]
  
  if ( DEBUG ) {
    print( 'cell.list str:'); print(utils::str(cell.list) )
    print( 'cell.list.i str:'); print(utils::str(cell.list.i) )
    print( 'cell.indices str:'); print(utils::str(cell.indices) )
    print( 'NetCDF_Extract returns varData dimensions' ); print( dimensions )

  df = ExtractCellData( variable, Dates, dimensions, varData,
                        cell.list, cell.indices, digits.precision )  
  
  if ( DEBUG ) {
    print( paste( variable, 'dimensions:' ) ); print( dim( df ) )
    #print( paste( variable, 'data:' ) ); print( head( df ) )
    print( paste( variable, 'data:' ) ); print( df[1:5, 1:min(dim(df)[2],10) ])
  }
  
  if ( ! is.null( file.out ) ) {
    # csv.file = paste( out.path, '/',
    #                   model.version,     '-',
    #                   model.alternative, '-',
    #                   indicator.region,  '-',
    #                   file.out, sep = '' )
    csv.file = paste( out.path, '/',
                      file.out, sep = '' )
    
    print( paste( 'NetCDF_IR_Cell_Data: Writing', variable, 'to', csv.file ) )
    
    utils::write.csv( df, file = csv.file, quote = FALSE, row.names = FALSE )
  }
  
  invisible( df )
  }
}
