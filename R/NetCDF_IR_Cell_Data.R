#' NetCDF_IR_Cell_Data
#'
#' @description Read an RSM generated NetCDF file (globalmonitors.nc).
#' Read the indicator.region list of RSM cell IDs for the model.version 
#' and model.alternative from a binary R dictionary in IR.cell.list.file
#'  (see R_utils/ReadIRList.R).
#' Subset the variable data from the indicator.region cells.
#' Write the data.frame to file.out as a .csv file and returns the data.frame. 
#' 
#' @param data.path  paste0(baseDir,'/models/rsm')
#' @param model.version  'vWERP'
#' @param model.alternative  'WALT3RNL'
#' @param indicator.region  'IR129'
#' @param file.in    'globalmonitors.nc'
#' @param variable    'ComputedHead'
#' @param file.out  'ComputedHead.csv'
#' @param out.path  tempdir()
#' @param outFile tempfile(fileext = ".csv")
#' @param start.date  '1965-01-01'
#' @param end.date   '2005-12-31'
#' @param IR.cell.list.path   '../../listsDB'
#' @param IR.cell.list.file  'rsmWERPindicatorRegion.csv'
#' @param indexOffset Whether or not to add 1 to cell indices. Default =  TRUE
#' @param digits.precision decimal places reported
#' @param DEBUG not sure what this is for
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
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 ncvar_get
#' @importFrom utils write.table
#' @importFrom utils write.csv
#' 
#' @export
#' 

NetCDF_IR_Cell_Data  = function(
  data.path          = paste0(baseDir,'/models/rsm'),
  model.version      = 'vWERP',
  model.alternative  = 'WALT3RNL',
  indicator.region   = 'IR129',
  station.type       = 'IR',
  file.in            = 'globalmonitors.nc',
  variable           = 'ComputedHead',
  file.out           = 'ComputedHead.csv',
  out.path           = tempdir(),
  outFile            = tempfile(fileext = ".csv"),
  start.date         = '1965-01-01',
  end.date           = '2005-12-31',
  IR.cell.list.path  = '../../listsDB',
  IR.cell.list.file  = 'rsmWERPindicatorRegion.csv',
  indexOffset        = TRUE, # Add +1 to cell indices?
  digits.precision   = 3, 
  DEBUG              = TRUE
) {
  
  #--------------------------------------------------------------
  # Get list of [ dimensions, cellmap, varData ] from NetCDF_Extract
  #--------------------------------------------------------------
  NetCDF.data = NetCDF_Extract(
    data.path         = data.path,
    model.version     = model.version,
    model.alternative = model.alternative,
    IR.cell.list.path = IR.cell.list.path,
    IR.cell.list.file = IR.cell.list.file,
    indicator.region  = indicator.region,
    file.in           = file.in,
    variable          = variable,
    start.date        = start.date,
    end.date          = end.date,
    indexOffset       = indexOffset,
    DEBUG             = DEBUG )
  
  #  print(start.date)
  #  print(end.date)
  Dates       = NetCDF.data[[ 'Dates'      ]]
  #  if ( !is.null(start.date)) {
  #    if ( start.date == end.date ) {
  #      Dates <- paste(Dates,Dates,sep=' ')
  #    }
  #  }
  
  dimensions  = NetCDF.data[[ 'dimensions' ]]
  cellmap     = NetCDF.data[[ 'cellmap'    ]]
  #meshNodeMap = NetCDF.data[[ 'meshNodeMap']]  # added by GR 10Dec2018; meshNodeMap is a list of paired node lists: nodeid to nodeindex
  #tricons     = NetCDF.data[[ 'tricons'    ]]  # added by GR 10Dec2018; map of node IDs to cell IDs; 3 per cell
  #locations   = NetCDF.data[[ 'locations'  ]]  # added by GR 10Dec2018; coordinates of 2995 nodes defining mesh cells; use if converting from cfs/ft to cfs
  varData     = NetCDF.data[[ 'varData'    ]]
  
  #--------------------------------------------------------------
  # Retreive list of RSM cell IDs for model.version & indicator.region
  # Example: L[['V235']][['IR129']] returns [ 4516 4517 4518 ]
  #L = get( load( paste( IR.cell.list.path, IR.cell.list.file, sep = '/' ) ) )
  ##  cell.list = L[[ toupper( model.version ) ]] [[ indicator.region ]]
  
  ##  IR.cell.list.path = '/opt/physical/ML/graphics_new/fork/listsDB'
  
  #  BELOW COMMENTED OUT 16APR2019; IR.cell.list.file filename now passed to function #
  #  IR.cell.list.file = paste('rsm',model.version,'indicatorRegion.csv',sep='')
  
  #  if ( tolower(model.alternative) == 'nsrsm' ) {
  #  IR.cell.list.file = paste('nsrsm',model.version,'indicatorRegion.csv',sep='')
  #  }
  
  if ( DEBUG ) {
    print("")
    print(paste('IR.cell.list.path = ', IR.cell.list.path, sep=''))
    print(paste('IR.cell.list.file = ', IR.cell.list.file, sep=''))  # this can be an IR list or a transect list
    print(paste('IR = ', indicator.region, sep=''))
    print("")
  }
  
  if ( tolower(indicator.region) == 'rsmgl_cellids' | tolower(indicator.region) == 'nsrsm_cellids') {
    IRList <- read.csv(outFile, header=T, sep=",")
    cell.list  <- as.vector(t(IRList$cellID))
  }
  else if ( tolower(station.type) == 'cell' ) {
    cell.list <- as.vector(as.integer(as.character(indicator.region)))
    #    print(cell.list)
    #    print(str(cell.list))
    #    #print('to here')
    #    print('')
  }
  else {
    IRList    <- read.csv(paste(IR.cell.list.path, IR.cell.list.file, sep = '/' ), header=T, sep=",")
    cell.list <- as.vector(t(subset(IRList, IR == indicator.region, select=c(cell))))
    #cell.list         = c( 1317, 1533, 1892, 2333, 2508, 3117, 3329, 2942 )
    #    print(cell.list)
    print('head cell list: ')
    print(head(cell.list))
    #    print(str(cell.list))
    #    print('')
  }
  
  if ( length( cell.list ) < 1 ) {
    stop( 'NetCDF_IR_Cell_Data: Cell list for ', model.version, ' : ', 
          indicator.region, ' is empty.  File is: ', IR.cell.list.file )
  }
  
  if ( DEBUG ) {
    #print( paste( IR.cell.list.file, 'has', names( L ) ) )
    #print( 'cell.list:' ); print( cell.list )
    #print(str(cell.list))
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
    #    #print( 'cell.list:'   ); print( cell.list     )
    print( 'cell.list str:'); print(str(cell.list) )
    #print( paste( 'head cell.list:' ) ); print( cell.list[1:min(dim(cell.list)[1],10)] )
    #    #print( 'cell.list.i'  ); print( cell.list.i   )
    print( 'cell.list.i str:'); print(str(cell.list.i) )
    #print( paste( 'head cell.list.i:' ) ); print( cell.list.i[1:min(dim(cell.list.i)[1],10)] )
    #    #print( 'cell.indices' ); print( cell.indices  )
    print( 'cell.indices str:'); print(str(cell.indices) )
    #print( paste( 'head cell.indices:' ) ); print( cell.indices[1:min(dim(cell.list.i)[1],10)] )
    
    print( 'NetCDF_Extract returns varData dimensions' ); print( dimensions )
    
    #print( 'variable'     ); print( variable[1:5] )  # fails if fewer than 5 variables
    #print( 'varData'      ); print( varData[1:5]  )  # why does this seem to be the indices, and possibly need to add 1
    
    #if ( dimensions[1] >= 2 ) {
    #  print( 'variable[1:2,1:5]' ); print( variable[1:2,1:5] )
    #} else {
    #  print( 'variable' ); print( variable[1:5]   )
    #}
    
  }
  #print( 'variable' ); print( variable[1:2,1:5]   )
  #print( 'variable' ); print( variable[1:5]   )
  
  df = ExtractCellData( variable, Dates, dimensions, varData,
                        cell.list, cell.indices, digits.precision )  
  
  if ( DEBUG ) {
    print( paste( variable, 'dimensions:' ) ); print( dim( df ) )
    #print( paste( variable, 'data:' ) ); print( head( df ) )
    print( paste( variable, 'data:' ) ); print( df[1:5, 1:min(dim(df)[2],10) ])
  }
  
  if ( ! is.null( file.out ) ) {
    csv.file = paste( out.path, '/',
                      model.version,     '-',
                      model.alternative, '-',
                      indicator.region,  '-',
                      file.out, sep = '' )
    
    print( paste( 'NetCDF_IR_Cell_Data: Writing', variable, 'to', csv.file ) )
    
    utils::write.csv( df, file = csv.file, quote = FALSE, row.names = FALSE )
  }
  
  invisible( df )
}