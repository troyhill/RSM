
library( ncdf4 )

#-------------------------------------------------------------------------#
# NetCDF_IR_Cell_Data
# 
# Read an RSM generated NetCDF file (globalmonitors.nc).
# Read the indicator.region list of RSM cell IDs for the model.version
# and model.alternative from a binary R dictionary in IR.cell.list.file
# (see R_utils/ReadIRList.R).
# Subset the variable data from the indicator.region cells.
# Write the data.frame to file.out as a .csv file.
# Return the data.frame (invisibly).
# 
# The 2-D data frame will have columns of: Date, Cell_ID, Cell_ID,...
# Date       Cell_4516 Cell_4517 Cell_4518 Cell_4512
# 1965-01-01 17.85     17.85     17.85     17.84
# 1965-01-02 17.77     17.77     17.77     17.76
# 
# The 3-D data frame: 
# Date       Cell_4516_X Cell_4517_X ... Cell_4516_Y Cell_4517_Y
# 1965-01-01 -0.00522    -0.00515    ... -0.008      -0.00604
# 1965-01-02 -0.00075     0.00176    ... -0.00485    -0.00456
# 
# 
# Notes: See Notes in NetCDF_Extract()
# 
#-------------------------------------------------------------------------
NetCDF_IR_Cell_Data  = function(
  data.path          = '/opt/physics/models/rsm',
  model.version      = 'vWERP',
  model.alternative  = 'WALT3RNL',
  indicator.region   = 'IR129',
  station.type       = 'IR',
  file.in            = 'globalmonitors.nc',
  variable           = 'ComputedHead',
  file.out           = 'ComputedHead.csv',
  out.path           = '../../temp',
  #start.date         = NULL, # '1965-01-01',
  #end.date           = NULL, # '2005-12-31',
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
    IRList <- read.csv('tmpCellIDs.csv', header=T, sep=",")
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
    
    write.csv( df, file = csv.file, quote = FALSE, row.names = FALSE )
  }
    
  invisible( df )
}

#-------------------------------------------------------------------------
# Subset the requested data, round to digits.precision and create data.frame
#-------------------------------------------------------------------------
ExtractCellData = function(
  variable, Dates, dimensions, varData,
  cell.list, cell.indices, digits.precision
) {
  
  print('variable:')
  print(variable)
  print('dimensions:')
  print(dimensions)
  print('Dates:')
  print('length Dates:')
  print(length(Dates))

# easiest solution found for extracting values for a single day after code was simplified, only done for maps, without this it goes to wrong section of if-statement #
  if ( length(Dates) == 1 ) {
    varData <- as.matrix(varData)
    #print(varData[1:5])
    varData <- cbind(varData, varData)
    print('varData: ')
    print(varData[1:5,])
  }

  print('cell.indices:')
  print(head(cell.indices))
  print('varData:')
  print(head(varData))
  print('str varData: ')
  print(str(varData))
  print('is matrix?:')
  print(is.matrix(varData))
  print('length passed dimensions:')
  print(length(dimensions))
  print('length dimensions[2]:')
  print(length(dimensions[2]))
  print('actual varData dimensions:')
  print(dim(varData))
  #--------------------------------------------------------------
  # Extract requested cell data from 1-D, 2-D or 3-D varData matrix
  if ( length( dimensions ) == 2 && dimensions[2] == 1 ) {
    #cell.data = varData[ cell.indices ]
    #varData <- as.matrix(varData)
    cell.data = as.array(varData[ cell.indices ])
    #print('cell.data:')
    #print(cell.data[1:5, ])
    #print(head(cell.data))
    print('cell.data dimensions:')
    print(dim(cell.data))
    #print( 'cell.data:' ) ; print( cell.data[1:min(dim(cell.data)[1],10), ] )
    print( 'cell.data:' ) ; print( cell.data[1:min(dim(cell.data)[1],10) ] )
  }
  else if ( length( dimensions ) == 2 ) {  # e.g. computedHead
    #cell.data = varData[ cell.indices, ]
    #print('length(dimensions) = 2')
    cell.data = as.array(varData[ cell.indices, ])
    #print('cell.data:')
    #print(cell.data[1:5, ])
    #print(head(cell.data))
    print('cell.data dimensions:')
    print(dim(cell.data))
    #######print( 'cell.data:' ); print( cell.data[1:min(dim(cell.data)[1],10), 1:min(dim(cell.data)[2],10) ] ) 
    print( 'cell.data:' ) ; print( cell.data[1:min(dim(cell.data)[1],10) ] )
    
  }
  else if ( length( dimensions ) == 3 ) {
    #cell.data = varData[ , cell.indices, ]
    print( 'cell.data has 3 dimensions' )
    cell.data = as.array(varData[ , cell.indices, ])
  }
  else if ( length( dimensions ) == 1 ) {  # cell area from wbbudget.nc
    #cell.data   <- as.array(varData[ , cell.indices, ])
    ###result <- array(c(vector1,vector2),dim = c(3,3,2))  # example
    varDataTmp <- as.matrix(varData)
    print('varData matrix dimensions:')
    print(dim(varDataTmp))
    cell.data   <- as.array(varDataTmp[ cell.indices ])
    print('cell.data dimensions:')
    print(dim(cell.data))
  }
  else {
    stop( 'ExtractCellData: Invalid number of dimensions for ', variable )
  }

  print( 'In ExtractCellData cell.list str:'); print(str(cell.list) )
  print( 'In ExtractCellData cell.indices str:'); print(str(cell.indices) )

  #print('The structure of cell.data is: ')
  #print(str(cell.data))
  #print(paste('length(dimensions) = ', length(dimensions), sep=''))
  #print(paste('dimensions[2] = ', dimensions[2], sep=''))
  #print(paste('dimensions[1] = ', dimensions[1], sep=''))

  #--------------------------------------------------------------
  # Round the data to the specified level of precision.
  # Coerce the resultant vector (1-D), transposed matrix (2-D) or matrices (3-D)
  # into a data.frame with Dates corresponding to rows (1st column)
  # and CellIDs in remaining columns
  #print(head(cell.data))
  #print(tail(cell.data))
  print(paste('cell.data length = ', length(cell.data), sep=''))
  #print(length(cell.data))
  print( 'In ExtractCellData length dimensions:' ); print(length( dimensions ))
  #print( 'In ExtractCellData dimensions[2]:' ); print(dimensions[2] )

  if ( length( dimensions ) == 2  && dimensions[2] == 1 ) {
    cell.data = round( cell.data, digits.precision )

    df = as.data.frame( cbind( cell.list, cell.data ) ) 
    names( df ) = c( "Cell_ID", variable )
  }
  else if ( length( dimensions ) == 1 ) {
    cell.data = round( cell.data, digits.precision )

    df = as.data.frame( cbind( cell.list, cell.data ) ) 
    names( df ) = c( "Cell_ID", variable )
  }
  else if ( ncol(as.data.frame(cell.data)) == 1 ) {
    #print('computing cell.data ')
    cell.data = round( cell.data, digits.precision )
    #print('cell.data rounded ')
    #df = as.data.frame( cbind( cell.list, cell.data ) ) 

    #df = cbind( Dates, as.data.frame( t( cell.data ) ) )
    df = cbind( Dates, as.data.frame( cell.data ) )
    names( df ) = c( 'Date', paste( 'Cell_', cell.list, sep = '' ) )

  }
  else if ( length( dimensions ) == 2 ) {
    #print('computing cell.data ')
    cell.data = apply( cell.data, MARGIN = 2,
                       function(x, p = digits.precision) { round( x, p ) } )

    df = cbind( Dates, as.data.frame( t( cell.data ) ) )
    names( df ) = c( 'Date', paste( 'Cell_', cell.list, sep = '' ) )
  }
  else if ( length( dimensions ) == 3 ) {
    # Create two separate rounded matrices, one for each "axis" X and Y
    cell.data.X = apply( cell.data[ 1, , ], MARGIN = 2,
                         function(x, p = digits.precision) { round( x, p ) } )
    
    cell.data.Y = apply( cell.data[ 2, , ], MARGIN = 2,
                         function(x, p = digits.precision) { round( x, p ) } )

    df = cbind( Dates, as.data.frame( t( cell.data.X ) ),
                       as.data.frame( t( cell.data.Y ) ) )
    names( df ) = c( 'Date',
                     paste( 'Cell_', cell.list, '_X', sep = '' ),
                     paste( 'Cell_', cell.list, '_Y', sep = '' ) )
  }
  #print(head(df))
  return( df )

}

#-------------------------------------------------------------------------
# NetCDF_Extract
# 
# Read an RSM generated NetCDF file (globalmonitors.nc).
# Extract the start date from the NetCDF 'timestamps' attribute.
# Extract the 'cellmap' (RSM cell number to NetCDF index) matrix.
# Extract data of the specified variable.
# Return list of [[ Dates, dimensions, cellmap, varData ]]
# 
# Notes: Assumes that timesteps are days.
# 
#        The input file pathname is constructed as:
#        data.path/model.version/model.alternative/'output'/file.in
# 
#        If variable is 1-D (Topography),   varData is a vector
#        If variable is 2-D (ComputedHead), varData is 2-D matrix 
#        If variable is 3-D (TotalVector),  varData is 3-D matrix
# 
# To do: Validate that 3-D "axis" dimensions correspond to 1 : X, 2 : Y
#        Validate that indexOffset +1 provides the correct cell mapping
#        Add ability to process multiple variables?
#        Add ability for arbitray timesteps?
#-------------------------------------------------------------------------

# modified to include flow data #
NetCDF_Extract = function(
  data.path          = paste(baseDir,'/models/rsm',sep=''),
  model.version      = 'vWERP',
  model.alternative  = 'WALT3RNL',
  IR.cell.list.path  = '../../listsDB',
  IR.cell.list.file  = 'rsmWERPindicatorRegion.csv',
  indicator.region   = 'IR129',
  file.in            = 'globalmonitors.nc',
  variable           = 'olvector',
  #start.date         = NULL, # '1965-01-01',
  #end.date           = NULL, # '2005-12-31',
  start.date         = '1965-01-01',
  end.date           = '2005-12-31',
  indexOffset        = TRUE, # Add +1 to cell indices?
  DEBUG              = TRUE
) {

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
  ncdf = nc_open( ncdf.file )

  #--------------------------------------------------------------
  # Read variables from NetCDF file
  #--------------------------------------------------------------
  # Get starting date from "timestamps" variable
  if ( ! ( 'timestamps' %in% names( ncdf $ var ) ) ) {
    nc_close( ncdf )
    stop( 'NetCDF_Extract: timestamps not found in ', ncdf.file )
  }
  # start.ncdf : "days since 1965-1-1 24:0:00"
  start.ncdf = ncatt_get( ncdf, "timestamps" ) $ units
  
  if ( ! ( "days" %in% strsplit( start.ncdf, ' ' )[[1]][1] ) ) {
    nc_close( ncdf )
    stop( 'NetCDF_Extract: timestamps must be in days for ', ncdf.file )
  }
  start.ncdf = strsplit( start.ncdf, ' ' )[[1]][3] # "1965-1-1"
  start.ncdf.Date = as.Date( start.ncdf )

  if ( ! ( 'time' %in% names( ncdf $ dim ) ) ) {
    nc_close( ncdf )
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
    nc_close( ncdf )
    stop( 'NetCDF_Extract: cellmap variable not found in ', ncdf.file )
  }

  cellmap               <- t( ncvar_get( ncdf, ncdf $ var[[ 'cellmap' ]] ) )
  colnames( cellmap )   <- c( 'cellID', 'index' )
  str(cellmap)
  head(cellmap)
  print('creating CellID list')
  tmpCellIDfile         <- 'tmpCellIDs.csv'
  tmpCellIDdf           <- data.frame(cellID=as.character(cellmap[ ,1]))
  #print(head(tmpCellIDdf))
  print( 'head cellIDs:' ); print( tmpCellIDdf[1:min(dim(tmpCellIDdf)[1],10), 1:min(dim(tmpCellIDdf)[2],10) ])

  #write.table(tmpCellIDdf,file=tmpCellIDfile, sep = ",")
  write.table(tmpCellIDdf,file=tmpCellIDfile, row.names=F, col.names=T)

  # Add +1 to the index values; netCDF index starts at zero, R starts at 1
  if ( indexOffset ) {
    cellmap[ , 'index' ] = cellmap[ , 'index' ] + 1
  }

  ###############################################################################################
  ## Insert meshNodeMap, tricons & locations here to compute distances between nodes (flow-line length)
  if ( 'olvector' %in% tolower( variable ) || 'gwvector' %in% tolower( variable ) || 'TotalVector' %in% tolower( variable ) ) {

    ## meshNodeMap ##
    if ( ! ( 'meshNodeMap' %in% names( ncdf $ var ) ) ) {
      nc_close( ncdf )
      stop( 'NetCDF_Extract: meshNodeMap variable not found in ', ncdf.file )
    }
    nodeMap = t( ncvar_get( ncdf, ncdf $ var[[ 'meshNodeMap' ]] ) )  # Map of Mesh Node ID's to mesh node indicies
    colnames( nodeMap ) = c( 'nodeID', 'index' )     
  
    # Add +1 to the index values to account for R non-zero offset (start at 1)
    if ( indexOffset ) {
      nodeMap[ , 'index' ] = nodeMap[ , 'index' ] + 1
    }

    ## tricons ##
    if ( ! ( 'tricons' %in% names( ncdf $ var ) ) ) {
      nc_close( ncdf )
      stop( 'NetCDF_Extract: tricons variable not found in ', ncdf.file )
    }
    tricons = t( ncvar_get( ncdf, ncdf $ var[[ 'tricons' ]] ) )      # The ID's of the nodes associated with each cell
    colnames( tricons ) = c( 'tricon1', 'tricon2', 'tricon3' )  
    # not sure why following is needed, but tricon IDs can be confirmed by checking output in temp directory   
    tricons[ , 'tricon1' ] = tricons[ , 'tricon1' ] + 1
    tricons[ , 'tricon2' ] = tricons[ , 'tricon2' ] + 1
    tricons[ , 'tricon3' ] = tricons[ , 'tricon3' ] + 1

    ## node locations ##
    if ( ! ( 'locations' %in% names( ncdf $ var ) ) ) {
      nc_close( ncdf )
      stop( 'NetCDF_Extract: locations variable not found in ', ncdf.file )
    }
    locations = t( ncvar_get( ncdf, ncdf $ var[[ 'locations' ]] ) )
    colnames( locations ) = c( 'location1', 'location2' )     

    ###############################################################################################
  }
  #--------------------------------------------------------------
  # The requested variable is expected to represent either a
  # 1-D vector, viz: Topography  (single=1,   cells=5794); or
  # 2-D matrix, viz: ComputedHead(time=14975, cells=5794); or
  # 3-D vector map:  TotalVector (time=14975, cells=5794, axis=2);
  # Note that when retrieved via ncvar_get() time is always
  # the last dimension: dim( ComputedHead ) = 5794, 14975
  #                     dim( TotalVector  ) = 2, 5794, 14975

  # Validate that the requested data variable is in the NetCDF            #
  if ( ! ( variable %in% names( ncdf $ var ) ) ) {
    nc_close( ncdf )
    stop( 'NetCDF_Extract: ', variable, ' not found in ', ncdf.file )
  }
  # Get NetCDF dimension vector [5794], [5794, 14975] or [2, 5794, 14975] #
  #dimensions = ncdf $ var [[ variable ]] $ size
  dimensions = ncdf $ var [[ as.character(variable) ]] $ size
  print( paste( 'variable: ', variable ) )
  print('variable dimensions: ')
  print( dimensions )

  # Based on the specified dates, setup start and count for ncvar_get()   #
  # start & count are vectors of start indices and number of points to    #
  # read, one vector element for each dimension                           #
  if ( is.null( start.date ) ) {
    startDate = start.ncdf.Date
    start = NA                                                            # start at beginning 
  }
  else {
    #--         startDate specified by user in start.date               --#
    startDate = as.Date( start.date )
    
    # Recall that the time indice is last, others will be 1 (cells etc.)  #
    # i.e. start is a vector of startDate indices: [ 1, ..., 123 ]        #
    start = rep( 1, length( dimensions ) ) # vector of all 1's
    start[ length( start ) ] = as.integer( startDate - start.ncdf.Date ) + 1
  }

  if ( is.null( end.date ) ) {
    endDate = start.ncdf.Date + N.time.steps - 1
    count = NA                                                            # extract all data
  }
  else {
    endDate = as.Date( end.date )
    # -1 indicates that all entries along that dimension should be read   #
    count = rep( -1, length( dimensions ) )                               # vector of all -1's
    count[ length( count ) ] = as.integer( endDate - startDate ) + 1
  }
  
  Dates = seq( startDate, endDate, 1 )

  print( paste( 'NetCDF_Extract():', variable, startDate, 'to', endDate ) )


  # Extract requested data #

  if ( 'olvector' %in% tolower( variable ) || 'gwvector' %in% tolower( variable ) || 'totalvector' %in% tolower( variable ) ) {

    #print(str(variable))
    variable <- as.character(variable)
    #flowData = ncvar_get( ncdf, as.character(variable) )
    flowData = ncvar_get( ncdf, ncdf $ var[[ variable ]], start = start, count = count )
    #flowData = ncvar_get( ncdf, ncdf $ var[[ variable ]] )

    print(summary(flowData))
    #print( paste( 'start: ', start ) )
    #print( paste( 'count: ', count ) )
    print( paste( 'variable: ', variable ) )
    #print('flow data extracted' )
    print( 'flow data dimensions: ' )
    print( dim(flowData) )

    #### reformat flow data #####  Note: this could be either ol or gw flow
    olMatrixX <- flowData[1,,]
    olMatrixY <- flowData[2,,]

    cellmapdft      <- as.data.frame(cellmap)
    print('cellmapdft: ')
    print(head(cellmapdft))
    colnames(cellmapdft) <- c('cellID', 'index')
    triconsmapdft   <- as.data.frame(tricons)
    print('triconsmapdft: ')
    print(head(triconsmapdft))
    colnames(triconsmapdft) <- c('tricon1', 'tricon2', 'tricon3')
    locationsmapdft <- as.data.frame(locations)
    print('locationsmapdft: ')
    print(head(locationsmapdft))
    colnames(locationsmapdft) <- c('location1', 'location2')
    meshNodeMapdft  <- as.data.frame(nodeMap)
    print('meshNodemapdft: ')
    print(head(meshNodeMapdft))
    colnames(meshNodeMapdft) <- c('nodeID', 'index')

    ## nodeID to locations ##
    node2Location <- data.frame(nodeID=meshNodeMapdft$nodeID, locationN=locationsmapdft$location1, locationW=locationsmapdft$location2)  # location1 E-W; location2 N-S, feet from reference ... NEEDS TO BE VERIFIED
    print('node2Location dimensions: ')
    print(dim(node2Location))  # should be 3465    3
    location.file = paste( '../../temp/', model.version,     '-', model.alternative, '-', 'node2Location.csv', sep = '' )
    write.table( node2Location, file = location.file, row.names=FALSE, na="na", col.names=TRUE, quote = FALSE, sep=",")

    ##  calculate distances between nodes  ##
    #node2cellMap <- data.frame(index=cellmapdft$index, node1=triconsmapdft$tricon1, node2=triconsmapdft$tricon2, node3=triconsmapdft$tricon3)
    node2cellMap <- data.frame(cellID=cellmapdft$cellID, node1=triconsmapdft$tricon1, node2=triconsmapdft$tricon2, node3=triconsmapdft$tricon3)

    node.file = paste( '../../temp/', model.version,     '-', model.alternative, '-', 'node2cellMap.csv', sep = '' )
    write.table( node2cellMap, file = node.file, row.names=FALSE, na="na", col.names=TRUE, quote = FALSE, sep=",")

    print('node2cellMap dimensions: ')
    print(dim(node2cellMap))  # should be 6719    4, not 6458    4
    print(head(node2cellMap))

    node1toCellID <- data.frame(cellID=node2cellMap$cellID, nodeID=node2cellMap$node1)
    node2toCellID <- data.frame(cellID=node2cellMap$cellID, nodeID=node2cellMap$node2)
    node3toCellID <- data.frame(cellID=node2cellMap$cellID, nodeID=node2cellMap$node3)
    print('node3toCellID dimensions: ')
    print(dim(node3toCellID))
    #

    cellID2Location1tmp <- merge(node1toCellID, node2Location, by="nodeID", all=T)
    cellID2Location2tmp <- merge(node2toCellID, node2Location, by="nodeID", all=T)
    cellID2Location3tmp <- merge(node3toCellID, node2Location, by="nodeID", all=T)

    cellID2Location1 <- cellID2Location1tmp[complete.cases(cellID2Location1tmp[ ,2]), ]  # remove NAs for cellID only
    cellID2Location2 <- cellID2Location2tmp[complete.cases(cellID2Location2tmp[ ,2]), ]  # remove NAs for cellID only
    cellID2Location3 <- cellID2Location3tmp[complete.cases(cellID2Location3tmp[ ,2]), ]  # remove NAs for cellID only
    print('cellID2Location3 dimensions: ')
    print(dim(cellID2Location3))

    cellID2Location1Ordered <- cellID2Location1[with(cellID2Location1, order(cellID,nodeID,locationW,locationN)), ]
    cellID2Location2Ordered <- cellID2Location2[with(cellID2Location2, order(cellID,nodeID,locationW,locationN)), ]
    cellID2Location3Ordered <- cellID2Location3[with(cellID2Location3, order(cellID,nodeID,locationW,locationN)), ]

    index2Location1 <- merge(cellmapdft, cellID2Location1Ordered, by="cellID")
    index2Location2 <- merge(cellmapdft, cellID2Location2Ordered, by="cellID")
    index2Location3 <- merge(cellmapdft, cellID2Location3Ordered, by="cellID")

    # just in case, insure that df is ordered by index #
    index2Location1Ordered <- index2Location1[with(index2Location1, order(index,cellID,nodeID,locationW,locationN)), ]
    index2Location2Ordered <- index2Location2[with(index2Location2, order(index,cellID,nodeID,locationW,locationN)), ]
    index2Location3Ordered <- index2Location3[with(index2Location3, order(index,cellID,nodeID,locationW,locationN)), ]

    indexNordered <- data.frame(locationN1=index2Location1Ordered$locationN,locationN2=index2Location2Ordered$locationN,locationN3=index2Location3Ordered$locationN)
    indexWordered <- data.frame(locationW1=index2Location1Ordered$locationW,locationW2=index2Location2Ordered$locationW,locationW3=index2Location3Ordered$locationW)

    distNmax <- pmax(indexNordered$locationN1,indexNordered$locationN2,indexNordered$locationN3,na.rm=T)
    distNmin <- pmin(indexNordered$locationN1,indexNordered$locationN2,indexNordered$locationN3,na.rm=T)

    distWmax <- pmax(indexWordered$locationW1,indexWordered$locationW2,indexWordered$locationW3,na.rm=T)
    distWmin <- pmin(indexWordered$locationW1,indexWordered$locationW2,indexWordered$locationW3,na.rm=T)

    distX   <- pmax(distWmax - distWmin)
    distY   <- pmax(distNmax - distNmin)
    distX[is.na(distX)] <- 0
    distY[is.na(distY)] <- 0

    # note: "olMatrixX" and "olMatrixY" could be overland flow, groundwater flow or total flow #
    qX <- apply(olMatrixX, 2, function(x) x*distX*1.9835/1000*-1)   # output in kaf; probably should use conversion factor 'cf' here instead of hard coding
    qY <- apply(olMatrixY, 2, function(x) x*distY*1.9835/1000*-1)
    print('qX dimensions: ')
    print(dim(qX))

    # example transect: T12_S
    dir <- unlist(strsplit(indicator.region, "_")) [2]              # at this time can only do S and W directions; no diagonal transects. E or N flow will be negative
    #############################################
    if ( 's' %in% tolower( dir ) ) {
      varData <- qY
    }
    else {
      varData <- qX                                                 # note: if input direction is N, E, SE, SW etc this will fail.
    }
    #############################################
    dimensions <- dim(varData)  # need to report dimensions of varData, not olvector

  } else {
     #print('extracting var data')
     varData = ncvar_get( ncdf, ncdf $ var[[ variable ]], start = start, count = count )
  }

    print( 'varData dimensions: ' )
    print( dim(varData) )
    #print( 'varData'      ); print( varData[1:5]  )  # why does this seem to be the indices, and possibly need to add 1
  #############################
  nc_close( ncdf )

  #--------------------------------------------------------------#
  #       Finished reading variables from NetCDF file            #
  #--------------------------------------------------------------#

  return( list( Dates      = Dates,       # class: Date
                dimensions = dimensions,  # class: integer vector
                cellmap    = cellmap,     # class: matrix
                varData    = varData ) )  # class: vector or matrix
}

