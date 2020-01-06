#' ExtractCellData
#'
#' @description Subset the requested data, round to digits.precision and create data.frame 
#' 
#' @param variable    'ComputedHead'
#' @param Dates  not sure
#' @param dimensions  not sure
#' @param var.data  not sure
#' @param cell.list   not sure
#' @param cell.indices   not sure
#' @param digits.precision decimal places reported
#'
#' @return a dataframe.
#' Notes: See Notes in NetCDF_Extract()
#'
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 ncvar_get
#' @importFrom utils write.table
#' 
#' @export
#' 



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
