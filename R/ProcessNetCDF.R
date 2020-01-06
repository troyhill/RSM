#' ProcessNetCDF
#'
#' @description Wrapper for various functions to extract NetCDF data
#' 
#' @param  data.type not sure
#' @param  station.type station type. options: cell, gauge, structure, IR, transect
#' @param  data.path          paste(baseDir,'/models/rsm',sep='')
#' @param  model.version      'vWERP'
#' @param  model.alternative  'WALT3RNL'
#' @param  indicator.region   'IR129'
#' @param  file.in            'globalmonitors.nc', # RSMGL_CEPP_output.dss, transect_flow.dss, etc.
#' @param  variable           'ComputedHead'
#' @param  land.surface.datum FALSE
#' @param  rainFall           FALSE
#' @param  datumOffset        0.0
#' @param  file.out           'ComputedHead.csv'
#' @param  out.path           tempdir()
#' @param outFile             tempfile(fileext = ".csv")
#' @param start.date         '1965-01-01'
#' @param end.date           '2005-12-31'
#' @param IR.cell.list.path   '../../listsDB'
#' @param IR.cell.list.file  'rsmWERPindicatorRegion.csv'
#' @param indexOffset        Whether or not to add 1 to cell indices. Default =  TRUE
#' @param  cell.list          c( 1317, 1533, 1892, 2333, 2508, 3117, 3329, 2942 )
#' @param digits.precision   decimal places reported
#' @param DEBUG              not sure what this is for
#'
#'
#' @return not sure
#'
#' @export
#' 



#-------------------------------------------------------------------------
# ProcessNetCDF
# Wrapper for various functions to extract NetCDF data
#-------------------------------------------------------------------------
ProcessNetCDF = function(
  data.type          = NULL,
  station.type       = NULL,
  data.path          = NULL,
  model.version      = NULL,
  model.alternative  = NULL,
  indicator.region   = NULL,
  file.in            = NULL,
  variable           = NULL,
  land.surface.datum = NULL,
  rainFall           = NULL,
  datumOffset        = NULL,
  file.out           = NULL,
  out.path           = NULL,
  outFile            = tempfile(fileext = ".csv"),
  start.date         = '1965-01-01',
  end.date           = '2005-12-31',
  IR.cell.list.path  = NULL,
  IR.cell.list.file  = NULL,
  cell.list          = NULL,
  indexOffset        = NULL,
  digits.precision   = 6,
  DEBUG              = FALSE
) {
  
  if ( DEBUG ) {
    print( paste( "ProcessNetCDF: ", station.type, data.type, start.date, end.date ) )
  }
  
  #  if ( "ir"    %in% tolower( station.type ) &&
  #       "stage" %in% tolower( data.type    ) ) {
  if ( "ir" %in% tolower( station.type ) || "cell" %in% tolower( station.type ) || "transect" %in% tolower( station.type ) ) {
    
    # IR with stage : Call NetCDF_IR_Cell_Data()
    #      data = NetCDF_IR_Cell_Data(
    df = NetCDF_IR_Cell_Data(
      data.path         = data.path,
      model.version     = model.version,
      model.alternative = model.alternative,
      indicator.region  = indicator.region,
      station.type      = station.type,
      IR.cell.list.path = IR.cell.list.path,
      IR.cell.list.file = IR.cell.list.file,
      file.in           = file.in,
      variable          = variable,
      file.out          = file.out,
      outFile            = outFile,
      out.path          = out.path,
      start.date        = start.date,
      end.date          = end.date,
      indexOffset       = indexOffset,
      digits.precision  = digits.precision, 
      DEBUG             = DEBUG )
    
    #print('the dataframe values: ')
    #print(head(df))
    #df[2:ncol(df)] <- df[2:ncol(df)]+datumOffset
    df[2:ncol(df)] <- df[2:ncol(df)]-datumOffset
    #print('new dataframe values: ')
    #print(head(df))
    
    if ( land.surface.datum ) {
      # Get topo data for the cell.list
      topo = NetCDF_IR_Cell_Data(
        data.path         = data.path,
        model.version     = model.version,
        model.alternative = model.alternative,
        indicator.region  = indicator.region,
        station.type      = station.type,
        IR.cell.list.path = IR.cell.list.path,
        IR.cell.list.file = IR.cell.list.file,
        file.in           = file.in,
        variable          = 'Topography',
        file.out          = file.out,
        out.path          = out.path ,
        outFile            = outFile,
        #start.date        = start.date,
        #end.date          = end.date,
        start.date         = NULL, # '1965-01-01',
        end.date           = NULL, # '2005-12-31',
        indexOffset       = indexOffset,
        digits.precision  = digits.precision, 
        DEBUG             = DEBUG )
    }
    
    if ( land.surface.datum ) {
      # Convert stage elevations in data to depth
      #  print(head(topo))
      for ( i in 1 : nrow( topo ) ) {
        cell.id = topo[ i, 'Cell_ID'    ]
        topo.ft = topo[ i, 'Topography' ]
        
        # Find the data column for this Cell_ID
        #        data.col.i = which( names(data) %in% paste('Cell_', cell.id, sep = ''))
        #
        #        # Convert stage elevation to water level relative to land surface
        #        data[ , data.col.i ] = data[ , data.col.i ] - topo.ft
        
        data.col.i = which( names(df) %in% paste('Cell_', cell.id, sep = ''))
        
        # Convert stage elevation to water level relative to land surface
        df[ , data.col.i ] = df[ , data.col.i ] - topo.ft
        
        
      }
    }
    
    ##################################################################################
    # note: extracting variable 'cellarea' to compute rainvolume/area
    #rainFall <- TRUE
    print('rainfall: ')
    print(rainFall)
    if ( isTRUE(rainFall) ) {
      
      area = NetCDF_IR_Cell_Data(
        data.path         = data.path,
        model.version     = model.version,
        model.alternative = model.alternative,
        indicator.region  = indicator.region,
        station.type      = station.type,
        IR.cell.list.path = IR.cell.list.path,
        IR.cell.list.file = IR.cell.list.file,
        file.in           = file.in,
        variable          = 'cellarea',
        file.out          = file.out,
        out.path          = out.path ,
        outFile            = outFile,
        #start.date        = start.date,
        #end.date          = end.date,
        start.date         = NULL, # '1965-01-01',
        end.date           = NULL, # '2005-12-31',
        indexOffset       = indexOffset,
        digits.precision  = digits.precision, 
        DEBUG             = DEBUG )
    }
    
    print('variable: ')
    print(variable)
    
    if ( isTRUE(rainFall) ) {
      print('area: ')
      print(head(area))
      for ( i in 1 : nrow( area ) ) {
        cell.id  = area[ i, 'Cell_ID'    ]
        area.ft2 = area[ i, 'cellarea' ]
        
        # Find the data column for this Cell_ID
        data.col.i = which( names(df) %in% paste('Cell_', cell.id, sep = ''))
        
        # Convert rainfall volumes to depth
        df[ , data.col.i ] = df[ , data.col.i ] / area.ft2
        
        
      }
    }
    
    ###################################################################################
  }
  
  invisible( df )
}
