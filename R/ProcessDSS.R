#' ProcessDSS
#'
#' @description See: src/DSSvue_python/ExtractDss.py
#' ExtractDss.py is a python 2.2.1 program passed to the HEC-DSSVue Java application.  It requires a .ini file specifying input parameters.
#' Note that this function creates a system2() call to the bash script hec-dssvue, which is a wrapper for hec-dssvue.sh
#' 
#' @param path.dss.py NULL by default
#' @param file.dss.py NULL by default
#' @param path.dss.ini NULL by default
#' @param file.dss.ini NULL by default
#' @param start.date         '1965-01-01'
#' @param end.date           '2005-12-31'
#' @param DEBUG              not sure what this is for
#'
#'
#' @return not sure
#'
#' 
#' @export
#' 


ProcessDSS = function(
  path.dss.py  = NULL,
  file.dss.py  = NULL,
  path.dss.ini = NULL,
  file.dss.ini = NULL,
  start.date   = '1965-01-01',
  end.date     = '2005-12-31',
  DEBUG        = FALSE
) {
  
  arguments = paste( ' "', path.dss.py, file.dss.py, ' -i ',
                     path.dss.ini, file.dss.ini, '"', sep = '' )
  
  if ( DEBUG ) {
    print( "ProcessDSS() : " )
    #print( paste( 'hec-dssvue', arguments, sep = '' ) )
    print( paste( './hec-dssvue', arguments, sep = '' ) )
    
  }
  
  #status = system2( 'hec-dssvue',  arguments )
  status = system2( './hec-dssvue',  arguments )
  
  print( paste( "ProcessDSS(): status:", status ) )
  
  if ( status != 0 ) {
    stop( "ProcessDSS(): system command failed." )
  }
}


