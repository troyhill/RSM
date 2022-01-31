#' extractDSS
#'
#' @description Extract data from .dss files and export them as an .RData file. This script is unlikely to work on all operating systems or for all users. Users must install 32-bit and 64-bit versions of Java SE Development Kit 8u301, as described here: https://stackoverflow.com/a/7604469/3723870. Users must also install rJava in the 32-bit version of R, and get dssrip to install either via devtools::install_github("dss-rip","eheisman",args="--no-multiarch") or through the latest guidance on eheisman's github page. Finally, the HECDSSVue program address is hardcoded as 'C://Program Files (x86)/HEC/HEC-DSSVue-v3.0.00.212/' in inst/extdata/scripts/script_extractDSS.R. If the provided script does not work, it can be modified and a new script specified in the `script` argument.
#' 
#' @param  parentFolder     directory (no spaces are permitted) with folders labeled by alternative name, containing 'RSMBN_output.dss' files. subfolder names will be used to label alternative data. Additionally, the output .RData file will appear in the parentFolder labeled with the dataType and date.
#' @param  RSM_type         type of RSM file. Can be 'RSMGL' or 'RSMBN'
#' @param  dataType         type of data sought. Can be 'STAGE' or 'FLOW'
#' @param  stations         a comma separated, single-element vector of station names. User must ensure these appear in dss files and have the specified dataType. Invalid data may result in an error similar to `Error in tsc$times * 60 : non-numeric argument to binary operator`
#' @param  category         type of input sought; 'SIMULATED' (gage data) or 'INPUT' (regulation schedules). These cannot be intermixed in a single call.
#' @param  endYear          final year in .dss data series. Typically 2005 (COP) or 2016 (default; LOSOM etc.). The period of record used may change in the future
#' @param  script           script run in 32-bit R to extract data from DSS files.
#'  
#' @return output           an .RData file saved in `parentDirectory` and labeled with the `dataType` and date. 
#'
#' @examples
#' extractDSS(parentFolder = 'G:\\data\\models\\LOSOM\\Iteration_3\\sensitivity',
#'            dataType     = 'STAGE',
#'            stations     = 'LOK,WCA1',
#'            category     = 'SIMULATED',
#'            endYear      = 2016,
#'            script       = system.file("extdata\\scripts","script_extractDSS.R",package="RSM"))
#' 
#' @export
#' 




extractDSS <- function(parentFolder, # directory with folders labeled by alternative name, containing 'RSMBN_output.dss' files. subfolder names will be used to label alternative data. Additionally, the output .RData file will appear in the parentFolder labeled with the dataType and date.
                          RSM_type = "RSMBN", # RSMBN or RSMGL
                          dataType = 'STAGE', # type of data sought. Can be 'STAGE' or 'FLOW'
                          stations = "LOK,WCA1", # a comma separated, single-element vector of station names. User must ensure these appear in dss files and have the specified dataType
                          category = "SIMULATED", # or 'INPUT' for e.g., regulation schedules
                          endYear  = 2016, # final year in output (not sure how to automate detection of this...)
                          script   = system.file("extdata\\scripts", "script_extractDSS.R", package = "RSM")
) {
  stations <- gsub("\\s", "", stations) # remove all whitespace
  # dataType <- toupper(dataType) # make sure no dataTypes are case sensitive
  ### assemble arguments
  all_args <- paste(' --args', parentFolder, RSM_type, dataType, stations, endYear, category)
  system(paste0(Sys.getenv("R_HOME"), "\\bin\\i386\\Rscript.exe ", 
                shQuote(script),# "C:\\RDATA\\EVER_misc\\script_pullDSS_20211006.R"), 
                all_args))
                # " --args G:\\data\\models\\LOSOM\\Iteration_3\\sensitivity STAGE LOK,WCA1")) 
  
  # system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", 
  #               shQuote("C:\\RDATA\\EVER_misc\\script_pullDSS_20211006.R"), 
  #               " --args G:\\data\\models\\LOSOM\\Iteration_3\\sensitivity STAGE LOK,WCA1")) #, wait = FALSE, invisible = FALSE)
}
