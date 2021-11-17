#' getFTP
#'
#' @description downloads selected RSM output from SFWMD FTP site to a local folder. This code relies on the FTP site's folder structure, which may change at any point.
#' 
#' @param  ftp          FTP site parent directory; should be a 'Model_Output' folder - see example.
#' @param  destination  destination directory. Output will appear in subfolders labeled with the alternative name.
#' @param  pattern      pattern that will define which files to download. Include any subfolders as part of this regex query.
#'
#' @return none Files will appear in subfolders of the `destination` directory. Subfolders will be labeled with the alternative name (based on the FTP folder structure).
#'
#' @importFrom RCurl getURLContent
#' @importFrom utils read.table
#' 
#' @examples
#' 
#' \dontrun{
#' getFTP(ftp = 
#'   'ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/',
#'   destination = "G:\\data\\models\\delete",
#'   pattern = "RSMBN/RSMBN_output.dss") # don't pull more than one file at a time, for now.
#' }
#' 
#' @export
#' 


getFTP <- function(ftp = 'ftp://ftppub.sfwmd.gov/outgoing/LOSOM/Iteration_2/PM_ECBr_NA25_AA_BB_CC/Model_Output/',
                          destination = tmpDir(), # alternative (positive value = alternative higher than baseline)
                          pattern = "RSMBN\\RSMBN_output.dss" # this is case insensitive. 
) {
  options(timeout = max(300, getOption("timeout")))
  
  url <- ftp
  dir_list <-
    read.table(
      textConnection(
        getURLContent(url)
      ),
      sep = "",
      strip.white = TRUE)$V9
  subfolders <- grep(x = dir_list, pattern = "\\.|\\..", value = TRUE, invert = TRUE)
  url_list   <- paste0(url, subfolders, "/", pattern)
  alts       <- sapply(strsplit(url_list, '/'), FUN = "[[", 9) #  identical to `subfolders` 
  subDir     <- sapply(strsplit(url_list, '/'), FUN = "[[", 10)
  
  ### this will fail if there are multiple files in the regex query
  files      <- sapply(strsplit(pattern, '/|\\\\'), FUN = tail, 1)
  
  for(i in 1:length(url_list)){
    directory <- paste0(destination,"\\Model_Output\\", alts[i], "\\") #, subDir[i], "/")
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    dest      <- paste0(directory, files) # "\\RSMBN_output.dss")
    
    tryCatch(
      download.file(url_list[i], dest, mode = "wb", cacheOK = F),
      error=function(e) print(paste(alts[i], ' data did not download'))
    )
  }
}
