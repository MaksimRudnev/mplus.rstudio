#' run inp file in Mplus via plugin
#'
#' @export

runMplusInput <- function() {
  rstudioapi::documentSave()
  st <- Sys.time()
  path = rstudioapi::getActiveDocumentContext()
  inp.file <- sub("^.*/", "", path$path)
  folder <- substring(path$path, 1, attr(regexec("^.*/", path$path)[[1]],"match.length"))
  if(!grepl("\\.inp", substring(inp.file, regexec("\\..*$", inp.file)[[1]][1]), ignore.case = T) ) {
    rstudioapi::showDialog("Warning", "The file isn't .inp, I will try to run it, but make sure it's an input Mplus file!")
    }

    mplus.path = ifelse(is.null(options("mplus.path")[[1]]), "mplus", options("mplus.path")[[1]])

    oldwd<-getwd()
    setwd(folder)
    bash.response = system(paste0(mplus.path, ' "', inp.file, '"'))
    #end.t <- Sys.time()
    setwd(oldwd)
    if(bash.response==127) {
      rstudioapi::showDialog("Warning", "Couldn't locate mplus program! Set the path using setMplusPath")
    } else {

      output.file.path <- paste0(folder, paste0(sub("\\..*$", "", inp.file), ".out"))
      if(file.exists(output.file.path)) {
      if( file.mtime(output.file.path)>st ) {
        rstudioapi::navigateToFile(output.file.path)
      } else {
        warning("Something went wrong, the output file is older than the input.")
      }
      } else {
        warning("Something went wrong, the output file was not created.")
      }

    }
}


#' set Mplus path
#'
#' @export
setMplusPath <- function(path) {
  path <- rstudioapi::showPrompt("Add a path", "Set path to Mplus executable command/file, for example, C://Program files/mplus.exe")
  options("mplus.path" = path)

}
