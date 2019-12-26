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


    mplus.path = if(is.null(options("mplus.path")[[1]])) {
                  if(is.null(rstudioapi::getPersistentValue("mplus.path"))) {
                    "mplus"
                  } else {
                    rstudioapi::getPersistentValue("mplus.path")
                  }
    } else {
      options("mplus.path")[[1]]
    }


    oldwd<-getwd()
    setwd(folder)
    bash.response = system(paste0(mplus.path, ' "', inp.file, '"'))
    #end.t <- Sys.time()
    setwd(oldwd)
    if(bash.response==127) {
      rstudioapi::showDialog("Warning", "Couldn't locate mplus program! Set the path using setMplusPath")
      setMplusPath()
      runMplusInput()
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
  rstudioapi::setPersistentValue("mplus.path", path)

}

#' mplus skeleton
#'
#' @param d2 Data frame to extract variable names
#'
#' @example cat(mplus_skeleton(cars), file = "mplus1.inp")
#' @export
mplus_skeleton <- function(d2, datafile = NULL) {
  var.names <- paste(gsub("\\.", "_", abbreviate(names(d2), 8)), " ! ", names(d2), "\n\t\t", collapse = "")
  if(sum(duplicated(gsub("\\.", "_", abbreviate(names(d2), 8))))>0) warning("Some abbreviated variable names are duplicated!!")
  if(is.null(datafile)) datafile = 'mplus_temp.tab'
  paste0(c("TITLE: New model;\n",
    "DATA:","\n",
           "\tfile = '", datafile, "';", "\n",
           "VARIABLE:", "\n",
           "\tnames =\n\t\t",

           var.names,


           ";\n",
           "\tmissing = .;", "\n",
           "\tusevariables = ",
           "\n\n\n\n",
           "ANALYSIS:\n",
           "\ttype = twolevel random;\n",
           "\testimator = mlr;\n\n\n\n",

           "MODEL:\n\n\n\n",

           "OUTPUT:  tech6;"

  ), collapse = "")
}



#' create Mplus input and prepare data file
#'
#' @export
createMplusInput <- function() {
  filename <- rstudioapi::showPrompt("New file name:", "Input and data file name? ('.inp' and '.dat' will be added automatically)")
  dat.name <- rstudioapi::showPrompt("Data source", "Specify an R data.frame to get the data")

  dat <- get(dat.name)
  dt <- sapply(dat, function(x) {
    if (is.character(x)) as.numeric(as.factor(x)) else as.numeric(x)
    })
  colnames(dt)<-colnames(dat)

  write.table(dt, file = paste0(filename, ".dat"), quote = F,sep = "\t", na = ".", row.names = F, col.names = F)

  mplusSKELETON <- mplus_skeleton(dat, paste0(filename, ".dat"))
  writeLines(mplusSKELETON, paste0(filename, ".inp"))
  rstudioapi::navigateToFile(paste0(filename, ".inp"))
}
