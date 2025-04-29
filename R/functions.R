#' run inp file in Mplus via plugin
#'
#' @export

runMplusInput <- function(x, as.job=F) {

  # preparation and checks
  rstudioapi::documentSave()

  path = rstudioapi::getActiveDocumentContext()

  if(path$id=="#console")
    stop("Please put cursor to the Mplus document you want to run and start again.")

  inp.file <- sub("^.*/", "", path$path)
  folder <- substring(path$path, 1, attr(regexec("^.*/", path$path)[[1]],"match.length"))
  if(!grepl("\\.inp", substring(inp.file, regexec("\\..*$", inp.file)[[1]][1]), ignore.case = T) ) {
    rstudioapi::showDialog("Warning",
                           "The file is not .inp.
                           I will try to run it anyways,
                           but make sure it's an input Mplus file!")
    }

   # search for Mplus path
    mplus.path = if(is.null(options("mplus.path")[[1]])) {
                      if(is.null(rstudioapi::getPersistentValue("mplus.path"))) {
                        "mplus"
                      } else {
                        rstudioapi::getPersistentValue("mplus.path")
                      }
                  } else {
                    options("mplus.path")[[1]]
                  }

    if(is.null(mplus.path)) {
      #rstudioapi::showDialog("Warning", "Couldn't locate mplus program! Set the path using setMplusPath and run again.")
      mplus.path = setMplusPath()
      stop("Couldn't locate Mplus program! Set the path using setMplusPath and run again.")
    } else if(mplus.path=="") {
      rstudioapi::showDialog("Warning", "Couldn't locate mplus program! Set the path.")
      mplus.path = setMplusPath()

    }


    oldwd <- getwd()
    setwd(folder)
    st <- Sys.time()
    output.file.path <- paste0(folder, paste0(sub("\\.inp$", "", inp.file), ".out"))

# AS JOB
    if(as.job) {

      temp.file <- tempfile(fileext = ".R")

      cat('
          st <- Sys.time()
          bash.response <<- system("', mplus.path, " '", inp.file, "'\")\n",
          'if(bash.response==127) { \n',
          '   rstudioapi::showDialog("Warning",
                  "The error has occurred, most likely could not find Mplus program")
          } else {

     output.file.path = "', output.file.path, '"\n',

     'if(file.exists(output.file.path) ) {

        if(file.mtime(output.file.path) < st) {
          warning("The output file is older than the input.")
        }

      rstudioapi::navigateToFile(output.file.path)


      } else {
        warning("Something went wrong, the output file was not created.")
      }

    }

    ',

          sep="",
          file=temp.file)


      #cat(readLines(con = temp.file), sep="\n")

      rstudioapi::jobRunScript(temp.file,
                               name = paste("Running", inp.file),
                               workingDir = getwd())



      #bash.response = 0
    } else {

      bash.response = system(paste0(mplus.path, ' "', inp.file, '"'))




    if(bash.response==127) {
      rstudioapi::showDialog("Warning",
                             "The error occurred, most likely couldn't find Mplus program")
      setMplusPath()

    } else {

      #output.file.path <- paste0(folder, paste0(sub("\\..*$", "", inp.file), ".out"))
      if(file.exists(output.file.path) ) {
        rstudioapi::navigateToFile(output.file.path)

        if( file.mtime(output.file.path) < st ) {
          warning("Something went wrong, the output file is older than the input.")
        }
      } else {
        warning("Something went wrong, the output file was not created.")
      }

    }
  }

    setwd(oldwd)
}



#' run inp file in Mplus via plugin as a background job
#'
#' @export
runMplusInputAsJob <- function(x) {
  runMplusInput(x, as.job=T)
}



#' set Mplus path
#'
#' @export
setMplusPath <- function(path) {
  path <- rstudioapi::showPrompt("Add a path", "Set path to Mplus executable command/file, for example, /Applications/Mplus/mplus")

  options("mplus.path" = path)
  rstudioapi::setPersistentValue("mplus.path", path)
  return(path)

}

#' mplus skeleton
#'
#' @param data Data frame to extract variable names
#' @param datafile Name of the datafile to save
#'
#' @example \dontrun{cat(mplus_skeleton(cars), file = "mplus1.inp")}
#' @export
mplus_skeleton <- function(data, datafile = 'mplus_temp.tab') {

  vnames = gsub("\\.", "_", abbreviate(names(data), 8))




comments <-  sapply(names(data), function(x) {
    if(is.numeric(data[,x] ) ) {
      return(x)
    } else  if (is.character(data[,x]) | is.factor(data[,x]) ) {

      a = as.data.frame(table(as.numeric(as.factor(data[,x])), data[,x], useNA="no"))
      a = a[a$Freq!=0,]
      a = paste(a[,1], "=", a[,2], collapse = " ")
      return(paste(x, "LABS", a, collapse = " "))
    } else {

      warning("Can't recognize the class of variable", x)
      return("check this variable")
    }
  })

  var.names <- paste(vnames, " ! ", comments, "\n\t\t", collapse = "")

    if(sum(duplicated(gsub("\\.", "_", abbreviate(names(data), 8))))>0) warning("Some abbreviated variable names are duplicated!!")
  #if(is.null(datafile)) datafile = 'mplus_temp.tab'
  #
  paste0(c("TITLE: New model;\n",
    "DATA:","\n",
           "\tfile = '", datafile, "';", "\n",
           "VARIABLE:", "\n",
           "\tnames =\n\t\t",

           var.names,


           ";\n",
           "\tmissing = .;", "\n",
           "\tusevariables = <ADD SEOM VARIABLES FROM THE LIST ABOVE>;",
           "\n\n\n\n",
           "ANALYSIS:\n",
           "\ttype = <e.g. TWOLEVEL RANDOM>;\n",
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
