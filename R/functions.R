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


#' Show Mplus manual in RStudio viewer
#
# showMplusManual <- function(local = "") {
#   if(local!="") {
#     if(!file.exists(local)) stop("File doesn't exist!")
#     if(!grepl("\\.pdf$", local, ignore.case = T)) stop("File is not a pdf!")
#     pdf_file <- local #normalizePath(local)
#   } else {
#     # online version
#     download.file("https://www.statmodel.com/download/UsersGuide.pdf",
#                   destfile = file.path(tempdir(), "MplusUserGuide.pdf"),
#                   mode = "wb")
#     pdf_file = file.path(tempdir(), "MplusUserGuide.pdf")
#   }
#   # pdf_file <- normalizePath("/Users/maksimrudnev/Library/Mobile Documents/com~apple~CloudDocs/литература/methodology/SEM/MplusUserGuideVer_8.pdf")
#   html_file <- tempfile(fileext = ".html")
#
#   # Write a simple HTML page with an iframe
#   writeLines(c(
#     '<html>',
#     '<head>',
#     '  <meta charset="UTF-8">',
#     '</head>',
#     '<body>
#     <td valign="TOP" colspan="2" width="660">
#
# <p class="gheader">Mplus HTML User\'s Guide</p>
#
#       <a name="top">
#
#       </a><ol><a name="top">
#       </a><li><a name="top"></a><a href="/HTML_UG/introV8.htm">Table of Contents</a>
#       </li><li><a href="/HTML_UG/chapter1V8.htm">Chapter 1</a>: Introduction
#     </li><li><a href="/HTML_UG/chapter2V8.htm">Chapter 2</a>: Getting started with Mplus
#     </li><li><a href="/HTML_UG/chapter3V8.htm">Chapter 3</a>: Regression and path analysis
#     </li><li><a href="/HTML_UG/chapter4V8.htm">Chapter 4</a>: Exploratory factor analysis
#     </li><li><a href="/HTML_UG/chapter5V8.htm">Chapter 5</a>: Confirmatory factor analysis and structural equation modeling
#     </li><li><a href="/HTML_UG/chapter6V8.htm">Chapter 6</a>: Growth modeling and survival analysis
#     </li><li><a href="/HTML_UG/chapter7V8.htm">Chapter 7</a>: Mixture modeling with cross-sectional data
#     </li><li><a href="/HTML_UG/chapter8V8.htm">Chapter 8</a>: Mixture modeling with longitudinal data
#     </li><li><a href="/HTML_UG/chapter9V8.htm">Chapter 9</a>: Multilevel modeling with complex survey data
#     </li><li><a href="/HTML_UG/chapter10V8.htm">Chapter 10</a>: Multilevel mixture modeling
#     </li><li><a href="/HTML_UG/chapter11V8.htm">Chapter 11</a>: Missing data modeling and Bayesian analysis
#     </li><li><a href="/HTML_UG/chapter12V8.htm">Chapter 12</a>: Monte Carlo simulation studies
#     </li><li><a href="/HTML_UG/chapter13V8.htm">Chapter 13</a>: Special features
#     </li><li><a href="/HTML_UG/chapter14V8.htm">Chapter 14</a>: Special modeling issues
#     </li><li><a href="/HTML_UG/chapter15V8.htm">Chapter 15</a>: TITLE, DATA, VARIABLE, and DEFINE commands
#     </li><li><a href="/HTML_UG/chapter16V8.htm">Chapter 16</a>: ANALYSIS command
#     </li><li><a href="/HTML_UG/chapter17V8.htm">Chapter 17</a>: MODEL command
#     </li><li><a href="/HTML_UG/chapter18V8.htm">Chapter 18</a>: OUTPUT, SAVEDATA, and PLOT commands
#     </li><li><a href="/HTML_UG/chapter19V8.htm">Chapter 19</a>: MONTECARLO command
#     </li><li><a href="/HTML_UG/chapter20V8.htm">Chapter 20</a>: A summary of the Mplus language
#     </li><li><a href="/HTML_UG/indexv8.htm">Index</a>
#       </li><li><a href="/ugexcerpts.shtml">User\'s Guide Examples</a>
# </li></ol>
#
# There is also a <a href="/download/usersguide/MplusUserGuideVer_8.pdf">PDF version</a> of the User\'s Guide available.
#     <br><br>
#       <br><br>
#       <br><br>
#       <br><br>
#       <br><br>
#       <br><br>
#       <br><br>
#       <br><br>
#       <!-- End of main text body -->
#       </td>
#
#     </body></html>'
#   ), html_file)
#
#
#
#   # Show in RStudio Viewer
#   viewer <- getOption("viewer")
#   if (!is.null(viewer)) {
#     viewer(html_file)
#   } else {
#     browseURL(html_file)
#   }
#
#   #rstudioapi::viewer("https://www.statmodel.com/download/UsersGuide.pdf")
# }
#
#
