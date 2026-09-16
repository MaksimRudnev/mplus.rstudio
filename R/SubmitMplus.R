#' Submit Mplus to HPC
#'
#' @export
submit_mplus_server <- function() {

  # 1. Get the currently open file in RStudio
  context <- rstudioapi::getActiveDocumentContext()
  filepath <- path.expand(context$path)
  print(filepath)


  if (filepath == "" || !grepl("\\.inp$", tolower(filepath))) {
    stop("Please save your file as an .inp file before submitting.")
  }

  filename <- basename(filepath)


  # 1b. Get defaults
  mplus.server.path =
    if(is.null(options("mplus.server.path")[[1]])) {
      if(is.null(rstudioapi::getPersistentValue("mplus.server.path"))) {
          "~/.mplus_server_config"
    } else {
      rstudioapi::getPersistentValue("mplus.server.path")
    }
  } else {
    options("mplus.server.path")[[1]]
  }

  if(is.null(mplus.server.path)) {
    #rstudioapi::showDialog("Warning", "Couldn't locate mplus program! Set the path using setup_mplus_server and run again.")
    mplus.server.path = setup_mplus_server()
    stop("Couldn't locate Mplus program! Set the path using setup_mplus_server and run again.")
  } else if(mplus.server.path=="") {
    rstudioapi::showDialog("Warning", "Couldn't locate mplus server path! Set the path.")
    mplus.server.path = setup_mplus_server()

  }

#  mplus.server.path = "~/Library/Mobile Documents/com~apple~CloudDocs/.mplus_server_config"
  mplus.server.config = read.table(mplus.server.path,  sep = "=",
                                   comment = "#", strip.white = T)
  mplus.server.config = setNames(as.list(mplus.server.config$V2), mplus.server.config$V1)


  # 2. Extract the number of processors from the Mplus script
  file_content <- paste(context$contents, collapse = "\n")
  # Look for PROCESSORS = X (case-insensitive)
  proc_match <- regexpr("proc\\s*=\\s*([0-9]+);", tolower(file_content))

  default_cores <- ifelse(is.null(mplus.server.config$cores), 1, as.numeric(mplus.server.config$cores))
  if (proc_match > 0) {
    # Extract just the number
    match_str <- regmatches(tolower(file_content), proc_match)
    default_cores <- as.numeric(gsub("\\D", "", match_str))
  }

  # 3. Build the UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Submit Mplus Job to HPC"),
    miniUI::miniContentPanel(
      shiny::textInput("server", "Server Login (e.g., user@myriad.rc.ucl.ac.uk):", value = mplus.server.config$login),



      shiny::textInput("remote_dir",
                       "Remote Folder path (must exist, inputs and outputs will be inside designated folders within this one):",
                       value = mplus.server.config$storage_dir),
      shiny::textInput("working_dir", "Working Folder path (must exist):",
                       value = mplus.server.config$working_dir),

      shiny::textInput("mplus.command", "Mplus executable path",
                       value = mplus.server.config$mplus_cmd),

      shiny::textInput("job_name", "Job Name:", value = paste0("Mplus_", format(Sys.time(), "%H%M%S"))),

      shiny::textInput("local_jobscript", "Local jobscript folder:",
                       value = mplus.server.config$local_jobscript_folder),

      shiny::numericInput("cores", "Cores (Extracted from .inp):", value = default_cores, min = 1),
      shiny::numericInput("ram", "RAM per core (GB):", value =  ifelse(is.null(mplus.server.config$ram), 1, as.numeric(mplus.server.config$ram)), min = 1),
      shiny::textInput("time", "Time Limit (HH:MM:SS):",

                       value = ifelse(is.null(mplus.server.config$time),
                                      "05:00:00",
                                      mplus.server.config$time)),
      shiny::checkboxInput("email_notify", "Enable email notification?", value = FALSE),
      shiny::textInput("email", "Enter email address to get notifications about status of the job:",
                       value = ifelse(is.null(mplus.server.config$email),"",
                                      mplus.server.config$email)),
      shiny::checkboxInput("debug", "Debug?", value = FALSE)


    )
  )

  # 4. Server Logic
  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {

      # Prepare local directories: create 'jobscripts'
      jobscript_dir <- file.path(dirname(filepath), input$local_jobscript)
      if (!dir.exists(jobscript_dir)) {
        dir.create(jobscript_dir, recursive = TRUE)
      }

      # Generate Jobscript content (qsub format)
      job_script_path <- file.path(jobscript_dir, paste0(input$job_name, ".sh"))

      script_lines <- c(
        "#!/bin/bash -l",
        paste0("#$ -N ", input$job_name),
        paste0("#$ -l h_rt=", input$time),
        paste0("#$ -l h_vmem=", input$ram, "G"),
        paste0("#$ -pe smp ", input$cores),

        ifelse(input$email_notify,
               paste0("#$ -M ", input$email, "\n#$ -m bea"), ""),

paste0(
      "# Set the working directory to somewhere in your scratch space.  This is
# necessary because the compute nodes cannot write to your $HOME
# NOTE: this directory must exist.
#$ -wd ", input$working_dir, input$job_name,
        "\n# Your work must be done in $TMPDIR (serial jobs particularly)", "\n",
        "cd $TMPDIR\n",
        "",
        "module -f unload compilers mpi gcc-libs\n",
        ""),
        paste0(input$mplus.command, " '",
               input$remote_dir, input$job_name, "/", filename, "' '",
               input$remote_dir, input$job_name, "/", gsub("\\.inp$", ".out", filename), "'"
               ),
"# Preferably, tar-up (archive) all output files to transfer them back
# to your space.",
paste0("tar zcvf ", sub("~/", "$HOME/", input$remote_dir), input$job_name, "technical.tgz $TMPDIR"
        ))

      writeLines(script_lines, job_script_path)

      # Generate the commands
      server_dest <- paste0(input$server, ":",
                            input$remote_dir, input$job_name)

      # SSH/SCP commands
      # Create remote directory
      cmd_mkdir <- sprintf("ssh %s 'mkdir -p %s'", input$server,
                           paste0(input$remote_dir, input$job_name)
                           )

      # upload job script
      cmd_scp_jobscript <- sprintf('scp "%s" "%s/"', job_script_path, server_dest)

      # upload mplus script
      cmd_scp_mplus <- sprintf('scp "%s" "%s/"', filepath, server_dest)

      # run the job
      cmd_submit <- sprintf("ssh %s 'cd %s && qsub %s'",
                            input$server,
                            paste0(input$remote_dir, input$job_name),
                            basename(job_script_path))

      # Execute commands (silently via system)
      message("Creating remote directory...")

      cat(cmd_mkdir, "\n")

      if(!input$debug) {
      resp1 <- system(cmd_mkdir, intern = TRUE)
      message(resp1)
      }

      message("Uploading files...")
      cat(cmd_scp_mplus, "\n")

      if(!input$debug) {
      resp2 = system(cmd_scp_mplus, intern = TRUE)
      message(resp2)
      }

      cat(cmd_scp_jobscript, "\n")
      if(!input$debug) {
      resp3 <- system(cmd_scp_jobscript, intern = TRUE)
      message(resp3)
      }

      message("Submitting job...")
      cat(cmd_submit, "\n")
      if(!input$debug) {
      resp4 = system(cmd_submit, intern = TRUE)
      #cat(resp4, "\n")
      message(resp4)
      }
      # Adding the download commands to the local Mplus file
      rsync_cmd <- sprintf('\n! DOWNLOAD RESULTS:\n! rsync -av --exclude="%s" %s "%s/"\n',
                           filename, paste0(input$server, ':"',
                                            input$remote_dir, input$job_name, '/"'),
                           dirname(filepath))


      # Insert at the very end of the active document
      rstudioapi::insertText(location = c(Inf, 1), text = rsync_cmd, id = context$id)

      shiny::stopApp()
    })

    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }

  # Run the gadget
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Mplus HPC Submitter", width = 500, height = 500))
}


#' Setup Mplus server
setup_mplus_server <- function() {
  # Create a default configuration file in the user's home directory
  path <- rstudioapi::showPrompt("Add a path",
                                 'Insert an plus server configuraion file, for example, ~/.mplus_server_config. This file should contain the defaults you want to use in the job script, in the format, e.g. login = "user@server.com" # this is your normal login\n
storage_dir = "~/models/mplus_runs"')

  options("mplus.server.path" = path)
  rstudioapi::setPersistentValue("mplus.server.path", path)
  return(path)

}
