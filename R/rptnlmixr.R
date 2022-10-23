#'@import nlmixr2extra 
#'@import onbrand
#'@importFrom stringr str_replace_all
#'@importFrom rxode2 model ini rxode2
#'@importFrom utils read.csv
#'@importFrom yaml read_yaml

#'@export
#'@title Report `nlmixr2` Fit Results to PowerPoint and Word
#'@description Appends `nlmixr2` fit results to an onbrand report object with the
#'content and format of the report in the supplied yaml file
#'@param obnd onbrand report object to have report elements appended to.
#'@param fit nlmixr2 fit object to be reported.
#'@param placeholders Manual placeholders, see \code{\link{yaml_read_fit}} for more.
#'@param rptyaml yaml file containing the report elements and structure.
#'@param cat_covars character vector of categorical covariates to overwrite defaults in yaml file.
#'@param cont_covars character vector of continuous covariates to overwrite defaults in yaml file. 
#'@param parameters  list with element names for each parameter to overwrite defaults in yaml file.
#'@param verbose Boolean variable when set to TRUE messages will be .
#'@return onbrand object with the report elements added.
#'@examples
#'\donttest{
#'library(onbrand)  
#'obnd = read_template(
#'  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
#'  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
#'
#'# This will create an example fit object to use in the examples below
#'fit = fetch_fit_example()
#'
#'# Appening fit results
#'obnd_pptx = report_fit(
#'  fit     = fit, 
#'  rptyaml = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml"),
#'  obnd    = obnd)
#'
#'# Writing the report to a file
#'save_report(obnd, file.path(tempdir(), "report.pptx"))
#'}
report_fit <- function(obnd          = NULL,
                       fit           = NULL,
                       placeholders  = NULL,
                       cat_covars    = NULL,
                       cont_covars   = NULL,
                       parameters    = NULL,
                       rptyaml       = system.file(package="nlmixr2rpt","templates", "report_fit.yaml"),
                       verbose       = FALSE){
  isgood       = TRUE
  msgs         = c()
  rptdetails   = NULL
  rptcont      = NULL
  rpttype      = "Unknown"
  rptfigfmt    = NULL
  rpttabfmt    = NULL
  output_dir   = NULL
  resolution   = NULL


  #------------------------
  # Checking for packages
  pkg_check_file = file.path(tempdir(), "nlmixr2rpt_pkg_check_file")

  pkgs = c("nlmixr2", "ggPMX", "xpose.nlmixr2")
  if(!file.exists(pkg_check_file)){
    for(pkg in pkgs){
      if(system.file(package=pkg) == ""){
        cli::cli_alert_warning(paste0("Suggested package: ", pkg," was not found"))
      }
    }
    # Creating the check file so this check is only run once
    file.create(pkg_check_file)
  }


  #------------------------
  # Checking user input
  # Should probably put a better check on the fit object
  if(is.null(fit)){
    isgood = FALSE
    msgs = c(msgs, "fit was not specified")
  }

  # Checking the onbrand object
  if(isgood){
    # First we make sure the user supplied something
    if(is.null(obnd)){
      isgood = FALSE
      msgs = c(msgs, "obnd was not specified")
    } else{
      # now we look and make sure the onbrand object is valid
      if(!obnd[["isgood"]]){
        # We figure out the type of report we're generating
        isgood = FALSE
        msgs = c(msgs, "Bad onbrand object supplied")
      }
    }
  }

  # reading the yaml file
  if(isgood){
  yamlrr = yaml_read_fit(obnd         = obnd,
                         placeholders = placeholders,
                         parameters   = parameters, 
                         fit          = fit,
                         rptyaml      = rptyaml)
    if(!yamlrr[["isgood"]]){
      isgood = FALSE
    }

    # Defining the pieces locally here.
    rptcont    = yamlrr[["rptcont"]]
    rptdetails = yamlrr[["rptdetails"]]
    rpttype    = yamlrr[["rpttype"]]
    rptfigfmt  = yamlrr[["rptfigfmt"]]
    rpttabfmt  = yamlrr[["rpttabfmt"]]
  }

  if(isgood){
    # Getting the output directory
    res_fo = fetch_option(rptdetails=rptdetails, option="output_dir", fit=fit, verbose=FALSE)
    if(res_fo[["isgood"]]){
      output_dir = res_fo[["value"]]
    } else {
      isgood = FALSE
    }
    # Getting the figure resolutioin
    res_fo = fetch_option(rptdetails=rptdetails, fit=fit, option="resolution")
    if(res_fo[["isgood"]]){
      resolution = res_fo[["value"]]
    } else {
      isgood = FALSE
    }
  }

  # If the user input is good we move forward :).
  if(isgood){
    #------------------------
    # Building Tables
    btres = build_tables(
      obnd        = obnd, 
      fit         = fit, 
      rptdetails  = rptdetails,
      cont_covars = cont_covars,
      cat_covars  = cat_covars,
      verbose     = verbose)
    #------------------------
    # Building Figures
    bfres = build_figures(
      obnd        = obnd, 
      fit         = fit, 
      rptdetails  = rptdetails, 
      cont_covars = cont_covars,
      cat_covars  = cat_covars,
      verbose     = verbose)
    #------------------------
    # Appending results to the open report
    # These are the allowed report elements for each document type
    pptx_allowed = c("figure", "table")
    docx_allowed = c("figure", "table", "text")
    # These are the default values for controlling orientation
    curr_orientation    = "portrait"
    last_eletype        = "None"
    ele_ctr             = 1
    wrap_up_orientation = FALSE
    add_page_break      = FALSE
    # Walking through each element in the content
    for(rptele in rptcont){
      ele_isgood = TRUE
      if(rpttype == "Word"){
        if(any(!(names(rptele) %in% docx_allowed))){
          message(paste0("Bad docx report elements: ", paste(names(rptele)[!(names(rptele) %in% pptx_allowed)], collapse=", ")))
          ele_isgood = FALSE
        }
      }
      if(rpttype == "PowerPoint"){
        if(any(!(names(rptele) %in% pptx_allowed))){
          message(paste0("Bad pptx report elements: ", paste(names(rptele)[!(names(rptele) %in% pptx_allowed)], collapse=", ")))
          ele_isgood = FALSE
        }
      }



      # Only appending the element if it's valid for the current report type
      if(ele_isgood){
        # Controlling page breaks and oreintation changes in Word reports
        #-----------------------------------
        if(rpttype == "Word"){
          # First we check for page orientation changes
          # We set this to NULL to indicate no change
          rptele_orientation = NULL
          # Figure orientation:
          if("figure"  %in% names(rptele)){
            rptele_orientation = bfres[["rptfigs"]][[rptele[["figure"]]]][["orientation"]]
          }
          # Table orientation:
          if("table"  %in% names(rptele)){
            rptele_orientation = btres[["rpttabs"]][[rptele[["table"]]]][["orientation"]]
          }
          # If the orientation in the report element is different from
          # the current orientation we have to change the orientation
          if(!is.null(rptele_orientation)){
            if(rptele_orientation != curr_orientation){
              # Wrapping up the current orientation
              obnd = onbrand::report_add_doc_content(obnd,
                type     = "section",
                content  = list(section_type  =curr_orientation))

              # Setting the current orientation to the rptele_orientation
              curr_orientation = rptele_orientation

              # We also set add_page_break to FALSE since the
              # orientation change will cause a page break
              add_page_break   = FALSE

              # Triggering a wrap up at the end of the document
              wrap_up_orientation = TRUE

            }
          }
          # Page breaks can be triggered by figures and tables below but will
          # not be necessary if there is a shift in orientation
          if(add_page_break){
            obnd = onbrand::report_add_doc_content(obnd,
              type     = "break",
              content  = NULL)
          }
          add_page_break   = FALSE
        }
        #-----------------------------------
        # Processing figures
        if("figure" %in% names(rptele)){
          # Pulling out the figure
          fid = rptele[["figure"]]
          if(fid %in% names(bfres[["rptfigs"]])){
            curr_fig = bfres[["rptfigs"]][[fid]]
          } else {
            # This happens when the user specifies a figure that
            # has not been defined.
            p_res        = mk_error_fig(c(paste0("The figure id ", fid, " was not"), " defined in the yaml file figures section"))
            fig_file     = file.path(output_dir, paste0(fid, "-", rpttype, ".png"))
            grDevices::png(width    = 4,        height = 4,      units = "in",
                filename = fig_file, res    = resolution)
            print(p_res)
            grDevices::dev.off()
            curr_fig = list(orientation   = "portrait",
                            isgood        = TRUE,
                            cmd           = "",
                            cmd_proc      = "",
                            fmsgs         = "",
                            width         = 4,
                            height        = 4,
                            caption       = "Error figure does not exist",
                            caption_proc  = "Error figure does not exist",
                            title         = "Error figure does not exist",
                            title_proc    = "Error figure does not exist",
                            figure        = fig_file)
          }
          if(rpttype == "PowerPoint"){
            # For each figure defined by fid we add all the pages
            # one slide at a time:
            if(curr_fig[["skip"]]){
              message("Skipping figure: ", fid, " (NA found, not generated)")
            } else {
              for(fpage in 1:length(curr_fig[["figure"]])){
                elements = list()
                # The title element is the figure caption:
                elements[[rptfigfmt[["master"]][["title_ph"]]]] =
                  list( content      = curr_fig[["title_proc"]],
                        type         = "text")
                elements[[ rptfigfmt[["master"]][["content_ph"]]]] =
                    list( content      = curr_fig[["figure"]][[fpage]],
                          type         = "imagefile")
                # Adding the slide
                obnd =onbrand::report_add_slide(obnd,
                         template = rptfigfmt[["master"]][["name"]],
                         elements = elements)
              }
            }
          }
          if(rpttype == "Word"){
            if(curr_fig[["skip"]]){
              message("Skipping figure: ", fid, " (NA found, not generated)")
            } else {
              for(fpage in 1:length(curr_fig[["figure"]])){
                obnd = onbrand::report_add_doc_content(obnd,
                  type     = "imagefile",
                  content  = list(image           = curr_fig[["figure"]][fpage],
                                  key             = fid, 
                                  caption         = curr_fig[["caption_proc"]],
                                  caption_format  = curr_fig[["caption_format"]],
                                  width           = curr_fig[["width"]],
                                  height          = curr_fig[["height"]]))

                # adding breaks between figures of multipage figures
                # between figures 1 & 2, 2 & 3,  ... n-1 & n
                if(length(curr_fig[["figure"]])  > 1){
                  if(fpage < length(curr_fig[["figure"]])){
                    obnd = onbrand::report_add_doc_content(obnd,
                      type     = "break",
                      content  = NULL)
                  }
                }
              }
              if(ele_ctr < length(rptcont)){
                add_page_break = TRUE
              }
            }
          }
          last_eletype = "figure"
        }
        #/figures
        #-----------------------------------
        # text
        if("text" %in% names(rptele)){
          if(rpttype == "Word"){
            if(all(c("style", "text") %in% names(rptele[["text"]]))){
              obnd = onbrand::report_add_doc_content(obnd,
                type     = "text",
                content  = list(text    = rptele[["text"]][["text"]],
                                style   = rptele[["text"]][["style"]]))
            } else {
              message(paste0('A "text" report element must contain both text and style fields'))
            }
          }
          last_eletype = "text"
        }
        #/text
        #-----------------------------------
        # table
        if("table" %in% names(rptele)){
          # Pulling out the table
          tid = rptele[["table"]]
          if(tid %in% names(btres[["rpttabs"]])){
            curr_tab = btres[["rpttabs"]][[tid]]
          } else {
            t_res        = mk_error_tab(c(paste0("The table id ", tid, " was not"), " defined in the yaml file tables section"))
            curr_tab = list(orientation   = "portrait",
                            isgood        = TRUE,
                            cmd           = "",
                            cmd_proc      = "",
                            tmsgs         = "",
                            caption       = "Error table does not exist",
                            caption_proc  = "Error table does not exist",
                            title         = "Error table does not exist",
                            title_proc    = "Error table does not exist",
                            table         = t_res)
          }
          if("table" %in% names(rptele)){
            if(rpttype == "PowerPoint"){
              if(curr_tab[["skip"]]){
                message("Skipping table: ", tid, " (NA found, not generated)")
              } else {
                # Adding one slide per table
                for(tpage in 1:length(curr_tab[["table"]][["ft"]])){
                  elements = list()
                  # The title element is the table  caption:
                  elements[[rpttabfmt[["master"]][["title_ph"]]]] =
                    list( content      = curr_tab[["title_proc"]],
                          type         = "text")
                  elements[[ rpttabfmt[["master"]][["content_ph"]]]] =
                      list( content      = curr_tab[["table"]][["ft"]][[tpage]],
                            type         = "flextable_object")
                  # Adding the slide
                  obnd =onbrand::report_add_slide(obnd,
                           template = rpttabfmt[["master"]][["name"]],
                           elements = elements)
                }
              }
            }
            if(rpttype == "Word"){
              if(curr_tab[["skip"]]){
                message("Skipping table: ", tid, " (NA found, not generated)")
              } else {
                for(tpage in 1:length(curr_tab[["table"]][["ft"]])){
                  obnd =  onbrand::report_add_doc_content(obnd,
                    type     = "flextable_object",
                    content  = list(ft              = curr_tab[["table"]][["ft"]][[tpage]],
                                    caption_format  = curr_tab[["caption_format"]],
                                    caption         = curr_tab[["caption_proc"]]))
                  # adding breaks between tables of multipage tables
                  # between tables 1 & 2, 2 & 3,  ... n-1 & n
                  if(length(curr_tab[["table"]][["ft"]])  > 1){
                    if( tpage < length(curr_tab[["table"]][["ft"]])){
                      obnd = onbrand::report_add_doc_content(obnd,
                        type     = "break",
                        content  = NULL)
                    }
                  }
                }
              }
              if(ele_ctr < length(rptcont)){
                add_page_break = TRUE
              }
            }
          }
          last_eletype = "table"
        }
        #/table
        #-----------------------------------
      }
      ele_ctr = ele_ctr + 1
    }

    # If we've changed orientation some where above
    # we need to wrap it up here at the end of the document
    if(rpttype == "Word"){
      if(wrap_up_orientation){
        obnd = onbrand::report_add_doc_content(obnd,
          type     = "section",
          content  = list(section_type  =curr_orientation))
      }
    }
  }


  if(!is.null(msgs)){
     msgs = c(msgs, paste0("report_fit()"))
     msgs = c(msgs, paste0("rptyaml: ", rptyaml))
     message(paste(msgs, collapse="\n"))
  }



return(obnd)}


#'@export
#'@title Substitutes Placehodlers in Strings
#'@description Takes placeholder information from the rptyaml file and
#'applies it to strings.
#'@param str String to process
#'@param rptdetails Object creating when reading in rptyaml file
#'(default: \code{NULL})
#'@return processed string
#'@examples
#'library(onbrand)  
#'obnd = read_template(
#'  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
#'  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
#'
#'# We also need an nlmixr fit object
#'fit = fetch_fit_example()
#'
#'# This reads in the report details as well
#'rptdetails = yaml_read_fit(
#'  obnd    = obnd,
#'  rptyaml = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml"),
#'  fit     = fit)$rptdetails
#'
#' str = "This is ===CMPD==="
#'
#' process_ph(str, rptdetails)
process_ph = function(str, rptdetails){
  if("placeholders" %in% names(rptdetails)){
    for(PHNAME in names(rptdetails[["placeholders"]])){
      str = stringr::str_replace_all(
              string      = str,
              pattern     = paste0("===", PHNAME, "==="),
              replacement = rptdetails[["placeholders"]][[PHNAME]])
    }
  }
str}

#'@export
#'@title Reads and Checks `report_fit.yaml` File
#'@description Reads in the report yaml file and looks it to make sure it has all
#'the necessary fields for the given report.
#'@param obnd onbrand report object to have report elements appended to.
#'@param fit nlmixr2 fit object to be reported.
#'@param rptyaml yaml file containing the report elements and structure.
#'@param placeholders list of placeholders to overwrite defaults in the yaml file.
#'@param parameters  list with element names for each parameter to overwrite at runtime.
#'with a named list for example RUN may be "RUNN" in the yaml file. To
#'overwrite this just provide \code{list(RUN="RUN_1")}
#'(default: \code{NULL})
#'@return List containing the following information about the report
#' \itemize{
#'   \item \code{"isgood"} - Boolean variable indicating success or failure
#'   \item \code{"msgs"} - Vector of messages
#'   \item \code{"rpttype"} - Type of onbrand report ("Word" or "PowerPoint")
#'   \item \code{"rptfigfmt"} - Default figure formatting (orientation and dimensions)
#'   \item \code{"rpttabfmt"} - Default table formatting (orientation and dimensions)
#'   \item \code{"rptdetails"} - Contents of the yaml file
#'   \item \code{"rptcont"} - Contents of the report to generate
#' }
#'@examples
#'# We need an onbrand object to use below
#'library(onbrand)  
#'obnd = read_template(
#'  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.docx"),
#'  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
#'
#'# We also need an nlmixr fit object
#'fit = fetch_fit_example()
#'
#'# This reads in the report details
#'yaml_read_res = yaml_read_fit(
#'  obnd    = obnd,
#'  rptyaml = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml"),
#'  fit     = fit)
yaml_read_fit = function(obnd = NULL,rptyaml=NULL, placeholders=NULL, parameters=NULL, fit=NULL){
  isgood     = TRUE
  msgs       = c()
  rptdetails = NULL
  rptfigfmt  = NULL
  rpttabfmt  = NULL
  rpttype    = obnd[["rpttype"]]
  rptcont    = NULL

  if(is.null(rptyaml)){
    isgood=FALSE
    msgs = c(msgs, "rptyaml was not specified")
  } else {
    if(file.exists(rptyaml)){
      #------------------------
      # Loading the yaml file
      rptdetails = yaml::read_yaml(rptyaml)
    } else {
      isgood = FALSE
      msgs = c(msgs, "rptyaml was not specified"               )
    }
  }

  # If we got this far then we should have the yamldetails
  if(isgood){
    # This pulls out the report elements for the type of report found in
    # the obnd report
    if((rpttype =="PowerPoint") & ("pptx" %in% names(rptdetails))){
      # Report content
      if("content" %in% names(rptdetails[["pptx"]])){
        rptcont= rptdetails[["pptx"]][["content"]]
      } else {
        isgood = FALSE
        msgs = c(msgs, "Unable to find PowerPoint content in the yaml file")
        msgs = c(msgs, "pptx:")
        msgs = c(msgs, "  content")
      }
      # Figure formatting
      if( !is.null(rptdetails[["pptx"]][["figures"]][["master"]][["name"]])          &
          !is.null(rptdetails[["pptx"]][["figures"]][["master"]][["title_ph"]])      &
          !is.null(rptdetails[["pptx"]][["figures"]][["width"]])                     &
          !is.null(rptdetails[["pptx"]][["figures"]][["height"]])                    &
          !is.null(rptdetails[["pptx"]][["figures"]][["master"]][["content_ph"]])){
        rptfigfmt = rptdetails[["pptx"]][["figures"]]
      } else {
        isgood = FALSE
        msgs = c(msgs, "Unable to find PowerPoint figure formatting details in the yaml file.")
        msgs = c(msgs, "The structure should look like this:")
        msgs = c(msgs, 'pptx:                           ')
        msgs = c(msgs, '  figures:                      ')
        msgs = c(msgs, '    master:                     ')
        msgs = c(msgs, '      name:        "content_text')
        msgs = c(msgs, '      title_ph:    "title"      ')
        msgs = c(msgs, '      contente_ph: "content_body')
        msgs = c(msgs, '    width:  9.5                 ')
        msgs = c(msgs, '    height: 5.0                 ')
      }
      # Table formatting
      if( !is.null(rptdetails[["pptx"]][["tables"]][["master"]][["name"]])          &
          !is.null(rptdetails[["pptx"]][["tables"]][["master"]][["title_ph"]])      &
          !is.null(rptdetails[["pptx"]][["tables"]][["master"]][["content_ph"]])){
        rpttabfmt = rptdetails[["pptx"]][["tables"]]
      } else {
        isgood = FALSE
        msgs = c(msgs, "Unable to find PowerPoint table formatting details in the yaml file.")
        msgs = c(msgs, "The structure should look like this:")
        msgs = c(msgs, 'pptx:                           ')
        msgs = c(msgs, '  tables:                       ')
        msgs = c(msgs, '    master:                     ')
        msgs = c(msgs, '      name:        "content_text')
        msgs = c(msgs, '      title_ph:    "title"      ')
        msgs = c(msgs, '      contente_ph: "content_body')
      }
    }
    if((rpttype =="Word") & ("docx" %in% names(rptdetails))){
      # Report content
      if("content" %in% names(rptdetails[["docx"]])){
        rptcont= rptdetails[["docx"]][["content"]]
      } else {
        isgood = FALSE
        msgs = c(msgs, "Unable to find Word content in the yaml file")
        msgs = c(msgs, "docx:")
        msgs = c(msgs, "  content")
      }
      if( !is.null(rptdetails[["docx"]][["figures"]][["landscape"]][["width"]])     &
          !is.null(rptdetails[["docx"]][["figures"]][["landscape"]][["width"]])     &
          !is.null(rptdetails[["docx"]][["figures"]][["portrait"]][["height"]])     &
          !is.null(rptdetails[["docx"]][["figures"]][["portrait"]][["width"]])){
        rptfigfmt = rptdetails[["docx"]][["figures"]]
      } else {
        isgood = FALSE
        msgs = c(msgs, "Unable to find Word figure formatting details in the yaml file.")
        msgs = c(msgs, "The structure should look like this:")
        msgs = c(msgs, 'docx:                           ')
        msgs = c(msgs, '  figures:                      ')
        msgs = c(msgs, '    landscape:                  ')
        msgs = c(msgs, '      width:  9.5               ')
        msgs = c(msgs, '      height: 8.0               ')
        msgs = c(msgs, '    portrait:                   ')
        msgs = c(msgs, '      width:  6.5               ')
        msgs = c(msgs, '      height: 8.0               ')
      }
    }
  }



  # Processing manual placeholders specified at runtime
  if(!is.null(placeholders)){
    for(phname in names(placeholders)){
      # If the manual placeholder isnt present in the yaml file we
      # notify the user and flip the isgood flag
      if(is.null(rptdetails[["placeholders"]][[phname]])){
        msgs = c(msgs, paste0("Manual placeholder: ", phname, " not found in the yaml file"))
        isgood = FALSE
      } else {
        # If it's there we just overwrite the defaults from the yaml file
        rptdetails[["placeholders"]][[phname]] = placeholders[[phname]]
      }
    }
  }



  # Adding runtime defined parameters
  if(!is.null(parameters)){
    for(pname in names(parameters)){
      if("md" %in% names(parameters[[pname]]) &
         "txt" %in% names(parameters[[pname]])){
        rptdetails[["parameters"]][[pname]] = parameters[[pname]]
      } else {
        cli::cli_alert_warning(paste0("Parameter: ", pname, " must have both md and txt specified. Skipping."))
      }
    }
  }



  # Now we attempt to evaulate the placeholders. This allows the user to
  # define the

  for(phname in names(rptdetails[["placeholders"]])){
    phres = paste(as.character(eval_str(estr = rptdetails[["placeholders"]][[phname]],
                                        fit  = fit)), collapse = ", ")
    rptdetails[["placeholders"]][[phname]] = phres
  }


  if(!is.null(msgs)){
     msgs = c(msgs, paste0("yaml_read_fit()"))
     message(paste(msgs, collapse="\n"))
  }

  res = list(isgood      = isgood,
              msgs       = msgs,
              rptcont    = rptcont,
              rpttype    = rpttype,
              rptfigfmt  = rptfigfmt,
              rpttabfmt  = rpttabfmt,
              rptdetails = rptdetails)

return(res)}

#'@export
#'@title Evaluate R Code in String
#'@description Attempts to evaluate a string as a chunk of R code.
#'@param estr     Object creating when reading in rptyaml file
#'@param fit nlmixr2 fit object to be reported
#'@return String containing the evaled as a character or the original string
#'@examples
#' res = eval_str(estr="ls()")
eval_str  <- function(estr="", fit=NULL){

  res =
    tryCatch(
      {
       ev_res = eval(parse(text=estr))
       ev_res},
     error = function(e) {
      estr})

  return(res)}

#'@export
#'@title Fetch Analysis Options
#'@description Fetches analysis options from the report yaml
#'applies it to strings.
#'@param rptdetails Object creating when reading in rptyaml file
#'@param option String containing the option to fetch (see below)
#'@param fit nlmixr2 fit object to be reported
#'@param verbose Boolean variable when set to \code{TRUE} (default) messages will be
#'displayed on the terminal
#'following:
#'@return List containing the following information about the output directory
#' \itemize{
#'   \item \code{"isgood"} - Boolean variable indicating success or failure
#'   \item \code{"msgs"} - Vector of messages
#'   \item \code{"value"} - The value of the option or the default if not
#'   specified
#' }
#'@details The option can be one of the following (default: \code{NULL}):
#' \itemize{
#'   \item \code{"output_dir"} - Directory to place figures that are generated (default: \code{tempdir()})
#'   \item \code{"resolution"} - Resolution of figure files (default: \code{300})
#' }
#'@examples
#'library(onbrand)  
#'obnd = read_template(
#'  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
#'  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
#'
#'# This will create an example fit object to use in the examples below
#'fit = fetch_fit_example()
#'
#'# This reads in the report details as well
#'rptdetails = yaml_read_fit(
#'  obnd    = obnd,
#'  rptyaml = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml"),
#'  fit     = fit)$rptdetails
#'
#'
#'fetch_option(rptdetails, option="output_dir", fit=fit)
fetch_option  <- function(rptdetails, option=NULL, fit=NULL, verbose=TRUE){
  isgood            = TRUE
  msgs              = c()
  output_dir        = tempdir()
  resolution        = 300
  value             = NULL
  figenv_preamble   = NULL
  tabenv_preamble   = NULL
  fig_stamp         = NULL
  fig_stamp_postion = c(.1,.1)


  if(is.null(option)){
    isgood = FALSE
    msgs = c(msgs, "You must specify an option. Either:")
    msgs = c(msgs, '  "output_dir" ')
    msgs = c(msgs, '  "resolution"')
  } else if(option == "output_dir"){
    # This is triggered if we cannot find the output directory in the yaml file
    if(is.null(rptdetails[["options"]][["output_dir"]])){
      msgs = c(msgs, "rptdetails does not contain an output directory specification")
      msgs = c(msgs, "options:")
      msgs = c(msgs, paste0("  output_dir: ", '"',"'", "mydir" ,"'", '"'))
      msgs = c(msgs, paste0("using tempdir() instead"))
    } else {
      # If we do find it we do placeholders substituion:
      od_cmd = process_ph(rptdetails[["options"]][["output_dir"]], rptdetails=rptdetails)
      # Then we try to eval that command
      output_dir = as.character(eval_str(estr=od_cmd, fit=fit))
      # making sure the directory exists
      if(!dir.exists(output_dir)){
        # if it doesn't exist we try to create it:
        if(!dir.create(output_dir, recursive=TRUE)){
          isgood = FALSE
          msgs = c(msgs, paste0("The output directory: ", output_dir, " dose not exist",))
          msgs = c(msgs, paste0("and was unable to be created"))
        }
      }
    }
    value = output_dir
  } else if(option == "resolution"){
    if(is.null(rptdetails[["options"]][["output_dir"]])){
      msgs = c(msgs, "rptdetails does not contain a resolution specification")
      msgs = c(msgs, "options:")
      msgs = c(msgs, paste0("  resolution: 300 "))
      msgs = c(msgs, paste0("using ", resolution, " instead"))
    } else {
      # If we do find it we do placeholders substituion:
      resolution = as.numeric(eval_str(estr=rptdetails[["options"]][["resolution"]], fit=fit))
    }
    value = resolution
  } else if(option == "figenv_preamble"){
    if(!is.null(rptdetails[["options"]][["figenv_preamble"]])){
      figenv_preamble = rptdetails[["options"]][["figenv_preamble"]]
    }
    value = figenv_preamble
  } else if(option == "tabenv_preamble"){
    if(!is.null(rptdetails[["options"]][["tabenv_preamble"]])){
      tabenv_preamble = rptdetails[["options"]][["tabenv_preamble"]]
    }
    value = tabenv_preamble
  } else if(option == "fig_stamp"){
    if(is.null(rptdetails[["options"]][["fig_stamp"]])){
      msgs = c(msgs, "rptdetails does not contain a fig_stamp specification")
      msgs = c(msgs, paste0("using ", fig_stamp, " instead"))
    } else {
      fig_stamp    = process_ph(rptdetails[["options"]][["fig_stamp"]], rptdetails=rptdetails)
    }
    value = fig_stamp
  } else {
    msgs = c(msgs, paste0("Unknown option: ", option))

  }

  if(!is.null(msgs) & verbose){
    msgs = c(msgs, paste0("fetch_option()"))
    message(paste(msgs, collapse="\r\n"))
  }

  res = list(isgood     = isgood,
             msgs       = msgs,
             value      = value)
return(res)}



#'@export
#'@title Fetch Fit Example
#'@description Creates an nlmixr2 fit example using `posthoc` estimation
#'method for testing purposes.
#'displayed on the terminal
#'following:
#'@param use_cache Boolean variable used to cache the fit process for the
#'current R session.
#'@return Example nlmixr2 fit object
#'@examples
#' fit = fetch_fit_example()
fetch_fit_example  <- function(use_cache = TRUE){
  fit = nlmixr2extra::theoFitOde
return(fit)}

# fetch_fit_example  <- function(use_cache = TRUE){
# 
# 
#   # This is where the fit will be cached:
#   cache_file = file.path(tempdir(), "fetch_fit_example.rds")
# 
#   if(use_cache & file.exists(cache_file)){
#     # Loading the cache file
#     cli::cli_alert_info("Loading fit from cache file")
#     fit = readRDS(cache_file)
#   } else {
#     file_model = system.file(package="nlmixr2rpt", "examples", "model.R")
#     file_data  = system.file(package="nlmixr2rpt", "examples", "TEST_DATA.csv")
#   
#     my_model = NULL
#     eval(parse(text=paste(readLines(file_model), collapse="\n")))
#   
#     # For the dataset we remove the parameter definitions
#     # and we filter it down to the single dose data for 3, 
#     # 30 and 300 mg
#     DS = read.csv(file_data)                               |> 
#       dplyr::select(-.data[["F1"]])                        |> 
#       dplyr::select(-.data[["ka"]])                        |> 
#       dplyr::select(-.data[["CL"]])                        |> 
#       dplyr::select(-.data[["Vc"]])                        |> 
#       dplyr::select(-.data[["Vp"]])                        |> 
#       dplyr::select(-.data[["Q"]])                         |>  
#       dplyr::filter(.data[["Cohort"]]  %in%  c("SD 3 mg")) |>
#       dplyr::filter(.data[["ID"]]      %in%  c(1,2,3))
#   #   dplyr::filter(.data[["Cohort"]]  %in%  c("SD 3 mg", "SD 30 mg", "SD 300 mg"))
#     
#      model_ui =  rxode2::rxode2(my_model) 
#      model_ui =  eval(parse(text="rxode2::ini(model_ui, TV_ka=fix(log(0.5)))"))
#      model_ui =  eval(parse(text="rxode2::model(model_ui, ka=exp(TV_ka))"))
#     
#     fit = suppressMessages(
#           suppressWarnings(
#             nlmixr2::nlmixr(model_ui, DS, est="posthoc")
#           ))
#   
#     cli::cli_alert_info("Writing fit from cache file")
#     saveRDS(fit, cache_file)
#   }
# 
#   # This is mainly to "use" ggPMX to avoid declared imports should be used
#   # errors
#   ggPMX::is.pmx_gpar(NULL)
# 
# return(fit)}
