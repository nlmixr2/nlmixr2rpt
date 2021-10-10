#'@import onbrand
#'@importFrom yaml read_yaml
#'@importFrom stringr str_replace_all


#'@export
#'@title Report `nlmixr` Fit Results to PowerPoint and Word
#'@description Appends `nlmixr` fit results to an onbrand report object with the
#'content and format of the report in the supplied yaml file
#'@param obnd onbrand report object to have report elements appended to
#'@param fit nlmixr fit object to be reported
#'@param ph Manual placeholders, see \code{\link{yaml_read_fit}} for more
#'details
#'for example RUN may be "RUNN" in the yaml file. To overwrite this just
#'provide \code{list(RUN="RUN_1")} (default: \code{NULL})
#'@param rptyaml yaml file containing the report elements and structure
#'@return onbrand object with the report elements added
report_fit <- function(obnd    = NULL, 
                       fit     = NULL, 
                       rptyaml= system.file(package="rptnlmixr","templates", "report_fit.yaml"),
                       ph      = NULL){
  isgood       = TRUE
  msgs         = c()
  rptdetails   = NULL
  rptcont      = NULL
  rpttype      = "Unknown"
  rptfigfmt    = NULL
  output_dir   = NULL
  resolution   = NULL
  length_units = NULL

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
  yamlrr = yaml_read_fit(obnd    = obnd, 
                         ph      = ph, 
                         rptyaml = rptyaml)
    if(!yamlrr[["isgood"]]){
      isgood = FALSE
    }

    # Defining the pieces locally here. 
    rptcont    = yamlrr[["rptcont"]]
    rptdetails = yamlrr[["rptdetails"]]
    rpttype    = yamlrr[["rpttype"]]
    rptfigfmt  = yamlrr[["rptfigfmt"]]
  }

  if(isgood){
    # Getting the output directory
    res_fo = fetch_option(rptdetails=rptdetails, option="output_dir", verbose=FALSE)
    if(res_fo[["isgood"]]){
      output_dir = res_fo[["value"]]
    } else {
      isgood = FALSE
    }
    # Getting the figure resolutioin
    res_fo = fetch_option(rptdetails=rptdetails, option="resolution")
    if(res_fo[["isgood"]]){
      resolution = res_fo[["value"]]
    } else {
      isgood = FALSE
    }
    # Getting the length units
    res_fo = fetch_option(rptdetails=rptdetails, option="length_units")
    if(res_fo[["isgood"]]){
      length_units = res_fo[["value"]]
    } else {
      isgood = FALSE
    }
  }

  # If the user input is good we move forward :).
  if(isgood){
    #------------------------
    # Building Tables
    message("DEBUG: ---------------------------------------")
    message("DEBUG: Before tables")
    #btres = build_tables(obnd=obnd, fit=fit, rptdetails=rptdetails)
    message("DEBUG: after tables")
    message("DEBUG: ---------------------------------------")
    #------------------------
    # Building Figures
    message("DEBUG: ---------------------------------------")
    message("DEBUG: Before figures")
    bfres = build_figures(obnd=obnd, fit=fit, rptdetails=rptdetails)
    message("DEBUG: after figures")
    message("DEBUG: ---------------------------------------")

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
          # JMH Add for tables
        
          # If the orientation in the report element is different from
          # the current orientation we have to 
          if("figure"  %in% names(rptele)){
          if(rptele[["figure"]] == "res_vs_idv"){
            #browser()
          }
          }
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
            png(width    = 4,        height = 4,      units = length_units, 
                filename = fig_file, res    = resolution)
            print(p_res)
            dev.off()
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
          if(rpttype == "Word"){
            for(fpage in 1:length(curr_fig[["figure"]])){
              obnd = onbrand::report_add_doc_content(obnd,
                type     = "imagefile",
                content  = list(image   = curr_fig[["figure"]][fpage],
                                caption = curr_fig[["caption_proc"]],
                                width   = curr_fig[["width"]],
                                height  = curr_fig[["height"]]))
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
          if(rpttype == "PowerPoint"){
          }
          if(rpttype == "Word"){
          }
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
#'@param obnd onbrand report object to have report elements appended to
#'@param rptyaml yaml file containing the report elements and structure
#'@param ph Placeholders in the yaml file can be overwritten at runtime
#'with a named list for example RUN may be "RUNN" in the yaml file. To 
#'overwrite this just provide \code{list(RUN="RUN_1")} 
#'(default: \code{NULL})
#'@return List containing the following information about the report
#' \itemize{
#'   \item \code{"isgood"} - Boolean variable indicating success or failure
#'   \item \code{"msgs"} - Vector of messages 
#'   \item \code{"rpttype"} - Type of onbrand report ("Word" or "PowerPoint")
#'   \item \code{"rptfigfmt"} - Default figure formatting (orientation and dimensions)
#'   \item \code{"rptdetails"} - Contents of the yaml file
#'   \item \code{"rptcont"} - Contents of the report to generate
#' }
yaml_read_fit = function(obnd = NULL,rptyaml=NULL, ph=NULL){
  isgood     = TRUE
  msgs       = c()
  rptdetails = NULL
  rptfigfmt  = NULL
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
  if(!is.null(ph)){
    for(phname in names(ph)){

      # If the manual placeholder isnt present in the yaml file we
      # notify the user and flip the isgood flag
      if(is.null(rptdetails[["placeholders"]][[phname]])){
        msgs = c(msgs, paste0("Manual placeholder: ", phname, " not found in the yaml file"))
        isgood = FALSE
      } else {
        # If it's there we just overwrite the defaults from the yaml file
        rptdetails[["placeholders"]][[phname]] = ph[[phname]]
      }
    }
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
              rptdetails = rptdetails)

return(res)}




#'@export
#'@title Fetch Analysis Options
#'@description Fetches analysis options from the report yaml 
#'applies it to strings. 
#'@param rptdetails Object creating when reading in rptyaml file
#'@param verbose Boolean variable when set to TRUE (default) messages will be
#'displayed on the terminal
#'@param option String containing the option to fetch can be one of the
#'following:
#' \itemize{
#'   \item \code{"output_dir} - Directory to place figures that are generated (default: \code{tempdir()})
#'   \item \code{"resolution"} - Resolution of figure files (default: \code{300}) 
#'   \item \code{"length_units"} - Units (default: \code{"in"}) 
#' }
#'@return List containing the following information about the output directory
#' \itemize{
#'   \item \code{"isgood"} - Boolean variable indicating success or failure
#'   \item \code{"msgs"} - Vector of messages 
#'   \item \code{"value"} - The value of the option or the default if not
#'   specified
#' }
fetch_option  <- function(rptdetails, verbose=TRUE, option=NULL){
  isgood        = TRUE
  msgs          = c()
  output_dir    = tempdir()
  resolution    = 300
  value         = NULL
  length_units  = "in"


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
      output_dir = eval(parse(text=od_cmd))
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
      resolution = as.numeric(rptdetails[["options"]][["resolution"]])
    }
    value = resolution 
  } else if(option == "length_units"){
    if(is.null(rptdetails[["options"]][["length_units"]])){
      msgs = c(msgs, "rptdetails does not contain a length_units specification")
      msgs = c(msgs, "options:")
      msgs = c(msgs, paste0('  length_units: "in" '))
      msgs = c(msgs, paste0("using ", length_units, " instead"))
    } else {
      # If we do find it we do placeholders substituion:
      length_units = rptdetails[["options"]][["length_units"]]
    }
    value = length_units
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

