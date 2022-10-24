#'@importFrom cli cli_alert cli_h3
#'@importFrom dplyr mutate relocate
#'@importFrom flextable autofit compose flextable
#'@importFrom onbrand fetch_md_def md_to_oo

#'@export
#'@title Generates Tables  for an `nlmixr2` Report
#'@description Creates tables specified in a rptyaml file
#'@param obnd onbrand report object to have report elements appended to
#'@param fit nlmixr2 fit object to be reported
#'@param rptdetails object creating when reading in rptyaml file
#'@param cat_covars character vector of categorical covariates to overwrite defaults in yaml file
#'@param cont_covars character vector of continuous covariates to overwrite defaults in yaml file
#'@param verbose Boolean variable when set to TRUE (default) messages will be
#'displayed on the terminal
#'@return List containing the tables with the following structure:
#' \itemize{
#'   \item \code{"rpttabs"} - List of tables with names corresponding to the
#'   table ids in the yaml file. It contains the elements from the yamle file
#'   and the following elements:
#'   \itemize{
#'     \item \code{"table"}        - Result of build (t_res object)
#'     \item \code{"orientation"}  - Table orientation ("portrait" or "landscape")
#'     \item \code{"isgood"}       - Boolean variable indicating success or failure
#'     \item \code{"skip"}         - Boolean variable indicating whether the table should be skipped during reporting
#'     \item \code{"tmsgs"}        - Vector of messages
#'     \item \code{"cmd"}          - Original plot generation command
#'     \item \code{"cmd_proc"}     - Plot generation command after processing for placeholders
#'     \item \code{"height"}       - Table height
#'     \item \code{"width"}        - Table width
#'     \item \code{"caption"}      - Caption for Word
#'     \item \code{"caption_proc"} - Caption for Word after processing for placeholders
#'     \item \code{"title"}        - Slide title for PowerPoint
#'     \item \code{"title_proc"}   - Slide title for PowerPoint after processing for placeholders
#'   }
#'   \item \code{"isgood"} - Boolean variable indicating success or failure
#'   \item \code{"msgs"} - Vector of messages
#' }
#'@examples
#'# We need an onbrand object to use below
#'\donttest{
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
#'# Now we will build the tables 
#'btres = build_tables(obnd        = obnd,
#'                      fit        = fit, 
#'                      rptdetails = rptdetails)
#'}
build_tables  <- function(obnd        = NULL,
                          fit         = NULL,
                          rptdetails  = NULL,
                          cat_covars  = NULL,
                          cont_covars = NULL,
                          verbose     = TRUE){

  isgood     = TRUE
  SKIP       = FALSE
  msgs       = c()
  rpttabs    = list()
  btres      = list()
  output_dir   = NULL
  rpttype      = "Unknown"

  #------------------------
  # Checking user input
  if(is.null(fit)){
    isgood = FALSE
    msgs = c(msgs, "fit was not specified")
  }
  # First we make sure the user supplied something
  if(is.null(obnd)){
    isgood = FALSE
    msgs = c(msgs, "obnd was not specified")
  } else{
    # now we look and make sure the onbrand object is valid
    if(obnd[["isgood"]]){
      rpttype = obnd[["rpttype"]]
    } else {
      isgood = FALSE
      msgs = c(msgs, "Bad onbrand object supplied")
    }
  }

  if(isgood){
    # Getting the output directory
    res_fo = fetch_option(rptdetails=rptdetails, option="output_dir")
    if(res_fo[["isgood"]]){
      output_dir = res_fo[["value"]]
    } else {
      isgood = FALSE
    }
    # Getting any user defined preamble code
    res_fo = fetch_option(rptdetails=rptdetails, option="tabenv_preamble")
    if(res_fo[["isgood"]]){
      preamble_str = res_fo[["value"]]
      if(!is.null(preamble_str)){
        eval(parse(text=preamble_str))}
    } else {
      isgood = FALSE
    }
  }

  # Caption format information
  allowed_cap_fmts = c("md", "text")
  cap_fmt_def = "text"

  if(isgood){
    if(verbose){cli::cli_h3(paste0("Building report tables")) }
    #---------------------------------------------------
    # Sorting out covariates
    # Defining the covariate lists from the yaml file if 
    # none have been specified
    if(is.null(cat_covars)){
      cat_covars = rptdetails[["covariates"]][["cat"]]
    }
    
    # Checking the covariates to make sure they are in the dataset
    if(!is.null(cat_covars)){
      missing_covars = cat_covars[!(cat_covars %in% names(fit[["origData"]]))]
      if(length(missing_covars) > 0){
        if(verbose){
          cli::cli_alert_warning(paste0("The following categorical covariates were specified"))
          cli::cli_alert_warning(paste0("but not found in the dataset: ")) 
          cli::cli_alert_warning(paste0("   ", missing_covars, collapse=", "))
        }
      }
    
      # Removing the missing covariates
      cat_covars = cat_covars[cat_covars[cat_covars %in% names(fit$origData)]]
      if(length(cat_covars) == 0){
        cat_covars = NULL}
    }
    
    # continuous covariates
    if(is.null(cont_covars)){
      cont_covars = rptdetails[["covariates"]][["cont"]]
    }
    # Checking the covariates to make sure they are in the dataset
    if(!is.null(cont_covars)){
      missing_covars = cont_covars[!(cont_covars %in% names(fit$origData))]
      if(length(missing_covars) > 0){
        if(verbose){
          cli::cli_alert_warning(paste0("The following continuous covariates were specified"))
          cli::cli_alert_warning(paste0("but not found in the dataset: ")) 
          cli::cli_alert_warning(paste0("   ", missing_covars, collapse=", "))
        }
      }
    
      # Removing the missing covariates
      cont_covars = cont_covars[cont_covars[cont_covars %in% names(fit[["origData"]])]]
      if(length(cont_covars) == 0){
        cont_covars = NULL}
    }
    #---------------------------------------------------
    if("tables" %in% names(rptdetails)){
      if(verbose){cli::cli_ul()}
      for(tid in names(rptdetails[["tables"]])){
        # By default we don't skip the figure
        SKIP       = FALSE
        # Pulling out the current table information:
        tinfo = rptdetails[["tables"]][[tid]]
        if(verbose){cli::cli_alert(paste0(tid))}
        # Initializing information about the current table
        tmsgs = c(paste("table id:", tid))
        TISGOOD = TRUE
        t_res   = NULL

        # Figuring out the caption format
        if("caption_format" %in% names(tinfo)){
          if(tinfo[["caption_format"]] %in% allowed_cap_fmts){
            caption_format = tinfo[["caption_format"]]
          } else {
            fmsgs = c(fmsgs, paste("unknown caption format:", tinfo[["caption_format"]]))
            caption_format = cap_fmt_def
          }
        } else {
          caption_format = cap_fmt_def
        }

        # If we have a cmd field we try to evaluate that
        if("cmd" %in% names(tinfo)){
          # Here we apply any placeholder information that may be
          # stored in the command
          tinfo[["cmd_proc"]] = process_ph(tinfo[["cmd"]],rptdetails=rptdetails)
          # We also apply any placeholder informaiton found in the caption and
          # titles
          if("caption" %in% names(tinfo)){
            tinfo[["caption_proc"]] = process_ph(tinfo[["caption"]],rptdetails=rptdetails)
          }
          if("title" %in% names(tinfo)){
            tinfo[["title_proc"]] = process_ph(tinfo[["title"]],rptdetails=rptdetails)
          }
          tcres =
            tryCatch(
              {
               # Evaulating the figure generation code
               suppressMessages(eval(parse(text=tinfo[["cmd_proc"]])))
              list(isgood=TRUE, t_res=t_res)},
             error = function(e) {
              list(isgood=FALSE, error=e)})

          # Now we dig into the results of the trycatch above
          if(tcres[["isgood"]]){
            # If everything worked out we just return the result
            t_res = tcres[["t_res"]]
          } else {
            # Otherwise we capture erro information here:
            tmsgs = c(
            "Unable to generate table",
            paste(" -> call:   ", toString(tcres[["error"]][["call"]])),
            paste(" -> message:", toString(tcres[["error"]][["message"]])),
            tmsgs,
            "command run:",
            tinfo[["cmd_proc"]]
            )

            # We now set the table good flag to false
            TISGOOD = FALSE
          }
        } else {
          TISGOOD = FALSE
          tmsgs = c(tmsgs, "No 'cmd' field found")
        }

        df_found = FALSE
        ft_found = FALSE

        # Detecting NAs to skip
        if(length(t_res) == 1){
          if(is.na(t_res)){
            SKIP = TRUE
          }
        }

        if(TISGOOD){
        if(!SKIP){
          # Now we need to check the t_res to make sure it
          # has the correct fields
            #looking for either a data frame or a flextable
            if("ft" %in% names(t_res)){
              if(length(t_res[["ft"]]) >= 1){
                # We start by indicating that flextables were found:
                ft_found = TRUE
                # Now we check each table to make sure it's a flextable
                for(tnum in 1:length(t_res[["ft"]])){
                  if(!inherits(t_res[["ft"]][[tnum]], "flextable")){
                    # If it's not a flextable we flip the found bit
                    ft_found = FALSE
                    tmsgs = c(tmsgs, paste("ft number", tnum, "should be a flextable but", class(t_res[["ft"]][[1]]), "was found."))
                  }
                }
              } else {
                tmsgs = c(tmsgs, "ft found but empty")
              }
            }
            if("df" %in% names(t_res)){
              if(length(t_res[["df"]]) >= 1){
                # We start by indicating that data.frames were found:
                df_found = TRUE
                # Now we check each table to make sure it's a data.frame
                for(tnum in 1:length(t_res[["df"]])){
                  if(!inherits(t_res[["df"]][[tnum]], "data.frame")){
                    # If it's not a dataframe we flip the found bit
                    df_found = FALSE
                    tmsgs = c(tmsgs, paste("df number", tnum, "should be a data.frame but", class(t_res[["df"]][[1]]), "was found."))
                  }
                }
              } else {
                tmsgs = c(tmsgs, "df found but empty")
              }
            }
            # If we haven't found either we create an error
            if(!df_found &!ft_found){
              tmsgs = c(tmsgs, paste0("Error processing table id ", tid, "."))
              tmsgs = c(tmsgs, paste0("t_res must contain at least one of the following fields:"))
              tmsgs = c(tmsgs, paste0("  - df: list of data frames for the table."))
              tmsgs = c(tmsgs, paste0("  - ft: list of flextables for the table."))
              TISGOOD = FALSE
            }
        }


          # If data.frames were found but flextables were not we just convert
          # the data frame into a flextable
          if(df_found &!ft_found){
            ft_list = list()
            tmsgs = c(tmsgs, paste("Converting data.frames to flextables"))
            for(tnum in 1:length(t_res[["df"]])){
              ft_list[[tnum]] = flextable::flextable(t_res[["df"]][[tnum]])
              # Now we stick the flextable back in the results
              t_res[["ft"]] = ft_list
            }
            ft_found = TRUE
          }

        }

        # If we get to this point and the table isn't good then we generate
        # a table holding any error information so it will be obvious
        # something went wrong for the user when they look at the final report

        if(!TISGOOD){
          t_res = mk_error_tab(tmsgs)

          # Dumping the messages to the console as well
          if(verbose){
            cli::cli_h3("Table generation failed")
            for(msg in tmsgs){
              cli::cli_alert(msg)
            }
          }
        }
        # Storing the caption format for alter use
        tinfo[["caption_format"]] = caption_format

        # We carry forward the table info from
        # the yaml file
        rpttabs[[tid]] = tinfo

        # Store the stauts of the table
        rpttabs[[tid]][["isgood"]] = TISGOOD

        # Store the skip state of the table
        rpttabs[[tid]][["skip"]] = SKIP

        # Now we append any messages generated
        rpttabs[[tid]][["msgs"]] = tmsgs

        # Now we append the table
        rpttabs[[tid]][["table"]] = t_res
      }
    if(verbose){cli::cli_end() }
    } else {
      isgood = FALSE
      msgs   = c(msgs, "No tables found in rptdetails")
    }
  }

  if(verbose){
    if(!is.null(msgs)){
      msgs = c(msgs, "build_tables()")
      message(paste(msgs, collapse="\r\n"))
    }
  }

  btres   = list(
    rpttabs = rpttabs,
    isgood  = isgood,
    msgsg   = msgs)


btres}


#'@export
#'@title Makes `nlmixr2` Parameter Estimate Table for Reporting
#'@description Generates a flextable containting the parameter estimates.
#'@param obnd onbrand report object to have report elements appended to
#'@param fit nlmixr2 fit object to be reported
#'@param rptdetails object creating when reading in rptyaml file
#'@param verbose Boolean variable when set to TRUE (default) messages will be
#'displayed on the terminal
#'@return List with the following elements
#'\itemize{
#'  \item \code{"isgood"}  - Boolean variable indicating success or failure
#'  \item \code{"msgs"}    - Vector of messages
#'  \item \code{"ft"}      - Parameter estimates as a `flextable` object
#'  \item \code{"df"}      - Parameter estimates as a `data.frame`
#'}
#'@examples
#'library(onbrand)  
#'obnd = read_template(
#'  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
#'  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))
#'
#'# This will create an example fit object to use in the examples below
#'fit = fetch_fit_example()
#'
#'#'# This reads in the report details as well
#'rptdetails = yaml_read_fit(
#'  obnd    = obnd,
#'  rptyaml = system.file(package="nlmixr2rpt", "examples", "report_fit_test.yaml"),
#'  fit     = fit)$rptdetails
#'
#'gen_pest_table(obnd = obnd, fit = fit, rptdetails = rptdetails, verbose = TRUE)
gen_pest_table  <- function(obnd       = NULL,
                            fit        = NULL,
                            rptdetails = NULL,
                            verbose    = TRUE){
  isgood = TRUE
  msgs   = c()

  # Pulling out the data frame with the parameter estimates:
  fex_df = fit$par.fixed

  # If there is no Parameter column we add it here
  if(!("Parameter" %in% names(fex_df))){
    fex_df = fex_df %>%
      dplyr::mutate("Parameter" = rownames(fex_df)) %>%
      dplyr::relocate(.data[["Parameter"]])
  }

  # Adding parameter names:
  for(rname in rownames(fex_df)){
    if(!is.null(rptdetails[["parameters"]][[rname]][["md"]])){
      fex_df[rname, "Parameter"] = rptdetails[["parameters"]][[rname]][["md"]]
    } else {
      # If there is no value for the current parameter name we
      # replace it with the row name
      if( fex_df[rname, "Parameter"] == ""){
        fex_df[rname, "Parameter"] = rname
      }
    }
  }

  # Pulling out the default formating
  dft_body = onbrand::fetch_md_def(obnd, style="Table")$md_def

  fex_ft = flextable::flextable(fex_df)
  for(pstr in fex_df$Parameter){
    fex_ft = fex_ft %>%
    flextable::compose(
      j     = "Parameter",
      i     = match(pstr, fex_df$Parameter),
      part  = "body",
      value = c(onbrand::md_to_oo(pstr, dft_body)$oo))
  }

  fex_ft = flextable::autofit(fex_ft)

  res = list(isgood = isgood,
             msgs   = msgs,
             ft     = list(fex_ft),
             df     = list(fex_df))
res}

#'@export
#'@title Generates a `flextable` Object with Error Message
#'@description Takes a vector of messages and returns a `flextable` object with the
#'text in the table. This can be used in automated table generation to
#'cascade an error message to the end user.
#'@param msgs Vector of error messages
#'@return list with a single `flextable` object
#'@examples
#'error_tab = mk_error_tab("This is an error")
#'error_tab$ft[[1]]
mk_error_tab <- function(msgs){
  df = data.frame(Error=paste(msgs, collapse="\n"))
  ft = flextable::flextable(df) %>%
       flextable::autofit()
  t_res = list()
  t_res[["ft"]][[1]] = ft
t_res}
