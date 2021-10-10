#'@importFrom flextable flextable
#'@export
#'@title Generates Tables  for an `nlmixr` Report
#'@description Creates tables specified in a rptyaml file
#'@param obnd onbrand report object to have report elements appended to
#'@param fit nlmixr fit object to be reported
#'@param rptdetails object creating when reading in rptyaml file
#'@param verbose Boolean variable when set to TRUE (default) messages will be
#'displayed on the terminal
#'@return list of tables objects with names taken from the tables element in
#'the rptdetails object
build_tables  <- function(obnd       = NULL, 
                          fit        = NULL, 
                          rptdetails = NULL,
                          verbose    = TRUE){

  isgood     = TRUE
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
  }


  if(isgood){
    if("tables" %in% names(rptdetails)){
      #xpdb <- xpose.nlmixr::xpose_data_nlmixr(fit)
      for(tid in names(rptdetails[["tables"]])){
        # Pulling out the current table information:
        tinfo = rptdetails[["tables"]][[tid]]
        # Initializing information about the current table  
        tmsgs = c(paste("table id:", tid))
        TISGOOD = TRUE
        t_res   = NULL

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
            tmsgs = c(tmsgs, 
            "Unable to generate table", 
            "command run:", 
            tinfo[["cmd_proc"]],
            paste(" -> call:   ", toString(tcres[["error"]][["call"]])),
            paste(" -> message:", toString(tcres[["error"]][["message"]])))

            # We now set the table good flag to false
            TISGOOD = FALSE
          }
        } else {
          TISGOOD = FALSE
          tmsgs = c(tmsgs, "No 'cmd' field found")
        }


        browser()
        # Now we need to check the t_res to make sure it 
        # has the correct fields
        if(TISGOOD){
          df_found = FALSE
          ft_found = FALSE

          #looking for either a data frame or a flextable


          # If we haven't found either we create an error
          if(!df_found &!ft_found){
            tmsgs = c(tmsgs, paste0("Error processing table id ", tid, "."))
            tmsgs = c(tmsgs, paste0("t_res must contain at least one of the follwoing fields:"))
            tmsgs = c(tmsgs, paste0("  - df: list of data frames for the table."))
            tmsgs = c(tmsgs, paste0("  - ft: list of flextables for the table."))
            TISGOOD = FALSE
          }
          

        }


        # If we get to this point and the table isn't good then we generate 
        # a table holding any error information so it will be obvious 
        # something went wrong for the user when they look at the final report

        if(!TISGOOD){
          t_res = mk_error_tab(tmsgs)
        }

        # We carry forward the table info from
        # the yaml file
        rpttabs[[tid]] = tinfo

        # Store the stauts of the table    
        rpttabs[[tid]][["isgood"]] = TISGOOD

        # Now we append any messages generated
        rpttabs[[tid]][["msgs"]] = tmsgs

        # Now we append the table  
        rpttabs[[tid]][["table"]] = t_res


      }
    
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
#'@title Makes `nlmixr` Parameter Estimate Table for Reporting
#'@description Generates a flextable containting the parameter estimates.
#'@param obnd onbrand report object to have report elements appended to
#'@param fit nlmixr fit object to be reported
#'@param rptdetails object creating when reading in rptyaml file
#'@param verbose Boolean variable when set to TRUE (default) messages will be
#'displayed on the terminal
#'@return # JMH
gen_pest_table  <- function(obnd       = NULL, 
                            fit        = NULL, 
                            rptdetails = NULL,
                            verbose    = TRUE){
  isgood = TRUE
  msgs   = c()

  # Pulling out the data frame with the parameter estimates:
  fex_df = fit$par.fixed

  # Adding parameter names:
  fex_df = fex_df %>%
    dplyr::mutate(Parameter = rownames(fex_df)) %>%
    dplyr::relocate(Parameter)
  for(rname in names(rptdetails$parameters)){
    fex_df  = fex_df %>% 
    dplyr::mutate(Parameter = ifelse(Parameter == rname, 
                                     rptdetails$parameters[[rname]],
                                     Parameter))
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
mk_error_table <- function(msgs){
  browser()
  ft = NULL
  # t_res = ggplot()+annotate("text", 
  #                  hjust= 0, vjust=1,  
  #                  x=0, y=0, 
  #                  label = paste(msgs, collapse="\n")) + 
  #   xlab(NULL) + ylab(NULL)  + theme(axis.ticks = element_blank()) + 
  #   scale_x_continuous(labels = NULL, limits = c(0,1))        +
  #   scale_y_continuous(labels = NULL, limits = c(-1,0)) 
  t_res = list(ft)
t_res}
