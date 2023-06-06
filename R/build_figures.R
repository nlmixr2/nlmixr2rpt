#'@import ggplot2
#'@import grDevices
#'@import xpose
#'@import stringr
#'@importFrom cli cli_alert cli_h3
#'@importFrom ggforce n_pages
#'@importFrom xpose.nlmixr2 xpose_data_nlmixr

#'@export
#'@title Generates Figures for an `nlmixr2` Report
#'@description Creates figures specified in a rptyaml file
#'@param obnd `onbrand` report object to have report elements appended to
#'@param fit `nlmixr2` fit object to be reported
#'@param rptdetails Object created  when reading in rptyaml file
#'@param cat_covars character vector of categorical covariates to overwrite defaults in yaml file
#'@param cont_covars character vector of continuous covariates to overwrite defaults in yaml file
#'@param verbose Boolean variable when set to TRUE (default) messages will be
#'displayed on the terminal
#'@return List containing the figures with the following structure:
#' \itemize{
#'   \item \code{"rptfigs"} - List of figures with names corresponding to the
#'   figure ids in the yaml file. Each figure ID contains the following elements:
#'   \itemize{
#'     \item \code{"figure"}       - list of figure file names for the current fid
#'     \item \code{"orientation"}  - Figure orientation ("portrait" or "landscape")
#'     \item \code{"isgood"}       - Boolean variable indicating success or failure
#'     \item \code{"skip"}         - Boolean variable indicating whether the figure should be skipped during reporting
#'     \item \code{"fmsgs"}        - Vector of messages
#'     \item \code{"cmd"}          - Original plot generation command
#'     \item \code{"cmd_proc"}     - Plot generation command after processing for placeholders
#'     \item \code{"height"}       - Figure height
#'     \item \code{"width"}        - Figure width
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
#'# Now we will build the figures
#'bfres = build_figures(obnd       = obnd,
#'                      fit        = fit, 
#'                      rptdetails = rptdetails)
build_figures <- function(obnd        = NULL,
                          fit         = NULL,
                          rptdetails  = NULL,
                          cat_covars  = NULL,
                          cont_covars = NULL,
                          verbose     = TRUE){
  isgood             = TRUE
  msgs               = c()
  rptfigs            = list()
  bfres              = list()
  output_dir         = NULL
  resolution         = NULL
  rpttype            = "Unknown"
                     
  fig_stamp          = NULL


  # I need to include a useage of nlmixr2 so I can include it in the 
  # imports section of the package. The generated code is using it.
  # Specifically xpdb = xpose.nlmixr2::xpose_data_nlmixr(fit) seems to be
  # calling it. So I'm 
  if(FALSE){
    tmp = nlmixr2::model()
  }
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
    # Getting the figure resolutioin
    res_fo = fetch_option(rptdetails=rptdetails, option="resolution")
    if(res_fo[["isgood"]]){
      resolution = res_fo[["value"]]
    } else {
      isgood = FALSE
    }
    # Getting any user defined preamble code
    res_fo = fetch_option(rptdetails=rptdetails, option="figenv_preamble")
    if(res_fo[["isgood"]]){
      preamble_str = res_fo[["value"]]
      if(!is.null(preamble_str)){
        eval(parse(text=preamble_str)) }
    } else {
      isgood = FALSE
    }

    # Getting the fig_stamp
    res_fo = fetch_option(rptdetails=rptdetails, option="fig_stamp")
    if(res_fo[["isgood"]]){
      fig_stamp    = res_fo[["value"]]
    } else {
      isgood = FALSE
    }


  }

  # Caption format information
  allowed_cap_fmts = c("md", "text")
  cap_fmt_def = "text"

  if(isgood){
    if("figures" %in% names(rptdetails)){
      if(verbose){cli::cli_h3(paste0("Building report figures")) }

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


      if(verbose){cli::cli_ul()}
      for(fid in names(rptdetails[["figures"]])){
        # Pulling out the current figure information:
        finfo = rptdetails[["figures"]][[fid]]
        if(verbose){cli::cli_alert(paste0(fid))}

        # Pulling out the figure width and height
        width  = fetch_fdim(obnd, fid, "width",  rptdetails)
        height = fetch_fdim(obnd, fid, "height", rptdetails)

        # Initializing information about the current figure
        fmsgs = c(paste("figure id:", fid))
        FISGOOD = TRUE
        SKIP    = FALSE
        p_res   = NULL


        # Figuring out the caption format
        if("caption_format" %in% names(finfo)){
          if(finfo[["caption_format"]] %in% allowed_cap_fmts){
            caption_format = finfo[["caption_format"]]
          } else {
            fmsgs = c(fmsgs, paste("unknown caption format:", finfo[["caption_format"]]))
            caption_format = cap_fmt_def
          }
        } else {
          caption_format = cap_fmt_def
        }

        # If we have a cmd field we try to evaluate that
        if("cmd" %in% names(finfo)){
          # Here we apply any placeholder information that may be
          # stored in the command
          finfo[["cmd_proc"]] = process_ph(finfo[["cmd"]],rptdetails=rptdetails)
          # We also apply any placeholder informaiton found in the caption
          if("caption" %in% names(finfo)){
            finfo[["caption_proc"]] = process_ph(finfo[["caption"]],rptdetails=rptdetails)
          }
          if("title" %in% names(finfo)){
            finfo[["title_proc"]] = process_ph(finfo[["title"]],rptdetails=rptdetails)
          }

          # This will attempt to build the figure and trap any errors that are
          # encountered
          tcres =
            tryCatch(
              {
               # Evaulating the figure generation code
               suppressMessages(eval(parse(text=finfo[["cmd_proc"]])))
               # Some errors don't show up until the figures are built
               # while saving. This will force ggplot objects to be built
               # and trap any errors to be passed on to the user.
               if(is.ggplot(p_res)){
                 ggplot2::ggplot_build(p_res)
               }
              list(isgood=TRUE, p_res=p_res)},
             error = function(e) {
              list(isgood=FALSE, error=e)})

          # Now we dig into the results of the trycatch above
          if(tcres[["isgood"]]){
            # If everything worked out we just return the result
            p_res = tcres[["p_res"]]
          } else {
            # Otherwise we capture erro information here:
            fmsgs = c(fmsgs,
            "Unable to generate figure",
            paste(" -> call:   ", toString(tcres[["error"]][["call"]])),
            paste(" -> message:", toString(tcres[["error"]][["message"]])),
            "command run:",
            finfo[["cmd_proc"]])

            # Then we generate a figure holding that information so it will
            # be obvious something went wrong for the user
            p_res = mk_error_fig(fmsgs)

            # Dumping the messages to the console as well
            if(verbose){
              cli::cli_h3("Figure generation failed")
              for(msg in fmsgs){
                cli::cli_alert(msg)
              }
            }

            # We now set the figure good flag to false
            FISGOOD = FALSE
          }
        } else {
          FISGOOD = FALSE
          fmsgs = c(fmsgs, "No 'cmd' field found")
        }

        figure = c()
        # Figuring out if we have a ggplot or an image file:

        if(is.ggplot(p_res)){
          # This is the number of figure pages in the current figure. If
          # The figure isn't paginated, it will return NULL
          nfpages = ggforce::n_pages(p_res)

          # Starting the sub bullets
          if(verbose){ cli_list_fn     = cli::cli_ul() }
          
          if(is.null(nfpages)){
            # creating and storing the output file name:
            fig_file = file.path(output_dir, paste0(fid, "-", rpttype, ".png"))
            figure   = c(fig_file)

            if(verbose){ cli::cli_li(fig_file) }
             wfres = write_figure(
               p_res              = p_res,
               page               = NULL, 
               width              = width,
               height             = height,
               resolution         = resolution, 
               fig_file           = fig_file,
               fig_stamp          = fig_stamp,
               verbose            = verbose)
          } else {
            # This will create a figure for each page
            # page so that they can be accessed individually
            for(fpage in 1:nfpages){
              # creating and storing the output file name:
              fig_file =  file.path(output_dir, paste0(fid, "-", fpage,"-", rpttype, ".png"))
              figure   = c(figure, fig_file)

              # dumping the figure to a file
              if(verbose){ cli::cli_li(fig_file) }
              wfres = write_figure(
                p_res              = p_res,
                page               = fpage,
                width              = width,
                height             = height,
                resolution         = resolution, 
                fig_file           = fig_file,
                fig_stamp          = fig_stamp,
                verbose            = verbose)
             # grDevices::png(width    = width,    height = height, units = "in",
             #     filename = fig_file, res    = resolution)
             # suppressMessages( print(p_res, page=fpage))
             # grDevices::dev.off()
            }
          }
          # Closing the subbullets
          if(verbose){cli::cli_end(cli_list_fn)}
        } else if(is.na(p_res)){
          # Figure was set to NA to skip
          SKIP   = TRUE
          figure = p_res
        } else if(is.character(p_res)){
          #JMH test this with a vector of image files
          if(file.exists(p_res)){
            figure = p_res
          }
        }

        # Storing the caption format for alter use
        finfo[["caption_format"]] = caption_format

        # We carry forward the figure info from
        # the yaml file
        rptfigs[[fid]] = finfo

        # Store the stauts of the figure
        rptfigs[[fid]][["isgood"]] = FISGOOD

        # Store the skip state of the figure
        rptfigs[[fid]][["skip"]] = SKIP   

        # Now we append any messages generated
        rptfigs[[fid]][["msgs"]] = fmsgs

        # Now we append the figure
        rptfigs[[fid]][["figure"]] = figure

        # Type of figure
        rptfigs[[fid]][["height"]] = height
        rptfigs[[fid]][["width"]]  = width
      }
      if(verbose){cli::cli_end() }
    } else {
      isgood = FALSE
      msgs   = c(msgs, "No figures found in rptdetails")
    }
  }

  if(verbose){
    if(!is.null(msgs)){
      msgs = c(msgs, "build_figures()")
      message(paste(msgs, collapse="\r\n"))
    }
  }

  # Packing everythign up to return it to the user
  bfres   = list(
    rptfigs = rptfigs,
    isgood  = isgood,
    msgs    = msgs)


return(bfres)}


#'@export
#'@title Generates `ggplot` Object with Error Message
#'@description Takes a vector of messages and returns a ggplot object with the
#'text in the figure. This can be used in automated figure generation to
#'cascade an error message to the end user.
#'@param msgs Vector of error messages
#'@return ggplot object
#'@examples
#'mk_error_fig("This is an error")

mk_error_fig  <- function(msgs){
  p_res = ggplot()+annotate("text",
                   hjust= 0, vjust=1,
                   x=0, y=0,
                   label = paste(msgs, collapse="\n")) +
    xlab(NULL) + ylab(NULL)  + theme(axis.ticks = element_blank()) +
    scale_x_continuous(labels = NULL, limits = c(0,1))        +
    scale_y_continuous(labels = NULL, limits = c(-1,0))

p_res}

#'@export
#'@title Gets Figure Dimensions
#'@description For a given figure id and report type this will pull out the
#'dimensions of the figure.
#'@param obnd onbrand report object to have report elements appended to
#'@param fid Figure ID used in the figures section of the yaml file
#'@param rptdetails Object creating when reading in rptyaml file
#'@param fdim Dimension to fetch either "width" or "height"
#'@return ggplot object
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
#'fetch_fdim(obnd=obnd, fid="bad_figure", fdim="width", rptdetails=rptdetails)
#'
#'fetch_fdim(obnd=obnd, fid="bad_figure", fdim="height", rptdetails=rptdetails)
fetch_fdim  <- function(obnd       = NULL,
                        fid        = NULL,
                        fdim       = "width",
                        rptdetails = NULL){

  # default dimension
  res     = 4
  rpttype = obnd[["rpttype"]]

  if(fid %in% names(rptdetails[["figures"]])){
    finfo = rptdetails[["figures"]][[fid]]

    # Getting the orientation
    orientation = "portrait"
    if("orientation" %in% names(finfo)){
      orientation = finfo[["orientation"]]
    }

    # Getting the height and width defaults:
    if(rpttype == "PowerPoint"){
      res    = rptdetails[["pptx"]][["figures"]][[fdim]]
    }
    if(rpttype == "Word"){
      res    = rptdetails[["docx"]][["figures"]][[orientation]][[fdim]]
    }
  } else {
    message(paste0("Unable to find figure id", fid, "."))
    message("fetch_fdim()")
  }
res}

#'@export
#'@title Writes Figures to File
#'@description Takes a figure object, optionally
#'stamps the image, and writes to a file
#'dimensions of the figure.
#'@param p_res ggplot, ggforce paginated object, or ggarrange object.
#'@param page  Page number to write or NULL for a ggplot object.
#'@param width Width in inches.
#'@param height Height in inches.
#'@param resolution resolution in dpi.
#'@param fig_file File name to write the figure to.
#'@param fig_stamp Character object containing the text to stamp on the figure with optional ===file=== placeholder.
#'@param verbose Boolean variable when set to TRUE (default) messages will be.
#'displayed on the terminal
#'@return list with the following 
#' \itemize{
#'   \item \code{"isgood"} - Boolean variable indicating success or failure
#'   \item \code{"msgs"} - Vector of messages
#' }
#'@examples
#'library(ggplot2)
#'write_figure(
#'  p_res = ggplot(),
#'  page = NULL,
#'  width = 3,
#'  height = 3,
#'  resolution = 200,
#'  fig_file = file.path(tempdir(), "write_figure_example.png"),
#'  fig_stamp = "stamp",
#'  verbose = TRUE)
write_figure  <- function(p_res              = NULL,
                          page               = NULL,
                          width              = 3,
                          height             = 3,
                          resolution         = NULL,
                          fig_file           = NULL,
                          fig_stamp          = NULL,
                          verbose            = TRUE){
                                             
  isgood = TRUE
  msgs   = c()

  tcres =
    tryCatch(
      {
       # Adding stamps if necessary
       if(is.ggplot(p_res)){
         if(!is.null(fig_stamp)){
           # If we are processing a ggplot object and fig_stamp has been
           # defined we append the fig_stamp to the figure:
           fig_stamp = stringr::str_replace_all(fig_stamp, "===FILE===", fig_file)


          ## If the plot is a ggplot but hasn't been arranged we arrange it
          ## so we can stamp it
          #if(!("ggarrange" %in% class(p_res)) & ("ggplot" %in% class(p_res))){
          #  p_res <- ggpubr::ggarrange(p_res)
          #}

           # If we're using a manually paneled figure using ggarrange we have
           # to use annotate_figure:
           if("ggarrange" %in% class(p_res)){
           # p_res = ggpubr::annotate_figure(p_res, 
           #          fig.lab=paste(fig_stamp, "\n"),
           #          fig.lab.size=5,
           #          fig.lab.pos="bottom.left")

             p_res = ggpubr::annotate_figure(
                      bottom=ggpubr::text_grob(fig_stamp,
                                       #just="left",
                                       hjust=1,
                                       vjust=0,
                                       size=5), p_res)
         


           } else {
             p_res = p_res +
               labs(tag=fig_stamp) +
               theme(plot.tag.position=c(.01, -.01), 
                     plot.tag=element_text(size=5, vjust=0, hjust=0))
               
           }
         }
       }
       # Saving the figure:
       grDevices::png(width    = width,    
                      height   = height, 
                      units    = "in",
           filename = fig_file, 
           res      = resolution)
       if(is.null(page)){
         suppressMessages( print(p_res))
       } else {
         suppressMessages( print(p_res, page=page))
       }
       grDevices::dev.off()

      list(isgood=TRUE, p_res=p_res)},
     error = function(e) {
      list(isgood=FALSE, error=e)})

  # If we failed to save the figure we return the errors to the user
  if(!tcres[["isgood"]]){
    # Creating messages from the try catch failure
    msgs = c(msgs,
    "Unable to save figure",
    paste(" -> call:   ", toString(tcres[["error"]][["call"]])),
    paste(" -> message:", toString(tcres[["error"]][["message"]])))

    # Generating an error figure and putting it into the specified output file
    p_res = mk_error_fig(msgs)
    # Writing the error figure:
    grDevices::png(width    = width,    
                   height   = height, 
                   units    = "in",
        filename = fig_file, 
        res      = resolution)
    suppressMessages( print(p_res))
    grDevices::dev.off()

    if(verbose){
      cli::cli_h3("Writing figure failed:")
      for(msg in msgs){
        cli::cli_alert(msg)
      }
    }
  }

  res = list(isgood = isgood,
             msgs   = msgs)
res}
