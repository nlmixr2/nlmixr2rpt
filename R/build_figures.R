#'@import ggplot2
#'@import grDevices
#'@import xpose
#'@import xpose.nlmixr
#'@import stringr
#'@importFrom ggforce n_pages

#'@export
#'@title Generates Figures for an `nlmixr` Report
#'@description Creates figures specified in a rptyaml file
#'@param obnd `onbrand` report object to have report elements appended to
#'@param fit `nlmixr` fit object to be reported
#'@param rptdetails Object created  when reading in rptyaml file
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
build_figures <- function(obnd       = NULL,
                          fit        = NULL,
                          rptdetails = NULL,
                          verbose    = TRUE){
  isgood       = TRUE
  msgs         = c()
  rptfigs      = list()
  bfres        = list()
  output_dir   = NULL
  resolution   = NULL
  length_units = NULL
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
    # Getting any user defined preamble code
    res_fo = fetch_option(rptdetails=rptdetails, option="preamble")
    if(res_fo[["isgood"]]){
      preamble_str = res_fo[["value"]]
      eval(parse(text=preamble_str))
    } else {
      isgood = FALSE
    }
  }

  if(isgood){
    if("figures" %in% names(rptdetails)){
      # Creating the xpdb object for xpose figures:
      xpdb <- xpose.nlmixr::xpose_data_nlmixr(fit)
      for(fid in names(rptdetails[["figures"]])){
        # Pulling out the current figure information:
        finfo = rptdetails[["figures"]][[fid]]

        # Pulling out the figure width and height
        width  = fetch_fdim(obnd, fid, "width",  rptdetails)
        height = fetch_fdim(obnd, fid, "height", rptdetails)

        # Initializing information about the current figure
        fmsgs = c(paste("figure id:", fid))
        FISGOOD = TRUE
        p_res   = NULL

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
          tcres =
            tryCatch(
              {
               # Evaulating the figure generation code
               suppressMessages(eval(parse(text=finfo[["cmd_proc"]])))
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
            "command run:",
            finfo[["cmd_proc"]],
            paste(" -> call:   ", toString(tcres[["error"]][["call"]])),
            paste(" -> message:", toString(tcres[["error"]][["message"]])))

            # Then we generate a figure holding that information so it will
            # be obvious something went wrong for the user
            p_res = mk_error_fig(fmsgs)

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
          if(is.null(nfpages)){
            # creating and storing the output file name:
            fig_file = file.path(output_dir, paste0(fid, "-", rpttype, ".png"))
            figure   = c(fig_file)

            # dumping the figure to a file
            grDevices::png(width    = width,    height = height, units = length_units,
                filename = fig_file, res    = resolution)
            suppressMessages( print(p_res, page=fpage))
            grDevices::dev.off()
          } else {
            # This will create a figure for each page
            # page so that they can be accessed individually
            for(fpage in 1:nfpages){
              # creating and storing the output file name:
              fig_file =  file.path(output_dir, paste0(fid, "-", fpage,"-", rpttype, ".png"))
              figure   = c(figure, fig_file)

              # dumping the figure to a file
              grDevices::png(width    = width,    height = height, units = length_units,
                  filename = fig_file, res    = resolution)
              suppressMessages( print(p_res, page=fpage))
              grDevices::dev.off()
            }
          }
        } else if(is.character(p_res)){
          #JMH test this with a vector of image files
          if(file.exists(p_res)){
            figure = p_res
          }
        }

        # We carry forward the figure info from
        # the yaml file
        rptfigs[[fid]] = finfo

        # Store the stauts of the figurej
        rptfigs[[fid]][["isgood"]] = FISGOOD

        # Now we append any messages generated
        rptfigs[[fid]][["msgs"]] = fmsgs

        # Now we append the figure
        rptfigs[[fid]][["figure"]] = figure

        # Type of figure
        rptfigs[[fid]][["height"]] = height
        rptfigs[[fid]][["width"]]  = width
      }
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

