library(here)
setwd(file.path(here::here(), "tmp"))
library(rxode2)
library(nlmixr2)
library(devtools)
load_all()

library(nlmixr2rpt)


# Examples from the package
file_model = system.file(package="nlmixr2rpt", "examples", "model.R")
file_data  = system.file(package="nlmixr2rpt", "examples", "TEST_DATA.csv")

# Creating my_model locally
source(file_model)
DS = read.csv(file_data) |> 
  dplyr::select(-F1) |> 
  dplyr::select(-ka) |> 
  dplyr::select(-CL) |> 
  dplyr::select(-Vc) |> 
  dplyr::select(-Vp) |> 
  dplyr::select(-Q)  |>  
  dplyr::filter(CMT     %in%  c("C_ng_ml", "Ac")) |>
  dplyr::filter(Cohort  %in%  c("SD 3 mg IV", "SD 30 mg IV", "SD 300 mg IV")) 


model_ui = rxode2(my_model)   |> 
    ini(TV_ka=fix(log(0.5)))  |>
      model(ka=exp(TV_ka)) 

#fit = nlmixr2::nlmixr(model_ui, DS, est="focei")
fit = nlmixr(model_ui, DS, est="saem")

load_all()
obnd_pptx = read_template(
  template = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.pptx"),
  mapping  = system.file(package="nlmixr2rpt", "templates","nlmixr_obnd_template.yaml"))

obnd_pptx      = report_fit(
  fit          = fit,
  obnd         = obnd_pptx,
  verbose      = TRUE,
  cat_covars   = c("SEX","SUBTYPE"),
  cont_covars  = c("WT") )

save_report(obnd_pptx, "report.pptx")



      # Population prediction  
      xpdb = xpose.nlmixr2::xpose_data_nlmixr(fit)
      p_pred <- dv_vs_pred(xpdb, caption=NULL, title=NULL, subtitle=NULL) +
      ggtitle("===CMPD=== (===CUNITS===)") +
      coord_fixed()+
      ylab("Observed") +
      xlab("Population Predicted") +
      theme_light()
      yrange = layer_scales(p_pred)$y$range$range
      xrange = layer_scales(p_pred)$x$range$range
      lb = min(c(yrange,xrange))
      ub = max(c(yrange,xrange))
      p_pred = p_pred + xlim(c(lb, ub)) + ylim(c(lb,ub))

      ## Individual prediction  
      p_ipred <- dv_vs_ipred(xpdb, caption=NULL, title=NULL, subtitle=NULL) +
      ggtitle("===CMPD=== (===CUNITS===)") +
      coord_fixed()+
      ylab("Observed") +
      xlab("Individual Predicted") +
      theme_light()
      yrange = layer_scales(p_ipred)$y$range$range
      xrange = layer_scales(p_ipred)$x$range$range
      lb = min(c(yrange,xrange))
      ub = max(c(yrange,xrange))
      p_ipred = p_ipred + xlim(c(lb, ub)) + ylim(c(lb,ub))
      # Combining figures
      p_res <- ggpubr::ggarrange(p_pred, p_ipred, ncol=2, nrow=1 )


# library(ggPMX)
#
# ctr = ggPMX::pmx_nlmixr(fit,
#         vpc = TRUE,
#         conts = c("WT"),
#         cats  = c("SEX","SUBTYPE", "ROUTE", "Cohort"))
#
#
# pmx_plot_vpc(ctr, strat.facet="DOSE~NDOSE") +
#   scale_y_continuous(trans='log10')
#

