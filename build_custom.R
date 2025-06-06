rm(list=ls())
repo_root = here::here()
setwd(repo_root)

devtools::document(roclets = c('rd', 'collate', 'namespace'))

devtools::load_all()

# Rebuilding the pkgdown site
pkgdown::build_site()


# Fixing any broken image references
art_dir = file.path("docs", "articles")

# Getting all of the html files in the article dir
htds = dir(art_dir, "*.html")

for(htd in htds){
  fn = file.path(art_dir, htd)

  cfn = file(fn, open="r")
  htd_lines = readLines(cfn)
  close(cfn)

  # For some reason it's doing this weird relative path thing, so I'm stripping that out here:
  trim_txt = "../../../../../../projects/nlmixr2rpt/github/nlmixr2rpt/articles/"
  htd_lines = gsub(trim_txt, "", htd_lines)

  write(htd_lines, file=fn, append=FALSE)

}
