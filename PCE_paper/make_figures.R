to_pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}

source("functions and packages/functions.R")
source("master_scripts/plot_objects.R")

fn <- function(...)file.path("PCE_paper",...)

#individual pdfs for submission
to_pdf(source("master_scripts/par2.R"), fn("Figure1_test.pdf"), width=7, height=7)
to_pdf(source("master_scripts/amax_nitro.R"), fn("Figure2.pdf"), width=6, height=6)
to_pdf(source("master_scripts/Agmgs.R"), fn("Figure3.pdf"), width=7, height=7)
to_pdf(source("master_scripts/physiology_boxplots.R"), fn("Figure4.pdf"), width=10, height=6)
to_pdf(source("master_scripts/wateruse.R"), fn("Figure5.pdf"), width=7, height=7)
to_pdf(source("master_scripts/gmes_shadeleaves_figure.R"), fn("Figure6.pdf"), width=6, height=6)
to_pdf(source("master_scripts/cicc_boxplots.R"), fn("Figure7.pdf"), width=10, height=6)



#make one file of all manuscript figures--------------------------------

# to_pdf_onepdf <- function(sourcelist, filename, ..., verbose=TRUE) {
#   if(!file.exists(dirname(filename)))
#     dir.create(dirname(filename), recursive=TRUE)
#   if ( verbose )
#     cat(sprintf("Creating %s\n", filename))
#   pdf(filename, onefile = TRUE, ...)
#   # for (i in 1:length(sourcelist)){
#   #   sl <- sourcelist[[i]]
#   #   par(fin = c(sl$w, sl$l))
#   #   eval.parent(sl$ss)
#   # }
#   on.exit(dev.off())
#   l_ply(sourcelist, eval.parent)
# }

# 
# plot_source <- function(ss, w, l) {
#   par(fin = c(w, l))
#   eval.parent(ss)
# }

# slist <- list(substitute(source("master_scripts/par2.R")),
#          substitute(source("master_scripts/cicc_boxplots.R")))
#              
# to_pdf_onepdf(sourcelist = slist, fn("manusript_figures_test.pdf"))
# 
# 
# 
# slist <- list(substitute(source("master_scripts/par2.R")),
#               substitute(source("master_scripts/amax_nitro.R")),
#               substitute(source("master_scripts/Agmgs.R")),
#               substitute(source("master_scripts/physiology_boxplots.R")),
#               substitute(source("master_scripts/physiology_boxplots.R")),
#               substitute(source("master_scripts/wateruse.R")),
#               substitute(source("master_scripts/gmes_shadeleaves_figure.R")),
#               substitute(source("master_scripts/cicc_boxplots.R")))


