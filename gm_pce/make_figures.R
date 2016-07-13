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


fn <- function(...)file.path("gm_pce",...)

to_pdf(source("master_scripts/par2.R"), fn("Figure1.pdf"), width=7, height=7)
to_pdf(source("master_scripts/aci_vcmaxnitro.R"), fn("Figure2.pdf"), width=8, height=10)
to_pdf(source("master_scripts/Agmgs.R"), fn("Figure3.pdf"), width=7, height=7)
to_pdf(source("master_scripts/physiology_boxplots.R"), fn("Figure4.pdf"), width=10, height=6)
to_pdf(source("master_scripts/wateruse.R"), fn("Figure5.pdf"), width=7, height=7)
to_pdf(source("master_scripts/gmes_shadeleaves_figure.R"), fn("Figure6.pdf"), width=6, height=6)
to_pdf(source("master_scripts/cicc_boxplots.R"), fn("Figure7.pdf"), width=10, height=6)

