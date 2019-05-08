# `isocat`
`isocat`: Isotope Clustering and Assignment Tools

The `isocat` package provides multiple tools in `R` for creating and quantitatively analyzing and summarizing probability-of-origin surfaces generated from stable isotope analyses of animal tissue.

## Vignette

To install `isocat` and its vignette directly from github, use the following code in R when using devtools >= 2.0.0:

`devtools::install_github("cjcampbell/isocat", build = TRUE, force = TRUE, build_opts = c("--no-resave-data", "--no-manual"))`

The vignette requires the certain packages to be installed in order to compile. 

`
vignettePackages <- c("kableExtra", "pvclust", "rasterVis", "ggplot2", "viridisLite", "gridExtra", "dplyr", "dendextend", "isocat")

lapply(vignettePackages, function(pckg) {
    if (!require(pckg, character.only = T, quietly = F)) {
      install.packages(x, dependencies = T)
      require(pckg)
    }
  })
}
`
