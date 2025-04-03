# ------------------------------------------------------------------------------
# Setup ------------------------------------------------------------------------

if (!require(renv)) install.packages('renv')

renv::install(
  c(
    'dplyr', 'foreign', 'Matrix', 'boot', 'boot.pval', 'jtools',
    'http://www.econ.uiuc.edu/~roger/research/rq/quantreg/quantreg_5.87.tar.gz', 
    'officer', 'gtsummary', 'flextable', 'here', 'broom', 
    'broom.mixed', 'ordinal', 'lme4', 'lavaan', 'lavaanPlot', #"dr-JT/semoutput", 
    "sjPlot", "semPlot", "semTools", "DiagrammeRsvg", "rsvg", "ggpubr", "ggthemes",
    "patchwork"
    )
)

renv::snapshot(prompt = FALSE)
