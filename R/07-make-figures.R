
# --------------------- setup ---------------------

source("R/04-model-ml.R")
source("R/helpers-figures/figure-funs.R")
source("R/helpers-figures/analysis-ph.R")
library("patchwork")
library("assertr")
library("ggtext")
library("scales")
library("janitor")
library("posterior")
library("devtools", include.only = "session_info")

# --------------------- theme ---------------------

theme_set(
  theme_bw() + 
    theme(
      plot.tag = element_text(face = "bold"),
      legend.position = "bottom",
      legend.margin = margin(),
      legend.box.margin = margin(),
      strip.text = ggtext::element_markdown()
    )
)

palette <- wesanderson::wes_palette("Zissou1", 6, "continuous")
pal_short <- palette[c(6,4,3,1)]
alpha <- .3

striplabs <- as_labeller(c(
  "z1.r1" = "Zone 1 (Rack 1)", "z1.r2" = "Zone 1 (Rack 2)", 
  "z2.r1" = "Zone 2 (Rack 1)", "global" = "Global"
))

# --------------------- read ---------------------

wq <- read_csv(here::here("data-clean/data-wq-si.csv"))

finished_ortho <- read_csv(here::here("data-clean/finished-water-orthophosphate-zones-1-2.csv"))

temp_z1_r2 <- read_csv(here::here("data-clean/temperature-ds-z1-r2.csv"))

temp_flushed <- read_csv(here::here("data-clean/flushed-sample-temp.csv"))

# for XRD data:

xrd <- read_csv(here::here("data-clean/xrd-cleaned.csv"))

stds <- read_csv(here::here("data-clean/xrd-standards.csv")) 

# --------------------- figures ---------------------

# n.b., these have to be run in order

# paper

source("R/helpers-figures/figure-1.R")
source("R/helpers-figures/figure-2.R")
source("R/helpers-figures/figure-3.R")
source("R/helpers-figures/figure-toc.R")

# supplement

source("R/helpers-figures/figure-s2.R")
source("R/helpers-figures/figure-s4.R")
source("R/helpers-figures/figure-s5.R")
source("R/helpers-figures/figure-s6.R")

#------------------ session info ------------------

writeLines(capture.output(devtools::session_info()), "session-info.txt")
