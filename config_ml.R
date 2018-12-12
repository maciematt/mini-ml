
library(tidyverse)
library(yaml)



parse_args <- function (args) {
  args <- args %>% str_remove("--")
  args_df <- data.frame(args = args) %>% separate(args, c("args_names", "args_vals"), sep = "=")
  args <- as.list(args_df$args_vals)
  names(args) <- args_df$args_names

  return(args)
}



(main <- function() {
  args <- commandArgs(trailingOnly = T)
  args <- parse_args(args)
  out_file <- ifelse("out" %in% names(args), args$out, "./ml_config.yml")
  sink(out_file)
  cat(as.yaml(args))
  sink()
})()
