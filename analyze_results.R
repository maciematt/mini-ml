
library(tidyverse)
library(ROCR)
library(ggplot2)
library(cowplot)



filter_proba <- function (fit) {
  temp <- names(fit$bestTune) %>% 
    reduce(function (x, y) {
      x[x[, y] == fit$bestTune[1, y], ]
    }, .init = fit$pred)
}




local_scores <- function () {
  
  ## Calculates scores in current directory.
  
  
  to_analyze <- list.files(pattern = "*_model.rds")
  
  models <- to_analyze %>% lapply(function (x) {readRDS(x)})
  optimized_filtered <- models %>% lapply(function (x) filter_proba(x))
  
  
  for (x in 1:length(optimized_filtered))
    optimized_filtered[[x]]$model <- to_analyze[x] %>% str_remove("_model.rds")
  
  #print(optimized_filtered)
  
  roc_data <- optimized_filtered %>% lapply(function (mod) {
    rbind(mod, mod %>% mutate(Resample = "All")) %>% group_by(Resample) %>% do(
      roc = (function (y) {
        z <- prediction(y$Zero, y$obs) %>% performance(measure = "tpr", x.measure = "fpr")
        data.frame(x = z@x.values[[1]], y = z@y.values[[1]])
      })(.)
    ) %>% unnest %>% mutate(model = mod[1, "model"])
  }) %>% do.call(rbind, .)
  
  
  g <- ggplot() + facet_wrap(~ model) + geom_abline(linetype = "dashed") + geom_line(data = roc_data %>% filter(Resample != "All"), aes(x = x, y = y, group = Resample), alpha = .2, size = .2) + geom_line(data = roc_data %>% filter(Resample == "All"), aes(x = x, y = y), color = "red") + coord_equal() + theme(legend.position = "none") + xlab("False positive rate") + ylab("True positive rate")
  ggsave("./roc_all.pdf", g)
  
  g <- ggplot(data = roc_data %>% filter(Resample != "All"), aes(x = x, y = y, group = model, fill = model)) + geom_abline(linetype = "dashed") + stat_summary(geom = "ribbon", fun.data = mean_se, alpha = .2) + stat_summary(geom = "line", fun.y = mean, aes(color = model)) + coord_equal() + xlab("False positive rate") + ylab("True positive rate") + xlim(c(0, 1)) + ylim(c(0, 1)) + theme_bw()
  ggsave("./roc_means.pdf", g)
  
  
  roc_aucs <- optimized_filtered %>% lapply(function (mod) {
    mod %>% group_by(Resample) %>% summarize(auc = performance(prediction(Zero, obs), measure = "auc")@y.values[[1]])
  })
  names(roc_aucs) <- to_analyze %>% str_remove("_model.rds")
  
  
  saveRDS(roc_aucs, "./scores.rds")
  
}



summarize_scores <- function (score_config) {
  
}




(main <- function () {
  
  mode <- commandArgs(trailingOnly = T) %>% str_subset("--mode=") %>% str_remove("--mode=")
  if (identical(mode, character(0)) | "local" %in% mode)
    local_scores()
  else if ("multiple" %in% mode) {
    score_config <- read_yaml(
      commandArgs(trailingOnly = T) %>% str_subset("--config=") %>% str_remove("--config=")
    )
    summarize_scores(score_config)
  }
  
})()


