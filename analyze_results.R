
library(tidyverse)
library(ROCR)
library(ggplot2)
library(cowplot)
library(yardstick)




local_scores <- function () {
  
  ## Calculates scores in current directory.
  
  to_analyze <- list.files(pattern = "*_model.rds")
  
  models <- to_analyze %>% lapply(function (x) {readRDS(x)})

  models_probs <- lapply(1:length(models), function (x) {
    mods_all <- models[[x]]
    probs_ <- lapply(1:length(mods_all), function (mod_num) {
      mods_all[[mod_num]]$prediction %>% mutate(Resample = mod_num, model = to_analyze[x] %>% str_remove("_model.rds"))
    }) %>% bind_rows %>% mutate(ground_truth = ground_truth %>% factor(levels = c("One", "Zero")))
  })
  
  
  #roc_data <- models_probs %>% lapply(function (mod) {
  #  rbind(mod, mod %>% mutate(Resample = "All")) %>% group_by(Resample) %>% do(
  #    roc = (function (y) {
  #      if ((y$ground_truth %>% unique %>% length) != 2)
  #        return(data.frame(x = NA, y = NA))
  #      z <- prediction(y$Zero, y$ground_truth) %>% performance(measure = "tpr", x.measure = "fpr") ## as a breakpoint we can check that it looks good after "prediction"
  #      data.frame(x = z@x.values[[1]], y = z@y.values[[1]])
  #    })(.)
  #  ) %>% unnest %>% mutate(model = mod[1, "model"]) %>% drop_na
  #}) %>% do.call(rbind, .)


  roc_data <- models_probs %>% lapply(function (mod) {
    rbind(mod, mod %>% mutate(Resample = "All")) %>% group_by(Resample) %>% 
        roc_curve(ground_truth, One) %>% 
        unnest %>% mutate(model = mod[1, "model"])
  }) %>% bind_rows
  
  #g <- ggplot() + facet_wrap(~ model) + geom_abline(linetype = "dashed") + geom_line(data = roc_data %>% filter(Resample != "All"), aes(x = x, y = y, group = Resample), alpha = .2, size = .2) + geom_line(data = roc_data %>% filter(Resample == "All"), aes(x = x, y = y), color = "red") + coord_equal() + theme(legend.position = "none") + xlab("False positive rate") + ylab("True positive rate")
  g <- ggplot() + facet_wrap(~ model) + geom_abline(linetype = "dashed") + geom_line(data = roc_data %>% filter(Resample != "All"), aes(x = 1 - specificity, y = sensitivity, group = Resample), alpha = .2, size = .2) + geom_line(data = roc_data %>% filter(Resample == "All"), aes(x = 1 - specificity, y = sensitivity), color = "red") + coord_equal() + theme(legend.position = "none") + xlab("False positive rate") + ylab("True positive rate")
  ggsave("./roc_all.pdf", g)
  
  g <- ggplot(data = roc_data %>% filter(Resample != "All"), aes(x = 1 - specificity, y = sensitivity, group = model, fill = model)) + geom_abline(linetype = "dashed") + stat_summary(geom = "ribbon", fun.data = mean_se, alpha = .2) + stat_summary(geom = "line", fun.y = mean, aes(color = model)) + coord_equal() + xlab("False positive rate") + ylab("True positive rate") + xlim(c(0, 1)) + ylim(c(0, 1)) + theme_bw()
  ggsave("./roc_means.pdf", g)
  
  
  roc_aucs <- models_probs %>% lapply(function (mod) {
    mod %>% group_by(Resample) %>%
    roc_auc(ground_truth %>% as.factor, One)
    #summarize(auc = ifelse((ground_truth %>% unique %>% length) != 2, NA, performance(prediction(Zero, ground_truth), measure = "auc")@y.values[[1]])) %>% drop_na
  })
  #names(roc_aucs) <- to_analyze %>% str_remove("_model.RData")
  names(roc_aucs) <- to_analyze %>% str_remove("_model.rds")
  

  saveRDS(roc_aucs, "./scores.rds")
  
}



summarize_scores <- function (score_config) {
  ## FUTURE
  NULL
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


