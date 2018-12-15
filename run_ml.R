
library(tidyverse)
library(caret)
library(doMC)
library(yaml)



# for (i in seq(along = list.files(pattern = "\\.R$")))
#   source(i)
pipeline_location <- commandArgs(trailingOnly = F) %>% str_subset("--file=") %>% str_remove("--file=")
c("transfrom_data.R", "elastic_forest.R") %>% sapply(function (x) {
  source(file.path(pipeline_location, x))
})


set.seed(123)




run_caret <- function (X_y, learning_method, number_folds = 5, number_repeats = 10, parallelization = "local", sample_balance = "up", tune_length = 100, search = "random", preprocessing = NULL) {


  if (parallelization == "local") {
    n_cores <- detectCores()
    registerDoMC(cores = n_cores)
  }

  print("about to model in R/caret")


  learning_method_name <- learning_method #%>% str_replace("_caret$", "")

  if (learning_method_name == "lasso")
    learning_method_name <- "glmnet"
  else if (learning_method_name == "elastic_forest")
    learning_method_name <- elastic_forest


  #############################
  ## Preprocessing full data ##
  #############################

  preprocessing %>% sapply(function (x) {
    X_y <<- X_y %>% list(
      "mad_prune_features" = mad_prune_features
    )[[x]]
  })


  ###########################################################################
  ## Since theoretically all caret methods could be supported here, I'm    ##
  ## using partial functions to accommodate a possible differential number ##
  ## of arguments - only sometimes the grid is specified etc.              ##
  ###########################################################################


  fit_control_ <- trainControl %>% 
    partial(
      method = "repeatedcv",
      number = number_folds,
      repeats = number_repeats,
      summaryFunction = twoClassSummary,
      classProbs = T,
      sampling = sample_balance,
      savePredictions = "all",
      verbose = T
    )


  # if (repeats > 1)
  #   fit_control_ <- fit_control_ %>% partial(repeats = repeats)

  ## repeats are fully covered in this case by sending specific folds into the index
  ## function parameter.


  print("about to run caret fit")



  fit_ <- train %>% partial(
      response ~ .,
      data = X_y,
      method = learning_method_name,
      preProc = c("zv", "center", "scale"),
      metric = "ROC"
  )

  if (!(learning_method_name %in% c("knn", "gbm")))
    fit_ <- fit_ %>% partial(family = "binomial")

  if (learning_method == "lasso")
    if (search == "random")
      fit_ <- fit_ %>% partial(
        tuneGrid = expand.grid(alpha = 1, lambda = 10^runif(tune_length, -3, 2))
      )
    else
      fit_ <- fit_ %>% partial(
        tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-3, 2, length = tune_length))
      )
  else {
    fit_control_ <- fit_control_ %>% partial(search = search)
    fit_ <- fit_ %>% partial(tuneLength = tune_length)
  }



  fit_control <- fit_control_()
  fit_ <- fit_ %>% partial(trControl = fit_control)

  fit <- fit_()


  print("caret fit completed")


  return(fit)

}




main <- (function () {


  ## This first part deals with reading in the necessary paramenters and data.
  ## It follows the convention from the rest of the pipeline.


  args <- commandArgs(trailingOnly = T)

  ml_config <- read_yaml(
    ifelse(length(args) == 1, args[1], "./ml_config.yml")
  )

  X_y <- read_delim(ml_config$data, del = "\t") %>% prepare_data
  learning_method <- ml_config$learning_method

  log_file <- file(paste0(learning_method, ".log"), open = "wt")
  sink(log_file)
  sink(log_file, type = "message")


  sample_balance <- ifelse("sample_balance" %in% names(ml_config), ml_config$sample_balance, "up")
  tune_length <- ifelse("tune_length" %in% names(ml_config), ml_config$tune_length, 100)
  parallelization <- ifelse("parallelization" %in% names(ml_config), ml_config$parallelization, "local")
  preprocessing <- ifelse("preprocessing" %in% names(ml_config), ml_config$preprocessing, "local")
  number_folds <- ifelse("number_folds" %in% names(ml_config), ml_config$number_folds, 5)
  number_repeats <- ifelse("number_repeats" %in% names(ml_config), ml_config$number_repeats, 10)
  search <- ifelse("search" %in% names(ml_config), ml_config$search, "random")


  ## At this point all data and parameters are ready. As the next step, the method
  ## is ran on the data.


  optimized_fit <- run_caret(X_y, number_folds = number_folds, number_repeats = number_repeats, sample_balance = sample_balance, learning_method = learning_method, parallelization = parallelization, tune_length = tune_length, search = search, preprocessing = preprocessing)


  print(paste0("optimized: ", mean(optimized_fit$resample$ROC), " +/- ", sd(optimized_fit$resample$ROC)))

  saveRDS(optimized_fit, paste0(learning_method, "_model.rds"))


})()

