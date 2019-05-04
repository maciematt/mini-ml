
library(dplyr)
library(readr)
library(stringr)
library(rsample)
library(purrr)
library(caret)
library(doMC)
library(yaml)
library(future)
#library(future.apply)

require(doFuture)
require(future.batchtools)



# for (i in seq(along = list.files(pattern = "\\.R$")))
#   source(i)
pipeline_location <- commandArgs(trailingOnly = F) %>% str_subset("--file=") %>% str_remove("--file=") %>% str_remove("/run_ml.R")
c("transform_data.R") %>% sapply(function (x) {
  source(file.path(pipeline_location, x))
})


set.seed(123)




run_caret <- function (X_y, learning_method, number_folds = 5, number_repeats = 10, hyper_folds = 5, learning_type = "binary_classification", parallelization = "local", sample_balance = "up", tune_length = 100, search = "random", preprocessing = "none", n_parallel_cores = NULL, store_options = NULL) {


  #if (parallelization == "local")
  #  n_parallel_cores <- min(n_parallel_cores, availableCores())


  print("about to model in R/caret")


  learning_method_name <- learning_method #%>% str_replace("_caret$", "")

  if (learning_method_name == "lasso")
    learning_method_name <- "glmnet"
  else if (learning_method_name == "elastic_forest")
    learning_method_name <- elastic_forest



  #############################
  ## Preprocessing full data ##
  #############################


  if (preprocessing != "none") {
    preprocessing %>% sapply(function (x) {
      X_y <<- X_y %>% list(
        "mad_prune_features" = mad_prune_features
      )[[x]]()
    })
  }

  pre_process <- preProcess(X_y[, -1], c("zv", "center", "scale")) # standard preprocessing; normally it would be handled as part of `train`
  X_y[, -1] <- predict(pre_process, X_y[, -1])


  ###########################################################################
  ## Since theoretically all caret methods could be supported here, I'm    ##
  ## using partial functions to accommodate a possible differential number ##
  ## of arguments - only sometimes the grid is specified etc.              ##
  ###########################################################################


  fit_control_ <- trainControl %>% 
    partial(
      method = "repeatedcv",
      number = hyper_folds, ## This is the INNER loop where the hyperparameters get optimized
      #repeats = number_repeats,
      classProbs = T,
      sampling = sample_balance,
      savePredictions = "all",
      allowParallel = F,
      verbose = T
    )

  if (learning_type == "binary_classification")
    fit_control_ <- fit_control_ %>% partial(summaryFunction = twoClassSummary)
  ## otherwise it defaults to summaryFunction = defaultSummary. Add a else if here if
  ## you'd like to handle this differently.


  print("setting up caret fit")


  fit_ <- train %>% partial(
      response ~ .,
      method = learning_method_name,
      #preProc = c("zv", "center", "scale"),
      metric = "ROC"
  )

  if ((learning_type == "binary_classification") & !(learning_method_name %in% c("knn", "gbm")))
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


  ## Working out indices for test and train sets
  train_test_ix <- (X_y %>% vfold_cv(strata = "response", v = number_folds, repeats = number_repeats))$splits %>% 
    lapply(function(x) {
      list(train = x$in_id, test = setdiff(1:nrow(X_y), x$in_id), fold_ = x$id$id2, repeat_ = x$id$id)
    })


  ## here the data needs to be split and pumped into fit in a parallelizable loop - gotta figure out what to actually use here
  if (parallelization == "local") {
    #plan(multisession, workers = n_parallel_cores)
    registerDoMC(cores = n_parallel_cores)
  } else if (parallelization == "lsf") {
    plan(batchtools_lsf)
  } else if (parallelization == "slurm") {
    plan(batchtools_slurm)
  }



  print("running caret fit")
  #fits <- future_lapply(train_test_ix, function (ix) {
  #  fit_ <- fit_ %>% partial(data = X_y[ix$train, ])
  #  fit <- fit_()
  #})
  fits <- foreach(ix = train_test_ix) %dopar% {
    fit_ <- fit_ %>% partial(data = X_y[ix$train, ])
    fit <- fit_()
    prediction <- predict(fit, newdata = X_y[ix$test, ], type = "prob")
    prediction$predicted <- colnames(prediction)[prediction %>% max.col]
    prediction$ground_truth <- X_y[ix$test, ] %>% pull(1)
    variable_importances <- possibly(varImp, otherwise = NULL)(fit) # svmRadial throws an $ operator in atomic vector error; not sure why, so just making this be NULL whenever varImp can't be computed
    optimized_hyperparameters <- possibly(function () {fit$finalModel$tuneValue}, NULL)()
    if (store_options == "summary")
      fit <- NULL
    list(
      fit = fit,
      train_test_indices = ix,
      optimized_hyperparameters = optimized_hyperparameters,
      prediction = prediction,
      variable_importances = variable_importances
    )
  }

  print("caret fit completed")

  return(fits)

}




main <- (function () {


  ## This first part deals with reading in the necessary paramenters and data.
  ## It follows the convention from the rest of the pipeline.

  args <- commandArgs(trailingOnly = T)

  ml_config <- read_yaml(
    ifelse(length(args) == 1, args[1], "./ml_config.yml")
  )

  learning_method <- ml_config$learning_method

  log_file <- file(paste0(learning_method, ".log"), open = "wt")
  sink(log_file)
  sink(log_file, type = "message")


  sample_balance <- ifelse("sample_balance" %in% names(ml_config), ml_config$sample_balance, "up") %>% (function (x) {if (x == "none") {x <- NULL}; return (x)})
  tune_length <- ifelse("tune_length" %in% names(ml_config), ml_config$tune_length, 100) %>% as.integer
  learning_type <- ifelse("learning_type" %in% names(ml_config), ml_config$learning_type, "binary_classification")
  parallelization <- ifelse("parallelization" %in% names(ml_config), ml_config$parallelization, "local")
  n_parallel_cores <- ifelse("n_parallel_cores" %in% names(ml_config), ml_config$n_parallel_cores, availableCores()) %>% as.integer
  preprocessing <- ifelse("preprocessing" %in% names(ml_config), ml_config$preprocessing, "none")
  number_folds <- ifelse("number_folds" %in% names(ml_config), ml_config$number_folds, 5) %>% as.integer
  number_repeats <- ifelse("number_repeats" %in% names(ml_config), ml_config$number_repeats, 10) %>% as.integer
  search <- ifelse("search" %in% names(ml_config), ml_config$search, "random")
  store_options <- ifelse("store_options" %in% names(ml_config), ml_config$store_options, "summary") # don't store the full model by default, only a summary


  X_y <- read_delim(ml_config$data, del = "\t") %>% prepare_data(learning_type)


  ## At this point all data and parameters are ready. As the next step, the method
  ## is ran on the data.


  optimized_fit <- run_caret(X_y, number_folds = number_folds, number_repeats = number_repeats, sample_balance = sample_balance, learning_method = learning_method, learning_type = learning_type, parallelization = parallelization, tune_length = tune_length, search = search, preprocessing = preprocessing, n_parallel_cores = n_parallel_cores, store_options = store_options)


  #print(paste0("optimized: ", mean(optimized_fit$resample$ROC), " +/- ", sd(optimized_fit$resample$ROC)))

  saveRDS(optimized_fit, paste0(learning_method, "_model.rds"))
  #save(optimized_fit, file = paste0(learning_method, "_model.RData"))


})()

