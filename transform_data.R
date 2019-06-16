

prepare_data <- function (X_y, learning_type) {

  ## Preparing the data to work with caret.
  ##   - caret seems to be unable to deal with / autoremove single level factors,
  ##     so also removing those here.

  # cols_with_zero_var <- X_y[, -1] %>% nearZeroVar(saveMetrics = T) %>% select(zeroVar) %>% rownames_to_column %>% filter(zeroVar == T) %>% pull(rowname)
  # X_y <- X_y %>% select(-one_of(cols_with_zero_var))
  ## commenting these two lines above to see if I can add "zv" in preProcess in train instead for the same effect; apparently I can (as 2 mo later it's still commented out)
  colnames(X_y)[1] <- "response"

  if (X_y$response %>% is.na %>% any)
    stop("NA's detected in the response vector!")


  if (learning_type == "binary_classification") {

    if (is.numeric(X_y$response)) #| (X_y$response %>% is.na %>% any %>% `!`))
      X_y$response <- ifelse(X_y$response == 0, "Zero", "One")

    X_y$response <- as.factor(X_y$response)

  }

  return(X_y %>% as_data_frame)

}



mad_prune_features <- function (X_y) {

  ## MAD feature pruning seems to work well in high dimensional datasets,
  ## For example in Dincer et al. 2018.
  ## This works only if all cols (except first) are numeric.

  response_col <- colnames(X_y)[1]

  print("Running feature prunning via mean MAD threshold...")
  mean_mad <- X_y[, -1] %>% preProcess(c("center", "scale")) %>% predict(X_y[, -1]) %>% sapply(mad) %>% mean
  print("mean MAD:")
  print(mean_mad)

  cols_above_mean_mad <- X_y[, -1] %>% preProcess(c("center", "scale")) %>% select_if(function (col) mad(col) > mean_mad) %>% colnames

  print(paste0("No. cols above mean MAD: ", length(cols_above_mean_mad), " out of total ", ncol(X_y[, -1]), " dependent variable columns."))
  print(paste0(length(cols_above_mean_mad) / ncol(X_y[, -1]) * 100, "% dependent variable columns will be kept."))


  return(X_y[, c(response_col, cols_above_mean_mad)])

}


stratify_resample <- function (X_y, strata = "response", v = 5, repeats = 10) {
  #' Alternative function for deriving stratified folds given a requested number of
  #' folds, repeats, and a stratifying variable. This approach was developed for instances
  #' where one of the classes has very few samples, in particular less than v. Unfortu-
  #' nately, this still fails in the hyperparameter tuning loop.

  s1_rows <- which(X_y[, strata, drop = T] == levels(X_y[, strata, drop = T])[1])
  if (length(s1_rows) == 1)
    s1_rows <- c(s1_rows, s1_rows) # This is only to counter a weird behavior of `sample` when passed a single element vector
  s1_len <- s1_rows %>% length

  s2_rows <- which(X_y[, strata, drop = T] == levels(X_y[, strata, drop = T])[2])
  if (length(s2_rows) == 1)
    s2_rows <- c(s2_rows, s2_rows) # This is only to counter a weird behavior of `sample` when passed a single element vector
  s2_len <- s2_rows %>% length

  if (s1_len < v) {
    s1_missing <- v - s1_len
    s1_train <- lapply(1:repeats, function (r_) {
      s1_surrogate <- c(s1_rows, sample(s1_rows, s1_missing, replace = T))
      createFolds(s1_surrogate, k = v) %>% 
        lapply(function (f_) {
          s1_surrogate[f_]
        })
    })
  } else {
    s1_train <- lapply(1:repeats, function (r_) {
      createFolds(s1_rows, k = v) %>% 
        lapply(function (f_) {
          s1_rows[f_]
        })
    })
  }

  if (s2_len < v) {
    s2_missing <- v - s2_len
    s2_train <- lapply(1:repeats, function (r_) {
      s2_surrogate <- c(s2_rows, sample(s2_rows, s2_missing, replace = T))
      createFolds(s2_surrogate, k = v) %>% 
        lapply(function (f_) {
          s2_surrogate[f_]
        })
    })
  } else {
    s2_train <- lapply(1:repeats, function (r_) {
      createFolds(s2_rows, k = v) %>% 
        lapply(function (f_) {
          s2_rows[f_]
        })
    })
  }

  s_train <- lapply(1:repeats, function (r_) {
    lapply(1:v, function (f_) {
      c(s1_train[[r_]][[f_]], s2_train[[r_]][[f_]])
    })
  })
  s_test <- lapply(1:repeats, function (r_) {
    lapply(1:v, function (f_) {
      setdiff(1:nrow(X_y), s_train[[r_]][[f_]])
    })
  })

  combined <- list()
  for (r_ in 1:repeats) {
    for (fold_ in 1:v) {
      combined[[length(combined) + 1]] <- list(
        train = s_train[[r_]][[fold_]],
        test = s_test[[r_]][[fold_]],
        fold_ = paste0("Fold", fold_),
        repeat_ = paste0("Repeat", r_)
      )
    }
  }

  return(combined)
}