

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
