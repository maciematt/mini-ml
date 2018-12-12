

prepare_data <- function (X_y) {

  ## Preparing the data to work with caret.
  ##   - caret seems to be unable to deal with / autoremove single level factors,
  ##     so also removing those here.

  cols_with_zero_var <- X_y[, -1] %>% nearZeroVar(saveMetrics = T) %>% select(zeroVar) %>% rownames_to_column %>% filter(zeroVar == T) %>% pull(rowname)
  X_y <- X_y %>% select(-one_of(cols_with_zero_var))
  colnames(X_y)[1] <- "response"

  if (is.integer(X_y$response))
    X_y$response <- ifelse(X_y$response == 0, "Zero", "One")

  X_y$response <- as.factor(X_y$response)

  return(X_y %>% as_data_frame)

}



mad_prune_features <- function (X_y) {

  ## MAD feature pruning seems to work well in high dimensional datasets,
  ## For example in Dincer et al. 2018.
  ## This works only if all cols (except first) are numeric.

  mean_mad <- X_y[, -1] %>% scale %>% sapply(mad) %>% mean
  cols_above_mean_mad <- X_y[, -1] %>% select_if(function (col) mad(col) > mean_mad) %>% colnames
  cols_above_mean_mad <- c(colnames(X_y)[1], cols_above_mean_mad)

  return(X_y[, cols_above_mean_mad])

}
