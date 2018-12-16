
elastic_forest <- list(label = "elastic_forest",
                  library = c("glmnet", "Matrix", "randomForest"),
                  type = c("Regression", "Classification"),
                  parameters = data.frame(parameter = c("alpha", "lambda", "mtry"),
                                          class = c("numeric", "numeric", "numeric"),
                                          label = c("Mixing Percentage", "Regularization Parameter", "#Randomly Selected Predictors")),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      numLev <- if(is.character(y) | is.factor(y)) length(levels(y)) else NA
                      if(!is.na(numLev)) {
                        fam <- ifelse(numLev > 2, "multinomial", "binomial")
                      } else fam <- "gaussian"
                      init <- glmnet::glmnet(Matrix::as.matrix(x), y,
                                     family = fam,
                                     nlambda = len+2,
                                     alpha = .5)
                      lambda <- unique(init$lambda)
                      lambda <- lambda[-c(1, length(lambda))]
                      lambda <- lambda[1:min(length(lambda), len)]
                      out <- cbind(expand.grid(alpha = seq(0.1, 1, length = len),
                                         lambda = lambda),
                                         data.frame(mtry = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len))
                      )
                    } else {
                      out <- data.frame(alpha = runif(len, min = 0, 1),
                                        lambda = 2^runif(len, min = -10, 3),
                                        mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)))
                    }
                    out
                  },
                  loop = function(grid) {
                    alph <- unique(grid$alpha)
                    loop <- data.frame(alpha = alph)
                    loop$lambda <- NA
                    submodels <- vector(mode = "list", length = length(alph))
                    for(i in seq(along = alph)) {
                      np <- grid[grid$alpha == alph[i],"lambda"]
                      loop$lambda[loop$alpha == alph[i]] <- np[which.max(np)]
                      submodels[[i]] <- data.frame(lambda = np[-which.max(np)])
                    }
                    list(loop = loop, submodels = submodels)
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    numLev <- if(is.character(y) | is.factor(y)) length(levels(y)) else NA

                    theDots <- list(...)

                    if(all(names(theDots) != "family")) {
                      if(!is.na(numLev)) {
                        fam <- ifelse(numLev > 2, "multinomial", "binomial")
                      } else fam <- "gaussian"
                      theDots$family <- fam
                    }

                    ## pass in any model weights
                    if(!is.null(wts)) theDots$weights <- wts

                    if(!(class(x)[1] %in% c("matrix", "sparseMatrix")))
                      x <- Matrix::as.matrix(x)

                    modelArgs <- c(list(x = x,
                                        y = y,
                                        alpha = param$alpha),
                                   theDots)

                    out <- do.call(glmnet::glmnet, modelArgs)
                    if(!is.na(param$lambda[1])) out$lambdaOpt <- param$lambda[1]

                    keep_feats <- coef(out$finalModel, out$bestTune$lambda) %>% as.matrix %>% as.data.frame %>% rownames_to_column(var = "var_name") %>% filter(`1` != 0) %>% filter(var_name != "(Intercept)") %>% pull(var_name) # figure out which variables make the model work well
                    x <- x %>% select(one_of(keep_feats))
                    out <- randomForest::randomForest(x, y, mtry = param$mtry, ...)
                    out$keep_feats <- keep_feats
                    out
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                      print("predict is on")
                      if(!is.null(newdata)) predict(modelFit, newdata %>% select(response, one_of(modelFit$keep_feats))) else predict(modelFit)
                    },
                  prob = function(modelFit, newdata, submodels = NULL)
                    if(!is.null(newdata))
                      predict(modelFit, newdata %>% select(response, one_of(modelFit$keep_feats)), type = "prob") else predict(modelFit, type = "prob"),
                  predictors = function(x, ...) {
                    ## After doing some testing, it looks like randomForest
                    ## will only try to split on plain main effects (instead
                    ## of interactions or terms like I(x^2).
                    varIndex <- as.numeric(names(table(x$forest$bestvar)))
                    varIndex <- varIndex[varIndex > 0]
                    varsUsed <- names(x$forest$ncat)[varIndex]
                    varsUsed
                  },
                  varImp = function(object, ...){
                    varImp <- randomForest::importance(object, ...)
                    if(object$type == "regression")
                      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
                    else {
                      retainNames <- levels(object$y)
                      if(all(retainNames %in% colnames(varImp))) {
                        varImp <- varImp[, retainNames]
                      } else {
                        varImp <- data.frame(Overall = varImp[,1])
                      }
                    }
                    
                    out <- as.data.frame(varImp)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp  
                    }
                    out
                  },
                  levels = function(x) x$classes,
                  tags = c("Elastic Random Forest", "Elastic Net Pruned Ensemble Model", "Elastic Net", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),],
                  oob = function(x) {
                    out <- switch(x$type,
                                  regression =   c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
                                  classification =  c(1 - x$err.rate[x$ntree, "OOB"],
                                                      e1071::classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]]))
                    names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
                    out
                  })
