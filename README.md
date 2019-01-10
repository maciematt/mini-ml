* HowTo

The pipeline can be ran as follows:

```
Rscript --vanilla ~/mini_ml/config_ml.R --data=/location/of/data --number-repeats=5 --number-folds=10 --learning_method=glmnet --preprocessing=mad_prune_features --n_parallel_cores=20 --out=glmnet.yml
Rscript --max-ppsize=500000 --vanilla ~/mini_ml/run_ml.R glmnet.yml
Rscript --max-ppsize=500000 --vanilla ~/mini_ml/analyze_results.R
```

- The first of the commands above prepares a configuration file (in this case to run glmnet)
- The second actually runs the ML method
- The third is used once all methods in a given folder are successfully completed

