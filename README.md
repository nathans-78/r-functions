# r-functions
Repository of all of my awesome R functions used for statistics and data science purposes.

Functions are as below:
* roc.R: Receiver Operating Characteristic curve, user can use parameters to compile ROC statistics for any model. AUC to be added. _Function is parallelised for MacOS and Linux operating systems, Windows users will need to update function from `mclapply` to `lapply`._
* bow.R: Bag-of-words function, user can compile either up to or equal to the specified n-gram. Output can be either a list of bag-of-words count or data.table.
* bow_output.R: Transforms the bag-of-words data.table input to either binary or TF-IDF. Missing values can be maintained as either NA or transformed to 0.
* model_performance.R: Function for testing multi-class classification models with relevant performance statistics.
* permutation_importance.R: Function for evaluating permutation importance and can be used for both classification and regression scenarios. Can also be applied to any model, user needs to specify both __model__ and __prediction function__. _Function is parallelised for MacOS and Linux operating systems, Windows users will need to update function from `mclapply` to `lapply`._
