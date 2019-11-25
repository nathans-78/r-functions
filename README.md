# r-functions
Repository of all of my awesome R functions used for statistics and data science purposes.

Functions are as below:
* roc.R: Receiver Operating Characteristic curve, user can use parameters to compile ROC statistics for any model. AUC to be added.
* bow.R: Bag-of-words function, user can compile either up to or equal to the specified n-gram. Output can be either a list of bag-of-words count or data.table.
* bow_output.R: Transforms the bag-of-words data.table input to either binary or TF-IDF. Missing values can be maintained as either NA or transformed to 0.
* model_performance.R: Function for testing multi-class classification models with relevant performance statistics.
