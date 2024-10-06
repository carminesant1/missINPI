#' Missing Data imputation with BINPI
#' @description
#' The binpi function implements the Boosted Incremental Non-Parametric Imputation (BINPI) method.
#' This method is designed for single imputation of missing data, utilizing AdaBoost algorithms and decision trees.
#' @param df \code{\link{data.frame}} with missing values
#' @param ntree number of trees, input for \code{\link{boosting}} function
#' @return a complete \code{\link{data.frame}}
#' @details
#' Based on Master's Thesis in Computational Statistics:
#'
#' \emph{Santone, C. (2024). Iterative Incremental Non-Parametric Imputation Random Forests-based: una proposta di r-package per l'imputazione dei missing data.}
#' @references \emph{D'Ambrosio, A., Aria, M., & Siciliano, R. (2012). Accurate tree-based missing data imputation and data fusion within the statistical learning paradigm. Journal of classification, 29, 227-258.}
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom dplyr mutate
#' @importFrom dplyr all_of
#' @importFrom data.table :=
#' @importFrom randomForest randomForest
#' @importFrom stats na.omit
#' @importFrom stats predict
#' @importFrom stats complete.cases
#' @importFrom adabag boosting
#' @importFrom tree tree
#' @export

binpi <- function(df, ntree=100){

  #object containing variables' name with NAs
  MV <- c()
  #object containing complete variables' name
  CV <- c()

  for (i in 1:ncol(df)) {
    n_na <- sum(is.na(df[,i]))
    if (n_na != 0) {
      MV[i] <- n_na
      names(MV)[i] <- colnames(df)[i]}
    else {
      CV <- c(CV, colnames(df)[i])}}

  #reorders the variables with NA based on the number of missing values,
  #in ascending order
  MV <- sort(MV)

  #if all the variables are incomplete, the observations with NAs on the first variable
  #(whith the least NAs) are subtracted and
  #store in order to obtain at least one complete variable
  if (length(MV) == ncol(df)) {
    stop("BINPI doesn't work without complete variables")}
  else {
    df_complete <- df %>%
      select(all_of(CV))
  }

  #for loop to impute NAs
  for (i in 1:length(MV)) {
    #each iteration adds a variable with the least NAs
    df_complete <- df_complete %>%
      mutate(df[names(MV[i])])
    #each incomplete variable becomes Y for computational reason
    colnames(df_complete)[colnames(df_complete)==names(MV[i])] <- "Y"
    #NA prediction with decision's tree model
    if (is.factor(df_complete$Y) & length(levels(df_complete$Y)) == 2) {
      mod <- boosting(Y~., data = df_complete[complete.cases(df_complete),], boos = T, mfinal = ntree)
      p<-predict(mod, newdata = df_complete[which(!complete.cases(df_complete)),], type = "class")$class
    }
    else if (is.factor(df_complete$Y) & length(levels(df_complete$Y)) > 2) {
      mod <- tree(Y~., data = df_complete[complete.cases(df_complete),])
      p<-predict(mod, newdata = df_complete[which(!complete.cases(df_complete)),], type = "class")}
    else {
      mod <- tree(Y~., data = df_complete[complete.cases(df_complete),])
      p<-predict(mod, newdata = df_complete[which(!complete.cases(df_complete)),])
    }
    #NAs imputation
    df_complete[is.na(df_complete)] <- p
    #reset variable's name
    colnames(df_complete)[colnames(df_complete)=="Y"] <- names(MV[i])
  }

  #ordering the complete dataset as the original one
  df_complete <- df_complete %>%
    select(colnames(df))
  df_complete <- df_complete[match(rownames(df), rownames(df_complete)), ]

  #return complete dataset
  return(df_complete)
}
