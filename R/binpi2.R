#' Missing Data imputation with a BINPI's update
#' @description
#' The binpi2 function is an enhanced version of the BINPI method, extending its capability to handle datasets entirely composed of incomplete variables.
#' It employs an incremental approach to impute missing data using decision trees and boosting techniques.
#' @param df \code{\link{data.frame}} with missing values
#' @param ntree number of trees, input for \code{\link{boosting}} function
#' @param maxit number of iterations
#' @param eps value for the stop criterion
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

binpi2 <- function(df, ntree = 100, maxit = 10, eps = 0.001){

  #function that separates complete variables from those with NAs
  ord_df <- function(df){

    #object containing variables' name with NAs
    MV <- c()
    #object containing complete variables'name
    CV <- c()

    for (i in 1:ncol(df)) {
      n_na<-sum(is.na(df[,i]))
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
      row_na_cv <- which(is.na(df[names(MV[1])]))
      CV[1] <- names(MV[1])
      MV <- MV[-1]
      df_complete <- df %>%
        select(all_of(CV)) %>%
        slice(-row_na_cv)}

    else {
      row_na_cv <- NULL
      df_complete <- df %>%
        select(all_of(CV))}

    #a dataframe with only complete observations, the MV and CV objects and
    #the indexes of incomplete observations
    #on the first variable (if all variables contain NAs)
    #are returned
    return(list(MV = MV, CV = CV, df_complete = df_complete, row_na_cv = row_na_cv))
  }

  #function to use if one complete variable exists at least
  binpi <- function(df){

    #object that contains incomplete variables' name
    MV <- ord_df(df)$MV
    #the dataset with complete variables only
    df_complete <- ord_df(df)$df_complete

    #for loop to impute NAs
    for (i in 1:length(MV)) {
      #each iteration adds a variable with the least NAs
      df_complete <- df_complete %>%
        mutate(df[names(MV[i])])
      #each incomplete variable becomes Y for computational reason
      colnames(df_complete)[colnames(df_complete)==names(MV[i])] <- "Y"
      #NAs prediction, model to use depends on variable's type
      if (is.factor(df_complete$Y) & length(levels(df_complete$Y)) <= 2) {
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
      colnames(df_complete)[colnames(df_complete)=="Y"] <- names(MV[i])}

    #the function returns the complete dataset
    return(df_complete)
  }

  #function to use if there are incomplete variables only
  binpi_na <- function(df){

    #object that contains incomplete variables' name
    MV <- ord_df(df)$MV
    #subset of the complete observations of the first variable (with the least NAs)
    df_incomplete <- ord_df(df)$df_complete
    #object that contains the first variable's name (with the least NAs)
    CV <- ord_df(df)$CV
    #row's indexes of observations with NAs on the first variable (with the least NAs)
    row_na_cv <- ord_df(df)$row_na_cv

    #for loop to impute NAs
    for (i in 1:length(MV)) {
      #each iteration adds each variable with NAs, only for the observations
      #that don't have a missing value in the first 'complete' variable CV
      df_incomplete <- df_incomplete %>%
        mutate(!!names(MV[i]) := df[!is.na(df[CV]),names(MV[i])])
      #each incomplete variable MV becomes Y for computational reason
      colnames(df_incomplete)[colnames(df_incomplete)==names(MV[i])] <- "Y"
      #NAs prediction, model to use depends on variable's type
      if (is.factor(df_incomplete$Y) & length(levels(df_incomplete$Y)) <= 2) {
        df_incomplete <- droplevels(df_incomplete)
        mod <- boosting(Y~., data = df_incomplete[complete.cases(df_incomplete),], boos = T, mfinal = ntree)
        p<-predict(mod, newdata = df_incomplete[which(!complete.cases(df_incomplete)),], type = "class")$class
      }
      else if (is.factor(df_incomplete$Y) & length(levels(df_incomplete$Y)) > 2) {
        df_incomplete <- droplevels(df_incomplete)
        mod <- tree(Y~., data = df_incomplete[complete.cases(df_incomplete),])
        p<-predict(mod, newdata = df_incomplete[which(!complete.cases(df_incomplete)),], type = "class")}
      else {
        mod <- tree(Y~., data = df_incomplete[complete.cases(df_incomplete),])
        p<-predict(mod, newdata = df_incomplete[which(!complete.cases(df_incomplete)),])
      }
      #NAs imputation
      df_incomplete[is.na(df_incomplete)] <- p
      #reset variable's name
      colnames(df_incomplete)[colnames(df_incomplete)=="Y"] <- names(MV[i])
    }

    #subset containing the observations with NA in the first variable (with the least NAs)
    df_na <- df %>%
      slice(row_na_cv) %>%
      select(all_of(c(CV, names(MV))))

    #for loop to impute the NA of the first variable (with the least NAs)
    for (i in 1:nrow(df_na)) {
      #each observation is extracted from the subset df_na, taking into consideration
      #only the first variable (with the least NA) and the complete variables
      i_na <- df_na[i, c(1, which(!is.na(df_na[i,-1]))+1)]
      #columns in common between those previously extracted and
      #those of the complete dataset
      com_col <- intersect(names(i_na), names(df_incomplete))
      #union between the extracted observation and the previously imputed dataset
      df_incomplete_upg <- rbind(i_na[com_col], df_incomplete[com_col])
      #the variable to be imputed becomes y for computational reason
      colnames(df_incomplete_upg)[1] <- "Y"
      #NAs prediction, model to use depends on variable's type
      if (is.factor(df_incomplete_upg$Y) & length(levels(df_incomplete_upg$Y)) <= 2) {
        df_incomplete_upg <- droplevels(df_incomplete_upg)
        mod <- boosting(Y~., data = df_incomplete_upg[complete.cases(df_incomplete_upg),], boos = T, mfinal = ntree)
        p<-predict(mod, newdata = df_incomplete_upg[which(!complete.cases(df_incomplete_upg)),], type = "class")$class
      }
      else if (is.factor(df_incomplete_upg$Y) & length(levels(df_incomplete_upg$Y)) > 2) {
        df_incomplete_upg <- droplevels(df_incomplete_upg)
        mod <- tree(Y~., data = df_incomplete_upg[complete.cases(df_incomplete_upg),])
        p<-predict(mod, newdata = df_incomplete_upg[which(!complete.cases(df_incomplete_upg)),], type = "class")}
      else {
        mod <- tree(Y~., data = df_incomplete_upg[complete.cases(df_incomplete_upg),])
        p<-predict(mod, newdata = df_incomplete_upg[which(!complete.cases(df_incomplete_upg)),])
      }
      #NAs imputation
      df_na[i,1] <- p
    }

    #union between the two imputed subset
    df_incomplete_upg <- rbind(df_incomplete, df_na)

    #NAs imputation for the incomplete variable which were in the second subset
    if (sum(is.na(df_incomplete_upg)) > 0) {
      df_complete <- binpi(df_incomplete_upg)
    }
    else {
      df_complete <- df_incomplete_upg}

    #return the complete imputated dataset
    return(df_complete = df_complete)
  }

  #removal of completely NAs observations
  ind_na_obs <- which(rowSums(is.na(df)) == ncol(df))
  if (length(ind_na_obs != 0)) {
    warning(paste("Indexes of removed full NA observations:", paste(ind_na_obs, collapse = ", ")))
    df <- df %>%
      slice(-ind_na_obs)}

  #removal of completely NAs variables
  ind_na_var <- which(colSums(is.na(df)) == nrow(df))
  if (length(ind_na_var != 0)) {
    warning(paste("Removed full NA variables:", paste(names(ind_na_var), collapse = ", ")))
    df <- df %>%
      select(-all_of(ind_na_var))}

  #stop is the dataset is complete
  if (sum(is.na(df)) == 0){
    stop("0 NA detected in the dataset!")}

  #stop if there are character variables
  if (any(sapply(df, is.character))) {
    stop("Character variables can't be used for NA imputation!")}

  #warning if there might be an id variable
  if (any(sapply(df, function(x) length(unique(x))) == nrow(df))) {
    warning("Probably, you have an ID variable to be removed for NA imputation!")}

  #which function to use, dependind on the number of comolete variables
  if (sum(colSums(is.na(df)) == 0) > 0) {
    df_complete <- binpi(df)
  }
  else {
    df_complete <- binpi_na(df)
  }

  #reorder rows and columns as the original dataset
  df_complete <- df_complete %>%
    select(colnames(df))
  df_complete <- df_complete[match(rownames(df), rownames(df_complete)), ]

  #return the complete imputed dataset
  return(df_complete = df_complete)
}
