#' Example Bivariate Classification Data
#'
#' @details These data are a simplified version of the segmentation data contained
#' in `caret`. There are three columns: `A` and `B` are predictors and the column
#' `Class` is a factor with levels "One" and "Two". There are three data sets:
#' one for training (n = 1009), validation (n = 300), and testing (n = 710).
#'
#' @name bivariate
#' @aliases bivariate_train  bivariate_test bivariate_val
#' @docType data
#' @return \item{bivariate_train, bivariate_test, bivariate_val}{tibbles}
#'
#' @keywords datasets
#' @examples
#' data(bivariate)
NULL


#' Annual Stack Overflow Developer Survey Data
#'
#' @details These data are a collection of 5,594 data points collected on
#' developers. These data could be used to try to predict who works remotely
#' (as used in the source listed below).
#'
#' @name stackoverflow
#' @aliases stackoverflow
#' @docType data
#' @return \item{stackoverflow}{a tibble}
#'
#' @source
#' Julia Silge, _Supervised Machine Learning Case Studies in R_,
#' \url{https://supervised-ml-course.netlify.com/chapter2}
#' @keywords datasets
#' @examples
#' data(stackoverflow)
NULL
