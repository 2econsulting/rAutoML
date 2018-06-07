#' @title grid_options
#' @description auto train GBM using pre-defined strategies
#' @param algo character string, name of machine learning algorithm
#' @export
grid_options <- function(algo){
  if(algo=="gbm"){
    gridOptions_list <- list(
      max_depth       = as.integer(seq(from=1, to=15, by=1)),
      learn_rate      = seq(from=0.001, to=0.1, by=0.01),
      sample_rate     = seq(from=0.3, to=1, by=0.1),
      col_sample_rate = seq(from=0.3, to=1, by=0.1)
    )
    return(gridOptions_list)
    cat(">> search options :", names(gridOptions_list), "\n")
    cat(">> search space :", max(cumprod(sapply(gridOptions_list, length))), "\n")
  } else{
    cat(">> currently not support! \n")
  }
}

