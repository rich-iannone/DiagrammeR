
combine_nodes <- function(df1, df2){

  # Determine whether data frames were supplied
  stopifnot(class(df1) == "data.frame")
  stopifnot(class(df2) == "data.frame")

  # Examine the column names for each data frame and determine
  # which columns are not common to each df
  if (any(!(colnames(df2) %in% colnames(df1)))){
    df_1_missing <- colnames(df2)[which(!(colnames(df2) %in% colnames(df1)))]
  } else {
    df_1_missing <- NA
  }

  if (any(!(colnames(df1) %in% colnames(df2)))){
    df_2_missing <- colnames(df1)[which(!(colnames(df1) %in% colnames(df2)))]
  } else {
    df_2_missing <- NA
  }


  return(df_new)
}
