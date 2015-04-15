
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

  # Determine the new column names for the appended data frames
  if (any(!is.na(df_1_missing))){
    new_column_names_df1 <- c(colnames(df1), df_1_missing)
  } else {
    new_column_names_df1 <- NA
  }

  if (any(!is.na(df_2_missing))){
    new_column_names_df2 <- c(colnames(df2), df_2_missing)
  } else {
    new_column_names_df2 <- NA
  }

  # Add missing columns to df1, with no values
  if (any(!is.na(df_1_missing))){

    for (i in 1:length(df_1_missing)){

      df1 <- cbind(df1, df_1_missing = rep("", nrow(df1)))

      if (i == length(df_1_missing)){
        colnames(df1) <- new_column_names_df1
      }
    }
  }

  # Add missing columns to df2, with no values
  if (any(!is.na(df_2_missing))){

    for (i in 1:length(df_2_missing)){

      df2 <- cbind(df2, df_2_missing = rep("", nrow(df2)))

      if (i == length(df_2_missing)){
        colnames(df2) <- new_column_names_df2
      }
    }
  }

  # Ensure that the same column names are in each data frame
  stopifnot(all(colnames(df1) %in% colnames(df2)))

  return(df_new)
}
