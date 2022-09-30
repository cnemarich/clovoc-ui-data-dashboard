#
# Utility tools
#
ExtractPatientIDs <- function(df) {
  df$"Patient ID" <-
    unlist(lapply(df$"Patient ID", function(x) {
      unlist(strsplit(x, "/"))[2]
    }))
}

ReplaceNA <- function(df) {
  return(replace(df, is.na(df), ""))
}

AdornPatientData <- function(df,
                             patient_df,
                             cache_columns = NULL,
                             drop_patient_id = TRUE) {

  # Merge the dataframe with the patient data dataframe
  df <- merge(df1,
              df2,
              by.x = "Patient ID",
              by.y = "Patient ID",
              all.y = TRUE)

  # If `cache_columns` is not NULL, then create a cache list
  if (!is.null(cache_columns)) {
    cached_data <- df[, cache_columns]
  }

  # If `drop_patient_id` is TRUE, then drop the Patient ID column
  if (drop_patient_id == TRUE) {
    df <- within(df, rm("Patient ID"))
  }

  # Replace NAs with empty strings
  df <- ReplaceNA(df)

  # Return list item with data and cache if `cache_columns` is not NULL
  if (!is.null(cache_columns)) {
    ls <- list(data = df,
               cache = cached_data)
    return(ls)
  }

  # Return dataframe
  return(df)
}
