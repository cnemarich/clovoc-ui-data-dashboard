FetchData <- function() {
  # Fetch data from each API endpoint
  clovoc_api_fhir_service <- FetchCLOVocFHIRData()
  include_api_fhir_service <- FetchIncludeFHIRData()

  # Bind the two datasets together
  dataset <- list(
    "Patient" = bind_rows(
      include_api_fhir_service[["Patient"]],
      clovoc_api_fhir_service[["Patient"]]
    ),
    "Condition" = bind_rows(
      include_api_fhir_service[["Condition"]],
      clovoc_api_fhir_service[["Condition"]]
    ),
    "Specimen" = bind_rows(
      include_api_fhir_service[["Specimen"]],
      clovoc_api_fhir_service[["Specimen"]]
    ),
    "DocumentReference" = bind_rows(
      include_api_fhir_service[["DocumentReference"]],
      clovoc_api_fhir_service[["DocumentReference"]]
    )
  )

  # Return dataset
  return(dataset)
}
