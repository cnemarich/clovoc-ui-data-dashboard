request_and_flatten <- function(fhir_api_url = NULL,
                        resource = NULL,
                        cookies = NULL,
                        cols = NULL,
                        format = NULL) {
  # Build a request URL for resource
  request <- fhircrackr::fhir_url(
    url = fhir_api_url,
    resource = resource,
    parameters = c("_count" = "100")
  )

  # Download bundles of resources
  bundles <- fhircrackr::fhir_search(
    request = request,
    add_headers = cookies,
    verbose = 2
  )

  # Define a resource table description
  description <- fhircrackr::fhir_table_description(
    resource = resource,
    cols = cols,
    sep = "~",
    brackets = c("<<", ">>"),
    rm_empty_cols = FALSE,
    format = format
  )

  # Flatten resources
  my_resource <- fhircrackr::fhir_crack(
    bundles = bundles,
    design = description,
    verbose = 2
  )

  # Return resource
  my_resource
}
