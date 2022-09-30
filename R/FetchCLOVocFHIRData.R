#
# This moudle extracts and tabulates FHIR resources from CLOVoc FHIR API
#
FetchCLOVocFHIRData <- function(fhir_api_url =
                                     Sys.getenv("CLOVOC_API_URL"),
                                   fhir_api_cookie =
                                     Sys.getenv("CLOVOC_FHIR_API_COOKIE")) {
  # Define headers
  cookies <- c(Cookie = fhir_api_cookie)

  # /Group
  groups <- RequestAndFlatten(
    fhir_api_url = fhir_api_url,
    resource = "Group",
    cookies = cookies,
    cols = c(
      "ResearchStudy Identifier" = "meta/tag/code",
      "Group Identifier" = "identifier/value",
      "Patient ID" = "member/entity/reference"
    ),
    drop_cols = "resource_identifier",
    format = "compact"
  )

  # Melt columns
  groups <- fhircrackr::fhir_melt(
    groups, columns = c("Patient ID"),
    sep = " ~ ",
    brackets = c("<<", ">>"),
    all_columns = TRUE
  )

  # Remove indices
  groups <- fhircrackr::fhir_rm_indices(groups, brackets = c("<<", ">>"))

  # Extract patient IDs
  groups < ExtractPatientIDs(groups)

  # /Patient
  patients <- RequestAndFlatten(
    fhir_api_url = fhir_api_url,
    resource = "Patient",
    cookies = cookies,
    desc_cols = c(
      "Patient ID" = "id",
      "Patient Identifier" = "identifier/value",
      "Race ~ Ethnicity" = "extension/extension/valueString",
      "Gender" = "gender"
    ),
    format = "wide"
  )

  # Right-join groups and patients on Patient ID
  patients <- merge(
    groups, patients, by.x = "Patient ID", by.y = "Patient ID", all.y = TRUE
  )

  # Cache Patient IDs and Identifiers
  patient_ids <- patients[, c("Patient ID", "Patient Identifier")]

  # Drop columns
  patients <- within(patients, rm("Patient ID"))


  # /Condition
  conditions <- RequestAndFlatten(
    fhir_api_url = fhir_api_url,
    resource = "Condition",
    cookies = cookies,
    desc_cols = c(
      "Patient ID" = "subject/reference",
      "Clinical Status" = "clinicalStatus/text",
      "Verification Status" = "verificationStatus/text",
      "Condition Name" = "code/text",
      "Condition Ontology URI" = "code/coding/system",
      "Condition Code" = "code/coding/code",
      "Body Site Name" = "bodySite/text",
      "Body Site Ontology URI" = "bodySite/coding/system",
      "Body Site Code" = "bodySite/coding/code"
    ),
    format = "compact"
  )

  # Remove indices
  conditions <- fhircrackr::fhir_rm_indices(conditions, brackets = c("<<", ">>"))

  # Extract patient IDs
  conditions <- ExtractPatientIDs(conditions)

  # Right-join patient_ids and conditions on Patient ID, then drop Patient ID col
  conditions <- AdornPatientData(conditions, patients, drop_patient_id = TRUE)


  # /Specimen
  specimens <- RequestAndFlatten(
    fhir_api_url = fhir_api_url,
    resource = "Specimen",
    cookies = cookies,
    cols = c(
      "Patient ID" = "subject/reference",
      "Specimen Identifier" = "identifier/value",
      "Specimen Status" = "status",
      "Specimen Type Name" = "type/text",
      "Specimen Type Ontology URI" = "type/coding/system",
      "Specimen Type Code" = "type/coding/code",
      "Body Site Name" = "collection/bodySite/text",
      "Body Site Ontology URI" = "collection/bodySite/coding/system",
      "Body Site Code" = "collection/bodySite/coding/code"
    ),
    format = "compact"
  )

  # Remove indices
  specimens <- fhircrackr::fhir_rm_indices(specimens, brackets = c("<<", ">>"))

  # Replace NA with empty string
  specimens <- ReplaceNA(specimens)

  # Extract patient IDs
  speciments <- ExtractPatientIDs(specimens)

  # Right-join patient_ids and specimens on Patient ID, then drop Patient ID col
  specimens <- AdornPatientData(specimens, patients, drop_patient_id = TRUE)

  # /DocumentReference
  document_references <- RequestAndFlatten(
    fhir_api_url = fhir_api_url,
    resource = "DocumentReference",
    cookies = cookies,
    desc_cols = c(
      "Patient ID" = "subject/reference",
      "DocumentReference Status" = "status",
      "Document Status" = "docStatus",
      "Document Type" = "type/text",
      "Experiment Strategy " = "_experiment_strategy",
      "Data Category" = "category/coding/display",
      "URL" = "content/attachment/url"
    ),
    format = "compact"
  )

  # Remove indices
  document_references <- fhircrackr::fhir_rm_indices(
    document_references, brackets = c("<<", ">>")
  )

  # Extract patient IDs
  document_references <- ExtractPatientIDs(document_references)

  # Right-join patient_ids and document_references on Patient ID
  document_references <- AdornPatientData(document_references,
                                            patients,
                                            drop_patient_id = TRUE)


  # Cache data frames
  clovoc_api_fhir_service <- list(
    "Patient" = patients,
    "Condition" = conditions,
    "Specimen" = specimens,
    "DocumentReference" = document_references
  )

  # Return clovoc_api_fhir_service
  return(clovoc_api_fhir_service)
}
