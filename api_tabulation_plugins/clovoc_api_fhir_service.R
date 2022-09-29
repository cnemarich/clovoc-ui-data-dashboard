#
# This moudle extracts and tabulates FHIR resources from CLOVoc FHIR API
#

# Get FHIR credentails
fhir_api_url <- Sys.getenv("CLOVOC_API_URL")
fhir_api_cookie <- Sys.getenv("CLOVOC_FHIR_API_COOKIE")

# Define headers
cookies <- c(Cookie = fhir_api_cookie)


# /Group
groups <- request_and_flatten(
  fhir_api_url = fhir_api_url,
  resource = "Group",
  cookies = cookies,
  cols = c(
    "ResearchStudy Identifier" = "meta/tag/code",
    "Group Identifier" = "identifier/value",
    "Patient ID" = "member/entity/reference"
  ),
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
groups$"Patient ID" <- unlist(lapply(groups$"Patient ID", ParsePatientID))

# Drop columns
groups <- within(groups, rm("resource_identifier"))

# Replace NA with empty string
groups <- ReplaceNA(groups)


# /Patient
patients <- request_and_flatten(
  fhir_api_url = fhir_api_url,
  resource = "Patient",
  cookies = cookies,
  cols = c(
    "Patient ID" = "id",
    "Patient Identifier" = "identifier/value",
    "Race ~ Ethnicity" = "extension/extension/valueString",
    "Gender" = "gender"
  ),
  format = "wide"
)

# Change column names
setnames(
    patients,
    old = c(
        "<<1>>Patient ID",
        "<<1.1>>Patient Identifier",
        "<<1.1.1>>Race ~ Ethnicity",
        "<<2.1.1>>Race ~ Ethnicity",
        "<<1>>Gender"
    ),
    new = c("Patient ID", "Patient Identifier", "Race", "Ethnicity", "Gender")
)

# Replace NA with empty string
patients <- ReplaceNA(patients)

# Right-join groups and patients on Patient ID
patients <- merge(
    groups, patients, by.x = "Patient ID", by.y = "Patient ID", all.y = TRUE
)

# Cache Patient IDs and Identifiers
patient_ids <- patients[, c("Patient ID", "Patient Identifier")]

# Drop columns
patients <- within(patients, rm("Patient ID"))


# /Condition
conditions <- request_and_flatten(
  fhir_api_url = fhir_api_url,
  resource = "Condition",
  cookies = cookies,
  cols = c(
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
conditions$"Patient ID" <- unlist(
    lapply(conditions$"Patient ID", ParsePatientID)
)

# Right-join patient_ids and conditions on Patient ID
conditions <- merge(
    patient_ids,
    conditions,
    by.x = "Patient ID",
    by.y = "Patient ID",
    all.y = TRUE
)

# Drop columns
conditions <- within(conditions, rm("Patient ID"))

# Replace NA with empty string
conditions <- ReplaceNA(conditions)


# /Specimen
specimens <- request_and_flatten(
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
specimens$"Patient ID" <- unlist(
    lapply(specimens$"Patient ID", ParsePatientID)
)

# Right-join patient_ids and specimens on Patient ID
specimens <- merge(
    patient_ids,
    specimens,
    by.x = "Patient ID",
    by.y = "Patient ID",
    all.y = TRUE
)

# Drop columns
specimens <- within(specimens, rm("Patient ID"))


# /DocumentReference
document_references <- request_and_flatten(
  fhir_api_url = fhir_api_url,
  resource = "DocumentReference",
  cookies = cookies,
  cols = c(
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

# Replace NA with empty string
document_references <- ReplaceNA(document_references)

# Extract patient IDs
document_references$"Patient ID" <- unlist(
    lapply(document_references$"Patient ID", ParsePatientID)
)

# Right-join patient_ids and document_references on Patient ID
document_references <- merge(
    patient_ids,
    document_references,
    by.x = "Patient ID",
    by.y = "Patient ID",
    all.y = TRUE
)

# Drop columns
document_references <- within(document_references, rm("Patient ID"))


# Cache data frames
clovoc_api_fhir_service <- list(
    "Patient" = patients,
    "Condition" = conditions,
    "Specimen" = specimens,
    "DocumentReference" = document_references
)
