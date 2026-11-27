tempest_claim_text_key <- function(text) {
  normalized <- stringi::stri_trans_tolower(
    tempest_trim(as.character(text)),
    locale = "root"
  )
  normalized <- gsub("[[:space:]]+", " ", normalized, perl = TRUE)
  normalized <- sub("[.]$", "", normalized, perl = TRUE)
  enc2utf8(normalized)
}
