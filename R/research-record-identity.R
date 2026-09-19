tempest_claim_text_key <- function(text) {
  normalized <- stringi::stri_trans_tolower(
    tempest_trim(as.character(text)),
    locale = "root"
  )
  normalized <- gsub("[[:space:]]+", " ", normalized, perl = TRUE)
  normalized <- sub("[.]$", "", normalized, perl = TRUE)
  enc2utf8(normalized)
}

# Accepted Sources are keyed on the exact locator together with the content
# hash, so the same document cited again resolves to its accepted record while
# a locator whose content changed becomes a new Source; evidence extracted
# from the earlier content keeps pointing at the earlier Source. Repeated
# pairs inside one bundle are coalesced before planning.
tempest_source_origin_keys <- function(locators, content_hashes) {
  if (length(locators) == 0L) {
    return(character())
  }
  content_hashes <- as.character(content_hashes)
  content_hashes[is.na(content_hashes)] <- ""
  vapply(
    paste(tempest_trim(as.character(locators)), content_hashes, sep = "\u001f"),
    function(key) {
      paste0(
        "tempest-source-locator-v1:",
        digest::digest(enc2utf8(key), algo = "sha256", serialize = FALSE)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

tempest_claim_origin_keys <- function(texts) {
  if (length(texts) == 0L) {
    return(character())
  }
  vapply(
    tempest_claim_text_key(texts),
    function(key) {
      paste0(
        "tempest-claim-text-v1:",
        digest::digest(key, algo = "sha256", serialize = FALSE)
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}
