pkgload::load_all(quiet = TRUE)
helpers <- new.env(parent = asNamespace("tempest"))
invisible(testthat::source_test_helpers("tests/testthat", env = helpers))
pins <- character()
for (day in c("initial", "correction")) {
  text <- if (day == "initial") {
    "The pilot recovered 82% of the material."
  } else {
    "The corrected pilot result is 62%, not 82%."
  }
  fixture <- helpers$test_promotion_storm_fixture(paste0("pilot-", day), text)
  bundle <- tempest_promotion_bundle(fixture$research)
  pins[[day]] <- bundle@bundle_id
  target <- file.path("inst/examples/accepted-research", day)
  tempest_save_promotion_bundle(bundle, target)
  writeLines(tempest_report(fixture$research), paste0(target, "-report.md"))
}

# Review these out-of-band pins and update the example after regenerating inputs.
dput(pins)
