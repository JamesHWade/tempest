test_that("promotion plan POSIX timestamps preserve instants and fail closed", {
  instants <- as.POSIXct(
    c("2026-11-01T05:30:00Z", "2026-11-01T06:15:00Z"),
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  fallback <- as.POSIXlt(instants, tz = "America/New_York")
  expected <- c(
    "2026-11-01T05:30:00.000000Z",
    "2026-11-01T06:15:00.000000Z"
  )

  normalized <- tempest:::tempest_knowledge_posix_value(fallback)
  expect_identical(normalized, expected)
  expect_identical(
    unname(vapply(
      normalized,
      tempest:::tempest_knowledge_value,
      character(1)
    )),
    expected
  )

  invalid <- structure(
    c(Inf, -Inf, 1e12, -1e12, 1e300, -1e300),
    class = c("POSIXct", "POSIXt"),
    tzone = "UTC"
  )
  expect_identical(
    tempest:::tempest_knowledge_posix_value(invalid),
    rep(NA_character_, length(invalid))
  )

  mixed <- as.POSIXct(
    c(NA_real_, -0.0000006, 0, 0.9999996),
    origin = "1970-01-01",
    tz = "UTC"
  )
  mixed_expected <- c(
    NA_character_,
    "1969-12-31T23:59:59.999999Z",
    "1970-01-01T00:00:00.000000Z",
    "1970-01-01T00:00:01.000000Z"
  )
  mixed_normalized <- tempest:::tempest_knowledge_posix_value(mixed)
  expect_identical(mixed_normalized, mixed_expected)
  expect_identical(
    vapply(
      stats::na.omit(mixed_normalized),
      \(value) {
        identical(
          tempest:::tempest_knowledge_timestamp_value(value),
          value
        )
      },
      logical(1)
    ),
    rep(TRUE, 3L)
  )
})

test_that("source origin keys follow the locator and content hash", {
  keys <- tempest:::tempest_source_origin_keys(
    c(
      "https://example.com/a",
      " https://example.com/a ",
      "https://example.com/a"
    ),
    c("sha256:1", "sha256:1", "sha256:2")
  )

  expect_identical(keys[[2L]], keys[[1L]])
  expect_match(keys[[1L]], "^tempest-source-locator-v1:[a-f0-9]{64}$")
  expect_identical(anyDuplicated(keys[c(1L, 3L)]), 0L)
  expect_identical(
    tempest:::tempest_source_origin_keys(character(), character()),
    character()
  )
})

test_that("claim origin keys normalize text", {
  keys <- tempest:::tempest_claim_origin_keys(c(
    "Output held steady.",
    "  output HELD   steady",
    "Output rose."
  ))

  expect_identical(keys[[2L]], keys[[1L]])
  expect_match(keys[[1L]], "^tempest-claim-text-v1:[a-f0-9]{64}$")
  expect_identical(anyDuplicated(keys[c(1L, 3L)]), 0L)
  expect_identical(
    tempest:::tempest_claim_origin_keys(character()),
    character()
  )
})
