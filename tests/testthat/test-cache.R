test_that("tempest_cache_lookup returns miss for expired entries", {
  cache_dir <- withr::local_tempdir()
  key <- tempest:::tempest_cache_key("ttl")
  tempest:::tempest_cache_set(cache_dir, key, "cached")
  cache_path <- tempest:::tempest_cache_path(cache_dir, key)
  Sys.setFileTime(cache_path, Sys.time() - 3600)

  expect_equal(tempest:::tempest_cache_lookup(cache_dir, key)$value, "cached")
  expired <- tempest:::tempest_cache_lookup(cache_dir, key, max_age = 60)
  expect_identical(expired$status, "expired")
  expect_null(expired$value)
})

test_that("cache replacement is atomic when serialization fails", {
  cache_dir <- withr::local_tempdir()
  key <- tempest:::tempest_cache_key("atomic")
  tempest:::tempest_cache_set(cache_dir, key, "previous")

  expect_warning(
    result <- tempest:::tempest_cache_set(
      cache_dir,
      key,
      "partial",
      writer = function(value, path) {
        writeBin(charToRaw("partial"), path)
        stop("injected write failure")
      }
    ),
    class = "rlang_warning"
  )

  expect_equal(result, FALSE)
  expect_equal(tempest:::tempest_cache_lookup(cache_dir, key)$value, "previous")
  expect_equal(
    list.files(cache_dir, pattern = "^[.]", all.files = TRUE, no.. = TRUE),
    character()
  )
})

test_that("corrupt cache entries are quarantined", {
  cache_dir <- withr::local_tempdir()
  key <- tempest:::tempest_cache_key("corrupt")
  path <- tempest:::tempest_cache_path(cache_dir, key)
  writeBin(charToRaw("not an rds"), path)

  expect_warning(
    lookup <- tempest:::tempest_cache_lookup(cache_dir, key),
    class = "rlang_warning"
  )

  expect_equal(lookup$status, "read_error")
  expect_null(lookup$value)
  expect_equal(file.exists(path), FALSE)
  expect_length(list.files(cache_dir, pattern = "[.]corrupt-"), 1L)
})
