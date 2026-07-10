test_that("tempest_cache_clear removes all or only stale cache files", {
  cache_dir <- withr::local_tempdir()
  fresh_key <- tempest:::tempest_cache_key("fresh")
  stale_key <- tempest:::tempest_cache_key("stale")

  tempest:::tempest_cache_set(cache_dir, fresh_key, list(value = "fresh"))
  tempest:::tempest_cache_set(cache_dir, stale_key, list(value = "stale"))
  stale_path <- tempest:::tempest_cache_path(cache_dir, stale_key)
  fresh_path <- tempest:::tempest_cache_path(cache_dir, fresh_key)
  Sys.setFileTime(stale_path, Sys.time() - 3600)

  suppressMessages(tempest_cache_clear(cache_dir, max_age = 60))

  expect_equal(file.exists(stale_path), FALSE)
  expect_equal(file.exists(fresh_path), TRUE)

  suppressMessages(tempest_cache_clear(cache_dir))

  expect_equal(
    list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE),
    character()
  )
})

test_that("tempest_cache_get returns NULL for expired entries", {
  cache_dir <- withr::local_tempdir()
  key <- tempest:::tempest_cache_key("ttl")
  tempest:::tempest_cache_set(cache_dir, key, "cached")
  cache_path <- tempest:::tempest_cache_path(cache_dir, key)
  Sys.setFileTime(cache_path, Sys.time() - 3600)

  expect_equal(tempest:::tempest_cache_get(cache_dir, key), "cached")
  expect_null(tempest:::tempest_cache_get(cache_dir, key, max_age = 60))
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
  expect_equal(tempest:::tempest_cache_get(cache_dir, key), "previous")
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
