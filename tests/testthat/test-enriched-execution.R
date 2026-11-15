test_that("the Deputy executor runs R and captures files and plots", {
  directory <- withr::local_tempdir()
  code <- paste(
    "write.csv(data.frame(x = 1:3, y = c(2, 4, 6)),",
    "  'summary.csv', row.names = FALSE)",
    "cat('mean =', mean(c(2, 4, 6)))",
    "plot(1:3)",
    sep = "\n"
  )

  result <- tempest_r_executor(timeout = 10)(
    list(code = code),
    directory
  )

  expect_named(result, c("output", "files", "metadata"))
  expect_match(result$output, "mean = 4", fixed = TRUE)
  expect_setequal(result$files, c("summary.csv", "plots/plot-01.png"))
  expect_equal(read.csv(file.path(directory, "summary.csv"))$y, c(2, 4, 6))
  expect_identical(
    readBin(
      file.path(directory, "plots", "plot-01.png"),
      "raw",
      n = 8L
    ),
    as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  )
  expect_identical(result$metadata$provider, "Deputy RSession")
  expect_identical(result$metadata$deputy_outcome, "complete")
  expect_identical(
    result$metadata$source_hash,
    digest::digest(code, algo = "sha256", serialize = FALSE)
  )
})

test_that("Deputy runtime and parse errors are thrown with their outcome", {
  directory <- withr::local_tempdir()
  execute <- tempest_r_executor(timeout = 10)

  runtime_error <- tryCatch(
    execute(list(code = "stop('analysis failed')"), directory),
    error = identity
  )
  expect_s3_class(runtime_error, "tempest_execution_error")
  expect_identical(runtime_error$deputy_outcome, "error")

  parse_error <- tryCatch(
    execute(list(code = "x <-"), directory),
    error = identity
  )
  expect_s3_class(parse_error, "tempest_execution_error")
  expect_identical(parse_error$deputy_outcome, "error")
})

test_that("Deputy timeout closes the worker before code can write late files", {
  directory <- withr::local_tempdir()
  execute <- tempest_r_executor(timeout = 0.25)

  error <- tryCatch(
    execute(
      list(code = "Sys.sleep(2); writeLines('late', 'late.txt')"),
      directory
    ),
    error = identity
  )

  expect_s3_class(error, "tempest_execution_error")
  expect_identical(error$deputy_outcome, "timed_out")
  expect_identical(file.exists(file.path(directory, "late.txt")), FALSE)
})

test_that("the Deputy executor rejects linked output trees", {
  directory <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  code <- paste0(
    "file.symlink(",
    deparse(outside),
    ", 'linked')"
  )

  error <- tryCatch(
    tempest_r_executor(timeout = 10)(list(code = code), directory),
    error = identity
  )

  expect_s3_class(error, "tempest_persistence_error")
  expect_identical(unname(fs::is_link(file.path(directory, "linked"))), TRUE)
})

test_that("the OpenAI image callback writes a PNG through the Images API", {
  directory <- withr::local_tempdir()
  png <- jsonlite::base64_dec(
    paste0(
      "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="
    )
  )
  response_body <- jsonlite::toJSON(
    list(
      data = list(list(
        b64_json = jsonlite::base64_enc(png),
        revised_prompt = "A small illustrated research scene."
      ))
    ),
    auto_unbox = TRUE
  )
  seen <- new.env(parent = emptyenv())
  withr::local_envvar(OPENAI_API_KEY = "offline-test-key")
  local_mocked_bindings(
    tempest_openai_image_perform = function(request) {
      seen$request <- request
      seen$request_headers <- paste(
        capture.output(httr2::req_dry_run(request, redact_headers = FALSE)),
        collapse = "\n"
      )
      httr2::response(
        status_code = 200L,
        url = request$url,
        method = request$method,
        headers = list(`content-type` = "application/json"),
        body = charToRaw(response_body)
      )
    },
    .env = environment(tempest_image_generator())
  )

  result <- tempest_image_generator()(
    list(prompt = "A small research illustration."),
    directory
  )

  expect_named(result, c("file", "metadata"))
  expect_identical(result$file, "illustration.png")
  expect_identical(
    readBin(file.path(directory, result$file), "raw", n = length(png)),
    png
  )
  expect_identical(result$metadata$provider, "openai")
  expect_identical(result$metadata$model, "gpt-image-2.6-flare")
  expect_identical(
    result$metadata$revised_prompt,
    "A small illustrated research scene."
  )
  expect_identical(
    seen$request$url,
    "https://api.openai.com/v1/images/generations"
  )
  expect_identical(seen$request$method, "POST")
  expect_match(seen$request_headers, "offline-test-key", fixed = TRUE)
  request_body <- seen$request$body$data
  expect_identical(request_body$model, "gpt-image-2.6-flare")
  expect_identical(request_body$prompt, "A small research illustration.")
  expect_identical(request_body$size, "1536x1024")
  expect_identical(request_body$quality, "medium")
  expect_no_match(jsonlite::toJSON(result$metadata), "offline-test-key")
})

test_that("the OpenAI image callback does not call the API without a key", {
  directory <- withr::local_tempdir()
  withr::local_envvar(OPENAI_API_KEY = "")

  error <- tryCatch(
    tempest_image_generator()(
      list(prompt = "A small research illustration."),
      directory
    ),
    error = identity
  )

  expect_s3_class(error, "tempest_image_api_key_input_error")
  expect_identical(
    file.exists(file.path(directory, "illustration.png")),
    FALSE
  )
})
