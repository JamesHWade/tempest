test_that("enriched reports preserve provenance and render as a portable dossier", {
  directory <- withr::local_tempdir()
  dir.create(file.path(directory, "analyses", "slope"), recursive = TRUE)
  dir.create(file.path(directory, "illustrations"), recursive = TRUE)
  writeLines(
    "# Analysis code",
    file.path(directory, "analyses", "slope", "analysis.R")
  )
  writeBin(
    as.raw(1:8),
    file.path(directory, "analyses", "slope", "output-1.png")
  )
  writeBin(as.raw(1:8), file.path(directory, "illustrations", "overview.png"))
  writeLines("# Original research", file.path(directory, "research.md"))

  bundle <- list(
    title = "Evidence & uncertainty in <script>field</script>",
    summary = "A proposed synthesis with limits stated up front.",
    research_md = paste(
      "## Findings",
      "The source describes a measured relationship [in its paper](https://example.org/paper).",
      "",
      "![remote figure](https://outside.example/chart.png)",
      "",
      "[unsafe destination](javascript:alert(1))",
      "",
      "<script>window.compromised = true</script>",
      "<img src=x onerror=alert(1)>",
      sep = "\n"
    ),
    sources = data.frame(
      id = "source-1",
      resource_kind = "web",
      locator = "https://example.org/paper",
      url = "https://example.org/paper",
      title = "A measured result & its context",
      snippet = "The source records a measured relationship.",
      fetched_at = "2026-11-14T10:00:00Z",
      stringsAsFactors = FALSE
    ),
    analyses = list(
      list(
        id = "slope",
        title = "Slope sensitivity",
        question = "How does the fitted slope change across the stated range?",
        code = "x <- seq(0, 1)\nplot(x, x)",
        source_ids = "source-1",
        assumptions = "Illustrative values are synthetic.",
        status = "succeeded",
        output = "The model returned a finite slope.",
        files = c(
          "analyses/slope/analysis.R",
          "analyses/slope/output-1.png"
        ),
        metadata = list(executor = "offline-test")
      ),
      list(
        id = "comparison",
        title = "Unfinished comparison",
        question = "Could a second model explain the same pattern?",
        code = "",
        source_ids = "source-1",
        assumptions = character(),
        status = "failed",
        output = NULL,
        files = character(),
        metadata = list(),
        error = "The comparison executor stopped before producing a result."
      ),
      list(
        id = "unsafe-paths",
        title = "Path safety check",
        question = "Do untrusted paths become links?",
        code = "",
        source_ids = character(),
        assumptions = character(),
        status = "succeeded",
        output = "No files were produced.",
        files = c("../outside.png", "javascript:alert(1)"),
        metadata = list()
      )
    ),
    illustrations = list(
      list(
        id = "overview",
        title = "A conceptual overview",
        prompt = "Draw a conceptual overview.",
        alt = "A diagram connecting research sources to analysis questions.",
        caption = "",
        status = "succeeded",
        output = NULL,
        files = "illustrations/overview.png",
        file = "illustrations/overview.png",
        metadata = list(provider = "offline-test")
      ),
      list(
        id = "timeline",
        title = "Timeline illustration",
        prompt = "Draw a timeline.",
        alt = "A timeline with two marked intervals.",
        caption = "An illustration that could not be generated.",
        status = "failed",
        output = NULL,
        files = character(),
        file = NULL,
        metadata = list(),
        error = "The image task failed before an image was written."
      )
    ),
    status = "partial"
  )

  rendered_path <- tempest:::tempest_enriched_render(bundle, directory)
  html <- paste(readLines(rendered_path, warn = FALSE), collapse = "\n")
  document <- xml2::read_html(html)

  expect_identical(basename(rendered_path), "index.html")
  expect_identical(file.exists(rendered_path), TRUE)
  expect_identical(file.exists(file.path(directory, "manifest.json")), FALSE)
  expect_match(html, "Proposed synthesis", fixed = TRUE)
  expect_match(html, "Enrichment status partial", fixed = TRUE)
  expect_identical(grepl("Research status", html, fixed = TRUE), FALSE)
  expect_match(html, "Original research", fixed = TRUE)
  expect_match(
    html,
    "The source describes a measured relationship",
    fixed = TRUE
  )
  expect_match(html, "Computational analyses", fixed = TRUE)
  expect_match(html, "Methods and code", fixed = TRUE)
  expect_match(html, "Slope sensitivity", fixed = TRUE)
  expect_match(html, "This analysis did not complete", fixed = TRUE)
  expect_match(html, "The comparison executor stopped", fixed = TRUE)
  expect_match(html, "Source library", fixed = TRUE)
  expect_match(html, "manifest.json", fixed = TRUE)
  expect_match(
    html,
    "href=\"manifest.json\" download=\"manifest.json\"",
    fixed = TRUE
  )
  expect_match(html, "analysis.R", fixed = TRUE)
  expect_match(html, "download=\"output-1.png\"", fixed = TRUE)
  expect_match(html, "Slope sensitivity — Computed figure", fixed = TRUE)
  expect_match(html, "Observed console output", fixed = TRUE)
  expect_match(
    html,
    "The model returned a finite slope.",
    fixed = TRUE
  )
  expect_length(
    xml2::xml_find_all(
      document,
      "//details[summary[contains(., 'Observed console output')]]"
    ),
    2L
  )
  expect_match(
    html,
    "AI-generated illustration · Not scientific evidence",
    fixed = TRUE
  )
  expect_match(html, "@media(max-width:1000px)", fixed = TRUE)
  expect_match(html, "@media print{", fixed = TRUE)
  expect_match(html, "name=\"viewport\"", fixed = TRUE)
  expect_length(
    xml2::xml_find_all(document, "//a[@href='#report-content']"),
    1L
  )
  expect_length(
    xml2::xml_find_all(
      document,
      "//main[@id='report-content'][@tabindex='-1']"
    ),
    1L
  )
  expect_length(
    xml2::xml_find_all(
      document,
      "//p[normalize-space(.)='A proposed synthesis with limits stated up front.']"
    ),
    1L
  )
  expect_length(
    xml2::xml_find_all(
      document,
      "//section[@id='illustrations']//img[@src='illustrations/overview.png']"
    ),
    1L
  )
  expect_length(
    xml2::xml_find_all(document, "//img[@src='illustrations/overview.png']"),
    1L
  )
  expect_identical(grepl("hero-art", html, fixed = TRUE), FALSE)
  expect_match(html, "Illustration prompt and provenance", fixed = TRUE)
  expect_match(
    html,
    "alt=\"A diagram connecting research sources",
    fixed = TRUE
  )
  expect_match(
    html,
    "target=\"_blank\" rel=\"noopener noreferrer\"",
    fixed = TRUE
  )
  expect_match(html, "src=\"illustrations/overview.png\"", fixed = TRUE)
  expect_match(html, "src=\"analyses/slope/output-1.png\"", fixed = TRUE)
  expect_identical(grepl("<script", html, fixed = TRUE), FALSE)
  expect_identical(grepl("onerror=", html, fixed = TRUE), FALSE)
  expect_identical(grepl("javascript:", html, fixed = TRUE), FALSE)
  expect_identical(grepl("outside.example", html, fixed = TRUE), FALSE)
  expect_identical(grepl("href=\"../outside.png\"", html, fixed = TRUE), FALSE)
  expect_identical(grepl("<link", html, fixed = TRUE), FALSE)
  expect_length(
    xml2::xml_find_all(document, "//script | //iframe | //object"),
    0L
  )
})

test_that("illustration notices survive untrusted and missing captions", {
  directory <- withr::local_tempdir()
  dir.create(file.path(directory, "illustrations"))
  writeBin(as.raw(1:8), file.path(directory, "illustrations", "proof.png"))
  bundle <- list(
    title = "Caption safety",
    summary = "",
    research_md = "",
    sources = data.frame(id = character(), stringsAsFactors = FALSE),
    analyses = list(),
    illustrations = list(list(
      id = "proof",
      title = "Verified scientific finding",
      prompt = "Draw a conceptual illustration.",
      alt = "A conceptual illustration.",
      caption = "The image proves the result. <script>window.x = 1</script>",
      status = "succeeded",
      output = NULL,
      files = "illustrations/proof.png",
      file = "illustrations/proof.png",
      metadata = list()
    )),
    status = "succeeded"
  )

  first_path <- tempest:::tempest_enriched_render(bundle, directory)
  first_html <- paste(readLines(first_path, warn = FALSE), collapse = "\n")
  expect_match(
    first_html,
    "AI-generated illustration · Not scientific evidence",
    fixed = TRUE
  )
  expect_match(
    first_html,
    "&lt;script&gt;window.x = 1&lt;/script&gt;",
    fixed = TRUE
  )
  expect_identical(grepl("<script", first_html, fixed = TRUE), FALSE)

  bundle$illustrations[[1L]]$caption <- ""
  second_path <- tempest:::tempest_enriched_render(bundle, directory)
  second_html <- paste(readLines(second_path, warn = FALSE), collapse = "\n")
  expect_match(second_html, "Illustration prompt and provenance", fixed = TRUE)
  expect_match(
    second_html,
    "AI-generated illustration · Not scientific evidence",
    fixed = TRUE
  )
})

test_that("enriched rendering rejects unsafe local assets and source schemes", {
  directory <- withr::local_tempdir()
  dir.create(file.path(directory, "assets"))
  writeBin(as.raw(1:8), file.path(directory, "assets", "image.png"))

  expect_null(tempest:::tempest_enriched_render_local_file(
    directory,
    "../image.png"
  ))
  expect_null(tempest:::tempest_enriched_render_local_file(
    directory,
    "https://example.org/image.png"
  ))
  expect_null(tempest:::tempest_enriched_render_local_file(
    directory,
    "assets/image.svg"
  ))
  expect_identical(
    tempest:::tempest_enriched_render_safe_url("javascript:alert(1)"),
    NULL
  )
  expect_identical(
    tempest:::tempest_enriched_render_safe_url("data:text/html,hello"),
    NULL
  )
  expect_identical(
    tempest:::tempest_enriched_render_safe_url("https://example.org/source"),
    "https://example.org/source"
  )
})
test_that("canonical research citations remain navigable footnotes", {
  markdown <- "Supported claim[^Sabc].\n\n[^Sabc]: Source [record](https://example.org/source)."
  rendered <- as.character(tempest_enriched_render_markdown(markdown))
  expect_match(rendered, 'href="#research-fn-Sabc"', fixed = TRUE)
  expect_match(rendered, 'id="research-fn-Sabc"', fixed = TRUE)
  expect_match(rendered, 'href="#research-fnref-Sabc"', fixed = TRUE)
  expect_no_match(rendered, "\\[\\^Sabc\\]")
  expect_match(rendered, 'href="https://example.org/source"', fixed = TRUE)
})
