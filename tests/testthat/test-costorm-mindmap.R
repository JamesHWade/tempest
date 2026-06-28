test_that("tempest_mindmap_node_sizes computes correctly", {
  mindmap <- list(
    nodes = list(
      list(
        id = "root",
        label = "Topic",
        parent = NULL,
        notes = "some notes here",
        source_ids = c("S1", "S2")
      ),
      list(
        id = "n1",
        label = "Sub1",
        parent = "root",
        notes = "a",
        source_ids = c("S3")
      ),
      list(
        id = "n2",
        label = "Sub2",
        parent = "root",
        notes = "",
        source_ids = character()
      )
    ),
    edges = list()
  )

  sizes <- tempest:::tempest_mindmap_node_sizes(mindmap)

  expect_type(sizes, "list")
  expect_contains(names(sizes), c("root", "n1", "n2"))

  # root has 3 words in notes + 2 sources = 5 total
  expect_equal(sizes[["root"]]$n_notes, 3L)
  expect_equal(sizes[["root"]]$n_sources, 2L)
  expect_equal(sizes[["root"]]$total, 5L)

  # n1 has 1 word + 1 source = 2 total
  expect_equal(sizes[["n1"]]$total, 2L)

  # n2 has 0 notes + 0 sources = 0 total
  expect_equal(sizes[["n2"]]$total, 0L)
})

test_that("tempest_mindmap_node_sizes handles empty mindmap", {
  mindmap <- list(nodes = list(), edges = list())
  sizes <- tempest:::tempest_mindmap_node_sizes(mindmap)
  expect_type(sizes, "list")
  expect_length(sizes, 0)
})

test_that("tempest_mindmap_oversized_nodes finds oversized nodes", {
  mindmap <- list(
    nodes = list(
      list(
        id = "root",
        label = "Topic",
        parent = NULL,
        notes = "word1 word2 word3",
        source_ids = c("S1", "S2", "S3")
      ),
      list(
        id = "n1",
        label = "Small",
        parent = "root",
        notes = "tiny",
        source_ids = character()
      )
    ),
    edges = list()
  )

  # root total = 3 + 3 = 6, n1 total = 1 + 0 = 1
  oversized <- tempest:::tempest_mindmap_oversized_nodes(
    mindmap,
    trigger_count = 5
  )
  expect_equal(oversized, "root")

  # With higher threshold, nothing is oversized
  oversized2 <- tempest:::tempest_mindmap_oversized_nodes(
    mindmap,
    trigger_count = 10
  )
  expect_length(oversized2, 0)
})

test_that("tempest_mindmap_oversized_nodes handles empty mindmap", {
  mindmap <- list(nodes = list(), edges = list())
  result <- tempest:::tempest_mindmap_oversized_nodes(
    mindmap,
    trigger_count = 5
  )
  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("tempest_type_node_expansion returns valid type", {
  skip_if_not_installed("ellmer")

  type <- tempest:::tempest_type_node_expansion()
  expect_s7_class(type, getFromNamespace("TypeObject", "ellmer"))
})
