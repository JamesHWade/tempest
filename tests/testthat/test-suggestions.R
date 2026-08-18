test_that("standalone suggestions are a deterministic product projection", {
  expected <- c(
    "What evidence best establishes the key claims about Animatronics?",
    paste0(
      "Which uncertainty or tradeoff matters most for understanding ",
      "Animatronics?"
    ),
    "What contrasting perspective could change the view of Animatronics?",
    paste0(
      "How could the strongest claim about Animatronics be independently ",
      "verified?"
    )
  )

  expect_equal(tempest_suggest_questions(" Animatronics "), expected)
  expect_equal(tempest_suggest_questions("Animatronics", n = 2), expected[1:2])
  expect_equal(
    tempest_suggest_questions(
      "Animatronics",
      context = "User: What standards apply?"
    )[[1]],
    paste0(
      "What evidence is still missing from the current discussion of ",
      "Animatronics?"
    )
  )
})

test_that("standalone suggestion API has no raw chat compatibility surface", {
  expect_named(
    formals(tempest_suggest_questions),
    c("topic", "context", "n")
  )
  expect_named(
    formals(tempest:::tempest_suggest_questions_async),
    c("topic", "context", "n")
  )
  expect_equal(tempest_suggest_questions(""), character())
  expect_equal(tempest_suggest_questions("   "), character())
})

test_that("async standalone suggestions resolve the same projection", {
  result <- NULL
  error <- NULL
  promises::then(
    tempest:::tempest_suggest_questions_async("Animatronics", n = 2),
    onFulfilled = function(value) result <<- value,
    onRejected = function(value) error <<- value
  )
  later::run_now(timeoutSecs = 1)

  expect_null(error)
  expect_equal(result, tempest_suggest_questions("Animatronics", n = 2))
})

test_that("moderator session prompt names the real delegation tool and roster", {
  expert <- test_expert(
    expert_id = "expert.safety",
    name = "Dr. Safety",
    title = "Safety engineer"
  )

  prompt <- tempest:::tempest_moderator_system_prompt(
    "Adaptive animatronics",
    list(expert)
  )

  expect_match(prompt, "delegate_to_expert", fixed = TRUE)
  expect_match(prompt, "expert.safety", fixed = TRUE)
  expect_match(prompt, "Adaptive animatronics", fixed = TRUE)
  expect_match(prompt, "at least once before answering", fixed = TRUE)
  expect_match(prompt, "at most once per moderator", fixed = TRUE)
  expect_match(prompt, "one narrow evidence question", fixed = TRUE)
  expect_match(prompt, "Do not append generic next-step menus", fixed = TRUE)
  expect_match(prompt, "clickable follow-up question cards", fixed = TRUE)
  expect_match(prompt, "specific evidence gap", fixed = TRUE)
  expect_no_match(prompt, "ask_*", fixed = TRUE)
  expect_no_match(prompt, "ask_expert", fixed = TRUE)
})
