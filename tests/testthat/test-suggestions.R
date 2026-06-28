test_that("tempest_suggest_questions trims, drops empties, and de-duplicates", {
  chat <- fake_chat(
    structured = list(list(
      questions = c(
        " What is animatronics? ",
        "What is animatronics?",
        "How do servos work?",
        ""
      )
    ))
  )
  out <- tempest_suggest_questions("Animatronics", chat = chat, n = 4)
  expect_equal(out, c("What is animatronics?", "How do servos work?"))
})

test_that("tempest_suggest_questions caps the result at n", {
  chat <- fake_chat(structured = list(list(questions = paste0("Q", 1:10))))
  out <- tempest_suggest_questions("Topic", chat = chat, n = 3)
  expect_equal(out, c("Q1", "Q2", "Q3"))
})

test_that("tempest_suggest_questions returns empty for a blank topic", {
  expect_equal(tempest_suggest_questions("", chat = fake_chat()), character())
  expect_equal(
    tempest_suggest_questions("   ", chat = fake_chat()),
    character()
  )
})

test_that("tempest_suggest_questions returns empty when the chat errors", {
  # fake_chat with an empty structured queue stops() on chat_structured()
  expect_equal(
    tempest_suggest_questions("Topic", chat = fake_chat()),
    character()
  )
})

test_that("tempest_suggest_questions returns empty when 'questions' is missing", {
  chat <- fake_chat(structured = list(list(other = "x")))
  expect_equal(tempest_suggest_questions("Topic", chat = chat), character())
})

test_that("tempest_suggest_questions includes conversation context in the prompt", {
  chat <- fake_chat(structured = list(list(questions = c("Q1"))))
  tempest_suggest_questions(
    "Topic",
    context = "User: hi\nModerator: hello",
    chat = chat,
    n = 1
  )
  prompts <- vapply(chat$.calls(), function(call) call$prompt, character(1))
  expect_match(
    paste(prompts, collapse = "\n"),
    "Conversation so far:",
    fixed = TRUE
  )
})

test_that("suggestion prompts separate cards from generic deliverable offers", {
  prompt <- tempest:::tempest_suggest_questions_prompt(
    "Adaptive animatronics",
    context = "User: What standards apply?\nModerator: ISO sources matter.",
    n = 3
  )

  expect_match(prompt, "clickable suggestion cards", fixed = TRUE)
  expect_match(prompt, "fill evidence gaps", fixed = TRUE)
  expect_match(prompt, "expand the mind map", fixed = TRUE)
  expect_match(prompt, "Do not suggest generic artifacts", fixed = TRUE)
  expect_match(prompt, "gap-analysis tables", fixed = TRUE)
})

test_that("TempestSession$suggest_questions delegates to the generator", {
  skip_if_not_installed("ellmer")
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      fake_chat(structured = list(list(questions = c("Q1", "Q2"))))
    }
  )
  ses <- tempest_session(
    "Test topic",
    config = cfg,
    personas = list(list(
      id = 1,
      name = "Dr. A",
      title = "Sci",
      perspective = "X"
    ))
  )
  out <- ses$suggest_questions(n = 2)
  expect_equal(out, c("Q1", "Q2"))
})

test_that("suggest_questions forwards transcript context when non-empty", {
  skip_if_not_installed("ellmer")
  fake <- fake_chat(structured = list(list(questions = "Q1")))
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) fake
  )
  ses <- tempest_session(
    "Test topic",
    config = cfg,
    personas = list(list(
      id = 1,
      name = "Dr. A",
      title = "Sci",
      perspective = "X"
    ))
  )
  ses$add_turn("User", "user", "hello there")
  ses$suggest_questions(n = 1)
  prompts <- vapply(fake$.calls(), function(call) call$prompt, character(1))
  expect_match(
    paste(prompts, collapse = "\n"),
    "Conversation so far:",
    fixed = TRUE
  )
})

test_that("Co-STORM moderator prompt suppresses generic next-step menus", {
  skip_if_not_installed("ellmer")
  moderator <- fake_chat(text = list("Evidence-grounded answer."))
  extractor <- fake_chat(structured = list(list(facts = list())))
  mindmap <- fake_chat(
    structured = list(list(
      nodes = list(list(
        id = "root",
        label = "Adaptive animatronics",
        summary = "Root topic",
        source_ids = character()
      )),
      edges = list()
    ))
  )
  cfg <- tempest_config(
    chat_fn = function(role, model, system_prompt, echo) {
      if (identical(system_prompt, tempest_prompt("moderator_system"))) {
        return(moderator)
      }
      if (identical(system_prompt, tempest_prompt("fact_extractor_system"))) {
        return(extractor)
      }
      if (identical(role, "mindmap")) {
        return(mindmap)
      }
      fake_chat()
    }
  )
  ses <- tempest_session(
    "Adaptive animatronics",
    config = cfg,
    personas = list(list(
      id = 1,
      name = "Dr. A",
      title = "Engineer",
      perspective = "Safety"
    ))
  )

  ses$step("What standards apply?")

  prompt <- paste(
    vapply(moderator$.calls(), function(call) call$prompt, character(1)),
    collapse = "\n"
  )
  expect_match(prompt, "Do not end with a generic menu", fixed = TRUE)
  expect_match(prompt, "Clickable follow-up cards", fixed = TRUE)
  expect_match(prompt, "specific evidence gap", fixed = TRUE)
})
