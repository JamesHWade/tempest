test_that("tempest_suggest_questions trims, drops empties, and de-duplicates", {
  chat <- fake_chat(structured = list(list(questions = c(
    " What is animatronics? ",
    "What is animatronics?",
    "How do servos work?",
    ""
  ))))
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
  expect_equal(tempest_suggest_questions("   ", chat = fake_chat()), character())
})

test_that("tempest_suggest_questions returns empty when the chat errors", {
  # fake_chat with an empty structured queue stops() on chat_structured()
  expect_equal(tempest_suggest_questions("Topic", chat = fake_chat()), character())
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
  expect_true(any(grepl("Conversation so far:", prompts, fixed = TRUE)))
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
    personas = list(list(id = 1, name = "Dr. A", title = "Sci", perspective = "X"))
  )
  out <- ses$suggest_questions(n = 2)
  expect_equal(out, c("Q1", "Q2"))
})
