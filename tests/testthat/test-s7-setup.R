test_that("S7 is available and a trivial class validates", {
  cls <- S7::new_class("tmp_cls", properties = list(x = S7::class_numeric))
  obj <- cls(x = 1)
  expect_equal(obj@x, 1)
})
