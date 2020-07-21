test_that("monoid applicators work", {
  expect_equal(apply_prod(1, function() 0), 0)
  expect_equal(apply_prod(1, function() 2), 2)
  expect_equal(apply_prod(1, function(t) t*2, t = 2), 4)
  expect_equal(apply_prod(0, function(t) t*2, t = 2), 0)

  expect_equal(apply_sum(1, function() 0), 1)
  expect_equal(apply_sum(1, function() 2), 3)
  expect_equal(apply_sum(1, function(t) t*2, t = 2), 5)
  expect_equal(apply_sum(0, function(t) t*2, t = 2), 4)
})
