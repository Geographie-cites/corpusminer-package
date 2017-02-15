context("keyword network")

test_that("MakeEdgesList", {
  expect_equal( MakeEdgesList("foo"), "foo_foo")
  
  expect_equal( MakeEdgesList(c("a", "b")), "a_b")
  expect_equal( MakeEdgesList(c("a", "b", "c")), c("a_b", "a_c", "b_c") )
  
})