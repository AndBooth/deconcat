
context("Testing deconcat function and subfunctions")


test_that("Main deconcat function", {

   s <- "teststringtosplit"
   load("default_dict.rda")
   N <- nrow(default_dict)
   max_length <- max(purrr::map_int(default_dict$word, ~ stringr::str_length(.)))

   expect_equal(deconcat_dev(s, default_dict, max_length), c("test string to split"))


})



