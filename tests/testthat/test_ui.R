library(shinytest)

test_that("shiny apps work", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.

  expect_pass(testApp(test_path("apps/ui_current/"), compareImages = FALSE))

  expect_pass(testApp(test_path("apps/ui_import/"), compareImages = FALSE))
  expect_pass(testApp(test_path("apps/ui_db/"), compareImages = FALSE))

  expect_pass(testApp(test_path("apps/ui_animate/"), compareImages = FALSE))
  expect_pass(testApp(test_path("apps/ui_trans/"), compareImages = FALSE))

})


#shinytest::recordTest(test_path("apps/ui_import/"))
#recordTest(test_path("apps/ui_animate/"))

shinytest::recordTest(test_path("apps/ui_trans/"))


