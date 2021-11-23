test_that("round_6 12 handles diff tz", {
  t1 <- as.POSIXct("2016-09-23 01:00:12", tz = "America/Vancouver")
  t2 <- as.POSIXct("2016-09-23 08:00:12", tz = "America/Moncton")
  t3 <- as.POSIXct("2016-09-23 19:00:12", tz = "Europe/London")

  expect_is(round_6(t1), "POSIXct")
  expect_equal(round_6(t1), as.POSIXct("2016-09-22 18:00:00", tz = "America/Vancouver"))
  expect_equal(round_6(t2), as.POSIXct("2016-09-23 06:00:00", tz = "America/Moncton"))
  expect_equal(round_6(t3), as.POSIXct("2016-09-23 18:00:00", tz = "Europe/London"))
})

test_that("round_6 24 handles diff tz", {
  t1 <- as.POSIXct("2016-09-23 01:00:12", tz = "America/Vancouver")
  t2 <- as.POSIXct("2016-09-23 08:00:12", tz = "America/Moncton")
  t3 <- as.POSIXct("2016-09-23 19:00:12", tz = "Europe/London")

  expect_is(round_6(t1, by = 24), "POSIXct")
  expect_equal(round_6(t1, by = 24), as.POSIXct("2016-09-22 06:00:00", tz = "America/Vancouver"))
  expect_equal(round_6(t2, by = 24), as.POSIXct("2016-09-23 06:00:00", tz = "America/Moncton"))
  expect_equal(round_6(t3, by = 24), as.POSIXct("2016-09-23 06:00:00", tz = "Europe/London"))
})

test_that("round_6 rounds for 12 hrs", {
  t1 <- as.POSIXct("2016-09-23 01:00:12")
  t2 <- as.POSIXct("2016-09-23 08:00:12")
  t3 <- as.POSIXct("2016-09-23 19:00:12")

  expect_is(round_6(t1), "POSIXct")
  expect_equal(round_6(t1), as.POSIXct("2016-09-22 18:00:00"))
  expect_equal(round_6(t2), as.POSIXct("2016-09-23 06:00:00"))
  expect_equal(round_6(t3), as.POSIXct("2016-09-23 18:00:00"))
})

test_that("round_6 rounds for 24 hrs", {
  t1 <- as.POSIXct("2016-09-23 01:00:12")
  t2 <- as.POSIXct("2016-09-23 08:00:12")
  t3 <- as.POSIXct("2016-09-23 19:00:12")

  expect_is(round_6(t1, by = 24), "POSIXct")
  expect_equal(round_6(t1, by = 24), as.POSIXct("2016-09-22 06:00:00"))
  expect_equal(round_6(t2, by = 24), as.POSIXct("2016-09-23 06:00:00"))
  expect_equal(round_6(t3, by = 24), as.POSIXct("2016-09-23 06:00:00"))
})
