# dl_data -----------------------------------------------------------------
test_that("dl_data loads and formats data correctly", {
  expect_silent(load <- dl_data(start = "2016-01-01", end = "2016-02-01"))
  expect_silent(load2 <- dl_data(start = "2016-01-01", end = "2016-02-01", tz_disp = "America/Toronto"))
  expect_is(load, "data.frame")
  expect_match(names(load)[1:4], "^animal_id$|^date$|^time$|^logger_id$")
  expect_is(load$animal_id, "factor")
  expect_is(load$logger_id, "factor")
  expect_is(load$time, "POSIXct")

  expect_equal(load$animal_id[1], factor("0620000514", levels = c("041868D396", "041868D861", "041868FF93", "062000043E", "06200004F8", "0620000514")))
  expect_equal(load$logger_id[1], factor("2200", levels = c("2100", "2200", "2400", "2700")))
  expect_equal(load$time[1], as.POSIXct("2016-01-28 12:34:25", tz = "Etc/GMT+8"))
  expect_equal(load2$time[1], as.POSIXct("2016-01-28 15:34:25", tz = "Etc/GMT+5"))
})

test_that("dl_data gracefully ends if no data to download", {
  expect_error(dl_data(start = "2010-01-01", end = "2010-02-02"), "There are no online data matching these parameters.")
})

test_that("dl_data filters by species_code and site_id", {
  all <- dl_data(start = "2016-01-28", end = "2016-02-01")
  hofi <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "House Finch")
  moch <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "Mountain Chickadee")

  expect_equal(unique(all$species), c("House Finch", "Mountain Chickadee"))
  expect_equal(unique(hofi$species), c("House Finch"))
  expect_equal(unique(moch$species), c("Mountain Chickadee"))
  expect_equal(nrow(all), 413)
  expect_equal(nrow(hofi), 412)
  expect_equal(nrow(moch), 1)
  expect_true(min(all$time) >= as.POSIXct("2016-01-28"))
  expect_true(max(all$time) <= as.POSIXct("2016-02-01"))

  hofi_sp <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "house finch")
  moch_sp <- dl_data(start = "2016-01-28", end = "2016-02-01", species = "mountain CHICKadee")

  expect_equal(hofi, hofi_sp)
  expect_equal(moch, moch_sp)
})

test_that("dl_data sites and that uses credentials appropriatly", {
  if(!is.null(check_db())) {
    both <- dl_data(start = "2013-05-24", end = "2015-09-04")
    expect_equal(unique(both$site_name), c("Costa Rica", "Kamloops, BC"))
    kl <- dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "kl")
    expect_equal(unique(kl$site_name), "Kamloops, BC")
    cr <- dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "cr")
    expect_equal(unique(cr$site_name), "Costa Rica")
  } else {
    both <- dl_data(start = "2013-05-24", end = "2015-09-04")
    expect_equal(unique(both$site_name), "Kamloops, BC")
    kl <- dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "kl")
    expect_equal(unique(kl$site_name), "Kamloops, BC")
    expect_error(dl_data(start = "2013-05-24", end = "2015-09-04", site_id = "cr"), "There are no online data matching these parameters.")
  }
})


