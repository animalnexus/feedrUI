app <- ShinyDriver$new("../../")
app$snapshotInit("test_import01")

# One file -------
app$uploadFile(`standalone-file1` =
                 system.file("extdata", "chickadees.csv", package = "feedr"))
app$snapshot()


# Multi files ----------------
app$uploadFile(`standalone-file1` =
                 c(system.file("extdata", "chickadees.csv", package = "feedr"),
                   system.file("extdata", "finches.csv", package = "feedr")))
app$snapshot()

# Single logger file --------------
app$uploadFile(`standalone-file1` =
                 system.file("extdata", "raw", "exp2",
                             "GR10DATA_2016_01_16.TXT", package = "feedr"))
app$snapshot() # Expect error
app$setInputs(`standalone-format` = "logger")
app$snapshot() # Expect no error

# Multi logger files -------------------
app$uploadFile(`standalone-file1` =
                 c(system.file("extdata", "raw", "exp2",
                               "GR10DATA_2016_01_16.TXT", package = "feedr"),
                   system.file("extdata", "raw", "exp2",
                               "GR11DATA_2016_01_16.TXT", package = "feedr")))
app$snapshot() # Expect no error

# Change col names -----------------------
app$uploadFile(`standalone-file1` =
                 system.file("extdata", "import_tests",
                             "finches_colnames2.csv", package = "feedr"))
app$setInputs(`standalone-format` = "all")
app$snapshot() # Expect no error

# Error col names ------------------
app$uploadFile(`standalone-file1` =
                 system.file("extdata", "import_tests",
                             "finches_colnames.csv", package = "feedr"))
app$setInputs(`standalone-format` = "all")
app$snapshot() # Expect error

app$uploadFile(`standalone-file1` =
                 system.file("extdata", "import_tests",
                             "finches_cols.csv", package = "feedr"))
app$setInputs(`standalone-format` = "all")
app$snapshot() # Expect error
