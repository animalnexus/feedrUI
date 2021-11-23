app <- ShinyDriver$new("../../")
app$snapshotInit("test_db")
app$snapshot()
