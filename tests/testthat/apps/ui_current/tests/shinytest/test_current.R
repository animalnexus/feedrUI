app <- ShinyDriver$new("../../")
app$snapshotInit("test_current")

app$setInputs(`standalone-current_update` = "click")
app$snapshot()
