app <- ShinyDriver$new("../../")
app$snapshotInit("test_animate")

app$waitForValue("standalone-setup_time-instant", ignore = list(NULL))
app$snapshot()
app$setInputs(`standalone-setup_time-instant` = 1454097600, timeout_ = 1000)
app$snapshot()
app$setInputs(`standalone-setup_time-instant` = 1454184000, timeout_ = 1000)
app$snapshot()

# Back to start - single bird
app$setInputs(`standalone-setup_time-instant` = 1454011200, timeout_ = 1000)
app$setInputs(`standalone-adv-animal_id` = "06200004F8", timeout_ = 1000)
app$snapshot()
app$setInputs(`standalone-setup_time-instant` = 1454097600, timeout_ = 1000)
app$snapshot()
app$setInputs(`standalone-setup_time-instant` = 1454184000, timeout_ = 000)
app$snapshot()

# Instant images
app$setInputs(`standalone-adv-type` = "instant")
app$setInputs(`standalone-setup_time-instant` = 1454097600, timeout_ = 1000)
app$setInputs(`standalone-setup_time-instant` = 1454011200, timeout_ = 1000)
app$setInputs(`standalone-setup_time-instant` = 1454097600, timeout_ = 1000)
app$snapshot()
