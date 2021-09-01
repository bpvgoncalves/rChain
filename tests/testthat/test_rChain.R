#source
library(testthat)

test_that("Class instantiates and genesis block created",{
  expect_invisible(source("./rChain.R"))
  expect_length(rChain$getBlocks(), 1)
})

test_that("Key matches expected parameters", {
  pub <- rChain$getPubKey()
  expect_equal(pub$type, "ecdsa")
  expect_equal(pub$size, 521)
})





rChain$addItem("Add new message")
rChain$addItem("Yet another message")

rChain$getItemPool()

rChain$createBlock()

rChain$getBlocks()

rChain$getBlocks(2)
rChain$getBlocks("a")  #must fail
rChain$getBlocks(c(1,2)) # must fail

rChain$validateChain()$Result
