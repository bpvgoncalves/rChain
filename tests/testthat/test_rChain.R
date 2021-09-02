#source
library(testthat)

test_that("Class instantiates and genesis block created",{
  expect_invisible(source("~/Documents/Projects/rChain/R/rChain.R"))
  expect_length(rChain$getBlocks(), 1)
  expect_identical(rChain$getBlocks(), rChain$getBlocks(1))
})

test_that("Key matches expected parameters", {
  pub <- rChain$getPubKey()
  expect_equal(pub$type, "ecdsa")
  expect_equal(pub$size, 521)
})

test_that("New Items can be added", {
  expect_type(rChain$addItem("Add new message"), "list")
  expect_type(rChain$addItem("Yet another message")$Id, "character")
  expect_equal(rChain$addItem("And a last one")$Result, TRUE)
  expect_error(rChain$addItem(), "'data' is required to create a new item.")
  expect_error(rChain$addItem(NULL), "'data' is required to create a new item.")
  expect_error(rChain$addItem(NA), "'data' is required to create a new item.")
})

test_that("Items Pool status can be retrieved", {
  pool <- rChain$getItemPool()
  expect_s3_class(pool, "data.frame")
  expect_equal(nrow(pool), 3)
  expect_type(pool$Id, "character")
  expect_s3_class(pool$Timestamp, "POSIXct")
  expect_type(pool$Data, "character")
  expect_type(pool$Check, "character")
  expect_equal(pool[1, 3], "Add new message")
})

test_that("New blocks can be minted", {
  expect_silent(nb <- rChain$createBlock())
  expect_type(nb, "list")
  expect_named(nb, c("Result", "Id", "Seq"))
  expect_equal(nb$Result, TRUE)
  expect_equal(nb$Seq, 2)

  # 2nd block not minted until new items pending
  expect_message(rChain$createBlock(), "No new items to add to block.")
})

test_that("Blocks can be retrieved", {
  expect_type(rChain$getBlocks(), "list")
  expect_length(rChain$getBlocks(), 2)
  expect_length(rChain$getBlocks(2), 1)
  expect_named(rChain$getBlocks(2)[[1]], c("Header", "Body"))

  # blocks with wrong parameter cannot be retrieved
  expect_error(rChain$getBlocks("abc"), "Block number expected to be 'numeric'")
  expect_error(rChain$getBlocks(c(1, 2)), "Block number expected to be a scalar")
  expect_error(rChain$getBlocks(1.1), "Block number expected to be an integer")
  expect_error(rChain$getBlocks(10), "Block number bigger than total number of blocks")
})

test_that("Chain can be validated", {
  expect_silent(val <- rChain$validateChain())
  expect_type(val, "list")
  expect_named(val, c("Result", "Details", "At"))
  expect_true(val$Result)
  expect_equal(val$Details, NA)
  expect_equal(val$At, "Chain")
})
