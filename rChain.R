
rChainEnc <- function() {
  library(openssl)
  library(uuid)
  library(jsonlite)

  items_pool <- NULL
  blocks <- list()
  last_block <- NA

  # Exposed functions
  addItem <- function(data) {

    id <- UUIDgenerate(TRUE)
    ts <- as.POSIXlt(Sys.time(), "UTC")
    ck <- as.character(sha384(paste(id, ts, data, sep = "+")))
    if (is.null(items_pool)) {
      items_pool <<- as.data.frame(list(Id = id,
                                        Timestamp = ts,
                                        Content = data,
                                        Check = ck))
    } else {
      items_pool <<- rbind(items_pool, list(id, ts, data, ck))
    }
  }

  getItemPool <- function() {
    return(items_pool)
  }

  getBlocks <- function() {
    return(blocks)
  }

  createBlock <- function() {
    if (is.null(items_pool)) {
      cat("No new items to add to block.")
    } else {

      it <- getItemPool()
      resetItemPool()

      block <- list(Header = list(Id = UUIDgenerate(TRUE),
                                  Timestamp = as.POSIXlt(Sys.time(), "UTC"),
                                  Parent = blocks[[last_block]]$Header$Id,
                                  Content = as.character(sha384(paste(it$Check,
                                                                      collapse = "+"))),
                                  Challenge = NA),
                    Body = it)

      blocks <<- append(blocks, list(block))
      last_block <<- last_block + 1
    }
  }

  # Internal functions
  init <- function() {
    addItem("Welcome to Genesis Block")
    it <- getItemPool()
    resetItemPool()

    block <- list(Header = list(Id = UUIDgenerate(TRUE),
                                Timestamp = as.POSIXlt(Sys.time(), "UTC"),
                                Parent = NA,
                                Content = as.character(sha384(paste(it$Check,
                                                                    collapse = "+"))),
                                Challenge = NA),
                  Body = it)

    blocks <<- list(block)
    last_block <<- 1
  }

  resetItemPool <- function() {
    items_pool <<- NULL
  }

  init()
  return(list(addItem = addItem,
              getItemPool = getItemPool,
              createBlock = createBlock,
              getBlocks = getBlocks))
}

rChain <- rChainEnc()
rm(rChainEnc)