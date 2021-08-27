
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
    if (is.null(items_pool)) {
      items_pool <<- as.data.frame(list(ID = id, Content = data))
    } else {
      items_pool <<- rbind(items_pool, c(id, data))
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
                                  Content = as.character(sha384(paste(it$ID,
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
                                Content = as.character(sha384(paste(it$ID,
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