
rChainEnc <- function() {
  library(openssl)
  library(uuid)
  library(jsonlite)
  
  items_pool <- NULL
  blocks <- NULL
  next_block <- 0
  
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
  
  resetItemPool <- function() {
    items_pool <<- NULL
  }
  
  init <- function() {
    addItem("Genesis Block")
    it <- getItemPool()
    resetItemPool()
    
    block <- list(header = list(ID = UUIDgenerate(TRUE),
                                Time = as.POSIXlt(Sys.time(), "UTC"),
                                Parent = NA,
                                Content = as.character(sha384(paste(it$ID,
                                                                    collapse = "+"))),
                                Challenge = NA),
                  body = it)
    blocks <<- block
    next_block <<- 1
  }
  
  init()
  
  return(list(addItem = addItem, 
              getItemPool = getItemPool,
              getBlocks = getBlocks))
  
}

rChain <- rChainEnc()
rm(rChainEnc)