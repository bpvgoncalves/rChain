

rChainEnc <- function() {
  library(openssl)
  library(uuid)
  library(jsonlite)
  
  items_pool <- NULL
  
  addItem <- function(data) {
    
    id <- UUIDgenerate(TRUE)
    if (is.null(items_pool)) {
      items_pool <<- as.data.frame(list(ID = id, Content = data))
    } else {
      items_pool <<- rbind(items_pool, c(id, data))
    }
  }
  
  getItemPool <- function() {
    return(toJSON(items_pool))
  }
  
  return(list(addItem = addItem, 
              getItemPool = getItemPool))
  
}

rChain <- rChainEnc()
rm(rChainEnc)