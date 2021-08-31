
rChainEnc <- function() {
  library(openssl)
  library(uuid)
  library(jsonlite)

  items_pool <- NULL
  blocks <- list()
  last_block <- 0L
  keypair <- ec_keygen("P-521")

  # Exposed functions
  addItem <- function(data) {

    id <- UUIDgenerate(TRUE)
    ts <- as.POSIXlt(Sys.time(), "UTC")
    ck <- as.character(sha384(paste(id, ts, data, sep = "+")))
    if (is.null(items_pool)) {
      items_pool <<- as.data.frame(list(Id = id,
                                        Timestamp = ts,
                                        Data = data,
                                        Check = ck))
    } else {
      items_pool <<- rbind(items_pool, list(id, ts, data, ck))
    }
  }

  getItemPool <- function() {
    return(items_pool)
  }

  getBlocks <- function(num = NULL) {
    if (is.null(num)) {
      return(blocks)
    } else {
      stopifnot(is.numeric(num))
      stopifnot(length(num) == 1)
      return(blocks[num])
    }

  }

  getPubKey <- function() {
    return(keypair$pubkey)
  }

  createBlock <- function() {
    if (last_block == 0L) {
      # Need to mint genesis block
      addItem("Welcome to Genesis Block")
    }

    if (is.null(items_pool)) {
      cat("No new items to add to block.")
    } else {

      it <- getItemPool()
      resetItemPool()

      id <- UUIDgenerate(FALSE)
      ts <- as.POSIXlt(Sys.time(), "UTC")
      prt1 <- ifelse(last_block == 0L, NA, blocks[[last_block]]$Header$Id)
      prt2 <- ifelse(last_block == 0L, NA, blocks[[last_block]]$Header$Check)
      hash <- as.character(sha512(paste(it$Check, collapse = "+")))
      ck_str <- paste(id, ts, prt1, prt2, hash, sep="+")
      ck <- signature_create(charToRaw(ck_str), sha512, keypair)

      block <- list(Header = list(Seq = last_block + 1,
                                  Id = id,
                                  Timestamp = ts,
                                  Parent = prt1,
                                  Content = hash,
                                  Check = base64_encode(ck)),
                    Body = it)

      blocks <<- append(blocks, list(block))
      last_block <<- last_block + 1
    }
  }

  # Internal functions
  resetItemPool <- function() {
    items_pool <<- NULL
  }

  # Initialize chain
  createBlock()
  return(list(addItem = addItem,
              getItemPool = getItemPool,
              createBlock = createBlock,
              getBlocks = getBlocks,
              getPubKey = getPubKey))
}

rChain <- rChainEnc()
rm(rChainEnc)