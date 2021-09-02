
rChainEnc <- function() {
  library(openssl)
  library(uuid)

  items_pool <- NULL
  blocks <- list()
  last_block <- 0L
  keypair <- ec_keygen("P-521")

  # Exposed functions
  addItem <- function(data = NULL) {

    if (is.null(data)) stop("'data' is required to create a new item.")
    if (is.na(data)) stop("'data' is required to create a new item.")
    if (!is.character(data)) data <- as.character(data)

    id <- UUIDgenerate(TRUE)
    ts <- as.POSIXlt(Sys.time(), "UTC")
    ck <- as.character(sha512(paste(id, ts, data, sep = "+")))
    ck <- base64_encode(signature_create(charToRaw(ck), sha512, keypair))
    if (is.null(items_pool)) {
      items_pool <<- as.data.frame(list(Id = id,
                                        Timestamp = ts,
                                        Data = data,
                                        Check = ck))
    } else {
      items_pool <<- rbind(items_pool, list(id, ts, data, ck))
    }
    return(list(Result=TRUE, Id=id))
  }

  getItemPool <- function() {
    return(items_pool)
  }

  getBlocks <- function(num = NULL) {
    if (is.null(num)) {
      return(blocks)
    } else {
      if(!is.numeric(num)) stop("Block number expected to be 'numeric'")
      if(length(num) != 1) stop("Block number expected to be a scalar")
      if(num %% 1 != 0) stop("Block number expected to be an integer")
      if(num > last_block) stop("Block number bigger than total number of blocks")
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
      message("No new items to add to block.")
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
      return(list(Result = TRUE, Id = id, Seq = last_block))
    }
  }

  validateItems <- function(items){
    if (is.null(items)) {
      message("No items to validate.")
    } else {rl
      for (i in 1:nrow(items)) {
        i <- items[i, ]
        ck <- as.character(sha512(paste(i$Id, i$Timestamp, i$Data, sep = "+")))
        chk_result <- try(signature_verify(charToRaw(ck),
                                           base64_decode(i$Check),
                                           sha512,
                                           keypair$pubkey),
                          silent = TRUE)
        if (class(chk_result) == "try-error"){
          cat("Failed signature validation for transaction. Id: ", i$Id, "\n")
          return(list(Result = FALSE, Details = i$Id, At = "Item"))
        }
      }
    }
  return(list(Result = TRUE, Details = NA, At = "Item"))
  }

  validateBlock <- function(num) {

    body_val <- validateItems(blocks[[num]]$Body)
    if (body_val$Result) {
      # Body items validated, lets check header integrity
      ck_str <- paste(blocks[[num]]$Header$Id,
                      blocks[[num]]$Header$Timestamp,
                      ifelse(num == 1, NA, blocks[[num - 1]]$Header$Id),
                      ifelse(num == 1, NA, blocks[[num - 1]]$Header$Check),
                      blocks[[num]]$Header$Content,
                      sep="+")
      chk_result <- try(signature_verify(charToRaw(ck_str),
                                         base64_decode(blocks[[num]]$Header$Check),
                                         sha512,
                                         keypair$pubkey),
                        silent = TRUE)
      if (class(chk_result) == "try-error"){
        cat("Failed signature validation for block. Id: ", blocks[[num]]$Header$Id, "\n")
        return(list(Result = FALSE, Details = blocks[[num]]$Header$Id, At = "Block"))
      } else {
        return(list(Result = TRUE, Details = NA, At = "Block"))
      }
    } else {
      return(body_val)
    }
  }

  validateChain <- function() {
    for (b in 1:last_block) {
      blk_val <- validateBlock(b)
      if (!blk_val$Result) {
        cat("Chain validation failed at block", b, "\n")
        return(blk_val)
      }
    }
    return(list(Result = TRUE, Details = NA, At = "Chain"))
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
              getPubKey = getPubKey,
              validateChain = validateChain))
}

rChain <- rChainEnc()
rm(rChainEnc)