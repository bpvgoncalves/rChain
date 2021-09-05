
rChain <- R6::R6Class(
  classname = "blockchain",

  # Exposed functions
  public = list(

    initialize = function() {
      private$keypair <- openssl::ec_keygen("P-521")
      self$createBlock()
    },

    addItem = function(data = NULL) {

      if (is.null(data)) stop("'data' is required to create a new item.")
      if (is.na(data)) stop("'data' is required to create a new item.")
      if (!is.character(data)) data <- as.character(data)

      id <- uuid::UUIDgenerate(TRUE)
      ts <- as.POSIXlt(Sys.time(), "UTC")
      ck <- as.character(openssl::sha512(paste(id, ts, data, sep = "+")))
      ck <- openssl::base64_encode(openssl::signature_create(charToRaw(ck),
                                                             openssl::sha512,
                                                             private$keypair))
      if (is.null(private$items_pool)) {
        private$items_pool <- as.data.frame(list(Id = id,
                                                 Timestamp = ts,
                                                 Data = data,
                                                 Check = ck))
      } else {
        private$items_pool <- rbind(private$items_pool, list(id, ts, data, ck))
      }
      return(list(Result=TRUE, Id=id))
    },

    getItemPool = function() {
      return(private$items_pool)
    },

    getBlocks = function(num = NULL) {
      if (is.null(num)) {
        return(private$blocks)
      } else {
        if(!is.numeric(num)) stop("Block number expected to be 'numeric'")
        if(length(num) != 1) stop("Block number expected to be a scalar")
        if(num %% 1 != 0) stop("Block number expected to be an integer")
        if(num > private$last_block) stop("Block number bigger than total number of blocks")
        return(private$blocks[num])
      }
    },

    getPubKey = function() {
      return(private$keypair$pubkey)
    },

    createBlock = function() {
      if (private$last_block == 0L) {
        # Need to mint genesis block
        self$addItem("Welcome to Genesis Block")
      }

      if (is.null(private$items_pool)) {
        message("No new items to add to block.")
      } else {

        it <- self$getItemPool()
        private$resetItemPool()

        id <- uuid::UUIDgenerate(FALSE)
        ts <- as.POSIXlt(Sys.time(), "UTC")
        prt1 <- ifelse(private$last_block == 0L, NA, private$blocks[[private$last_block]]$Header$Id)
        prt2 <- ifelse(private$last_block == 0L, NA, private$blocks[[private$last_block]]$Header$Check)
        hash <- as.character(openssl::sha512(paste(it$Check, collapse = "+")))
        ck_str <- paste(id, ts, prt1, prt2, hash, sep="+")
        ck <- openssl::signature_create(charToRaw(ck_str), openssl::sha512, private$keypair)

        block <- list(Header = list(Seq = private$last_block + 1,
                                    Id = id,
                                    Timestamp = ts,
                                    Parent = prt1,
                                    Content = hash,
                                    Check = openssl::base64_encode(ck)),
                      Body = it)

        private$blocks <- append(private$blocks, list(block))
        private$last_block <- private$last_block + 1
        return(list(Result = TRUE, Id = id, Seq = private$last_block))
      }
    },

    validateItems = function(items){
      if (is.null(items)) {
        message("No items to validate.")
      } else {
        for (i in 1:nrow(items)) {
          i <- items[i, ]
          ck <- as.character(openssl::sha512(paste(i$Id, i$Timestamp, i$Data, sep = "+")))
          chk_result <- try(openssl::signature_verify(charToRaw(ck),
                                                      openssl::base64_decode(i$Check),
                                                      openssl::sha512,
                                                      private$keypair$pubkey),
                            silent = TRUE)
          if (class(chk_result) == "try-error"){
            cat("Failed signature validation for transaction. Id: ", i$Id, "\n")
            return(list(Result = FALSE, Details = i$Id, At = "Item"))
          }
        }
      }
      return(list(Result = TRUE, Details = NA, At = "Item"))
    },

    validateBlock = function(num) {

      body_val <- self$validateItems(private$blocks[[num]]$Body)
      if (body_val$Result) {
        # Body items validated, lets check header integrity
        ck_str <- paste(private$blocks[[num]]$Header$Id,
                        private$blocks[[num]]$Header$Timestamp,
                        ifelse(num == 1, NA, private$blocks[[num - 1]]$Header$Id),
                        ifelse(num == 1, NA, private$blocks[[num - 1]]$Header$Check),
                        private$blocks[[num]]$Header$Content,
                        sep="+")
        chk_result <- try(openssl::signature_verify(charToRaw(ck_str),
                                                    openssl::base64_decode(private$blocks[[num]]$Header$Check),
                                                    openssl::sha512,
                                                    private$keypair$pubkey),
                          silent = TRUE)
        if (class(chk_result) == "try-error"){
          cat("Failed signature validation for block. Id: ", private$blocks[[num]]$Header$Id, "\n")
          return(list(Result = FALSE, Details = private$blocks[[num]]$Header$Id, At = "Block"))
        } else {
          return(list(Result = TRUE, Details = NA, At = "Block"))
        }
      } else {
        return(body_val)
      }
    },

    validateChain = function() {
      for (b in 1:private$last_block) {
        blk_val <- self$validateBlock(b)
        if (!blk_val$Result) {
          cat("Chain validation failed at block", b, "\n")
          return(blk_val)
        }
      }
      return(list(Result = TRUE, Details = NA, At = "Chain"))
    }
  ),

  # Internal functions
  private = list(

    items_pool = NULL,

    blocks = list(),

    last_block = 0L,

    keypair = NULL,

    resetItemPool = function() {
      private$items_pool <<- NULL
    }
  ),
  lock_objects = TRUE,
  portable = FALSE,
  cloneable = FALSE,
  lock_class = TRUE)
