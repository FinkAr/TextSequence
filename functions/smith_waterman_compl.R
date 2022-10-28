smith_waterman_compl <-function (a, b, FUN = identity, type = "words", tokenizer = function(x) unlist(strsplit(x, "[[:space:]]")), ...) 
{
  as_tif <- function(x) {
    if (is.character(x) | is.factor(x)) {
      if (is.null(names(x))) {
        x <- data.frame(doc_id = seq_along(x), text = as.character(x), 
                        stringsAsFactors = FALSE)
      }
      else {
        x <- data.frame(doc_id = names(x), text = as.character(x), 
                        stringsAsFactors = FALSE)
      }
    }
    x
  }
  set_names <- function(object, objectnames) {
    names(object) <- objectnames
    object
  }
  a <- as_tif(a)
  b <- as_tif(b)
  stopifnot(is.data.frame(a) & is.data.frame(b))
  stopifnot(all(c("doc_id", "text") %in% colnames(a)))
  stopifnot(all(c("doc_id", "text") %in% colnames(b)))
  a <- a[, c("doc_id", "text"), drop = FALSE]
  b <- b[, c("doc_id", "text"), drop = FALSE]
  combinations <- merge(a, b, by = character(), suffixes = c(".a", 
                                                             ".b"))
  x <- mapply(a = set_names(combinations$text.a, combinations$doc_id.a), 
              b = set_names(combinations$text.b, combinations$doc_id.b), 
              FUN = function(a, b, ...) {
                alignment <- smith_waterman(a = a, b = b, type = type, tokenizer = tokenizer,...)
                alignment
              }, ..., SIMPLIFY = FALSE, USE.NAMES = FALSE)
  x <- lapply(x, FUN = FUN)
  x <- mapply(x, a = combinations$doc_id.a, b = combinations$doc_id.b, 
              FUN = function(x, a, b, ...) {
                x$a_doc_id <- a
                x$b_doc_id <- b
                x
              }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  x
}
