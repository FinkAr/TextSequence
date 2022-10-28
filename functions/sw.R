sw <- function(file, output.file = NULL, gap.marker = "GP",rev = T, sec_order = F){ 
  print("Start smith-waterman analysis")
  if(grepl(".xlsx",file)) df<- read.xlsx(file)else if(grepl(".csv",file)) df<- read.csv2(file) else stop("The input file has to be csv or xlsx.")
  df <-as.data.frame(apply(df,2, function(x) gsub( " ", "", x)))
  if(!is.null(gap.marker)) df[df==gap.marker&!is.na(df)] <- paste0(df[df==gap.marker&!is.na(df)], 1:length(df[df==gap.marker&!is.na(df)])) 
 
  
  ###compare all sequences pairwise
  df_t <- as.matrix(t(df))
  colnames(df_t) <- NULL
  df_t[which(df_t=="")] <- NA
  q <- cbind(colnames(df),as.data.frame(apply(df_t, 1, function(x) cbind(names(x),paste(x[!is.na(x)], collapse = " ")))))
  colnames(q) <- c("doc_id", "text")
  p <- q
  alignments <- smith_waterman_compl(p, q, type="words")
  x <- lapply(alignments, FUN = function(a){
    x <- as.data.frame(a)
    x <- cbind(a$a_doc_id,a$b_doc_id, x[,c("sw", "similarity", "matches", 
                                           "mismatches", "a_n", "b_n",
                                           "a_aligned", "b_aligned",
                                           "a_gaps", "a_from", "a_to", "b_gaps",
                                           "b_from", "b_to")])
    x
  })
  x <- do.call(rbind, x)
  sw <- x
  if(rev ==T){
    print("Start reversed analysis")
    p_rev <- cbind(colnames(df),as.data.frame(apply(df_t, 1, function(x) cbind(names(x),paste(rev(x)[!is.na(rev(x))], collapse = " ")))))
    colnames(p_rev) <- c("doc_id", "text")
    alignments_rev <-  smith_waterman_compl(q, p_rev, type="words")
    y <- lapply(alignments_rev, FUN = function(b){
      y <- as.data.frame(b)
      y <- cbind(b$a_doc_id,b$b_doc_id, y[,c("sw", "similarity", "matches", 
                                             "mismatches", "a_n", "b_n",
                                             "a_aligned", "b_aligned",
                                             "a_gaps", "a_from", "a_to", "b_gaps",
                                             "b_from", "b_to")])
      y
    })
    y <- do.call(rbind, y)
    strReverse <- function(x)
      sapply(lapply(strsplit(x, " "), rev), paste, collapse = " ")
    
    y$b_aligned <- strReverse(y$b_aligned) 
    colnames(y) <- c("a_doc_id","b_doc_id","sw_inv", "similarity_inv", "matches_inv", 
                     "mismatches_inv", "a_n", "b_n",
                     "a_aligned_inv", "b_aligned_inv",
                     "a_gaps_rev", "a_from_inv", "a_to_inv", "b_gaps_inv",
                     "b_from_inv", "b_to_inv")
    y$b_from_inv <- y$b_n-y$b_from_inv + 1
    y$b_to_inv <- y$b_n-y$b_to_inv + 1
    sw <- cbind(x, y[,c("sw_inv", "similarity_inv", "matches_inv", 
                        "mismatches_inv", "a_aligned_inv", "b_aligned_inv",
                        "a_gaps_rev", "a_from_inv", "a_to_inv", "b_gaps_inv",
                        "b_from_inv", "b_to_inv")]
    )
  }
  if(is.null(output.file)) output.file <- "dotplots_results.xlsx"
  write.xlsx(sw,output.file)
  print(paste0("First order smith-waterman done. Results saved in ", output.file))
  ####second order sw
  if(sec_order==T){
    print("Start second order analysis")
    sw_first <- x$a_aligned
    comp_id <- apply(x, 1, function(x) paste(x[1:2], collapse = "-" ))
    sw_first <- as.data.frame(cbind(comp_id, sw_first))
    colnames(sw_first)<- c("doc_id", "text")
  
    for (i in 1:nrow(df_t)) {
    sw_first <- sw_first[-c((i*nrow(df_t)-nrow(df_t)+1):((i*nrow(df_t)-nrow(df_t))+i)),]
    }
    sw_first <- sw_first[!is.na(sw_first$text),]
    sw_first <- sw_first[unlist(lapply(strsplit(sw_first$text, " "), length))>1,]
    alignments_second <- smith_waterman_compl(sw_first, sw_first, type="words")
  
    sw_second <- lapply(alignments_second, FUN = function(a){
    sw_second <- as.data.frame(a)
    sw_second <- cbind(a$a_doc_id,a$b_doc_id, sw_second[,c("sw", "similarity", "matches", 
                                           "mismatches", "a_n", "b_n",
                                           "a_aligned", "b_aligned",
                                           "a_gaps", "a_from", "a_to", "b_gaps",
                                           "b_from", "b_to")])
    sw_second
  })
  sw_second <- do.call(rbind, sw_second)
  excl <- NULL
  for (i in 1:nrow(sw_first)) {
    excl_tmp <- c(((i-1)*nrow(sw_first)+1):(((i-1)*nrow(sw_first))+i))
    excl <- c(excl, excl_tmp)
  }
  sw_second <-sw_second[-excl,]
  sw_second <- sw_second[!is.na(sw_second$a_aligned),]
  sw_second <- sw_second[unlist(lapply(strsplit(sw_second$a_aligned, " "), length))>1,]
  
  write.xlsx(sw_second,"sec_order.xlsx")
  print(paste0("Second order smith-waterman done. Results saved in sw_second_order.xlsx"))
  
  }
}
