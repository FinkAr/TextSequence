dotplots <- function(file = NULL, sheet =1, output.file = NULL, gap.marker = "GP", sep = F, colors = c("#ffa667","#ffd966", "#00BFFF")){
  
  if(grepl(".xlsx",file)) df<- read.xlsx(file, sheet = sheet)else if(grepl(".csv",file)) df<- read.csv2(file) else stop("The input file has to be csv or xlsx.")
  if(!is.null(gap.marker)) df[df==gap.marker&!is.na(df)] <- paste0(df[df==gap.marker&!is.na(df)], 1:length(df[df==gap.marker&!is.na(df)])) 
  data <- as.list(df)
  wb <- openxlsx::createWorkbook(title = "Dotplots")
  pb <- txtProgressBar(min = 0, max =length(data)+1, style = 3)
  if(sep ==T){
    for (i in 1:length(data)) {
      setTxtProgressBar(pb, i)
      matched <- vector("list", length = (length(data)))
      names(matched) <- colnames(df)
      sheetname <- gsub( "\\*", "", stringr::str_trunc(names(data)[i],31))
      sheetname <- gsub( "\\/", "",sheetname)
      openxlsx::addWorksheet(wb,
                             sheetName = sheetname)
      for (q in (1:length(data))){
        a <- data[[i]]
        a <- a[a != ""]
        a <- a[!is.na(a)]
        b <- data[[q]]
        b <- b[b!=""]
        b <- b[!is.na(b)]
        match <- t(sapply(a, function(x) as.integer(x==b)))
        dimnames(match)=list(a,b)
        match[grepl(gap.marker, rownames(match)),] <- -1
        match[,grepl(gap.marker, colnames(match))] <- -1
        matched[[q]] <- match
        if(q==1){
          openxlsx::writeData(wb,
                              sheet = sheetname,
                              match,
                              startCol = 1,
                              startRow = 2,
                              rowNames = T)
          openxlsx::writeData(wb,
                              sheet = sheetname,
                              paste(names(data)[i],names(data)[q],sep="/"),
                              startRow = 1,
                              startCol = 1)
          
        }else{
          openxlsx::writeData(wb,
                              sheet = sheetname,
                              match,
                              startCol = sum(unlist(lapply(matched, ncol))[1:(q-1)])+q+1,
                              startRow = 2,
                              rowNames = F)
          openxlsx::writeData(wb,
                              sheet = sheetname,
                              paste(names(data)[i],names(data)[q],sep="/"),
                              startRow = 1,
                              startCol = sum(unlist(lapply(matched, ncol))[1:(q-1)])+q+1
                              
          )}
        
      }
      openxlsx::conditionalFormatting(wb,
                                      sheet =  sheetname,
                                      rows = 1:(nrow(match)+2),
                                      cols = 1:(sum(unlist(lapply(matched, ncol))[1:q])+q+1),
                                      style = colors,
                                      rule = c(-1, 0, 1),
                                      type = "colourScale"
      )
    }
   
  }else{
    openxlsx::addWorksheet(wb,
                           sheetName = "total")
    for (i in 1:length(data)) {
      setTxtProgressBar(pb, i)
      matched <- vector("list", length = (length(data)))
      names(matched) <- colnames(df)
      if(i==1)startRow = 1 else {
        startRow <- df[,1:(i-1)]
        startRow <- length(startRow[startRow!=""&!is.na(startRow)])+i+2
      }
      for (q in (1:length(data))){
        a <- data[[i]]
        a <- a[a != ""]
        a <- a[!is.na(a)]
        b <- data[[q]]
        b <- b[b!=""]
        b <- b[!is.na(b)]
        match <- t(sapply(a, function(x) as.integer(x==b)))
        dimnames(match)=list(a,b)
        match[grepl(gap.marker, rownames(match)),] <- -1
        match[,grepl(gap.marker, colnames(match))] <- -1
        matched[[q]] <- match
        
        if(q==1&i==1){
          openxlsx::writeData(wb,
                              sheet = "total",
                              match,
                              startCol = 2,
                              startRow = 2,
                              rowNames = T)
          openxlsx::writeData(wb,
                              sheet = "total",
                              paste(names(data)[i],names(data)[q],sep="/"),
                              startRow = 2,
                              startCol = 1)
          
        }else{
          rowNames <- F
          colNames <- F
          if(i==1){
            colNames = T
            startRowName <- 1
            startRow <- 2
          }else startRowName <- startRow-1
          if(q ==1){
            startCol <- 2
            startColName <- 1
            rowNames <- T
          }else startCol <- startColName <- sum(unlist(lapply(matched, ncol))[1:(q-1)])+q+2
          
          openxlsx::writeData(wb,
                              sheet = "total",
                              match,
                              startCol = startCol,
                              startRow = startRow,
                              rowNames = rowNames,
                              colNames = colNames)
          openxlsx::writeData(wb,
                              sheet = "total",
                              paste(names(data)[i],names(data)[q],sep="/"),
                              startRow = startRowName,
                              startCol = startColName
                              
          )}
        
      }
   
      
    }
    
    openxlsx::conditionalFormatting(wb,
                                    sheet =  "total",
                                    rows = 1:(sum(unlist(lapply(matched, ncol))[1:q])+q+1),
                                    cols = 1:(sum(unlist(lapply(matched, ncol))[1:q])+q+1),
                                    style = colors,
                                    rule = c(-1, 0, 1),
                                    type = "colourScale"
    )
  }
  
  if(is.null(output.file)) output.file <- "dotplots_results.xlsx"
  openxlsx::saveWorkbook(wb,
                         file = output.file,
                         overwrite = TRUE)
  setTxtProgressBar(pb, length(data)+1)
  close(pb)
  }
