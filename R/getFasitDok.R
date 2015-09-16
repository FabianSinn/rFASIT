#' Parse FASIT variable documentation
#' 
#' Parse FASIT variable documentation ("MinidokYYYY.lst") and save it as a database
#' 
#' @param file Path to file
#' @export

getFasitDokDf <- function(file) {
   rawdata <- read.table(file, sep="\n", stringsAsFactors=FALSE)
   
   rawtext <- readLines(con=file)
   
   rawraw <- paste(rawtext, collapse="\n")
   
   rawlist <- str_split(rawraw, "\n\\.\n")
   rawlist <- lapply(rawlist, str_trim)
   
   title <- rawlist[[1]][1]
   
   elements <- rawlist[[1]][2:length(rawlist[[1]])]
   elements_split <- lapply(str_split(elements, "\n"), str_trim)
   
   metatags <- c()
   
   rows <- sapply(elements_split, function(i) {
      
      #       datarow = list()
      #       
      #       datarow[["title"]] <- i[1]
      #       datarow[["description"]] <- i[2]
      
      rows_with_metatags <- str_detect(i[3:length(i)], "[A-ZÅÄÖ]: ")
      tag_names <- sapply(str_split(i[3:length(i)][rows_with_metatags], ": "), function(i) { return(i[1])})
      tags_data <- str_split(paste(i[3:length(i)], collapse="\n"), paste(paste0(tag_names, ": "),collapse="|"))[[1]]
      tags_data <- str_trim(tags_data[2:length(tags_data)])
      
      datarow <- c(i[1:2], tags_data)
      names(datarow) <- c("title", "description", tag_names)
      
      df_row <- data.frame(t(datarow), stringsAsFactors=FALSE)
      return(df_row)
   })
   
   df <- data.table(ldply(rows))
   df$ANALYS <- sapply(df$ANALYS, function(i) { if(!is.na(i)) return(paste0("DATABAS        ", i)) else return(i) } )
   
   return(df)
}


