# helper_load

library(stringr)

clean.record <- function(line, author){
  # Clean data files
  # INPUT: One raw line and th ename of the author
  # OUTPUT: List (author ID, paper ID, Coauthor List, Paper Title, Journal Title)
  
  # Remove unwanted characters
  char_notallowed <- "\\@#$%^&?" # characters to be removed
  line.str <- str_replace(line, char_notallowed, "")
  
  # Get author id
  line.str <- strsplit(line.str, "_")[[1]]
  author_id <- as.numeric(line.str[1])
  
  # Get paper id
  line.str <- line.str[2]
  paper_id <- strsplit(line.str, " ")[[1]][1]
  lin.str <- substring(line.str, nchar(paper_id)+1, nchar(line.str))
  paper_id <- as.numeric(paper_id)
  
  # Get coauthor list
  line.str <- strsplit(lin.str, "<>")[[1]]
  coauthor_list <- strsplit(lin.str[1], ";")[[1]]
  
  # Clean Coauhtor List
  for(j in 1:length(coauthor_list)){
    if(nchar(coauthor_list[j])>0){
      nam <- strsplit(coauthor_list[j], " ")[[1]]
      if(nchar(nam[1])>0){
        first.ini <- substring(nam[1], 1, 1)
      }else{
        first.ini <- substring(nam[2], 1, 1)
      }
    }
    last.name <- nam[length(nam)]
    nam.str <- paste(first.ini, last.name)
    coauthor_list[j] <- nam.str
  }
  
  # Remove Author from Coauthor list
  match_ind <- charmatch(author, coauthor_list, nomatch=-1)
  if(match_ind>0){
    coauthor_list <- coauthor_list[-match_ind]
  }
  
  coauthors <- paste(coauthor_list, collapse=" ")
  paper_title <- line.str[2]
  journal_name <- line.str[3]
  
  return(list(author.id = author_id, 
              paper.id = paper_id, 
              coauthors = coauthors, 
              paper = paper_title, 
              journal = journal_name))
}