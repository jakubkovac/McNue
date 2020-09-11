slovak_language_destroyer <- function(x){
  spec_chrs <- read_table("special_chars_sk.txt",col_names = F)[[1]]
  spec_chrs <- c(spec_chrs,str_to_lower(spec_chrs))
  r_spec_chrs <- c("a","a","c","d","e","e","i","l","l","n","o","o","r","s","t","u","y","z")
  r_spec_chrs <- c(str_to_upper(r_spec_chrs),r_spec_chrs)
  names(r_spec_chrs) <- spec_chrs #this creates a named vector that represents the mapping of letters
  x <- 
    x %>% 
    str_replace_all(r_spec_chrs)
  return(x)
}
