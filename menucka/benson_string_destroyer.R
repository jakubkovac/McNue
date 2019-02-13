benson_string_destroyer <- function(x){
  x <- 
    x %>% 
    str_replace_all("gril Big Green Egg","") %>%
    str_replace_all("–"," ") %>%
    str_squish() %>%
    str_replace_all("[()/,-]", " ") %>%
    str_replace_all("[–]", " ") %>%
    str_squish() %>%
    str_replace_all("[12346789]","") %>%
    str_replace_all("V$", "") %>%
    str_trim() %>%
    str_replace_all("B$", "") %>%
    str_replace_all("menu grilovane","") %>%
    str_replace_all("0g","") %>%
    str_trim()
  kde <- 
    x %>%
    str_sub(-1) %>%
    iconv("", "ASCII", "byte") %>%
    str_match("<e2><80><93>") %>% 
    is.na() %>% 
    as.vector() %>%
    `!` %>% 
    which()
  
  if(length(kde) >0) x[kde] <- str_sub(x[kde],end = -2)
  return(x)
}
