benson_string_destroyer <- function(x){
  x <- 
    x %>% 
    str_replace_all("[^[:alnum:]]", " ") %>%
    str_replace_all("gril Big Green Egg","") %>%
    str_replace_all("–"," ") %>%
    str_squish() %>%
    str_replace_all("[()/,-]", " ") %>%
    str_replace_all("[–]", " ") %>%
    str_squish() %>%
    str_replace_all("[0123456789]","") %>%
    str_replace_all("V$", "") %>%
    str_trim() %>%
    str_replace_all("B$", "") %>%
    str_trim() %>%
    str_replace_all("0$", "") %>%
    str_trim() %>%
    str_replace_all("menu grilovane","") %>%
    str_replace_all("0g","") %>%
    str_trim()

  return(x)
}
