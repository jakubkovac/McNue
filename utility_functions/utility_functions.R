str_to_1up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

fix_rtvs_rows <- function(x){
  x <- x[!is.na(x)]
  x <- paste0(x, collapse = " ")
  x <- x %>% str_split("[1-9]") %>% unlist()
  return(x[str_detect(x,"[a-zA-Z]{4,}")])
}

str_remove_1gl <- function(x) {
  substr(x,1,1)[substr(x,1,1) == "l" | substr(x,1,1) == "g"] <- ""
  x
}

same_length <- function(x, out.length = 6){
  if(length(x) != out.length) x <- head(c(x, rep(NA, out.length)), out.length)
  return(x)
}

remove_g_l <- function(x){
  x <- 
    x %>% 
    str_remove_all(paste0(c("120g","150g","200g", "240g", "250ml", "250g", "70g",
                            "300g","400g","0,33l","0.33l", "0.30 l", "0.33 l", "0.20 l", "0,25l",
                            "0,20 l", "140g", "320 g", "360 g", "400 g", "50ml", "50 ml", "350 g", "5 g", "0 g","50 g",
                            "0,30 l", "120 g", "250 g", "180 g", "150 g" , "300 g", "130 g", "5g", "0g"), collapse = "|")) %>%
    str_replace_all(" l ","") %>% 
    str_replace_all(" g "," ") %>% 
    str_replace_all("NA","") %>%
    str_replace_all(" ks","") %>%
    str_replace_all("  "," ") %>%
    str_replace_all(" ml "," ") %>%
    str_trim()
  y <- 
    x %>% 
    str_sub(end = 2) %>% 
    str_remove_all("g ") %>% 
    str_remove_all("l ")
  x <- paste0(y,str_sub(x,3))  
  return(x)
}