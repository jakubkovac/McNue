veglife <- function(sme = TRUE){
  if(sme){
    url <- "https://restauracie.sme.sk/restauracia/veg-life-blumental_10024-stare-mesto_2949/denne-menu"
    #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html(url)
    jedlo <- raw %>%
      html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
    jedlo <- str_trim(jedlo)
    jedlo <- jedlo[nchar(jedlo) != 0]
    jedlo <- jedlo[2:6] %>% str_remove_all("Polievka")
    
  }else{
   url_veglife <- "http://www.veglife.sk/sk/menu-2/" 
  #download.file(url_veglife, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html(url_veglife)
  jedlo <-
    raw %>% 
    html_nodes("#tab-1 > div:nth-child(2) > p:nth-child(3) > span:nth-child(1)") %>% 
    html_children() %>% 
    html_text() %>%
    str_trim() %>%
    .[2:6]
  jedlo <- str_sub(jedlo,3) %>% str_trim()
  jedlo[1] <- str_sub(jedlo[1],9)
  jedlo
  jedlo <- str_replace_all(jedlo, "([\n\t])", "") 
  }
  
  return(c("Veglife",jedlo))
}
