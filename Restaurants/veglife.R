veglife <- function(){
  url_veglife <- "http://www.veglife.sk/sk/" 
  download.file(url_veglife, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <-
    raw %>% 
    html_nodes(".pcs-content") %>% 
    html_children() %>% 
    html_text() %>%
    str_trim() %>%
    .[2:6]
  jedlo <- str_sub(jedlo,3) %>% str_trim()
  jedlo[1] <- str_sub(jedlo[1],9)
  jedlo
  jedlo <- str_replace_all(jedlo, "([\n\t])", "")
  return(c("Veglife",jedlo))
}
