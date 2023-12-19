redcafe <- function(){
  url <- "https://restauracie.sme.sk/restauracia/red-cafe-1_2457-nove-mesto_2653/denne-menu"
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html(url)
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo <- 
    jedlo %>% str_trim() %>% str_replace_all("\t", " ") %>% 
    str_remove_all("Polievka") %>% 
    str_remove_all("Hlavné jedlá") %>% 
    str_remove_all("Hlavné jedlo")
  jedlo <- jedlo[nchar(jedlo)>0]
  return(c("Red Cafe",jedlo))
}
