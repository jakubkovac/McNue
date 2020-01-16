veda <- function(){
  url_veda <- "https://restauracie.sme.sk/restauracia/veda-vegetarian-vegan_10202-stare-mesto_2949/denne-menu"
  download.file(url_veda, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo <- str_trim(jedlo)
  jedlo <- jedlo[c(7,seq(12,18,by = 2))]
  jedlo <- jedlo %>% str_replace("VEGAN","") %>% str_replace("NOT","")
  jedlo <- jedlo %>% str_sub(start =2) %>% str_trim()
  jedlo <- jedlo %>% str_to_lower()
  
  return(c("Veda",jedlo))
}
