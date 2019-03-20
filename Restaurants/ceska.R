ceska <- function(){
  url_ceska <- "https://restauracie.sme.sk/restauracia/ceska-pivnica-2_7243-stare-mesto_2949/denne-menu"
  download.file(url_ceska, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo[1:2] <- jedlo[1:2] %>% str_sub(2) %>% str_trim() %>% str_sub(6)
  jedlo[3:5] <- jedlo[3:5] %>% str_sub(4) %>% str_trim() %>% str_sub(8)
  jedlo[6] <- jedlo[6] %>% str_sub(3) %>% str_trim()
  return(c("Ceska pivn.",jedlo[-6]))
}