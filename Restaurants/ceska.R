ceska <- function(){
  url_ceska <- "https://restauracie.sme.sk/restauracia/ceska-pivnica-2_7243-stare-mesto_2949/denne-menu"
  #download.file(url_ceska, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html(url_ceska)
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  if(length(jedlo) > 5) jedlo <- jedlo[-1]
  jedlo <- str_trim(jedlo)
   jedlo[2:5] <- jedlo[2:5] %>% str_remove_all("[A-C][:]") %>% str_trim()
  # jedlo[5] <- jedlo[5] %>% str_sub(4) %>% str_trim() %>% str_sub(5)
  # jedlo[6] <- jedlo[6] %>% str_sub(3) %>% str_trim() %>% str_sub(4)
  return(c("Ceska piv",jedlo))
}
