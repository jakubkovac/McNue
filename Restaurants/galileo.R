galileo <- function(){
  url <- "https://restauracie.sme.sk/restauracia/galileo-restaurant_902-stare-mesto_2949/denne-menu"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
    html_text() %>%
    str_trim()
  jedlo <- jedlo[c(2,4)]
  jedlo <- c(jedlo[1], 
            str_trim(unlist(str_split(jedlo[2], "[1-4]\\."))))
  jedlo <- jedlo[nchar(jedlo)>0]

  return(c("Galileo",jedlo))
}
