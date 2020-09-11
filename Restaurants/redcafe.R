redcafe <- function(){
  url <- "https://restauracie.sme.sk/restauracia/red-cafe-1_2457-nove-mesto_2653/denne-menu"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo <- 
    jedlo %>% str_trim()
  return(c("Red Cafe",jedlo))
}
