dilema <- function(){
  url_dilema <- "https://restauracie.sme.sk/restauracia/dilema-restaurant_731-stare-mesto_2949/denne-menu"
  download.file(url_dilema, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
    html_text() %>%
    str_trim()
  if(length(jedlo)==4) jedlo <- c(str_trim(unlist(str_split(jedlo[1], "Menu"))),jedlo[-1])
  jedlo <- str_replace_all(jedlo, "([\n\t])", "")
  jedlo <- str_replace_all(jedlo, "-", "")
  jedlo <- str_replace_all(jedlo, "\u00bd", "0.5")
  
  # remove 'menu' & 'ponuka' 
  jedlo <- str_trim(str_replace_all(jedlo, "Menu [0-9]", ""))
  jedlo <- jedlo[!str_detect(jedlo, "(Menu)|(PONUKA NA TENTO)")]
  
  # not sure if this is now relevant
  # jedlo[2:5] <- str_sub(jedlo[2:5], start = 7)
  
  jedlo <- str_trim(jedlo)

  return(c("Dilema",jedlo))
}
