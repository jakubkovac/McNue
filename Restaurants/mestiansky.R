mestiansky <- function(){
  url_mestiansky <- "https://restauracie.sme.sk/restauracia/bratislavsky-mestiansky-pivovar-drevena_3951-stare-mesto_2949/denne-menu"
  #download.file(url_mestiansky, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html(url_mestiansky)
  jedlo <- raw %>% 
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% 
    html_text()
  jedlo <- str_trim(jedlo)
  #jedlo <- jedlo[c(2,4,5)]
  # jedlo[1] <- 
  #   jedlo[1] %>% 
  #   str_extract("3l(.*)") %>%
  #   str_sub(start = 3) %>%
  #   str_trim()
  # jedlo[2:3] <-
  #   jedlo[2:3] %>%
  #   str_extract("g(.*)") %>%
  #   str_sub(start = 3) %>%
  #   str_trim()
  jedlo <- str_replace_all(jedlo, "g[\t]","")
  jedlo <- str_replace_all(jedlo, "l[\t]","")
  jedlo <- str_replace_all(jedlo, "([\n\t])", "")
  if(length(jedlo) >= 4) jedlo <- jedlo[-which(str_length(jedlo) == min(str_length(jedlo)))]
  jedlo <- str_replace_all(jedlo,"polievka","")
  return(c("Mestiansky piv.",jedlo))
}
