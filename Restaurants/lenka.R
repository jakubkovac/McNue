lenka <- function(){
  url_lenka <- "https://www.jedlalenka.sk/"
  download.file(url_lenka, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>% 
    html_nodes(".insert-page-30") %>%
    html_children() %>%
    html_text()
  jedlo <- jedlo[2:9]
  a <- str_split(jedlo[1]," ") %>% unlist()
  kde <- str_match(a,"^[[:upper:]]") %>% is.na() %>% `!` %>% which()
  jedlo[1] <- paste(a[kde],collapse = "/")
  return(tibble(Jedla_lenka = jedlo))
}