lenka <- function(){
  url_lenka <- "https://www.jedlalenka.sk/"
  download.file(url_lenka, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>% 
    html_nodes(".insert-page-30") %>%
    html_children() %>%
    html_text()
  #jedlo <- jedlo[2:6]
  jedlo <- str_split(jedlo,"\r") %>% unlist() %>% str_remove("\n")
  jedlo <- jedlo[2:6]
  # kde <- str_match(a,"^[[:upper:]]") %>% is.na() %>% `!` %>% which()
  # jedlo[1] <- paste(a[kde],collapse = "/")
  return(c("Jedla lenka",jedlo))
}
