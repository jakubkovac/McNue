edokin <- function(){
  url <- "https://www.edo-kin.sk/datap/pdf/67/Obedov%C3%A1%20ponuka/"
  if(file.exists("edo_kin.pdf")){
    f <- file.info("edo_kin.pdf")
    f <- as.Date(f$mtime)
    t <- Sys.Date()
    if(lubridate::isoweek(t) != lubridate::isoweek(f)){
      message("PDF menu is not from this week. Downloading Edo kin menu.")
      download.file(url, destfile = "edo_kin.pdf", quiet=TRUE, mode = "wb", method = "wininet")
    }else{
      message("PDF for Edo is fine")
    }
  }else{
    download.file(url, destfile = "edo_kin.pdf", quiet=TRUE, mode = "wb", method = "wininet")
  }

  
  im <- magick::image_ocr(magick::image_read_pdf("edo_kin.pdf"),
                          language = "slk")
  
  im <- str_split(im, "\n") %>% unlist()
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  dni <- c("Pondelok", "Utorok", "Streda", "Štvrtok", "Piatok", "Sobota", "Nedela")
  today <- format(Sys.Date(), "%A")
  den <- dni[which(today == days_of_the_week)]
  s <- which(str_detect(im, den))
  if(den == "Piatok"){
    e <- tail(which(str_detect(im, "^Krajiny")),1)
  }else{
    e <- which(str_detect(im, dni[which(today == days_of_the_week) + 1])) 
  }
  
  jedlo <- im[(s+1):(e-1)] %>% 
    str_remove_all("Obedová ponuka:") %>% 
    str_remove_all("Rezancová polievka") %>% 
    str_remove_all("^Sushi rolka") %>% 
    str_squish() %>% 
    str_remove_all("^Wok:")
  
  jedlo <- jedlo[nchar(jedlo) >0]
  
  return(c("Edo Kin",jedlo))
  
}
