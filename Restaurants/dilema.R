dilema <- function(){
  url_dilema <- "http://restaurant-dilema.sk/Images/denneMenu.png"
  download.file(url_dilema, destfile = "dilema.png", quiet=TRUE, mode = "wb")
  
  im <- magick::image_ocr(magick::image_read("dilema.png"),
                          language = "slk")
  
  unlink("dilema.png")

  return(c("Dilema",jedlo))
}
