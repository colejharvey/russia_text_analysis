###A script for loading pdfs, converting to png, and enhancing image quality



###Defining the function

image_preprocess_ch <- function(filename){
  pagenum <- as.numeric(pdf_info(filename)$pages)
  filename.base <- str_sub(filename, 7, -5)  #Removes Scans/ and .pdf
  for (i in 1:pagenum){
    
    pdf_convert(filename, format = "png", dpi=300, page = i,  filenames = "page.png")
    color.image <- readImage("page.png")
    bw.image <- channel(color.image,"gray")
    #filename.bw <- paste(filename, "bw", sep="_")
    #writeImage(bw.image, file = filename.bw)
    image1 <- image_read(bw.image)
    image_bearb1 <- image1 %>%
      #image_scale("x2000") %>%                        # rescale
      image_background("white", flatten = TRUE) %>%   # set background to white
      image_trim() %>%                                # Trim edges that are the background color from the image.
      image_reducenoise() %>%                               # Reduce noise in image using a noise peak elimination filter
      image_enhance() %>%                             # Enhance image (minimize noise)
      image_normalize() %>%                           # Normalize image (increase contrast by normalizing the pixel values to span the full range of color values).
      image_contrast(sharpen = 1) %>%                 # increase contrast
      image_deskew(threshold = 20)
    filename.pp <- paste(filename.base, "pp", i, sep="_")
    image_write(image_bearb1, paste("./Scans/Processed images/", filename.pp, ".png", sep=""))
    
  }
  print(paste(filename, "complete.", sep = ""))
}

###Run the function on a batch of pdfs


library(pdftools)
library(EBImage)
library(magick)
library(stringr)


filelist <- list.files("Scans", full.names = TRUE)

j <- 1

for (j in 1:length(filelist)){
  image_preprocess_ch(filelist[j])
  print(paste("File", j, "of", length(filelist), "completed.", sep=" "))
}