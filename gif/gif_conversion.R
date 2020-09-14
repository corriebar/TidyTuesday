library(pdftools)
library(magick)
# stolen and adapted from Cedric Scherer
## convert pdf's to png's
setwd(here::here("dev"))
pdfs <- list.files(here::here("dev"), pattern = "2020_.*pdf")
for(pdf in pdfs) {
  pdf_convert(pdf = here::here("dev", pdf), 
              format = "png", dpi = 150)
}

# convert svg to png
system("convert -density 300 -resize 1700x850 -gravity center -background black -extent 1700x850 waffle_plot-2020-09-0*.svg out-%03d.png")
## convert png's to gif
system("convert -delay 20 out*.png -delay 500 out-144.png -loop 0 CropYield.gif")