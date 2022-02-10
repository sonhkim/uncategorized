rm(list = ls()) ### clear all variables

projectpath <- "/Users/songkim/GoogleDrive/Primary/Projects/Spendl/Results"
setwd(projectpath)

library(grid)
library(gridExtra)
library(ggplot2)
library(magick)

### functions
make_annot <- function(file) {
  tokens <- strsplit(file, "_")
  if (length(tokens[[1]]) < 6 || length(tokens[[1]]) > 7){
    stop('check file name.')
  }
  label <- paste(as.numeric(tokens[[1]][4])*1000, '-', as.numeric(tokens[[1]][5])*1000, "ms", sep="")
  img <- image_read(file)
  img_new <- image_annotate(img, label, size = 20, color = "white", location = "+10+10")
  return (img_new)
}

make_gridplot <- function(freq, contrast, view, draw=draw) {
  loc <- file.path(projectpath, 'TF', freq, contrast, fsep = "/")
  setwd(loc)
  allfiles <- list.files()
  files <- grep(paste0(freq, '_', contrast, '\\S*', view, '\\.tif'), allfiles, value=TRUE)
  if (length(files)!=8){stop('The # of files are not 8.')}
  annotated_list <- purrr::map(files, make_annot)
  plot <- arrangeGrob(rasterGrob(annotated_list[[1]]), rasterGrob(annotated_list[[2]]), rasterGrob(annotated_list[[3]]), rasterGrob(annotated_list[[4]]), rasterGrob(annotated_list[[5]]), rasterGrob(annotated_list[[6]]), rasterGrob(annotated_list[[7]]), rasterGrob(annotated_list[[8]]), ncol=4, nrow=2)
  if (draw==TRUE){
    grid.arrange(rasterGrob(annotated_list[[1]]), rasterGrob(annotated_list[[2]]), rasterGrob(annotated_list[[3]]), rasterGrob(annotated_list[[4]]), rasterGrob(annotated_list[[5]]), rasterGrob(annotated_list[[6]]), rasterGrob(annotated_list[[7]]), rasterGrob(annotated_list[[8]]), ncol=4, nrow=2)
  }
  outname <- paste0("Grid_", freq, "_", contrast, "_", view, ".png")
  ggsave(file=outname, plot, width = 20, height = 10)
  print(paste("Plot is saved as", outname))
  return (plot)
}


  
### get the variables
freq <- readline(prompt="Enter the frequency (alpha, beta, theta, gamma):")
d <- list.dirs(paste("./TF/", freq, sep=""), full.names = FALSE)
print(d)
idx <- readline(prompt="Choose the contrast:")
idx <- as.integer(idx)
contrast <- d[idx]

views <- c("left", "right", "bottom", "top", "left_intern", "right_intern")
# df <- data.frame("views"=views)
# print(df)
# vw <- readline(prompt="Choose the view:")
# vw <- df[vw, ]
# print(paste(contrast, "for", vw, "view is chosen."))


#execute

draw <- as.numeric(readline(prompt="Display plot? (0 for No, 1 for Yes):"))
# make_gridplot(freq, contrast, vw, draw)

for (v in views){
  make_gridplot(freq, contrast, v, draw)
}


print('End of Script=================================')


####previous attemps
# ff <- grep(paste0(freq, '_', contrast, '\\S*', view, '\\S*'),allfiles, value=TRUE)
# magick::image_read(ff) %>% image_annotate(img, label, size = 20, color = "white", location = "+10+10")
# 
# 
# #read them, annotate
# #make grid and save
# img1 <- image_read("beta_Anam_Expec_0.7_1_top.tif")
# img2 <- image_read("beta_Anam_Expec_0.8_1.1_top.tif")
# img3 <- image_read("beta_Anam_Expec_0.1_0.4_top.tif")
# img4 <- image_read("beta_Anam_Expec_0.2_0.5_top.tif")
# 
# image_annotate(frink, "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",
#                degrees = 60, location = "+50+100")
# 
# grid.arrange(rasterGrob(img1),rasterGrob(img2),rasterGrob(img3), rasterGrob(img4), nrow=2, ncol=2)
# 
# 
# 
# #method2
# # img1 <- readTIFF("beta_Anam_Expec_0.7_1_top.tif", native=TRUE)
# # img2 <- readTIFF("beta_Anam_Expec_0.8_1.1_top.tif", native=TRUE)
# # img3 <- readTIFF("beta_Anam_Expec_0.1_0.4_top.tif", native=TRUE)
# # img4 <- readTIFF("beta_Anam_Expec_0.2_0.5_top.tif", native=TRUE)
# # writeJPEG(img1, target = "test3.jpeg", quality = 1)
# # writeJPEG(img2, target = "test4.jpeg", quality = 1)
#   