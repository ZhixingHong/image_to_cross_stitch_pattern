library(imager) 
library(tidyverse) 
library(tidymodels) 
library(sp) 
library(scales)
library(broom)
library(cowplot)
# devtools::install_github("sharlagelfand/dmc") 
library(dmc)

source("change_resolution-1.R")


process_image <- function(image_file_name, k_list){
  
  # # process_image(image_file_name, k_list) computed the clusters based on the 
  # # desired cluster number for the input image. It gives a tibble of information
  # # derived from the k-means.
  # # Input:
  # #   - image_file_name: a PNG or JPEG image, or the directory where the image
  # # is stored. The image should not to be too high resolution, it should
  # # also not have a very noisy background. Otherwise, the quality of the
  # # cross-stitch will be reduced.
  # # 
  # # - k_list: The number of color wanted to get in the final cross-stitch image.
  # # Or in other words, it is the number of centres in the clustering.
  # # 
  # # Output:
  # #   - A list or tibble of information derived from the k_means.
  # # 
  # # In the tibble it includes:
  # #   ∗ the original output of the kclust calls,
  # # ∗ the tidied clusters, their associated RGB values and their nearest DMC
  # # thread colour information.
  # # 
  # # 
  # # 
  # # Example:
  # #   library(imager)
  # #   library(dplyr)
  # #   library(broom)
  # #   library(purrr)
  # #   library(dplyr)
  # #   #devtools::install_github("sharlagelfand/dmc")
  # #   library(dmc)
  # 
  # #   clusterings<- process_image( "~/Downloads/d5496755a.jpg", 5:6)
  
  image <- imager::load.image(image_file_name)
  
  tidy_dat <- as.data.frame(image, wide = "c") %>% rename(R = c.1, G = c.2, B = c.3)
  
  dat <- select(tidy_dat,c(-x,-y)) 
  
  kclusts <- 
    tibble(k = c(k_list)) %>% mutate(
      kclust = map(k, ~kmeans(x = dat , centers = .x, nstart=4)),
      glanced = map(kclust, glance))
  
  clusterings <-
    kclusts %>%
    unnest(cols = c(glanced))
  
  
  a <- ~augment(.x, tidy_dat) %>% rename(cluster = .cluster)
  
  clusterings <- clusterings %>% mutate(cluster_center = map(kclust, tidy), 
                         tidy_dat = map(kclust, a))
  
  
  for (c in k_list) {
    center <- clusterings[clusterings$k == c,]$cluster_center[[1]]
    
    center <- center %>% mutate(colour = rgb(R, G, B))%>% 
                         mutate(dmc = map(colour, ~dmc(.x)))
    
    clusterings[clusterings$k == c,]$cluster_center[[1]] <- center
  }
  
  return(clusterings)
  
}





scree_plot <- function(cluster_info){

  # # scree_plot(cluster_info) produces and plots a scree plot for the clusters
  # # of the image. 
  # # 
  # # Input:
  # #   - cluster_info: 
  # #       A list of information about the clustering.(The list input is 
  # #       restricted to the output of the process_image.)
  # # 
  # # 
  # # Output:
  # #       - The scree plot for the clustering information: y-axis is stands 
  # #       for variance, x-axis is for the number of the cluster. The detailed
  # #       variance is labeled above the point.
  # # 
  # # 
  # # Example:
  # #   library(ggplot2)
  # #   library(dplyr)
  # # 
  # #   scree_plot(cluster_info)
  
  ggplot(cluster_info, aes(k, tot.withinss)) +
    geom_line() +
    geom_point() +
    geom_text(aes(label=round(tot.withinss, digits = 3)),hjust=0, vjust=0) +
    xlim(min(info$k)-1, max(info$k)+1) +
    xlab("Number of Clusters k") + ylab("Within groups sum of squares") +
    labs(title = "Scree Plot")

}





color_strips <- function(cluster_info){
  
  # # color_strips(cluster_info) produces and plots different color strips
  # # based on the number of the clusters assigned to the image. The color 
  # # of each box demostrates the DMC colour of each cluster center.
  # # 
  # # Input:
  # #   - cluster_info: 
  # #     A list of information about the clustering.(The list input is 
  # #     restricted to the output of the process_image.)
  # # 
  # # 
  # # Output:
  # #     - A figure composed by several different color strips, based on the
  # #     number of the cluster centers. The color displayed in each strip is
  # #     the DMC colour of each cluster center. (If the original cluster 
  # #     center's color exists in DMC, the original color is displayed;
  # #     otherwise, the closest color in DMC is displayed.)
  # # 
  # # 
  # # Example:
  # #   library(ggplot2)
  # #   library(cowplot)
  # #   library(purrr)
  # #   
  # #   color_strips(cluster_info)

  square <- function(x, label_size) { 
    ggplot()  + 
      coord_fixed(xlim=c(0,1), ylim = c(0,1)) + theme_void() + 
      theme(plot.background = element_rect(fill = x)) + 
      geom_text(aes(0.5,0.5),label = x , size = label_size)
  }
  
  t <- tibble(colours = viridis::magma(2),
              squares = purrr::map(colours, ~ square(.x, 24/length(colours))))
  
  strips <- c()
  for (i in 1:nrow(cluster_info)){
    t <- tibble(colours = sapply(cluster_info$cluster_center[[i]]$dmc, "[[", 3),
                squares = purrr::map(colours, ~square(.x, 24/length(colours))))
    
    
    n_col = length(t$colours)
    
    rect_dat <- tibble(x1 = c(0:(n_col-1)), x2 = c(1:n_col), y1 = rep(0,n_col),
                       y2 =rep(1,n_col), colour = t$colours)
    
    plot <- rect_dat %>% ggplot() + coord_fixed() + 
      geom_rect(aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=colour), color="black") +
      geom_text(aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=colour), size=24/n_col) + 
      scale_fill_manual(values = rect_dat$colour)+ theme_void() + theme(legend.position = "none")

    strips[[i]] <- plot
  }
  
  
  c_strips <- plot_grid(plotlist = strips,
                        labels = paste0( "no. of clusters = ", c(cluster_info$k)), 
                        ncol = 1)
  
  title <- ggdraw() +
    draw_label("Colour of Each Cluster Centre In DMC", fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 7))
  
  plot_grid( title, c_strips, 
             ncol = 1, rel_heights = c(0.1, 1))
  
}




make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL){
  

  # # make_pattern(cluster_info, k, x_size, 
  # #              black_white = FALSE, background_colour = NULL) 
  # # is the final step to get the cross stitch. The function used the result
  # # from the process_image() as one part of the input to produce the cross-
  # #   stitch pattern with colored shapes.
  # # 
  # # Input:
  # #   - cluster_info: 
  # #     A list of information about the clustering.(The list input is 
  # #     restricted to the output of the process_image.)
  # # - k:
  # #     The chosen cluster size, selected by the user based on the information
  # #     given by scree_plot and color_strips.
  # # 
  # # - x_size:
  # #     The (approximate) total number of possible stitches in the horizontal 
  # #     direction.
  # # - black_white:
  # #     It is a logical input. With TRUE, the function will print the pattern 
  # #     in black and white, or colour when the input is set as FALSE. The default
  # #     setting is FALSE.
  # # - background_colour:
  # #     The colour of the background, which should not be stitched in the pattern. 
  # #     The default sets that there is not background color.
  # # 
  # # Output:
  # #   - The function will produce the final  cross-stitch pattern as desired. With
  # #   a legend indicating the color and shape for each box, the cross-stitch pattern
  # #   can be followed,  and easy complete. Change_resolution function is used in the
  # #   function acting as a helper function, to reduce the resolution of the image.
  # # 
  # # 
  # # Example:
  # #   library(tidyverse) 
  # #   library(tidymodels) 
  # #   library(sp) 
  # #   library(scales)
  # #   library(broom)
  # #   # devtools::install_github("sharlagelfand/dmc") 
  # #   library(dmc)
  # # 
  # # 
  # #   make_pattern(cluster_info, 6, 50)
  # # 
  # #   make_pattern(cluster_info, 6, 50, black_white = TRUE)
  
  
  dmc4cluster <- cluster_info[cluster_info$k == k,]$cluster_center[[1]]
  dmc4cluster$DMC <- sapply(dmc4cluster$dmc, "[[", 1)
  
  
  basic_info <- cluster_info[cluster_info$k == k,]$tidy_dat[[1]]
  low_resolution_info <- change_resolution(basic_info, x_size = x_size)
  
  n <- nrow(low_resolution_info)
  low_resolution_info$DMC <- c(NA)
  
  
  for (i in 1:n) {
    no4cluster <- low_resolution_info[i,]$cluster
    low_resolution_info[i,]$DMC <- dmc4cluster[dmc4cluster$cluster == no4cluster,]$DMC
  }
  
  dmc_info <- dmc4cluster$dmc
  
  dmc_combine <- tibble(DMC = sapply(dmc_info, "[[", 1),
                        col = sapply(dmc_info, "[[", 3),
                        name = sapply(dmc_info, "[[", 2))
  
  if(!is.null(background_colour)){
    dmc_combine <- dmc_combine %>% mutate(col = ifelse(col == background_colour,
                                                       NA, col))
  }
  
  shape <- factor(low_resolution_info$DMC)
  colour <- factor(low_resolution_info$DMC)
  
  if (black_white){
    final <- low_resolution_info %>% ggplot()+
      geom_point(aes(x,y, shape = shape))+
      scale_color_manual(values = dmc_combine %>% dplyr::select(DMC, col) %>% deframe,
                         label = dmc_combine %>% dplyr::select(DMC, name) %>% deframe) +
      scale_y_reverse()
  }else{
    final <- low_resolution_info %>% ggplot()+
      geom_point(aes(x, y, col = colour, shape = shape))+
      scale_color_manual(values = dmc_combine %>% dplyr::select(DMC, col) %>% deframe,
                         label = dmc_combine %>% dplyr::select(DMC, name) %>% deframe) +
      scale_y_reverse()
  }
  
  final
}












