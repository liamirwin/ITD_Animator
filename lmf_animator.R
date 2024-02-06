#' Create Animation of Tree Detection Using Local Maxima Filter
#'
#' Generates an animation showing the process of detecting trees in a Canopy Height Model (CHM)
#' using a local maxima filter. The animation illustrates the moving window technique and
#' highlights detected tree tops. The function saves the animation as a GIF file.
#'
#' @param chm A Canopy Height Model (CHM) raster layer.
#' @param f A function or fixed number defining the moving window size, defaults to a variable window size based on CHM height (Popescu and Wynne 2004)
#' @param dir_out Directory path where the output GIF and temporary frames will be stored.
#' @param gif_out_name Name of the output GIF file, defaults to "ITD_animation.gif".
#' @param buffer Buffer distance added around the animation frame, defaults to 2.5.
#' @param delay Delay in seconds between frames in the GIF, defaults to 0.05.
#' @param circular Logical, if TRUE, the moving window is circular, defaults to TRUE.
#' @param subset Logical, if TRUE, only a subset of the CHM defined by `sub_range` is used for animation, defaults to TRUE.
#' @param sub_range Range of indices to define the CHM subset for the animation, defaults to 450:1450.
#' @param col_pal Color palette for the CHM visualization, defaults to `scale_fill_viridis_c()`.
#' @param clear_frames Logical, if TRUE, temporary frame images are deleted after creating the GIF, defaults to FALSE.
#' @param skip_cells Integer, number of CHM cells to skip when creating the animation, defaults to 1 (no skipping).
#' @param ttop_size Size of the points representing tree tops, defaults to 4.
#' @param ttop_shape Shape of the points representing tree tops, defaults to 4.
#' @param ttop_col Color of the points representing tree tops, defaults to "black".
#' @param hillshade Logical, if TRUE, a hillshade layer is created and used as the background, defaults to FALSE.
#'
#'
#'
#' @return NULL. Function saves gif and frames locally
#' @importFrom tictoc tic toc
#' @importFrom gifski gifski
#' @importFrom terra extract
#' @importFrom lidR locate_trees lmf
#' @importFrom ggplot2 ggplot geom_raster coord_fixed theme_bw theme geom_point geom_rect
#' @importFrom dplyr mutate select left_join row_number
#' @importFrom ggforce geom_circle
#' @export
#' @examples
#' library(lidR)
#' library(dplyr)
#' library(terra)
#' library(glue)
#' library(gifski)
#' library(tictoc)
#' library(ggplot2)
#' # Using the built-in lidR dataset for this example
#' LASfile <- system.file("extdata", "MixedConifer.laz", package="lidR")
#' las <- readLAS(LASfile, filter = "-inside 481280 3812940 481330 3812990")
#' chm <- rasterize_canopy(las, res = 0.5, algorithm = pitfree(c(0,2,5,10,15), c(0, 1.5)))
#'
#' # Ensure the output directory exists or adjust 'dir_out' to an existing directory
#' lmf_animation(chm, dir_out = "path/to/output/directory",
#'               gif_out_name = "example_sub.gif",
#'               delay = 0.05,
#'               subset = TRUE,
#'               sub_range = 450:500)
#' @references
#' Popescu, S. C., & Wynne, R. H. (2004). Seeing the trees in the forest.
#' Photogrammetric Engineering & Remote Sensing, 70(5), 589-604.
#'
#' Tompalski, P. (n.d.). lmf_animation Function for Tree Detection Visualization.
#' Retrieved from \url{https://gist.github.com/ptompalski/94904eca2e1628fb52010c2890431715}.
lmf_animator <- function(chm, f = function(x) { x * 0.07 + 3},
                          dir_out,
                          gif_out_name = "ITD_animation.gif",
                          buffer = 2.5,
                          delay = 0.05,
                          circular = T,
                          subset_chm = T,
                          sub_range = 450:1450,
                          col_pal = scale_fill_viridis_c(),
                          clear_frames = F,
                          skip_cells = 1,
                          ttop_size = 4,
                          ttop_shape = 4,
                          ttop_col = "black",
                          hillshade = F) {

  #start timer
  tictoc::tic()

  #check if gifski is installed
  if(!requireNamespace("gifski", quietly = TRUE)) {
    stop("gifski package is required for creating gifs. Please install it.")
  }

  #temp dir will be a subfolder of the main dir
  dir_temp_frames <- file.path(dir_out, "frames_temp")

  #run tree detection
  print(glue::glue("Finding local maxima across {terra::ncell(chm)} pixels in chm"))

  #rename chm layer to Z
  names(chm)[1] <- "Z"

  ttops <- lidR::locate_trees(chm, lidR::lmf(ws = f))

  #if hillshade is true, create a hillshade layer and use as background
  if(hillshade) {
    print("Creating hillshade layer")
    slope <- terra::terrain(chm, v = "slope", unit = 'radians')
    aspect <- terra::terrain(chm, v = "aspect", unit = 'radians')
    hs <- terra::shade(slope, aspect, 45, 0, normalize = T)
    names(hs) <- "hs"
    chm$hs <- hs
  }

  #link treetops to chm pixels
  ttops_extract <- terra::extract(chm, ttops, xy=T, ID=T)

  #convert CHM to a data frame (to the plot using ggplot, easier than dealing with rasters)
  R <- as.data.frame(chm, xy=T)

  #calculate window size (mimicing what lidR::locate_trees does)
  if(is.function(f)) {
    R <- R %>% mutate(ws = f(Z)) #ws is in meters (same units as the data)
  } else {
    # if f isnt a function, use a fixed window size
    R <- R %>% mutate(ws = f) #ws is in meters (same units as the data)
  }

  #calculate window extent
  R <- R %>% mutate(ws_x_min = x - ws/2,
                    ws_x_max = x + ws/2,
                    ws_y_min = y - ws/2,
                    ws_y_max = y + ws/2)

  #calculate radius for circular window
  R <- R %>% mutate(radius = ws / 2)

  #if subset is true, create a second df to show tree detection on a subset of the CHM (entire data would take too long)
  if(subset_chm) {
    print(glue::glue("Creating subset of CHM for animation"))
    R1 <- R
    R1 <- R1 %>% mutate(id = row_number())
    R1 <- R1[sub_range,] #modify this range to change what is shown in the animation
  } else {
    R1 <- R
  }

  # copy x and y coords of treetops to two different fields (so that the treetops are separated from CHM pixel coords)
  ttops_extract <- ttops_extract %>% mutate(x_top = x, y_top = y)

  #link R1 with treetops
  R1 <- R1 %>% left_join(ttops_extract, by = join_by(x, y))

  #cleanup
  R1 <- R1 %>% select(-Z.y) %>% rename(Z = Z.x)

  #remove the temp dir if it exists and print message if it does
  if(dir.exists(dir_temp_frames)) {
    print(glue::glue("Removing existing temp dir {dir_temp_frames}"))
    unlink(dir_temp_frames, recursive = T)
  }
  #create the temp dir
  dir.create(dir_temp_frames, recursive = T)

  # Skip rows (only ones where ID is NA, merge together with ID rows in same order)

  if(skip_cells > 1) {
    print(glue::glue("Skipping every {skip_cells} cell of CHM for animation"))
    # Add row_id to R1
    R1 <- R1 %>% mutate(row_id = row_number())
    # Keep all cells where trees are detected for animation
    id_rows <- R1 %>% filter(!is.na(ID))
    # Keep every skip_cells cell where trees are not detected for animation
    non_id_rows <- R1 %>% filter(is.na(ID))
    non_id_rows <- non_id_rows[seq(1, nrow(non_id_rows), by = skip_cells),]
    # Merge together and order by original row_id
    R1 <- rbind(id_rows, non_id_rows) %>% arrange(row_id)

    if(hillshade){
      R1 <- R1 %>% select(-hs.y) %>%
        rename(hs = hs.x)
    }
  }

  print(glue::glue('Savings {nrow(R1)} frames...'))

  #initialize progress bar
  pb <- txtProgressBar(min = 0,
                       max = nrow(R1),
                       style = 3,
                       width = 50,
                       char = "=")



  if(hillshade){
    R <- R %>% dplyr::select(-Z) %>% dplyr::rename(Z = 'hs')
  }

  for(i in 1:nrow(R1)) {

    pix <- R1[i,]

    current_tops <- R1[1:i,]

    # Start by creating the base plot with universal components
    p <- ggplot(data=R, aes(x, y, fill=Z)) +
      geom_raster() +
      coord_fixed() +
      col_pal +
      geom_point(data=pix, aes(x, y), color="red") +
      theme_void() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none",
            plot.margin = unit(c(0, 0, 0, 0), "cm"))

    # Add the conditional layer for the moving window
    if(circular) {
      # check if ggforce package is installed
      if(!requireNamespace("ggforce", quietly = TRUE)) {
        stop("ggforce package is required for circular window. Please install it.")
      }
      p <- p + ggforce::geom_circle(data=pix, aes(x0=x, y0=y, r=radius), fill=NA, color="red")
    } else {
      p <- p + geom_rect(data=pix, aes(xmin = ws_x_min , xmax = ws_x_max , ymin = ws_y_min, ymax = ws_y_max),
                         fill=NA, color="red")
    }

    # Continue adding the rest of the components
    p <- p + geom_point(data=current_tops, aes(x=x_top, y=y_top), color=ttop_col, inherit.aes = FALSE, size=ttop_size, shape=ttop_shape) +
      xlim(min(R$x)-buffer, max(R$x)+buffer) +
      ylim(min(R$y)-buffer, max(R$y)+buffer)

    fout <- file.path(dir_temp_frames, glue("frame{sprintf('%04d', i)}.png"))

    ggsave(plot = p,
           filename = fout,
           width = 4, height = 3)

    #update progress bar
    setTxtProgressBar(pb, i)
  }

  #close progress bar
  close(pb)

  #make a gif
  png_files <- list.files(dir_temp_frames, full.names = T)


  gif_file <- file.path(dir_out, gif_out_name)
  gifski::gifski(png_files, gif_file, delay = delay)

  #if clear_frames is true, remove the temp dir
  if(clear_frames) {
    print(glue::glue("Deleting temp frame dir {dir_temp_frames}"))
    unlink(dir_temp_frames)
  }

  tictoc::toc()

}
