# ITD Animator

This function allows the user to visualize the working operation of the local maxima finder tree detection algorithm by creating a beautiful GIF of the process.

The function works by taking an input CHM, performing local maxima tree detection with lidR [locate_trees](https://rdrr.io/cran/lidR/man/locate_trees.html "lidR info page on locate_trees"), and subsequently plotting the moving window as it proceeds through the CHM cells.

This work is based on the demonstration by [ptompalski](https://github.com/ptompalski) his original code can be viewed [here](https://gist.github.com/ptompalski/94904eca2e1628fb52010c2890431715).

# [**View the tutorial on how to use this script**](https://liamirwin.github.io/ITD_Animator/)

![](output/readme.gif)

## Animator Arguments:

**chm**: A Canopy Height Model (CHM) raster layer. ([SpatRaster](https://rdrr.io/cran/terra/man/rast.html))

**f**: A function or fixed number defining the moving window size, defaults to a variable window size based on CHM height ([Popescu and Wynne 2004](https://doi.org/10.14358/PERS.70.5.589)).

**dir_out**: Directory path where the output GIF and temporary frames will be stored.

**gif_out_name**: Name of the output GIF file, defaults to "ITD_animation.gif".

**buffer**: Buffer distance added around the animation frame, defaults to 2.5m.

**delay**: Delay in seconds between frames in the GIF, defaults to 0.05 (5ms).

**circular**: Logical, if TRUE, the moving window is circular, defaults to TRUE. Otherwise, window is square.

**subset**: Logical, if TRUE, only a subset of the CHM cells defined by `sub_range` is used for animation, defaults to TRUE.

**sub_range**: Range of indices to define the CHM subset for the animation, defaults to 450:1450.

**col_pal**: Color palette for the CHM visualization, defaults to `scale_fill_viridis_c()`.

**clear_frames**: Logical, if TRUE, temporary frame images are deleted after creating the GIF, defaults to FALSE.

**skip_cells**: Integer, number of CHM cells to skip when creating the animation, defaults to 1 (no skipping).

**ttop_size**: Size of the points representing tree tops, defaults to 4. (see [plot.sf](https://r-spatial.github.io/sf/reference/plot.html))

**ttop_shape**: Shape of the points representing tree tops, defaults to 4. (see [plot.sf](https://r-spatial.github.io/sf/reference/plot.html))

**ttop_col**: Color of the points representing tree tops, defaults to "black". (see [plot.sf](https://r-spatial.github.io/sf/reference/plot.html))

**hillshade**: Logical, if TRUE, a hillshade layer is created and used as the background, defaults to FALSE.
