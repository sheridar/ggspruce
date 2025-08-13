
#' @importFrom ggplot2 ggplot_add
#' @method ggplot_add spruce_theme
#' @export
ggplot_add.spruce_theme <- function(object, plot, object_name) {

  # Add the theme
  # * remove spruce_theme class so theme is added using normal ggplot_add method
  cls <- class(object)
  cls <- cls[cls != "spruce_theme"]

  class(object) <- cls

  plot <- plot + object

  # Mark the plot with a new class for print override
  class(plot) <- c("spruce", class(plot))

  plot
}

#' @export
spruce_theme <- function(...) {

  th <- ggplot2::theme(...)

  # Tag theme with a custom class for ggplot_add
  class(th) <- c("spruce_theme", class(th))
  
  th
}

#' @method print spruce
#' @export
print.spruce <- function(x, ...) {

  # Strip spruce class to allow for printing using default method
  plt <- x

  cls <- class(plt)
  cls <- cls[cls != "spruce"]

  class(plt) <- cls

  # Identify theme elements to modify
  # * set NULL device to prevent anything from being drawn during mock
  #   rendering of plot
  # * match params for each theme element
  th_spruce <- purrr::map_lgl(plt$theme, ~ "element_text_spruce" %in% class(.x))
  th_spruce <- plt$theme[th_spruce]

  th_nms <- names(th_spruce)
  th_nms <- th_nms[th_nms %in% names(grob_name_key)]

  el_params <- c("property", "padding", "overhang", "range")
  el_params <- purrr::map(th_spruce[th_nms], ~ .x[el_params])

  grob_re <- grob_name_key[th_nms]

  dev_sz <- grDevices::dev.size()

  pdf(NULL, width = dev_sz[1], height = dev_sz[2])

  for (el in names(grob_re)) {
    plt <- .adjust_theme(
      plt,
      element    = el,
      grob_regex = grob_re[[el]],
      params     = el_params[[el]] 
    )
  }

  invisible(grDevices::dev.off())

  print(plt)
}

.adjust_theme <- function(plt, element, grob_regex, params) {
  
  # Pull grobs to calculate overlap
  gtbl <- ggplot2::ggplotGrob(plt)
  
  grob_nms <- grep(grob_regex, gtbl$layout$name, value = TRUE)

  if (purrr::is_empty(grob_nms)) {
    cli::cli_abort("grob matching {grob_regex} not found")
  }
  
  ## SHOULD ADJUST RANGE AFTER EACH ITERATION TO REDUCE SEARCH AREA
  # for (nm in grob_nms) {
  #   .adjust_params(nm, gtbl, params)
  # }

  ## SHOULD SELECT PARAMS BASED ON DIFFERENCE WITH ORIGINAL VALUES
  ## BUT NEED TO PROVIDE ACCESS TO THE ORIGINAL VALUES HERE
  new_params <- purrr::map(grob_nms, ~ .adjust_params(.x, gtbl, params))
  new_params <- dplyr::bind_rows(new_params)
  
  .sort_param_fns <- list(
    size  = dplyr::row_number,
    angle = function(x) {
      dif <- purrr::map_dbl(x, ~ min(.x, 360 - .x))
      
      dplyr::row_number(dplyr::desc(dif))
    }
  )

  for (prop in params$property) {
    new_params <- dplyr::arrange(new_params, .sort_param_fns[[prop]](!!sym(prop)))
  }

  new_params$dist <- NULL

  new_params <- head(new_params, 1)

  # Set new theme
  new_th <- .lift(ggplot2::element_text)(new_params)
  new_th <- purrr::set_names(list(new_th), element)
  new_th <- do.call(theme, new_th)
  
  # IS THIS THE BEST WAY???
  # Remove element_text_spruce class from theme elements to allow for
  # merging
  el <- plt$theme[[element]]
  
  cls <- class(el)
  cls <- cls[cls != "element_text_spruce"]
  
  class(el) <- cls
  
  plt$theme[element] <- list(el) 
  
  # Merge new theme
  plt <- plt + new_th

  plt
}

## UGHH CHANGE PARAMS ARGUMENT NAMING IS CONFUSING!!
.adjust_params <- function(grob_name, gtable, params) {

  # Pull grob
  grob_idx <- which(gtable$layout$name == grob_name)
  grob     <- gtable$grobs[[grob_idx]]

  if (inherits(grob, "zeroGrob")) return(NULL)

  # If there is no viewpoint, determine dimensions of each layout cell
  # to calculate bounding box for the grob
  null_vp <- is.null(grob$vp)

  bbox <- NULL

  if (null_vp) {
    # Fill in dimensions for null cells
    grid::grid.newpage()
    grid::grid.draw(gtable)
    grid::grid.force()
    
    # Find null cells
    null_x <- which(grid::unitType(gtable$widths) == "null")
    null_y <- which(grid::unitType(gtable$heights) == "null")
  
    # Identify grob with absolute dimensions for the null cell
    # * use this grob to fill in the missing dimensions in the gTable
    null_grobs <- dplyr::rowwise(gtable$layout)

    null_grobs <- dplyr::mutate(
      null_grobs,
      null_x = any(null_x %in% l:r),
      null_y = any(null_y %in% t:b)
    )

    null_grobs <- dplyr::ungroup(null_grobs)
    
    null_grobs <- dplyr::mutate(
      null_grobs,
      idx = dplyr::row_number(),
      absolute = purrr::map_lgl(gtable$grobs, ~ inherits(.x, "absoluteGrob"))
    )
    
    null_grobs <- dplyr::filter(null_grobs, absolute & (null_x | null_y))
    
    # Calculate missing dimensions for null cells
    null_grobs <- dplyr::mutate(
      null_grobs,
      dim = purrr::map2_dbl(idx, null_x, ~ {
        gb <- gtable$grobs[[.x]]
        
        grid::seekViewport(gb$vp$name)
        
        if (.y) {
          grid::convertWidth(
            grid::grobWidth(gb),
            "inches", valueOnly = TRUE
          )
        } else {
          grid::convertHeight(
            grid::grobHeight(gb),
            "inches", valueOnly = TRUE
          )
        }
      }),
      dim_idx = ifelse(null_x, l, t)
    )

    null_grobs <- dplyr::select(null_grobs, -c(t, l, b, r, z, clip))
    
    grid::upViewport(3)
  
    # Add in missing dimensions for null cells
    null_dims <- dplyr::pull(null_grobs, dim, dim_idx)
  
    null_wd <- null_dims[null_grobs$null_x]
    null_ht <- null_dims[null_grobs$null_y]
  
    layout_wd <- grid::convertWidth(gtable$widths, "inches", valueOnly = TRUE)
    layout_wd[as.numeric(names(null_wd))] <- unname(null_wd)
  
    layout_ht <- grid::convertHeight(gtable$heights, "inches", valueOnly = TRUE)
    layout_ht[as.numeric(names(null_ht))] <- unname(null_ht)
  
    ## It would be good to check this, but need to account for
    #  small rounding differences
    # if (sum(layout_wd) != (dev.size()[1]) || sum(layout_ht) != dev.size()[2]) {
    #   cli::cli_abort("gtable dimensions incorrectly calculated")
    # }

    # Create viewport
    vp_cells <- gtable$layout[grob_idx, ]
  
    vp_wd <- sum(layout_wd[vp_cells$l:vp_cells$r])
    vp_ht <- sum(layout_ht[vp_cells$t:vp_cells$b])
  
    vp <- grid::viewport(
      width  = grid::unit(vp_wd, "inches"),
      height = grid::unit(vp_ht, "inches")
    )
    
    grob$vp <- vp
    
    bbox <- .create_ply(
      x = c(0, 0, vp_wd, vp_wd),
      y = c(0, vp_ht, vp_ht, 0)
    )

    gtable$grobs[[grob_idx]] <- grob
  }

  # Determine theme adjustment for each grob
  grid::grid.newpage()  ## WHY DO I NEED THIS???
  grid::grid.draw(gtable)
  grid::grid.force()

  grid::seekViewport(grob$vp$name)

  new_params <- .adjust_grob(grob, bbox = bbox, params)
  
  new_params
}

.adjust_grob <- function(grob, bbox, params) {

  # Calculate label dimensions
  lab_grob <- .find_text_grob(grob)

  gp   <- lab_grob$gp
  labs <- lab_grob$label

  x <- grid::convertX(lab_grob$x, "inches")
  y <- grid::convertY(lab_grob$y, "inches")
  
  # Calculate bounding box for grob
  # * do not calculate if overhang is FALSE
  if (!params$overhang) bbox <- NULL

  if (is.null(bbox) && params$overhang) {
    bb_ht <- grid::convertHeight(grid::grobHeight(grob), "inches", valueOnly = TRUE)
    bb_wd <- grid::convertWidth(grid::grobWidth(grob), "inches", valueOnly = TRUE)

    # bbox <- grid::grobCoords(lab_grob)[[1]]
    bbox <- .create_ply(
      x = c(0, 0, bb_wd, bb_wd),
      y = c(0, bb_ht, bb_ht, 0)
    )
  }

  # Automatically set ranges for each text property
  size  <- lab_grob$gp$fontsize
  angle <- lab_grob$rot
  hjust <- lab_grob$hjust
  vjust <- lab_grob$vjust

  params$range$size <- params$range$size %||% c(max(4, size / 10), size)
  
  if (is.null(params$range$angle)) {
    if (angle >= 0 && angle <= 90) {
      params$range$angle <- c(0, 90)
    
    } else if (angle >= 270 && angle <= 360) {
      params$range$angle <- list(c(270, 360))
      
    } else {
      params$range$angle <- list(c(0, 360))
    }
  }
  
  # Objective function
  # * optimize using the difference between the optimal text padding and
  #   actual distance between labels
  # * penalize negative values (overlapped area)
  padding <- grid::convertUnit(params$padding, "inches", valueOnly = TRUE)

  .get_obj_fn <- function(prop) {
    args <- list(
      labels  = labs,
      x       = x,
      y       = y,
      gp      = gp,
      bbox    = bbox,
      padding = padding,
      return_value = TRUE
    )
    
    function(value, size, angle, hjust, vjust) {
      args$size  <- size
      args$angle <- angle
      args$hjust <- hjust
      args$vjust <- vjust

      # If adjusting angle, automatically adjust hjust/vjust based on value
      # being checked
      if (identical(prop, "angle")) {
        just <- .get_just(value)
        
        args$hjust <- just[1]
        args$vjust <- just[2]
      }

      args[[prop]] <- value
      
      ovlp <- .lift(.check_overlap)(args)
      
      if (ovlp < 0) ovlp <- (ovlp - 1) * 1e6  # penalize
      
      abs(ovlp)
    }
  }
  
  # Optimize parameters
  fns <- purrr::map(
    purrr::set_names(params$property),
    ~ .get_obj_fn(.x)
  )

  res <- list(
    size = size, angle = angle,
    hjust = hjust, vjust = vjust,
    dist = Inf
  )

  for (prop in params$property) {
    optim <- coarse_fine_threshold_auto(
      fns[[prop]],
      lower             = params$range[[prop]][1],
      upper             = params$range[[prop]][2],
      threshold         = padding,
      decreasing        = !identical(prop, "angle"),
      target_coarse_pts = 10,

      size  = res$size,
      angle = res$angle,
      hjust = res$hjust,
      vjust = res$vjust
    )
    
    ## OLD APPROACH
    # optim <- stats::optimize(
    #   fns[[prop]], params$range[[prop]],
    #   size  = res$size,
    #   angle = res$angle,
    #   hjust = res$hjust,
    #   vjust = res$vjust
    # )
    
    # Only save optimized values if they are better than the previous ones
    if (optim$value < res$dist) {
      res[[prop]] <- optim$value
      res$dist    <- optim$dist
      
      # Update hjust/vjust based on optimal angle
      # * this is important since adjusted hjust/vjust values were used when
      #   checking for overlap
      if (identical(prop, "angle")) {
        just <- .get_just(optim$value)
        
        res$hjust <- just[1]
        res$vjust <- just[2]
      }
    }

    if (optim$dist <= padding) break()
  }

  res <- as.data.frame(res)

  res
}

.find_text_grob <- function(grob, depth = 4) {
  # grob$children[[1]]                         # plot.title
  # grob$children[[1]]                         # axis.title
  # grob$children[[2]]$grob[[2]]$children[[1]] # axis.text
  
  chld <- grob

  cls <- c("text", "gtable", "titleGrob", "gTree")

  for (i in seq_len(depth)) {
    # Strip text grobs are gTables
    if (inherits(chld, "gtable")) {
      chld <- purrr::keep(chld$grob, ~ inherits(.x, cls))

      .chk_grob(chld)

      chld <- chld[[1]]
    }

    if (is.null(chld$children)) break()
    
    chld <- purrr::keep(chld$children, ~ inherits(.x, cls))
    
    if (inherits(chld[[1]], "gtable")) {
      chld <- purrr::keep(chld[[1]]$grob, ~ inherits(.x, cls))
    }

    # Forced grobs will include gTrees
    chld <- purrr::map(chld, ~ {
      if (inherits(.x, "gTree")) {
        .x <- .x$children[[1]]

        if (inherits(.x, "zeroGrob")) {
          return(NULL)
        }
      }

      .x
    })

    chld <- purrr::discard(chld, is.null)

    # For each list of children there should only be 1 grob
    # match cls class
    .chk_grob(chld)

    chld <- chld[[1]]
  }

  .chk_grob(chld)

  chld
}

.chk_grob <- function(grob) {
  if (inherits(grob, "list") && length(grob) > 1) {
      cli::cli_abort("Multiple grob labels found")
  }

  if (is.null(grob)) {
    cli::cli_abort("Grob label not found")
  }
}

.get_just <- function(angle) {
  key <- tibble::tribble(
    ~ agl, ~ hjust, ~ vjust,
    0,       0.5,     1,
    89,      1,       1,
    135,     1,       0.5,
    180,     0.5,     0,
    315,     0,       1,
    360,     0.5,     1
  )
  
  just <- dplyr::filter(key, agl >= angle)
  just <- head(just, 1)
  
  res <- c(just$hjust, just$vjust)
  res
}

#' Check for overlap between labels
#' 
#' @param labels character vector of labels
#' @param x,y vector of x and y coordinates for labels
#' @param ... arguments to pass to .calc_overlap
#' @param return_value if TRUE return the max overlap between the provided
#'   labels, if FALSE return TRUE/FALSE
#' @return max overlap between labels
.check_overlap <- function(labels, x, y, ..., return_value = TRUE) {

  max_ovlp <- Inf

  lab_idx <- seq_along(labels)[-1]

  if (length(labels) == 1) lab_idx <- 1
  
  for (i in lab_idx) {
    idx <- c(i - 1, i)
    
    ovlp <- .calc_overlap(
      labels = labels[idx],
      x      = x[idx],
      y      = y[idx],
      ...
    )

    overlap <- ovlp < 0
    
    if (!return_value && overlap) break()

    max_ovlp <- min(ovlp, max_ovlp)
  }
  
  if (!return_value) return(overlap)

  max_ovlp
}

#' Calculate overlap between pair of labels
#' 
#' @param labels character vector with pair of labels
#' @param x vector of x coordinates for each label
#' @param y vector of y coordinates for each label
#' @param gp graphical parameters for labels
#' @param size label text size to use for calculating overlap
#' @param angle label text angle to use for calculating overlap
#' @param hjust,vjust label text justification
#' @param bbox sf polygon for the grob bounding box,
#'   this is used for calcaulting overhang.
#'   If provided the greater of label overlap and overhang will be returned,
#'   if NULL only overlap will be considered
#' @param padding text padding to use when calculating overlap/overhang
#' @return overlap between pair of labels
.calc_overlap <- function(labels, x, y, gp, size, angle, hjust, vjust,
  bbox = NULL, padding = grid::unit(0, "pt")
) {
  
  if (length(labels) > 2) {
    cli::cli_abort("Can provide no more than two labels to compare")
  }

  if (length(labels) == 1 && is.null(bbox)) {
    cli::cli_abort("Must provide bbox when a single label is provided")
  }

  gp$fontsize <- size

  ovlp <- Inf

  # Create polygons for provided labels
  # * ideally padding would be incorporated when creating polygons,
  #   but this will result in unintended overhang
  plys <- purrr::imap(labels, ~ {
    create_poly(
      .x, x[.y], y[.y],
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      gp    = gp
    )
  })

  # We are iterating through the polygons two separate times
  # * this needs to be consolidated
  # * return negative value for overlap
  # * adjust overlap based on text padding
  if (length(labels) > 1) {
    ovlp <- sf::st_intersection(plys[[1]], plys[[2]])
    ovlp <- sf::st_area(ovlp)

    if (purrr::is_empty(ovlp)) {
      ovlp <- as.numeric(sf::st_distance(plys[[1]], plys[[2]]))
    } else {
      ovlp <- -ovlp
    }

    ovlp <- ovlp - padding
  }

  # Check if labels extend past grob boundaries
  # * need to extact boundaries to use st_distance
  if (!is.null(bbox)) {
    ovhg <- purrr::map_dbl(plys, ~ {
      dif <- sf::st_difference(.x, bbox)
      dif <- sf::st_area(dif)
      
      if (purrr::is_empty(dif)) {
        dif <- sf::st_distance(sf::st_boundary(.x), sf::st_boundary(bbox))
        dif <- as.numeric(dif)
      } else {
        dif <- -dif
      }

      if (hjust > 0 && hjust < 1 && vjust > 0 && vjust < 1) {
        dif <- dif - padding
      }

      dif
    })

    # Only use overhang when text extends past boundaries
    # * or if there is only a single label, e.g. plot.title
    # * when there are multiple labels the distance between labels is
    #   all that matters as long as there is no overhang
    if (any(ovhg < 0) || length(ovhg) == 1) {
      ovlp <- min(c(ovlp, ovhg))
    }
  }

  ovlp
}

# Grob maps for theme elements
grob_name_key <- list(
  "plot.title"          = "^title$",
  "plot.subtitle"       = "^subtitle$",
  "plot.caption"        = "^caption$",

  "axis.text"           = "^axis-[trbl](-[0-9-]+)?$",
  "axis.text.x"         = "^axis-b(-[0-9-]+)?$",
  "axis.text.x.bottom"  = "^axis-b(-[0-9-]+)?$",
  "axis.text.x.top"     = "^axis-t(-[0-9-]+)?$",
  "axis.text.y"         = "^axis-l(-[0-9-]+)?$",
  "axis.text.y.left"    = "^axis-l(-[0-9-]+)?$",
  "axis.text.y.right"   = "^axis-r(-[0-9-]+)?$",
  
  "axis.title"          = "^xlab-[trbl]$",
  "axis.title.x"        = "^xlab-b$",
  "axis.title.x.bottom" = "^xlab-b$",
  "axis.title.x.top"    = "^xlab-t$",
  "axis.title.y"        = "^ylab-l$",
  "axis.title.y.left"   = "^ylab-l$",
  "axis.title.y.right"  = "^ylab-r$",

  "strip.text"          = "^strip-[trbl](-[0-9-]+)?$",       # generic
  "strip.text.x"        = "^strip-t(-[0-9-]+)?$",            # all top/bottom
  "strip.text.x.top"    = "^strip-t(-[0-9-]+)?$",            # top strips
  "strip.text.x.bottom" = "^strip-b(-[0-9-]+)?$",            # bottom strips (rare, if strip.position = "bottom")
  "strip.text.y"        = "^strip-r(-[0-9-]+)?$",            # all left/right
  "strip.text.y.right"  = "^strip-r(-[0-9-]+)?$",            # right strips
  "strip.text.y.left"   = "^strip-l(-[0-9-]+)?$"             # left strips (rare, if strip.position = "left")
)

create_poly <- function(label, x, y, hjust, vjust, angle, gp) {
  dims <- calc_grob_dims(
    label, x, y,
    angle = 0,
    hjust = hjust,
    vjust = vjust,
    gp    = gp
  )

  ply <- .create_ply(
    x = as.numeric(dims[c("x1", "x1", "x2", "x2", "x1")]),
    y = as.numeric(dims[c("y1", "y2", "y2", "y1", "y1")])
  )

  ply <- rotate_poly(ply, angle, c(dims$x, dims$y))
  
  ply
}

.create_ply <- function(x, y) {
  x <- round(x, 3)
  y <- round(y, 3)

  mtx <- matrix(
    c(x, x[1], y, y[1]),
    byrow = FALSE,
    ncol = 2
  )
  
  ply <- sf::st_sfc(sf::st_polygon(list(mtx)))
  
  ply
}

rotate_poly <- function(polygon, angle, center) {
  
  angle <- -angle * (pi / 180)
  
  coords <- sf::st_coordinates(polygon[[1]])
  coords <- coords[, 1:2] - matrix(
    rep(center, nrow(coords)),
    ncol = 2,
    byrow = TRUE
  )
  
  # Translate coordinates for rotation center
  angl <- pi / 4
  
  rot_mat <- matrix(
    c(
      cos(angle), -sin(angle),
      sin(angle), cos(angle)
    ),
    ncol = 2
  )
  
  rot_coords <- t(rot_mat %*% t(coords))
  rot_coords <- rot_coords + matrix(rep(center, nrow(coords)), ncol = 2, byrow = TRUE)
  
  res <- sf::st_sfc(sf::st_polygon(list(rot_coords)))
  
  res
}

#' calc grob dimensions
#' @importFrom grid textGrob convertHeight convertWidth
calc_grob_dims <- function(label, x, y, hjust, vjust, angle,
                           gp = grid::gpar(), units = "inches"
                          ) {

  if (is.null(label)) return(zeroGrob())

  # We rotate the justifiation values to obtain the correct x and y reference point,
  # since hjust and vjust are applied relative to the rotated text frame in textGrob
  just <- .rotate_just(angle, hjust, vjust)

  n <- max(length(x), length(y), 1)

  x <- x %||% ggplot2::unit(rep(just$hjust, n), "npc")
  y <- y %||% ggplot2::unit(rep(just$vjust, n), "npc")

  text_grob <- titleGrob(
    label, x, y,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    gp    = gp
  )

  # The grob dimensions don't include the text descenders, so these need to be added
  # manually. Because descentDetails calculates the actual descenders of the specific
  # text label, which depends on the label content, we replace the label with one that
  # has the common letters with descenders. This guarantees that the grob always has
  # the same height regardless of whether the text actually contains letters with
  # descenders or not. The same happens automatically with ascenders already.
  descent <- font_descent(gp$fontfamily, gp$fontface, gp$fontsize, gp$cex)

  # Use trigonometry to calculate grobheight and width for rotated grobs. This is only
  # exactly correct when vjust = 1. We need to take the absolute value so we don't make
  # the grob smaller when it's flipped over.

  # Convert units and apply buffer space
  text_height <- ggplot2::unit(1, "grobheight", text_grob) + abs(cos(angle[1] / 180 * pi)) * descent
  text_width  <- ggplot2::unit(1, "grobwidth", text_grob) + abs(sin(angle[1] / 180 * pi)) * descent

  text_height <- grid::convertHeight(text_height, units, valueOnly = TRUE)
  text_width  <- grid::convertWidth(text_width, units, valueOnly = TRUE)

  # ## THE SIZE OF THE GROB IS NOT CORRECT ##
  # text_height <- text_height * SIZE_MTPLYR
  # text_width  <- text_width  * SIZE_MTPLYR

  xadj_1 <- hjust
  xadj_2 <- 1 - xadj_1
  yadj_1 <- vjust
  yadj_2 <- 1 - yadj_1


  x <- grid::convertX(x, units, valueOnly = TRUE)
  y <- grid::convertY(y, units, valueOnly = TRUE)

  res <- data.frame(
    x  = x,  # center x
    y  = y,  # center y
    x1 = x - (text_width  * xadj_1),
    x2 = x + (text_width  * xadj_2),
    y1 = y - (text_height * yadj_1),
    y2 = y + (text_height * yadj_2)
  )

  res
}

#' Rotate justification parameters counter-clockwise
#'
#' @param angle angle of rotation, in degrees
#' @param hjust horizontal justification
#' @param vjust vertical justification
#' @return A list with two components, `hjust` and `vjust`, containing the rotated hjust and vjust values
#'
#' @noRd
.rotate_just <- function(angle, hjust, vjust) {
  ## Ideally we would like to do something like the following commented-out lines,
  ## but it currently yields unexpected results for angles other than 0, 90, 180, 270.
  ## Problems arise in particular in cases where the horizontal and the vertical
  ## alignment model differ, for example, where horizontal alignment is relative to a
  ## point but vertical alignment is relative to an interval. This case arises for
  ## x and y axis tick labels.
  ##
  ## For more details, see: https://github.com/tidyverse/ggplot2/issues/2653

  # # convert angle to radians
  #rad <- (angle %||% 0) * pi / 180
  #
  #hnew <- cos(rad) * hjust - sin(rad) * vjust + (1 - cos(rad) + sin(rad)) / 2
  #vnew <- sin(rad) * hjust + cos(rad) * vjust + (1 - cos(rad) - sin(rad)) / 2

  angle <- (angle %||% 0) %% 360

  if (0 <= angle & angle < 90) {
    hnew <- hjust
    vnew <- vjust

  } else if (90 <= angle & angle < 180) {
    hnew <- 1 - vjust
    vnew <- hjust

  } else if (180 <= angle & angle < 270) {
    hnew <- 1 - hjust
    vnew <- 1 - vjust

  } else if (270 <= angle & angle < 360) {
    hnew <- vjust
    vnew <- 1 - hjust
  }

  list(hjust = hnew, vjust = vnew)
}

#' Text grob, height, and width
#'
#' This function returns a list containing a text grob (and, optionally,
#' debugging grobs) and the height and width of the text grob.
#'
#' @param label Either `NULL`, a string (length 1 character vector), or
#'   an expression.
#' @param x,y x and y locations where the text is to be placed. If `x` and `y`
#'   are `NULL`, `hjust` and `vjust` are used to determine the location.
#' @inheritParams titleGrob
#' @importFrom grid gList
#' @noRd
title_spec <- function(label, x, y, hjust, vjust, angle, gp = grid::gpar(),
                       debug = FALSE, check.overlap = FALSE
                      ) {

  if (is.null(label)) return(zeroGrob())

  # We rotate the justifiation values to obtain the correct x and y reference point,
  # since hjust and vjust are applied relative to the rotated text frame in textGrob
  just <- .rotate_just(angle, hjust, vjust)

  n <- max(length(x), length(y), 1)
  x <- x %||% ggplot2::unit(rep(just$hjust, n), "npc")
  y <- y %||% ggplot2::unit(rep(just$vjust, n), "npc")

  text_grob <- grid::textGrob(
    label,
    x,
    y,
    hjust = hjust,
    vjust = vjust,
    rot   = angle,
    gp    = gp,
    check.overlap = check.overlap
  )

  # The grob dimensions don't include the text descenders, so these need to be added
  # manually. Because descentDetails calculates the actual descenders of the specific
  # text label, which depends on the label content, we replace the label with one that
  # has the common letters with descenders. This guarantees that the grob always has
  # the same height regardless of whether the text actually contains letters with
  # descenders or not. The same happens automatically with ascenders already.
  descent <- font_descent(gp$fontfamily, gp$fontface, gp$fontsize, gp$cex)

  # Use trigonometry to calculate grobheight and width for rotated grobs. This is only
  # exactly correct when vjust = 1. We need to take the absolute value so we don't make
  # the grob smaller when it's flipped over.
  text_height <- ggplot2::unit(1, "grobheight", text_grob) + abs(cos(angle[1] / 180 * pi)) * descent
  text_width <- ggplot2::unit(1, "grobwidth", text_grob) + abs(sin(angle[1] / 180 * pi)) * descent

  if (isTRUE(debug)) {
    children <- grid::gList(
      rectGrob(gp = grid::gpar(fill = "cornsilk", col = NA)),
      pointsGrob(x, y, pch = 20, gp = grid::gpar(col = "gold")),
      text_grob
    )
  } else {
    children <- grid::gList(text_grob)
  }

  list(
    text_grob   = children,
    text_height = text_height,
    text_width  = text_width
  )
}

#' Add margins
#'
#' Given a text grob, `add_margins()` adds margins around the grob in the
#' directions determined by `margin_x` and `margin_y`.
#'
#' @param grob A gList containing a grob, such as a text grob
#' @param height,width Usually the height and width of the text grob. Passed as
#'   separate arguments from the grob itself because in the special case of
#'   facet strip labels each set of strips should share the same height and
#'   width, even if the labels are of different length.
#' @inheritParams titleGrob
#' @importFrom grid unit.c viewport grid.layout gTree vpTree vpList
#'
#' @noRd
add_margins <- function(grob, height, width, margin = NULL,
                        gp = grid::gpar(), margin_x = FALSE, margin_y = FALSE
                       ) {

  if (is.null(margin)) {
    margin <- margin(0, 0, 0, 0)
  }

  if (margin_x && margin_y) {
    widths <- grid::unit.c(margin[4], width, margin[2])
    heights <- grid::unit.c(margin[1], height, margin[3])

    vp <- grid::viewport(
      layout = grid::grid.layout(3, 3, heights = heights, widths = widths),
      gp = gp
    )
    child_vp <- grid::viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (margin_x) {
    widths <- grid::unit.c(margin[4], width, margin[2])
    vp <- grid::viewport(layout = grid::grid.layout(1, 3, widths = widths), gp = gp)
    child_vp <- grid::viewport(layout.pos.col = 2)

    heights <- ggplot2::unit(1, "null")
  } else if (margin_y) {
    heights <- grid::unit.c(margin[1], height, margin[3])

    vp <- grid::viewport(layout = grid::grid.layout(3, 1, heights = heights), gp = gp)
    child_vp <- grid::viewport(layout.pos.row = 2)

    widths <- ggplot2::unit(1, "null")
  } else {
    widths <- width
    heights <- height
    return(
      grid::gTree(
        children = grob,
        widths = widths,
        heights = heights,
        cl = "titleGrob"
      )
    )
  }

  grid::gTree(
    children = grob,
    vp = grid::vpTree(vp, grid::vpList(child_vp)),
    widths = widths,
    heights = heights,
    cl = "titleGrob"
  )
}

#' Create a text grob with the proper location and margins
#'
#' `titleGrob()` is called when creating titles and labels for axes, legends,
#' and facet strips.
#'
#' @param label Text to place on the plot. These maybe axis titles, axis labels,
#'   facet strip titles, etc.
#' @param x,y x and y locations where the text is to be placed.
#' @param hjust,vjust Horizontal and vertical justification of the text.
#' @param angle Angle of rotation of the text.
#' @param gp Additional graphical parameters in a call to `gpar()`.
#' @param margin Margins around the text. See [margin()] for more
#'   details.
#' @param margin_x,margin_y Whether or not to add margins in the x/y direction.
#' @param debug If `TRUE`, aids visual debugging by drawing a solid
#'   rectangle behind the complete text area, and a point where each label
#'   is anchored.
#'
#' @noRd
titleGrob <- function(label, x, y, hjust, vjust, angle = 0, gp = grid::gpar(),
                      margin = NULL, margin_x = FALSE, margin_y = FALSE,
                      debug = FALSE, check.overlap = FALSE
                     ) {

  if (is.null(label)) return(zeroGrob())

  # Get text grob, text height, and text width
  grob_details <- title_spec(
    label,
    x     = x,
    y     = y,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    gp    = gp,
    debug = debug,
    check.overlap = check.overlap
  )

  add_margins(
    grob     = grob_details$text_grob,
    height   = grob_details$text_height,
    width    = grob_details$text_width,
    gp       = gp,
    margin   = margin,
    margin_x = margin_x,
    margin_y = margin_y
  )
}

