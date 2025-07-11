# 1. Constructor for Circle
Circle <- function(radius) {
  if (!is.numeric(radius) || radius <= 0) {
    stop("radius must be a positive number")
  }
  obj <- list(radius = radius)
  class(obj) <- "Circle"
  obj
}

# 2. print method for Circle
print.Circle <- function(x, ...) {
  radius <- x$radius
  area   <- pi * radius^2
  cat("Circle object\n")
  cat(" • Radius:", radius, "\n")
  cat(" • Area:  ", round(area, 4), "\n")
  invisible(x)
}

# — Example usage —
c1 <- Circle(3)
print(c1)
# Or simply:
c1
# 1. Updated constructor for Circle, now with center (x, y)
Circle <- function(radius, x = 0, y = 0) {
  if (!is.numeric(radius) || radius <= 0) {
    stop("radius must be a positive number")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric coordinates")
  }
  obj <- list(radius = radius, x = x, y = y)
  class(obj) <- "Circle"
  obj
}

# 2. print method for the extended Circle
print.Circle <- function(circ, ...) {
  r <- circ$radius
  area <- pi * r^2
  cat("Circle object\n")
  cat(" • Center: (", circ$x, ",", circ$y, ")\n", sep = "")
  cat(" • Radius:", r, "\n")
  cat(" • Area:  ", round(area, 4), "\n")
  invisible(circ)
}

# 3. Define the custom operator %>% to test intersection
`%>%` <- function(c1, c2) {
  if (!(inherits(c1, "Circle") && inherits(c2, "Circle"))) {
    stop("%>% requires two Circle objects")
  }
  dx <- c2$x - c1$x
  dy <- c2$y - c1$y
  dist <- sqrt(dx^2 + dy^2)
  return(dist <= (c1$radius + c2$radius))
}

# — Example usage —
circle1 <- Circle(radius = 3, x = 0, y = 0)
circle2 <- Circle(radius = 4, x = 5, y = 0)

circle1            # prints center, radius, area
circle2

# test intersection
circle1 %>% circle2  # TRUE, since distance 5 ≤ 3+4
# 1. Define a generic circumference function
circumference <- function(x, ...) {
  UseMethod("circumference")
}

# 2. Implement the Circle method
circumference.Circle <- function(x, ...) {
  # x is a Circle object
  r <- x$radius
  2 * pi * r
}

# — Example usage —
my_circle <- Circle(radius = 5)
circumference(my_circle)  
#> [1] 31.41593
# 1. Define the S4 class
setClass(
  "Rectangle",
  slots = c(
    length = "numeric",
    width  = "numeric"
  ),
  prototype = list(
    length = 1,
    width  = 1
  ),
  validity = function(object) {
    if (object@length <= 0 || object@width <= 0) {
      return("length and width must be positive numbers")
    }
    TRUE
  }
)

# 2. Constructor function
Rectangle <- function(length, width) {
  new("Rectangle", length = length, width = width)
}

# 3. show() method
setMethod(
  "show",
  "Rectangle",
  function(object) {
    area <- object@length * object@width
    cat("Rectangle object\n")
    cat(" • Length:", object@length, "\n")
    cat(" • Width: ", object@width,  "\n")
    cat(" • Area:  ", area,          "\n")
    invisible(object)
  }
)

# — Example usage —
my_rectangle <- Rectangle(length = 4, width = 6)
show(my_rectangle)
# or simply:
my_rectangle

# 1. Define the generic infix operator %==%
setGeneric(
  "%==%",
  function(e1, e2) standardGeneric("%==%")
)

# 2. Define the method for two Rectangle objects
setMethod(
  "%==%",
  signature(e1 = "Rectangle", e2 = "Rectangle"),
  function(e1, e2) {
    area1 <- e1@length * e1@width
    area2 <- e2@length * e2@width
    return(area1 == area2)
  }
)

# — Example usage —
rectangle1 <- Rectangle(length = 4, width = 6)
rectangle2 <- Rectangle(length = 3, width = 8)

rectangle1 %==% rectangle2  # TRUE, since both have area 24
