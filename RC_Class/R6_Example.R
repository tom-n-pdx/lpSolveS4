#
# R6 Class Example
#
# from: R6 Vingette
#

library(R6)

Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    hair = NULL,
                    initialize = function(name = NA, hair = NA) {
                      self$name <- name
                      self$hair <- hair
                      self$greet()
                    },
                    set_hair = function(val) {
                      self$hair <- val
                    },
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  )
)

#
# Use new to instaniate
#
ann <- Person$new("Ann", "black")
#> Hello, my name is Ann.


ann
#> <Person>
#>   Public:
#>     clone: function (deep = FALSE)
#>     greet: function ()
#>     hair: black
#>     initialize: function (name = NA, hair = NA)
#>     name: Ann
#>     set_hair: function (val)

# The $new() method creates the object and calls the initialize() method, if it exists.

ann$hair

ann$hair <- "blue"

ann$greet()

ann$set_hair("red")
ann$hair

#
# Private Methods
#
Queue <- R6Class("Queue",
                public = list(
                  initialize = function(...) {
                    for (item in list(...)) {
                      self$add(item)
                    }
                  },
                  add = function(x) {
                    private$queue <- c(private$queue, list(x))
                    invisible(self)
                  },
                  remove = function() {
                    if (private$length() == 0) return(NULL)
                    # Can use private$queue for explicit access
                    head <- private$queue[[1]]
                    private$queue <- private$queue[-1]
                    head
                  }
                ),
                private = list(
                  queue = list(),
                  length = function() base::length(private$queue)
                )
)

q <- Queue$new(5, 6, "foo")

# Add and remove items
q$add("something")
q$add("another thing")
q$add(17)
q$remove()
#> [1] 5
q$remove()
#> [1] 6

#
# However, private members can’t be accessed directly:
#
q$queue
#> NULL
q$length()
#> Error: attempt to apply non-function

#
# add retuns  self invisabbly so that it's chainable
#
q$add(10)$add(11)$add(12)

# On the other hand, remove() returns the value removed, so it’s not chainable:
q$remove()
#> [1] "foo"
q$remove()
#> [1] "something"
q$remove()
#> [1] "another thing"
q$remove()
#> [1] 17

#
# Active Bindsings Allow Checking. Allways Public
#
Numbers <- R6Class("Numbers",
                   public = list(
                     x = 100
                   ),
                   active = list(
                     x2 = function(value) {
                       if (missing(value)) return(self$x * 2)
                       else self$x <- value/2
                     },
                     rand = function() rnorm(1)
                   )
)

n <- Numbers$new()
n$x
#> [1] 100


# When an active binding is accessed as if reading a value, it calls the function with value as a missing argument:
n$x2
#> [1] 200

#
# If the function takes no arguments, it’s not possible to use it with <-:

n$rand
#> [1] 0.2648
n$rand
#> [1] 2.171
n$rand <- 3
#> Error: unused argument (quote(3))




