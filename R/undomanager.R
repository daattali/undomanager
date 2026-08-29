#' Undo/Redo manager
#'
#' @description
#' With the undo manager, you can manage the history of an object by
#' using undo and redo operations.
#'
#' @field value Get the value
#' @field can_undo Whether there are any undo operations available
#' @field can_redo Whether there are any redo operations available
#' @field undo_size Get the number of undo operations
#' @field redo_size Get the number of redo operations
#' @examples
#' TODO
#' @export
UndoManager <- R6::R6Class(
  "UndoManager",
  cloneable = TRUE,

  private = list(
    .type = NULL,
    .undo_stack = list(),
    .redo_stack = list(),
    .current = NULL,

    .rx_dep = NULL,
    .rx_expr = NULL,
    .rx_count = 0,
    .invalidate = function() {
      private$.rx_count <- private$.rx_count + 1
      private$.rx_dep(private$.rx_count)
      invisible()
    }
  ),

  active = list(

    value = function() {
      private$.current
    },

    undo_size = function() {
      length(private$.undo_stack)
    },

    redo_size = function() {
      length(private$.redo_stack)
    },

    can_undo = function() {
      self$undo_size > 0
    },

    can_redo = function() {
      self$redo_size > 0
    }

  ),

  public = list(

    #' @description
    #' TODO
    #' @param type The permitted classes of the objects (`NULL` to allow any object)
    #' @examples
    #' TODO
    #' @return TODO
    initialize = function(type = NULL) {
      if (!is.null(type) &&
          !checkmate::test_character(type, any.missing = FALSE, unique = TRUE,
                                     min.chars = 1, min.len = 1, names = "unnamed")) {
        stop("UndoManager: `type` must either be `NULL` or an unnamed vector of strings",
             call. = FALSE)
      }
      private$.type <- type
      private$.rx_dep <- function(x) NULL
      invisible(self)
    },

    reactive = function() {
      # Idea borrowed from Winston Chang
      # https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class
      if (is.null(private$.rx_expr)) {
        private$.rx_dep <- shiny::reactiveVal(0)
        private$.rx_expr <- shiny::reactive({
          private$.rx_dep()
          self
        })
      }
      private$.rx_expr
    },

    #' @description
    #' TODO
    #' @param type The class of the object (`NULL` to allow any object)
    #' @examples
    #' TODO
    print = function(...) {

      if (is.null(private$.current)) {
        cat("Empty ")
      }

      cat("<UndoManager>")
      if (is.null(private$.type)) {
        cat(" of arbitrary items")
      } else {
        cat0(" of items of type ", paste0("<", private$.type, ">", collapse = "|"))
      }

      if (is.null(private$.current)) {
        cat("\n")
      } else {
        cat0(" with ")
        cat0(self$undo_size, if(self$undo_size == 1) " undo" else " undos", " and ")
        cat0(self$redo_size, if(self$redo_size == 1) " redo" else " redos", "\n")

        cat("\n### Current item ###\n")
        print(private$.current)

        if (self$undo_size > 0) {
          cat("\n### Undo stack ###\n")
          for (idx in seq_len(self$undo_size)) {
            idx_rev <- (self$undo_size - idx + 1)
            cat0(idx, ".\n")
            print(private$.undo_stack[[idx_rev]])
            cat("\n")
          }
        }

        if (self$redo_size > 0) {
          cat("\n### Redo stack ###\n")
          for (idx in seq_len(self$redo_size)) {
            idx_rev <- (self$redo_size - idx + 1)
            cat0(idx, ".\n")
            print(private$.redo_stack[[idx_rev]])
            cat("\n")
          }
        }
      }

      invisible(self)
    },

    #' @description
    #' TODO
    #' @param n The number of undo operations to perform. If `n` is larger
    #' than the number of available undo operations, all of them are
    #' performed. Use `Inf` to undo the entire history.
    #' @examples
    #' TODO
    #' @return TODO
    undo = function(n = 1) {
      if (!checkmate::test_number(n, lower = 0, na.ok = FALSE) ||
          (is.finite(n) && n != floor(n))) {
        stop("undo: `n` must be a single non-negative whole number, or `Inf`",
             call. = FALSE)
      }

      n <- as.integer(min(n, self$undo_size))
      if (n < 1L) {
        return(invisible(self))
      }
      len <- length(private$.undo_stack)
      private$.redo_stack <- c(
        private$.redo_stack,
        list(private$.current),
        rev(private$.undo_stack[seq.int(to = len, length.out = n - 1L)])
      )
      private$.current <- private$.undo_stack[[len - n + 1L]]
      private$.undo_stack <- private$.undo_stack[seq_len(len - n)]

      private$.invalidate()

      invisible(self)
    },

    #' @description
    #' TODO
    #' @param n The number of redo operations to perform. If `n` is larger
    #' than the number of available redo operations, all of them are
    #' performed. Use `Inf` to redo the entire history.
    #' @examples
    #' TODO
    #' @return TODO
    redo = function(n = 1) {
      if (!checkmate::test_number(n, lower = 0, na.ok = FALSE) ||
          (is.finite(n) && n != floor(n))) {
        stop("redo: `n` must be a single non-negative whole number, or `Inf`",
             call. = FALSE)
      }

      n <- as.integer(min(n, self$redo_size))
      if (n < 1L) {
        return(invisible(self))
      }
      len <- length(private$.redo_stack)

      private$.undo_stack <- c(
        private$.undo_stack,
        list(private$.current),
        rev(private$.redo_stack[seq.int(to = len, length.out = n - 1L)])
      )
      private$.current <- private$.redo_stack[[len - n + 1L]]
      private$.redo_stack <- private$.redo_stack[seq_len(len - n)]

      private$.invalidate()

      invisible(self)
    },

    #' @description
    #' TODO
    #' @param type The class of the object (`NULL` to allow any object)
    #' @examples
    #' TODO
    #' @return TODO
    do = function(item) {
      if (is.null(item)) {
        stop("do: item must not be NULL", call. = FALSE)
      }
      if (!is.null(private$.type) && !inherits(item, private$.type)) {
        stop("do: The provided item must have class ",
             paste0("<", private$.type, ">", collapse = "|"),
             call. = FALSE)
      }

      if (is.null(private$.current)) {
        private$.undo_stack <- list()
      } else {
        private$.undo_stack <- append(private$.undo_stack, list(private$.current))
      }
      private$.current <- item
      private$.redo_stack <- list()

      private$.invalidate()

      invisible(self)
    },

    clear = function(clear_value = FALSE) {
      if (!checkmate::test_logical(clear_value, any.missing = FALSE, len = 1, null.ok = FALSE)) {
        stop("clear: `clear_value` must be either `TRUE` or `FALSE`.", call. = FALSE)
      }

      private$.undo_stack <- list()
      private$.redo_stack <- list()

      if (clear_value) {
        private$.current <- NULL
      }

      private$.invalidate()

      invisible(self)
    }

  )
)
