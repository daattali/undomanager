#' Undo/Redo manager
#'
#' @description
#' With the undo manager, you can manage the history of an object by
#' using undo and redo operations.
#'
#' @field value Get the value
#' @field is_empty Whether the manager holds no value at all
#' @field can_undo Whether there are any undo operations available
#' @field can_redo Whether there are any redo operations available
#' @field undo_size Get the number of undo operations
#' @field redo_size Get the number of redo operations
#' @examples
#' TODO
#' @export
UndoManager <- R6::R6Class(
  "UndoManager",
  cloneable = FALSE,

  private = list(
    .type = NULL,
    .allow_null = FALSE,

    # The history is a single list plus a cursor. `.pos` is the index of the
    # current item, or 0 when the manager is empty. Everything before the
    # cursor is undo history; everything after it is redo history.
    # The history list lives inside a plain environment rather than directly
    # in this list because writing to an R6 private field is slow.
    .store = NULL,
    .pos = 0L,

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
      if (self$is_empty) NULL else private$.store$history[[private$.pos]]
    },

    is_empty = function() {
      private$.pos == 0L
    },

    undo_size = function() {
      max(private$.pos - 1L, 0L)
    },

    redo_size = function() {
      if (self$is_empty) 0L else length(private$.store$history) - private$.pos
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
    #' @param allow_null Whether `NULL` values are allowed. Only used when
    #' `type` is given; an untyped manager always accepts any object including `NULL`.
    #' @examples
    #' TODO
    #' @return TODO
    initialize = function(type = NULL, allow_null = FALSE) {
      if (!is.null(type) &&
          !checkmate::test_character(type, any.missing = FALSE, unique = TRUE,
                                     min.chars = 1, min.len = 1, names = "unnamed")) {
        stop("UndoManager: `type` must either be `NULL` or an unnamed vector of strings",
             call. = FALSE)
      }
      if (!is.null(type) && "NULL" %in% type) {
        stop("UndoManager: `type` cannot include \"NULL\"; use `allow_null = TRUE` instead",
             call. = FALSE)
      }
      if (!checkmate::test_logical(allow_null, any.missing = FALSE, len = 1, null.ok = FALSE)) {
        stop("UndoManager: `allow_null` must be either `TRUE` or `FALSE`.", call. = FALSE)
      }
      private$.type <- type
      private$.allow_null <- allow_null
      private$.store <- new.env(parent = emptyenv())
      private$.store$history <- list()
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

      if (self$is_empty) {
        cat("Empty ")
      }

      cat("<UndoManager>")
      if (is.null(private$.type)) {
        cat(" of arbitrary items")
      } else {
        cat0(" of items of type ", paste0("<", private$.type, ">", collapse = "|"))
      }

      if (self$is_empty) {
        cat("\n")
      } else {
        cat0(" with ")
        cat0(self$undo_size, if(self$undo_size == 1) " undo" else " undos", " and ")
        cat0(self$redo_size, if(self$redo_size == 1) " redo" else " redos", "\n")

        cat("\n### Current item ###\n")
        print(self$value)

        if (self$undo_size > 0) {
          cat("\n### Undo stack ###\n")
          for (idx in seq_len(self$undo_size)) {
            cat0(idx, ".\n")
            print(private$.store$history[[private$.pos - idx]])
            cat("\n")
          }
        }

        if (self$redo_size > 0) {
          cat("\n### Redo stack ###\n")
          for (idx in seq_len(self$redo_size)) {
            cat0(idx, ".\n")
            print(private$.store$history[[private$.pos + idx]])
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
      private$.pos <- private$.pos - n

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
      private$.pos <- private$.pos + n

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
      if (missing(item)) {
        stop("do: `item` must be provided", call. = FALSE)
      }
      force(item)

      if (!is.null(private$.type)) {
        if (is.null(item)) {
          if (!private$.allow_null) {
            stop("do: `item` must not be NULL; use `allow_null = TRUE` to permit it",
                 call. = FALSE)
          }
        } else if (!any(private$.type %in% .class2(item))) {
          stop("do: The provided item must have class ",
               paste0("<", private$.type, ">", collapse = "|"),
               call. = FALSE)
        }
      }

      private$.pos <- private$.pos + 1L

      # `[[<-` would delete the element when `item` is NULL, so assign
      # through `[` with a one-element list instead
      private$.store$history[private$.pos] <- list(item)

      if (length(private$.store$history) > private$.pos) {
        length(private$.store$history) <- private$.pos
      }

      private$.invalidate()

      invisible(self)
    },

    clear = function(clear_value = FALSE) {
      if (!checkmate::test_logical(clear_value, any.missing = FALSE, len = 1, null.ok = FALSE)) {
        stop("clear: `clear_value` must be either `TRUE` or `FALSE`.", call. = FALSE)
      }

      if (clear_value || self$is_empty) {
        private$.store$history <- list()
        private$.pos <- 0L
      } else {
        private$.store$history <- list(private$.store$history[[private$.pos]])
        private$.pos <- 1L
      }

      private$.invalidate()

      invisible(self)
    }

  )
)

#' Compare two UndoManager objects
#'
#' Two managers are equal when they hold the same value, the same undo and redo
#' history, and the same `type` restriction. Internal bookkeeping, such as the
#' counter used to trigger shiny reactivity, is ignored, so two managers that
#' reached the same state by different routes compare as equal.
#'
#' Note that [identical()] cannot be used to compare managers: R6 objects are
#' environments, so `identical()` always reports two separate managers as
#' different regardless of their contents.
#'
#' @param target,current The two `UndoManager` objects to compare.
#' @param ... Passed on to [all.equal()].
#' @return `TRUE` if the two managers are equal, otherwise a character vector
#' describing the differences.
#' @method all.equal UndoManager
#' @export
all.equal.UndoManager <- function(target, current, ...) {
  if (!inherits(current, "UndoManager")) {
    return("`current` is not an <UndoManager>")
  }
  all.equal(state(target), state(current), ...)
}

state <- function(x) {
  p <- x$.__enclos_env__$private
  list(
    type = p$.type,
    allow_null = p$.allow_null,
    history = p$.store$history,
    position = p$.pos
  )
}
