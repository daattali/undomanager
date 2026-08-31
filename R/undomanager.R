#' Create an undo manager
#'
#' Create a new [UndoManager] to track the history of an object and move
#' through it with undo and redo operations. The manager starts out empty; the
#' first call to `$do()` gives it a value.
#'
#' This is the recommended way to create a manager. It is equivalent to
#' `UndoManager$new()`, which is also available for anyone who wants to work
#' with the [R6][R6::R6Class] class directly, such as to subclass it.
#'
#' A manager is an R6 object, so it has reference semantics: its methods modify
#' the manager in place instead of returning a modified copy, and assigning a
#' manager to a second variable does not copy it.
#'
#' @param type The permitted classes of the items (`NULL` to allow any
#' object). An item is accepted when any of these classes appears among
#' the classes R would dispatch on, so `"numeric"` also accepts integers
#' and numeric matrices.
#' @param allow_null Whether `NULL` values are allowed. Only used when
#' `type` is given; an untyped manager accepts any object, including `NULL`.
#' @param max_size The maximum number of items to keep. Once the history
#' grows past this, the oldest items are dropped. Use `Inf` for no limit.
#' @return A new [UndoManager] object.
#' @seealso [UndoManager] for the full list of methods and active bindings.
#' @examples
#' nums <- undomanager()
#' nums$do(5)
#' nums$do(7)
#' nums$do(10)
#' nums$undo()$value
#' nums$redo()$value
#'
#' # operations return the manager, so they can be chained
#' undomanager()$do(1)$do(2)$do(3)$undo(2)$value
#'
#' undomanager(type = "numeric")
#' undomanager(type = "numeric", allow_null = TRUE)
#' undomanager(max_size = 10)
#' @export
undomanager <- function(type = NULL, allow_null = FALSE, max_size = Inf) {
  UndoManager$new(type = type, allow_null = allow_null, max_size = max_size)
}

#' Undo/Redo manager
#'
#' @description
#' With the undo manager, you can manage the history of an object by
#' using undo and redo operations.
#'
#' Managers are usually created with [undomanager()] rather than by calling
#' `UndoManager$new()` directly. The class itself is exported for anyone who
#' wants to work with it directly, such as to subclass it.
#'
#' @field value Get the value
#' @field is_empty Whether the manager holds no value at all
#' @field can_undo Whether there are any undo operations available
#' @field can_redo Whether there are any redo operations available
#' @field undo_size Get the number of undo operations
#' @field redo_size Get the number of redo operations
#' @seealso [undomanager()], the recommended way to create a manager.
#' @rdname UndoManager-class
#' @examples
#' nums <- undomanager()
#' nums$do(5)
#' nums$do(7)
#' nums$do(10)
#' nums$undo()$value
#' nums$redo()$value
#'
#' # operations return the manager, so they can be chained
#' undomanager()$do(1)$do(2)$do(3)$undo(2)$value
#' @export
UndoManager <- R6::R6Class(
  "UndoManager",
  cloneable = FALSE,

  private = list(
    .type = NULL,
    .allow_null = FALSE,
    .max_size = Inf,

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
    #' Create a new undo manager. The manager starts out empty; the first
    #' call to `do()` gives it a value. Most users should call
    #' [undomanager()] instead, which is equivalent.
    #' @param type The permitted classes of the items (`NULL` to allow any
    #' object). An item is accepted when any of these classes appears among
    #' the classes R would dispatch on, so `"numeric"` also accepts integers
    #' and numeric matrices.
    #' @param allow_null Whether `NULL` values are allowed. Only used when
    #' `type` is given; an untyped manager accepts any object, including `NULL`.
    #' @param max_size The maximum number of items to keep. Once the history
    #' grows past this, the oldest items are dropped. Use `Inf` for no limit.
    #' @return A new `UndoManager` object.
    #' @examples
    #' UndoManager$new()
    #' UndoManager$new(type = "numeric")
    #' UndoManager$new(type = "numeric", allow_null = TRUE)
    #' UndoManager$new(max_size = 10)
    initialize = function(type = NULL, allow_null = FALSE, max_size = Inf) {
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
      if (!checkmate::test_count(max_size, positive = TRUE) && !identical(max_size, Inf)) {
        stop("UndoManager: `max_size` must be a positive integer, or `Inf`",
             call. = FALSE)
      }
      private$.type <- type
      private$.max_size <- max_size
      private$.allow_null <- allow_null
      private$.store <- new.env(parent = emptyenv())
      private$.store$history <- list()
      private$.rx_dep <- function(x) NULL
      invisible(self)
    },

    #' @description
    #' Get a shiny reactive for this manager, so that it can be used inside a
    #' reactive context. The returned reactive invalidates whenever the value
    #' changes, and calling it returns the manager itself.
    #' @return A shiny reactive expression that returns the manager.
    #' @examples
    #' if (requireNamespace("shiny", quietly = TRUE)) {
    #'   nums <- undomanager()$do(5)
    #'   rx <- nums$reactive()
    #'   shiny::isolate(rx()$value)
    #' }
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
    #' Print the manager: the classes it accepts, how many undo and redo
    #' operations are available, the current item, and the items in the undo
    #' and redo history.
    #' @param ... Not used.
    #' @return The manager, invisibly.
    #' @examples
    #' undomanager()$do(5)$do(7)$do(10)$undo()$print()
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
    #' Move back in the history, making the previous item current. Undoing
    #' when there is nothing left to undo does nothing.
    #' @param n The number of undo operations to perform. If `n` is larger
    #' than the number of available undo operations, all of them are
    #' performed. Use `Inf` to undo the entire history.
    #' @return The manager, invisibly.
    #' @examples
    #' nums <- undomanager()$do(5)$do(7)$do(10)
    #' nums$undo()$value
    #' nums$undo(Inf)$value
    undo = function(n = 1) {
      if (!checkmate::test_count(n) && !identical(n, Inf)) {
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
    #' Move forward in the history, reversing an undo. Any redo history is
    #' discarded as soon as a new item is added with `do()`. Redoing when
    #' there is nothing left to redo does nothing.
    #' @param n The number of redo operations to perform. If `n` is larger
    #' than the number of available redo operations, all of them are
    #' performed. Use `Inf` to redo the entire history.
    #' @return The manager, invisibly.
    #' @examples
    #' nums <- undomanager()$do(5)$do(7)$do(10)$undo(2)
    #' nums$redo()$value
    #' nums$redo(Inf)$value
    redo = function(n = 1) {
      if (!checkmate::test_count(n) && !identical(n, Inf)) {
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
    #' Add an item and make it the current value. The item that was current
    #' becomes the most recent undo, and any redo history is discarded.
    #' @param item The item to add. It must satisfy the manager's `type`, if
    #' one was given.
    #' @return The manager, invisibly.
    #' @examples
    #' nums <- undomanager()
    #' nums$do(5)
    #' nums$do(7)$value
    #' nums$undo()$do(99)$redo_size
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

      if (length(private$.store$history) > private$.max_size) {
        private$.store$history <- private$.store$history[-1L]
        private$.pos <- private$.pos - 1L
      }

      private$.invalidate()

      invisible(self)
    },

    #' @description
    #' Forget the undo and redo history, keeping the current value.
    #' @param clear_value Whether to also discard the current value, leaving
    #' the manager empty.
    #' @return The manager, invisibly.
    #' @examples
    #' nums <- undomanager()$do(5)$do(7)
    #' nums$clear()$undo_size
    #' nums$value
    #' nums$clear(clear_value = TRUE)$is_empty
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
    max_size = p$.max_size,
    history = p$.store$history,
    position = p$.pos
  )
}
