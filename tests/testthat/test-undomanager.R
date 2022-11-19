test_that("UndoManager cannot add NULL items", {
  expect_error(UndoManager$new()$add(NULL))
  expect_error(UndoManager$new()$add(1)$add(2), NA)
  expect_error(UndoManager$new()$add(1)$add(2)$add(NULL))
})

test_that("UndoManager with no type accepts any object", {
  expect_error(UndoManager$new()$add("a"), NA)
  expect_error(UndoManager$new()$add("a")$add("b"), NA)
  expect_error(UndoManager$new()$add(1), NA)
  expect_error(UndoManager$new()$add(1)$add(2), NA)
  expect_error(UndoManager$new()$add("a")$add(1)$add("b")$add(2), NA)
})

test_that("UndoManager with a type accepts that object", {
  expect_error(UndoManager$new("character")$add("a"), NA)
  expect_error(UndoManager$new("character")$add("a")$add("b"), NA)
  expect_error(UndoManager$new("numeric")$add(1), NA)
  expect_error(UndoManager$new("numeric")$add(1)$add(2), NA)
})

test_that("UndoManager with a type rejects different objects", {
  expect_error(UndoManager$new("character")$add(1))
  expect_error(UndoManager$new("numeric")$add("a"))
  expect_error(UndoManager$new("numeric")$add(1)$add("a"))
  expect_error(UndoManager$new("character")$add("a")$add(a))
})

test_that("UndoManager add works", {
  expect_identical(UndoManager$new()$add(5)$value, 5)
  expect_identical(UndoManager$new()$add(5)$undo_size, 0L)
  expect_identical(UndoManager$new()$add(5)$redo_size, 0L)

  expect_identical(UndoManager$new()$add(5)$add(7)$value, 7)
  expect_identical(UndoManager$new()$add(5)$add(7)$undo_size, 1L)
  expect_identical(UndoManager$new()$add(5)$add(7)$redo_size, 0L)

  expect_identical(UndoManager$new()$add(5)$add(7)$add(3)$value, 3)
  expect_identical(UndoManager$new()$add(5)$add(7)$add(3)$undo_size, 2L)
  expect_identical(UndoManager$new()$add(5)$add(7)$add(3)$redo_size, 0L)

  expect_identical(UndoManager$new()$add(5)$add(7)$add(3)$add(8)$add(1)$value, 1)
  expect_identical(UndoManager$new()$add(5)$add(7)$add(3)$add(8)$add(1)$undo_size, 4L)
  expect_identical(UndoManager$new()$add(5)$add(7)$add(3)$add(8)$add(1)$redo_size, 0L)
})

test_that("UndoManager can't undo/redo past the beginning/end", {
  expect_error(UndoManager$new()$undo())
  expect_error(UndoManager$new()$redo())

  expect_error(UndoManager$new()$add(1)$undo())
  expect_error(UndoManager$new()$add(1)$redo())

  expect_error(UndoManager$new()$add(1)$add(2)$undo(), NA)
  expect_error(UndoManager$new()$add(1)$add(2)$undo()$undo())
  expect_error(UndoManager$new()$add(1)$add(2)$redo())
  expect_error(UndoManager$new()$add(1)$add(2)$undo()$redo(), NA)
  expect_error(UndoManager$new()$add(1)$add(2)$undo()$redo()$redo())

  expect_error(UndoManager$new()$add(1)$add(2)$add(3)$undo(), NA)
  expect_error(UndoManager$new()$add(1)$add(2)$add(3)$undo()$undo(), NA)
  expect_error(UndoManager$new()$add(1)$add(2)$add(3)$undo()$undo()$undo())
})

undoredo_test <- function() {
  UndoManager$new()$add(5)$add(7)$add(3)$add(8)$add(1)
}

test_that("UndoManager undo/redo works", {
  expect_identical(undoredo_test()$undo()$value, 8)
  expect_identical(undoredo_test()$undo()$undo_size, 3L)
  expect_identical(undoredo_test()$undo()$redo_size, 1L)

  expect_identical(
    undoredo_test()$undo()$redo()$value,
    undoredo_test()$value
  )
  expect_identical(
    undoredo_test()$undo()$redo()$undo_size,
    undoredo_test()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$redo()$redo_size,
    undoredo_test()$redo_size
  )

  expect_identical(undoredo_test()$undo()$undo()$value, 3)
  expect_identical(undoredo_test()$undo()$undo()$undo_size, 2L)
  expect_identical(undoredo_test()$undo()$undo()$redo_size, 2L)

  expect_identical(
    undoredo_test()$undo()$undo()$redo()$value,
    undoredo_test()$undo()$value
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo_size,
    undoredo_test()$undo()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$redo_size,
    undoredo_test()$undo()$redo_size
  )

  expect_identical(undoredo_test()$undo()$undo()$undo()$value, 7)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo_size, 1L)
  expect_identical(undoredo_test()$undo()$undo()$undo()$redo_size, 3L)

  expect_identical(
    undoredo_test()$undo()$undo()$undo()$redo()$value,
    undoredo_test()$undo()$undo()$value
  )
  expect_identical(
    undoredo_test()$undo()$undo()$undo()$redo()$undo_size,
    undoredo_test()$undo()$undo()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$undo()$redo()$redo_size,
    undoredo_test()$undo()$undo()$redo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo()$redo()$value,
    undoredo_test()$undo()$value
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo()$redo()$undo_size,
    undoredo_test()$undo()$undo_size
  )
  expect_identical(
    undoredo_test()$undo()$undo()$redo()$undo()$redo()$redo_size,
    undoredo_test()$undo()$redo_size
  )

  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$value, 5)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$redo()$redo()$redo()$redo()$value, 1)
  expect_identical(undoredo_test()$undo()$redo()$undo()$redo()$undo()$redo()$undo()$redo()$value, 1)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$undo_size, 0L)
  expect_identical(undoredo_test()$undo()$undo()$undo()$undo()$redo_size, 4L)
})
