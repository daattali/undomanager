# UndoManager print shows an empty manager

    Code
      print(UndoManager$new())
    Output
      Empty <UndoManager> of arbitrary items

---

    Code
      print(UndoManager$new("numeric"))
    Output
      Empty <UndoManager> of items of type <numeric>

---

    Code
      print(UndoManager$new(c("numeric", "character")))
    Output
      Empty <UndoManager> of items of type <numeric>|<character>

# UndoManager print shows the current item and both stacks

    Code
      print(UndoManager$new()$do(1))
    Output
      <UndoManager> of arbitrary items with 0 undos and 0 redos
      
      ### Current item ###
      [1] 1

---

    Code
      print(UndoManager$new("numeric")$do(1)$do(2)$do(3))
    Output
      <UndoManager> of items of type <numeric> with 2 undos and 0 redos
      
      ### Current item ###
      [1] 3
      
      ### Undo stack ###
      1.
      [1] 2
      
      2.
      [1] 1
      

---

    Code
      print(UndoManager$new()$do(1)$do(2)$do(3)$undo())
    Output
      <UndoManager> of arbitrary items with 1 undo and 1 redo
      
      ### Current item ###
      [1] 2
      
      ### Undo stack ###
      1.
      [1] 1
      
      
      ### Redo stack ###
      1.
      [1] 3
      

# UndoManager print pluralises undo and redo counts

    Code
      print(UndoManager$new()$do(1)$do(2))
    Output
      <UndoManager> of arbitrary items with 1 undo and 0 redos
      
      ### Current item ###
      [1] 2
      
      ### Undo stack ###
      1.
      [1] 1
      

---

    Code
      print(UndoManager$new()$do(1)$do(2)$do(3))
    Output
      <UndoManager> of arbitrary items with 2 undos and 0 redos
      
      ### Current item ###
      [1] 3
      
      ### Undo stack ###
      1.
      [1] 2
      
      2.
      [1] 1
      

---

    Code
      print(UndoManager$new()$do(1)$do(2)$undo())
    Output
      <UndoManager> of arbitrary items with 0 undos and 1 redo
      
      ### Current item ###
      [1] 1
      
      ### Redo stack ###
      1.
      [1] 2
      

---

    Code
      print(UndoManager$new()$do(1)$do(2)$do(3)$undo()$undo())
    Output
      <UndoManager> of arbitrary items with 0 undos and 2 redos
      
      ### Current item ###
      [1] 1
      
      ### Redo stack ###
      1.
      [1] 2
      
      2.
      [1] 3
      

# UndoManager print shows NULL items

    Code
      print(UndoManager$new()$do(NULL))
    Output
      <UndoManager> of arbitrary items with 0 undos and 0 redos
      
      ### Current item ###
      NULL

---

    Code
      print(UndoManager$new()$do(1)$do(NULL)$do(3)$undo())
    Output
      <UndoManager> of arbitrary items with 1 undo and 1 redo
      
      ### Current item ###
      NULL
      
      ### Undo stack ###
      1.
      [1] 1
      
      
      ### Redo stack ###
      1.
      [1] 3
      

