; temp variables
(setq row 0)
(setq column 0)
(setq get-element (-partial #'get-element-in-array vectors))
(funcall get-element 0 0)
                                        ; temp variables

(setq input (f-read-text "input.txt" 'utf-8))
(setq input-sequence (butlast (split-string input "\n")))

(defun to-vector (list)
  "Change a vector to a list"
  (vconcat [] list))

; 76 = L, 46 = .
(setq vectors (to-vector (-map #'to-vector input-sequence)))

(defun get-element-in-array (nested-array row column)
  "Get the element from the two-dimensional array"
  (if (or (< row 0) (< column 0) (>= column (length nested-array)) (>= row (length nested-array)))
      46; if out of bounds, return an empty seat
    (aref (aref nested-array row) column))
  )

(defun get-adjacent-elements (row column nested-array)
  "Get adjacent seats to element at index [row, column]"
  (let (
        (get-element (-partial #'get-element-in-array nested-array))
                    )
    (list
    (funcall get-element (+ row -1) (+ column -1))
    (funcall get-element (+ row -1) (+ column 0))
    (funcall get-element (+ row -1) (+ column 1))
    (funcall get-element (+ row 0) (+ column 1))
    (funcall get-element (+ row 1) (+ column 1))
    (funcall get-element (+ row 1) (+ column 0))
    (funcall get-element (+ row 1) (+ column -1))
    (funcall get-element (+ row 0) (+ column -1)))
  ))

(defun get-occupied-seat-count (row column nested-array)
       "get the count of adjacent seats that are occupied"
       (count 76 (get-adjacent-elements row column nested-array)))

(defun will-become-empty (row column nested-array)
  (if (>= (get-occupied-seat-count row column nested-array) 4) t nil))

(defun will-become-full (row column nested-array)
  (if (equal (get-occupied-seat-count row column nested-array) 0) t nil))

; how do I want to update it?  I'd rather not update the list but create a new one with new info.
; we can keep a track of which seats to change
; then re-create the array with this information
; map to a list of indexes I can operate on.

(defun transform-array-to-indices (nested-array)
  "Take an array and map to a list of indices"
  (append nested-array)
  )

(defun append-to-list (vector)
  (append vector nil))

(defun convert-vector-to-lists (vectors)
    "convert nested vectors into a nested list"
    (-flatten (-map #'append-to-list (append vectors nil)))
    )

(defun true (element)
  "Returns true"
  t)

(defun get-column (index size)
  (% index size))

(defun get-row (index size)
  (/ index size))

(defun get-indices (index size)
  (list (get-row index size) (get-column index size))
  )

(defun change-to-indices (vectors)
  (-find-indices #'true (convert-vector-to-lists vectors)))

(defun nested-vector-to-indices (vector)
  "Takes a nested array and returns (row, column) indices"
  (mapcar (-rpartial #'get-indices (length vectors))
          (-find-indices #'true (convert-vector-to-lists vectors))))





