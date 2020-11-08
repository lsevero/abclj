(ns game-of-life.life
  (:require [abclj.core :refer :all]))

(with-cl
  '(defvar *alive* 1)
  '(defvar *dead* 0))

(defun get-next-val (neighborhood)
  "Calculate the next value of the cell from its 3x3 neighborhood."
  (let ((live-neighbor-count (get-live-neighbor-count neighborhood))
	(alive (center-cell-is-alive neighborhood)))
    (if (or (and alive 
		 (or (eq live-neighbor-count 2)
		     (eq live-neighbor-count 3)))
	    (and (not alive)
		 (eq live-neighbor-count 3)))
      1
      0)))

(defun get-live-cell-count (neighborhood)
  "Count the number of live cells in a 3x3 neighborhood."
  (reduce (function +) 
	  (append (first neighborhood) 
		  (second neighborhood) 
		  (third neighborhood))))

(defun get-center-cell (neighborhood)
  "Get the value of the center cell in a 3x3 neighborhood."
  (second (second neighborhood)))

(defun center-cell-is-alive (neighborhood)
  "Return whether the center cell in a 3x3 neigborhood is alive."
  (eq (get-center-cell neighborhood) *alive*))

(defun get-live-neighbor-count (neighborhood)
  "Get the number of live neighbors in a 3x3 neighborhood."
  (let ((live-cell-count (get-live-cell-count neighborhood)))
    (if (center-cell-is-alive neighborhood)
	(- live-cell-count 1)
	live-cell-count)))

(defun update-universe (universe)
  (let ((new-universe nil))
    (dotimes (row (length universe))
      (let ((new-row nil))
	(dotimes (col (length (first universe)))
	  (setf new-row 
		(cons (get-next-val (construct-neighbors universe row col)) 
		      new-row)))
	(setf new-universe (cons (reverse new-row) new-universe))))
    (reverse new-universe)))

(defun gen-rand-universe (height width)
    (if (eq height 0)
	nil
	(cons (gen-rand-row width) (gen-rand-universe (- height 1) width))))

(defun gen-rand-row (width)
  (if (eq width 0)
      nil
    (cons (random 2) (gen-rand-row (- width 1)))))
	  
(defun construct-neighbors (universe row col)
  (let ((neighbors '()))
    (dotimes (i 3)
      (let ((new-row 
	     (wrapping-subseq (wrapping-nth (- (+ row i) 1) universe) 
			      (- col 1) (+ 2 col))))
	(setf neighbors (cons new-row neighbors))))
    (reverse neighbors)))

(defun wrapping-subseq (seq start end)
  (if (eq start end)
      '()
      (let ((element
	     (cond ((< start 0) (nth (+ start (length seq)) seq))
		   ((>= start (length seq)) (nth (- start (length seq)) seq))
		   (t (nth start seq)))))
	(cons element (wrapping-subseq seq (+ start 1) end)))))

(defun wrapping-nth (n list)
  (nth (mod (+ n (length list)) (length list)) list))
