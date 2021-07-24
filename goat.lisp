#!/usr/bin/lisp --script
;;; goat.lisp --- Game Object Appliance Toolkit  -*- lexical-binding: t -*-

;; Maintainer:		Tohan Marchand <tohan.marchand@protonmail.com>
;; Created:		2021 June the 14th
;; Keywords:		game-development, math, generator
;; Version:		20210618.1

;;;; License and Commentary

;; Copyright 2021 Tohan Marchand <tohan.marchand@protonmail.com>

;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

;; 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

;; 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Todo:

;; * [DONE] Multiple modes
;; ** [DONE] Reverse search mode
;; ** [DONE] Batch list mode
;; *** No duplicates
;; * Failsafe check
;; * [DONE] Dead ranges
;; *  [DONE] Optional values
;; ** [DONE] Optional Prompted values
;; ** [DONE] Optional evaluated values
;; * Debug mode

;;; Code:
(defmacro sum (list)
  "Returns the sum of a list."
  `(apply #'+ ,list))


(defun find-median (list)
  "Returns the median of a list of values.
If the list is odd, returns the centermost value.
If the list is odd, returns the average of the two centermost values."
  (if (oddp (length list))
      (nth (floor (/ (length list) 2))
	   list)
      (/ (+ (nth (- (/ (length list) 2) 1)
		 list)
	    (nth (/ (length list) 2)
		 list))
	 2)))


(defun find-average (list)
  "Returns the unfiltered average of a list of integer"
  (* 1.0 (/ (apply #'+ list) (length list))))


(defun find-proprieties (list)
  (list :list list
	:length-limit (length list)
	:average (find-average list)
	:median (find-median list)
	:smallest-int (apply #'min list)
	:biggest-int (apply #'max list)))


(defun my-random (min max &key smallest-dead-value biggest-dead-value)
  "Returns a random number between a set minimum and maximum value.
Supports dead range, both keys must be t to activate"
  (loop
    (let ((int (+ (random (+ (- max min) 1)) min)))
      (if (not (and smallest-dead-value biggest-dead-value))
	  (return int)
	  (if (not (and (>= int smallest-dead-value)
			(<= int biggest-dead-value)))
	      (return int))))))



(defun get-list (list-amount length-limit average median smallest-int biggest-int smallest-dead-value biggest-dead-value metadata)
  "Returns a list of proprieties for create-list."
  (list :list-amount list-amount :length-limit length-limit :average average :median median
	:smallest-int smallest-int :biggest-int biggest-int :smallest-dead-value smallest-dead-value
	:biggest-dead-value biggest-dead-value :metadata metadata))


(defun prompt-read (prompt &key mandatory type)
  "List of types; integer, list"
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (cond ((not type) (read-preserving-whitespace *query-io*))
	((eql type 'integer)
	 ;; If user did not enter a correct value, will loop indefinitely until he does
	 (loop
	   (let ((answer (read-preserving-whitespace *query-io*)))
	     (if (numberp answer)
		 (return answer)
		 (format t "~&Please, enter a number: ")))))
	((eql type 'list)
	 (loop
	   (let ((answer (read-preserving-whitespace *query-io*)))
	     (if (and (listp answer) (every #'numberp answer))
		 (return answer)
		 (format t "~&Please, enter a list filled with numbers")))))))


(defun prompt-for-research-list-propriety ()
  (format t "~%*Mandatory~%Enter a s-expression list~%e.g. (1 2 3 4 5)~%Paranthesis included~%")
  (append '(:metadata t) (list :list (prompt-read "List" :type 'list))))


(defun prompt-for-get-list ()
  (format t "~%*Mandatory~%Type nil to skip~2%")
  (get-list (prompt-read "*Amount of lists" :type 'integer)
	    (prompt-read "*List Length" :type 'integer)
	    (prompt-read "Average")
	    (prompt-read "Median")
	    (prompt-read "*Smallest Value" :type 'integer)
	    (prompt-read "*Biggest Value" :type 'integer)
	    (prompt-read "Smallest Dead Value")
	    (prompt-read "Biggest Dead Value")
	    (y-or-n-p "*Enable Metadata: ")))


(defun prompt-menu ()
  (loop
    (format t "Welcome to Game Object Appliance Toolkit.~%Please choose one.~2%1) Generate Lists~%2) Find List Proprieties~%")
    (let ((answer (prompt-read "Choice")))
      (cond ((eql 1 answer) (generate-list))
	    ((eql 2 answer) (research-list-proprieties))
	    (t (format t "Please, enter a valid choice~%")))
      (if (not (y-or-n-p "Another Operation ?"))
	  (progn
	    (format t "Byebye!~%")
	    (return))))))


(defun create-list-batch (&key list-amount length-limit average median smallest-int biggest-int smallest-dead-value biggest-dead-value metadata)
  "Loops create-list per list-amount. Returns a list of lists."
  (do ((i 0 (1+ i))
       (my-list-list nil (cons (create-list :length-limit length-limit
					    :average average
					    :median median
					    :smallest-int smallest-int
					    :biggest-int biggest-int
					    :smallest-dead-value smallest-dead-value
					    :biggest-dead-value biggest-dead-value)
			       my-list-list)))
      ((>= i list-amount) (list :metadata metadata :list my-list-list))))


(defun create-list (&key length-limit average median smallest-int biggest-int smallest-dead-value biggest-dead-value)
  "Returns a list of integers based on length, average, median, smallest and biggest values."
  ;; length-limit must be positive to make sense
  (let ((length-limit (abs length-limit)))
    ;; A new random int is evaluated every cycle and assigned to an originally empty list
    (do* ((int (my-random smallest-int biggest-int
			  :smallest-dead-value smallest-dead-value
			  :biggest-dead-value biggest-dead-value)
	       (my-random smallest-int biggest-int
			  :smallest-dead-value smallest-dead-value
			  :biggest-dead-value biggest-dead-value))
	  (my-list nil (cons int my-list)))

	 ((and (= (length my-list) length-limit)
	       (sort my-list #'<)
	       ;; Optional average and median
	       (if (numberp average)
		   (= (find-average my-list) average)
		   t)
	       (if (numberp median)
		   ;; A copy of my-list is used because sort is destructive
		   (= (find-median (sort (copy-list my-list) #'<)) median)
		   t))
	  my-list)

      (if (> (length my-list) length-limit)
	  (setq my-list nil)))))


(defun dump-list (&key metadata list)
  "Pretty print for the list of values."
  (if metadata
      (if (listp (car list))		; If the list is made of lists
	  (dolist (l list)
	    (format t "~{~a:~10t~a~%~}~%" (find-proprieties l))) ; Prints lists and their proprieties
	  (format t "~{~a:~10t~a~%~}~%" (find-proprieties list))) ; Prints lists and their proprieties
      (format t "~{~A~%~}~%" list)))			     ; Simply prints lists


(defun research-list-proprieties ()
  (apply #'dump-list (prompt-for-research-list-propriety)))

(defun generate-list ()
  (apply #'dump-list (apply #'create-list-batch (prompt-for-get-list))))

(prompt-menu)

;;;; goat.lisp ends here
