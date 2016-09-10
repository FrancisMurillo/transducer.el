;;; transducer.el --- Transducer implementation for elisp  -*- lexical-binding: t; -*-
;;
;; Filename: transducer.el
;; Description: Transducer implemntation for elisp
;; Author: Francis Murillo
;; Maintainer: Francis Murillo
;; Created: Thu Sep  8 19:36:33 2016 (+0800)
;; Version: 0.1.0
;; Package-Requires: (dash)
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'dash)


;;* Stream
(defconst transducer-stream-start 'stream-start
  "A value signifying the start of a stream.
Not to be used directly by the stream implementation.")

(defconst transducer-stream-stop 'stream-stop
  "A value signifying the end of a stream.
Should be used when the stream is done.")

(defun transducer-stream-start-value-p (value)
  "Check if VALUE is the start signal of a stream."
  (eq value transducer-stream-start))

(defun transducer-stream-stop-value-p (value)
  "Check if VALUE is the stop signal of a stream."
  (eq value transducer-stream-stop))

(defun transducer-stream (fn)
  "A simple stream or generator with FN function.
The function should not be idempotent and return `transducer-stream-stop'
when the function stops producing values."
  (lexical-let* ((streamer fn)
      (value transducer-stream-start))
    (lambda (&rest args)
      (cond
       ((transducer-stream-start-value-p value)
        (setq value (apply streamer args))
        transducer-stream-start)
       ((transducer-stream-stop-value-p value)
        transducer-stream-stop)
       (t
        (prog1
            value
          (setq value (apply streamer args))))))))

(defun transducer-stream-from-list (xs)
  "Create a stream from a list XS."
  (lexical-let ((ys xs))
    (transducer-stream
     (lambda (&rest _)
       (if (null ys)
           transducer-stream-stop
         (prog1
             (car ys)
           (setq ys (cdr ys))))))))

(defun transducer-stream-to-list (stream)
  "Unroll a STREAM for convenience."
  (let ((xs (list))
      (value (funcall stream)))
    (while (not (transducer-stream-stop-value-p value))
      (unless (transducer-stream-start-value-p value)
        (push value xs))
      (setq value (funcall stream)))
    (reverse xs)))

(defun transducer-stopped-stream ()
  "A stream that always return the end of signal."
  (lambda (&rest _) transducer-stream-stop))


(defun transducer-stream-cycle (n stream)
  "Repeat the stream values N times of a STREAM."
  (lexical-let ((repeating nil)
      (counter 1)
      (stored-values (list))
      (current-values nil))
    (if (zerop n)
        (transducer-stopped-stream)
      (transducer-stream
       (lambda (&rest args)
         (if (null repeating)
             (lexical-let ((value (apply stream args)))
               (cond
                ((transducer-stream-start-value-p value)
                 (let ((next-value (apply stream args)))
                   (push next-value stored-values)
                   next-value))
                ((transducer-stream-stop-value-p value)
                 (setq repeating t
                    counter 2 ; Second cycle
                    stored-values (reverse stored-values)
                    current-values stored-values)
                 (when (>= (1- counter) n) ; Adjust cycle check
                   (setq stored-values nil
                      current-values nil))
                 (if (null current-values)
                     transducer-stream-stop
                   (prog1
                       (car current-values)
                     (setq current-values (cdr current-values)))))
                (t
                 (push value stored-values)
                 value)))
           (if (null current-values)
               (if (>= counter n)
                   transducer-stream-stop
                 (progn
                   (setq current-values stored-values
                      counter (1+ counter))
                   (prog1
                       (car current-values)
                     (setq current-values (cdr current-values)))))
             (prog1
                 (car current-values)
               (setq current-values (cdr current-values))))))))))

(defun transducer-stream-append (&rest streams)
  "Append several STREAMS in execution."
  (lexical-let* ((streams streams)
      (stream (car streams)))
    (transducer-stream
     (lambda (&rest args)
       (lexical-let ((value (apply stream args)))
         (while (and (not (null streams))
                   (or (transducer-stream-start-value-p value)
                      (transducer-stream-stop-value-p value)))
           (cond
            ((transducer-stream-start-value-p value)
             (setq value (apply stream args)))
            ((transducer-stream-stop-value-p value)
             (setq streams (cdr streams))
             (unless (null streams)
               (setq stream (car streams)
                  value (apply stream args))))))
         value)))))


;;* Reducer
(defun transducer-reducer (initial-fn complete-fn step-fn)
  "Create a reducer with an initial seed function INITIAL-FN,
a final function COMPLETE-FN, and a step function STEP-FN."
  (lambda (&rest args)
    (let ((arity (length args)))
      (pcase arity
        (0 (funcall initial-fn))
        (1 (apply complete-fn args))
        (2 (apply step-fn args))))))

(defun transducer-list-reducer ()
  "A reducer for lists."
  (transducer-reducer
   (lambda () (list))
   (lambda (result) result)
   (lambda (result item) (append result (list item)))))

;;* Api
(defun transducer-identity ()
  "An identity reducer."
  (lambda (reducer)
    (transducer-reducer
     (lambda () (funcall reducer))
     (lambda (result) (funcall reducer result))
     (lambda (result item) (funcall reducer result item)))))

(defun transducer-map (mapper)
  "Map reducer with MAPPER function."
  (lambda (reducer)
    (transducer-reducer
     (lambda () (funcall reducer))
     (lambda (result) (funcall reducer result))
     (lambda (result item) (funcall reducer result (funcall mapper item))))))

(defun transducer-filter (filterer)
  "Filter redcuer with FILTERER predicate."
  (lambda (reducer)
    (transducer-reducer
     (lambda () (funcall reducer))
     (lambda (result) (funcall reducer result))
     (lambda (result item)
       (if (funcall filterer item)
           (funcall reducer result item)
         result)))))

(defun transducer-composes (&rest reducers)
  "Compose transducers REDUCERS.
The order is left to right instead of the standard right to left
due to the implementation of transducers in general."
  (lambda (reducer)
    (-reduce-from
     (lambda ( accumulated-reducer new-reducer)
       (funcall new-reducer accumulated-reducer))
     reducer
     (reverse reducers))))


(defun transducer-distinct ()
  "Distinct reducer with EQUALITY predicate."
  (lambda (reducer)
    (lexical-let ((cache-table (make-hash-table))
        (not-found (make-symbol "distinct-not-found")))
      (transducer-reducer
       (lambda () (funcall reducer))
       (lambda (result) (funcall reducer result))
       (lambda (result item)
         (let ((found-item (gethash item cache-table not-found)))
           (if (not (eq found-item not-found))
               result
             (puthash item t cache-table)
             (funcall reducer result item))))))))

(defun transducer-first (firster)
  "First reducer with FIRSTER predicate."
  (lambda (reducer)
    (transducer-reducer
     (lambda () (funcall reducer))
     (lambda (result) (funcall reducer result))
     (lambda (result item)
       (if (funcall firster item)
           (transducer-reduced-value
            (funcall reducer result item))
         result)))))


;;* Reductions
(defconst transducer-transduce-reduced 'transduce-reduced
  "A value signifying that a transducer should stop.")

(defun transducer-reduced-value (value)
  "Create a flag or sentinel to stop a reducer with VALUE."
  (if (transducer-reduced-value-p value)
      (cons transducer-transduce-reduced (cdr value))
    (cons transducer-transduce-reduced value)))

(defun transducer-reduced-value-p (value)
  "Check if the VALUE is reduced."
  (and (consp value)
     (eq (car value) transducer-transduce-reduced)))

(defun transducer-reduced-get-value (reduced)
  "Get the reduced value of a REDUCED."
  (cdr reduced))


(defun transducer-transduce (transducer reducer xs)
  "A transduce on a list with TRANSDUCER, REDUCER and a list XS."
  (let* ((reductor (funcall transducer reducer))
      (result (funcall reductor))
      (ys xs))
    (while (not (null ys))
      (setq result (funcall reductor result (car ys))
         ys (cdr ys))
      (when (transducer-reduced-value-p result)
        (setq result (transducer-reduced-get-value result)
           ys nil)))
    (funcall reductor result)))


(defconst transducer--stream-step 'stream-step
  "A value stating whether the stream should continue on.
Not to be used directly.")

(defconst transducer--stream-skip 'stream-skip
  "A value stating whether the stream should get another value.
Not to be used directly.")

(defun transducer-transduce-stream (transducer stream)
  "A transduce on a stream with a TRANSDUCER on STREAM."
  (lexical-let* ((reductor
       (funcall transducer
          (transducer-reducer
           (lambda () transducer--stream-skip)
           (lambda (_) transducer--stream-skip)
           (lambda (_ item) item)))))
    (transducer-stream
     (lambda (&rest args)
       (lexical-let* ((value (apply stream args))
           (state nil)
           (result nil))
         (while (not (eq state transducer--stream-step))
           (cond
            ((transducer-stream-start-value-p value)
             (setq value (apply stream args)
                state nil))
            ((transducer-stream-stop-value-p value)
             (setq result value
                state transducer--stream-step))
            (t
             (setq result (funcall reductor transducer--stream-skip value)
                state transducer--stream-step)
             (when (transducer-reduced-value-p result)
               (setq result (transducer-reduced-get-value result)
                  stream (transducer-stopped-stream)
                  state transducer--stream-step))
             (when (eq result transducer--stream-skip)
               (setq value (apply stream args)
                  state nil)))))
         result)))))


(provide 'transducer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer.el ends here
