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


;;* Interface
(defconst transducer-stream-start 'stream-start
  "A value signifying the start of a stream.
Not to be used directly by the stream implementation.")

(defconst transducer-stream-stop 'stream-stop
  "A value signifying the end of a stream.")

(defun transducer-stream (fn)
  "A simple stream or generator with FN function.
The function should not be idempotent and return `transducer-stream-stop'
when the function stops producing values."
  (lexical-let* ((streamer fn)
      (value transducer-stream-start))
    (lambda (&rest args)
      (cond
       ((eq value transducer-stream-start)
        (setq value (apply streamer args))
        transducer-stream-start)
       ((eq value transducer-stream-stop)
        transducer-stream-stop)
       (t
        (prog1
            value
          (setq value (apply streamer args))))))))

(defun transducer-stream-from-list (xs)
  "Create a stream from a list XS."
  (lexical-let ((ys xs))
    (transducer-stream
     (lambda (&rest args)
       (if (null ys)
           transducer-stream-stop
         (prog1
             (car ys)
           (setq ys (cdr ys))))))))

(defun transducer-stream-to-list (stream)
  "Unroll a STREAM for convenience."
  (let ((xs (list))
      (value (funcall stream)))
    (while (not (eq value transducer-stream-stop))
      (unless (eq value transducer-stream-start)
        (push value xs))
      (setq value (funcall stream)))
    (reverse xs)))

(defun transducer-stream-append (&rest streams)
  "Append several STREAMS in execution."
  (lexical-let* ((streams streams)
      (stream (car streams)))
    (transducer-stream
     (lambda (&rest args)
       (lexical-let ((value (apply stream args)))
         (while (and (not (null streams))
                   (or (eq value transducer-stream-start)
                      (eq value transducer-stream-stop)))
           (cond
            ((eq value transducer-stream-start)
             (setq value (apply stream args)))
            ((eq value transducer-stream-stop)
             (setq streams (cdr streams))
             (unless (null streams)
               (setq stream (car streams)
                  value (apply stream args))))))
         value)))))

(defun transducer-stream-reducer (stream)
  "A reducer for STREAM constructs."
  (lexical-let ((x 1))
    (transducer-stream
     (lambda () `1nil)
     (lambda (result) nil)
     (lambda (result item) nil))))

(defun transducer-reducer (initial-fn complete-fn step-fn)
  "Create a reducer with an initial seed function INITIAL-FN,
a final function COMPLETE-FN, and a step function STEP-FN."
  (lambda (&rest args)
    (let ((arity (length args)))
      (pcase arity
        (0 (funcall initial-fn))
        (1 (apply complete-fn args))
        (2 (apply step-fn args))))))

(defun transducer-transduce (transducer )
  "meo"
  1)

(defun transducer-mapping (mapper)
  "Map reducer with MAPPER function."
  (lambda (reducer)
    (transducer-reducer
     (lambda () (funcall reducer))
     (lambda (result) (funcall reducer result))
     (lambda (result item) (funcall reducer result (apply mapper item))))))

(defun transducer-filtering (filterer)
  "Filter redcuer with FILTERER predicate."
  (lambda (reducer)
    (transducer-reducer
     (lambda () (funcall reducer))
     (lambda (result) (funcall reducer result))
     (lambda (result item)
       (if (funcall filterer item)
           (funcall reducer result item)
         result)))))

(defun transducer-compose (&rest tns)
  "Compose transducers TNS."
  (lambda (reducer)
    (lexical-let (())
      1)))

(defun transducer-distinct-with (equality)
  "Distinct reducer with EQUALITY predicate."
  nil)

(defun transducer-max-with (maxer)
  "Max reducer with MAXER function.")

(provide 'transducer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer.el ends here
