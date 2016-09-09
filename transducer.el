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
  nil)


(defun transducer-map (mapper)
  "Map reducer with MAPPER function."
  (lambda (reducer)
    nil))
(defun transducer-distinct-with (equality)
  "Distinct reducer with EQUALITY predicate."
  nil)

(defun transducer-max-with (maxer)
  "Max reducer with MAXER function.")

(provide 'transducer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer.el ends here
