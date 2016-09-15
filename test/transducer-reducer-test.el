;;; transducer-reducer-test.el --- Test for transducer reducers
;;
;; Filename: transducer-reducer-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Fri Sep  9 15:31:03 2016 (+0800)
;; Version:
;; Package-Requires: ()
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

(ert-deftest transducer-reducer-test/base ()
  (let ((reductor
       (transducer-reducer
        (lambda () 'start)
        (lambda (result) 'complete)
        (lambda (result item) 'step))))
    (should (eq 'start (funcall reductor)))
    (should (eq 'complete (funcall reductor nil)))
    (should (eq 'step (funcall reductor nil nil)))))

(ert-deftest transducer-reducer-test/lister ()
  (let ((lister (transducer-list-reducer))
      (xs (list 1 2 3 4 5)))
    (should
     (list-equal
      #'=
      xs
      (-reduce-from
       lister
       (funcall lister)
       xs))))

  (let ((lister (transducer-list-reducer))
      (xs (list)))
    (should
     (list-equal
      #'=
      xs
      (-reduce-from
       lister
       (funcall lister)
       xs)))))

(ert-deftest transducer-reducer-test/plister ()
  (let* ((lister (transducer-plist-reducer))
      (symbolizer
       (lambda (n)
         (intern (concat ":" (number-to-string n)))))
      (mapper (lambda (n) (cons (funcall symbolizer n) n)))
      (xs (list 1 2 3 4 5))
      (xps (-map mapper xs))
      (yps (-reduce-from
            lister
            (funcall lister)
            xps)))
    (-each xs
      (lambda (x)
        (= (plist-get yps (funcall symbolizer x)) x))))
  (let* ((lister (transducer-plist-reducer))
      (symbolizer
       (lambda (n)
         (intern (concat ":" (number-to-string n)))))
      (mapper (lambda (n) (cons (funcall symbolizer n) n)))
      (xs (list))
      (xps (-map mapper xs))
      (yps (-reduce-from
            lister
            (funcall lister)
            xps)))
    (-each xs
      (lambda (x)
        (= (plist-get yps (funcall symbolizer x)) x)))))

(provide 'transducer-reducer-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer-reducer-test.el ends here
