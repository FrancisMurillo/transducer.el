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

(ert-deftest transducer-reducer-test/alister ()
  (let* ((lister (transducer-alist-reducer))
      (first-pair (cons 1 2))
      (second-pair (cons 3 4))
      (pairs (list first-pair second-pair)))
    (should
     (list-equal
      (-partial #'pair-equal #'=)
      pairs
      (-reduce-from
       lister
       (funcall lister)
       (unroll-pairs pairs))))
    (should
     (list-equal
      (-partial #'pair-equal #'=)
      pairs
      (-reduce-from
       lister
       (funcall lister)
       (append
        (unroll-pairs pairs)
        (list "should not be consumed"))))))
  (let* ((lister (transducer-alist-reducer))
      (pairs (list)))
    (should
     (list-equal
      (-partial #'pair-equal #'=)
      pairs
      (-reduce-from
       lister
       (funcall lister)
       (unroll-pairs pairs))))))


(provide 'transducer-reducer-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer-reducer-test.el ends here
