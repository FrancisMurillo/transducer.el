;;; transducer-stream-test.el --- Transducer stream test
;;
;; Filename: transducer-stream-test.el
;; Description:
;; Author: Francis Murillo
;; Maintainer:
;; Created: Fri Sep  9 12:07:52 2016 (+0800)
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

(ert-deftest transducer-stream-test/base ()
  (lexical-let* ((index 0)
      (stream
       (transducer-stream
        (lambda ()
          (if (= index 10)
              transducer-stream-stop
            (prog1
                index
              (setq index (1+ index))))))))
    (should (eq transducer-stream-start (funcall stream)))

    (should (= 0 (funcall stream)))
    (should (= 1 (funcall stream)))
    (should (= 2 (funcall stream)))
    (should (= 3 (funcall stream)))
    (should (= 4 (funcall stream)))
    (should (= 5 (funcall stream)))
    (should (= 6 (funcall stream)))
    (should (= 7 (funcall stream)))
    (should (= 8 (funcall stream)))
    (should (= 9 (funcall stream)))

    (should (eq transducer-stream-stop (funcall stream)))))

(ert-deftest transducer-stream-test/from-list ()
  (let* ((first "first")
      (second 'a)
      (third -1)
      (xs (list first second third))
      (stream (transducer-stream-from-list xs)))
    (should (eq transducer-stream-start (funcall stream)))

    (should (string-equal first (funcall stream)))
    (should (eq second (funcall stream)))
    (should (= third (funcall stream)))

    (should (eq transducer-stream-stop (funcall stream))))

  (let ((stream (transducer-stream-from-list (list))))
    (should (eq transducer-stream-start (funcall stream)))
    (should (eq transducer-stream-stop (funcall stream)))))

(ert-deftest transducer-stream-test/list-identity ()
  (let ((xs (list 1 2 3)))
    (should
     (list-equal
      #'=
      xs
      (transducer-stream-to-list (transducer-stream-from-list xs))))
    (should
     (list-equal
      #'=
      nil
      (transducer-stream-to-list (transducer-stream-from-list nil))))))

(ert-deftest transducer-stream-test/append ()
  (let* ((first (list 1 2 3))
      (second (list))
      (third (list 4 5))
      (all (append first second third))
      (stream
       (transducer-stream-append
        (transducer-stream-from-list first)
        (transducer-stream-from-list second)
        (transducer-stream-from-list third)))
      (values (transducer-stream-to-list stream)))
    (should
     (list-equal
      #'=
      all
      values))))


(provide 'transducer-stream-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transducer-stream-test.el ends here
