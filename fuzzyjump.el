;;; fuzzyjump.el --- Jump to where you (almost) want
;; -*- coding: utf-8; mode:emacs-lisp -*-
;;
;; Copyright (C) 2008 Tomoya Otake
;;
;; Author: Tomoya Otake <tomoya.ton@gmail.com>
;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; * Description
;;
;; fuzzyjump-mode is minor mode of Emacs that use fuzzyjump.el.
;; Jump to where you (almost) want.

;; * Usage
;;
;; To use this extension, locate this file to load-path directory,
;; and add the following code to your .emacs.
;;
;; (require 'fuzzyjump-mode)
;; (fuzzyjump-mode t)
;;
;; Toggle this mode M-x fuzzyjump-mode
;;
;; Default prefix key "C-c C-j"
;; You type "C-c C-j" after "1-0" or "q-p" or "a-l" or "z-." fuzzyjump!
;;
;; Enjoy!

(defvar fuzzyjump-mode nil) ; mode 変数。これで状態判定

(defvar fuzzyjump-prefix-key "C-c C-j"
  "Prefix keystrokes for fuzzyjump minor-mode commands.")


;; mode 行の設定。なくてもOK。
(if (not (assq 'fuzzyjump-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(fuzzyjump-mode " fuzzy")
                minor-mode-alist)))

;; mode の On / Off をするための関数
(defun fuzzyjump-mode (arg)
  "fuzzyjump-mode minor-mode"
  (interactive "P")
  ;; mode variable settings
  (setq fuzzyjump-mode (if arg
			   (> (prefix-numeric-value arg) 0)
			 (not fuzzyjump-mode)))
  (cond (fuzzyjump-mode
	 (message "fuzzyjump-mode on"))
	(t
	 (message "fuzzyjump-mode off"))))

(defun count-char-of-current-line ()
  "カレント行の文字数をカウント"
  (let ((start (funcall (lambda ()
		 (beginning-of-line)
		 (point))))
	(end (funcall (lambda ()
	       (end-of-line)
	       (point)))))
    (- end start)))

(defun fuzzyjump (line char)
  "うそ臭い fuzzy jump を実現します"
  (cond ((= line 0)
	 (move-to-window-line 0))
	(t
	 (setq move-line (* (/ (window-height) 4) line))
	 (move-to-window-line move-line)))
  
(setq count-char-of-current-line
      (cond ((> (count-char-of-current-line) (window-width)) (- (window-width) 1))
	    (t (count-char-of-current-line))))
  
  (cond ((= char 0)
	 (move-to-column 0))
	((= char 9)
	 (move-to-column count-char-of-current-line))
	(t
	 (setq move-char (* (/ count-char-of-current-line 9) char))
	 (move-to-column move-char))))

(defvar fuzzyjump-mode-map (make-sparse-keymap "fuzzyjump"))
(defvar fuzzyjump-cmd-map (make-sparse-keymap "fyzzyjump-cmd"))

(defun fuzzyjump-mode-key-prefix-set (val)
  (define-key fuzzyjump-mode-map (read-kbd-macro val) fuzzyjump-cmd-map))

(let ((map fuzzyjump-cmd-map))
  (define-key map (kbd "1") (lambda () (interactive) (fuzzyjump 0 0)))
  (define-key map (kbd "2") (lambda () (interactive) (fuzzyjump 0 1)))
  (define-key map (kbd "3") (lambda () (interactive) (fuzzyjump 0 2)))
  (define-key map (kbd "4") (lambda () (interactive) (fuzzyjump 0 3)))
  (define-key map (kbd "5") (lambda () (interactive) (fuzzyjump 0 4)))
  (define-key map (kbd "6") (lambda () (interactive) (fuzzyjump 0 5)))
  (define-key map (kbd "7") (lambda () (interactive) (fuzzyjump 0 6)))
  (define-key map (kbd "8") (lambda () (interactive) (fuzzyjump 0 7)))
  (define-key map (kbd "9") (lambda () (interactive) (fuzzyjump 0 8)))
  (define-key map (kbd "0") (lambda () (interactive) (fuzzyjump 0 9)))
  (define-key map (kbd "q") (lambda () (interactive) (fuzzyjump 1 0)))
  (define-key map (kbd "w") (lambda () (interactive) (fuzzyjump 1 1)))
  (define-key map (kbd "e") (lambda () (interactive) (fuzzyjump 1 2)))
  (define-key map (kbd "r") (lambda () (interactive) (fuzzyjump 1 3)))
  (define-key map (kbd "t") (lambda () (interactive) (fuzzyjump 1 4)))
  (define-key map (kbd "y") (lambda () (interactive) (fuzzyjump 1 5)))
  (define-key map (kbd "u") (lambda () (interactive) (fuzzyjump 1 6)))
  (define-key map (kbd "i") (lambda () (interactive) (fuzzyjump 1 7)))
  (define-key map (kbd "o") (lambda () (interactive) (fuzzyjump 1 8)))
  (define-key map (kbd "p") (lambda () (interactive) (fuzzyjump 1 9)))
  (define-key map (kbd "a") (lambda () (interactive) (fuzzyjump 2 0)))
  (define-key map (kbd "s") (lambda () (interactive) (fuzzyjump 2 1)))
  (define-key map (kbd "d") (lambda () (interactive) (fuzzyjump 2 2)))
  (define-key map (kbd "f") (lambda () (interactive) (fuzzyjump 2 3)))
  (define-key map (kbd "g") (lambda () (interactive) (fuzzyjump 2 4)))
  (define-key map (kbd "h") (lambda () (interactive) (fuzzyjump 2 5)))
  (define-key map (kbd "j") (lambda () (interactive) (fuzzyjump 2 6)))
  (define-key map (kbd "k") (lambda () (interactive) (fuzzyjump 2 7)))
  (define-key map (kbd "l") (lambda () (interactive) (fuzzyjump 2 9)))
  (define-key map (kbd "z") (lambda () (interactive) (fuzzyjump 3 0)))
  (define-key map (kbd "x") (lambda () (interactive) (fuzzyjump 3 1)))
  (define-key map (kbd "c") (lambda () (interactive) (fuzzyjump 3 2)))
  (define-key map (kbd "v") (lambda () (interactive) (fuzzyjump 3 3)))
  (define-key map (kbd "b") (lambda () (interactive) (fuzzyjump 3 4)))
  (define-key map (kbd "n") (lambda () (interactive) (fuzzyjump 3 5)))
  (define-key map (kbd "m") (lambda () (interactive) (fuzzyjump 3 6)))
  (define-key map (kbd ",") (lambda () (interactive) (fuzzyjump 3 7)))
  (define-key map (kbd ".") (lambda () (interactive) (fuzzyjump 3 9))))

(fuzzyjump-mode-key-prefix-set fuzzyjump-prefix-key)

(or (assq 'fuzzyjump-mode-map minor-mode-map-alist)
    (push (cons 'fuzzyjump-mode fuzzyjump-mode-map) minor-mode-map-alist))


(provide 'fuzzyjump)

;;; fuzzyjump.el ends here
