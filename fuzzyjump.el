;;; fuzzyjump.el --- Jump to where you (almost) want
;; -*- coding: utf-8; mode:emacs-lisp -*-

;; Copyright (C) 2008 Tomoya Otake
;; Author: Tomoya Otake <tomoya.ton@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; * Description
;;
;; html-key-chord-mode is minor mode of Emacs that use key-chord.el.
;; This mode can be coded very quickly.
;;
;; * Usage
;;
;; Just put the code like below into your .emacs:
;;
;; (require 'html-key-chord-mode)

;; default keybinds
;; (define-key map "\C-ct" 'quote-line-by)
;; (define-key map "\M-t" 'quote-region-by)
;; (define-key map "\C-cd" 'del-line-tag)
;; (define-key map "\M-d" 'del-region-tag)

;; If you want to change keybind, input your .emacs
;; (define-key hkc-mode-map "\C-t" 'quote-line-by)



(defvar fuzzyjump-mode nil) ; mode 変数。これで状態判定

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
  
  (setq count-char-of-current-line (count-char-of-current-line))
  (cond ((= char 0)
	 (move-to-column 0))
	((= char 9)
	 (move-to-column count-char-of-current-line))
	(t
	 (setq move-char (* (/ count-char-of-current-line 9) char))
	 (move-to-column move-char))))


(defvar fuzzyjump-prefix-key
  "Prefix keystrokes for fuzzyjump minor-mode commands."
  "C-1")

(defvar fuzzyjump-mode-map (make-sparse-keymap "fuzzyjump"))
(defvar fuzzyjump-cmd-map (make-sparse-keymap "fyzzyjump-cmd"))

(defun fuzzyjump-mode-key-set (val)
  (define-key fuzzyjump-mode-map (read-kbd-macro val) fuzzyjump-cmd-map))

(let ((map fuzzyjump-cmd-map))
  (define-key map (kbd "1") (fuzzyn)
  (define-key map (kbd "2") ')
  (define-key map (kbd "3") 'egg-status)
  (define-key map (kbd "4") 'egg-commit-log-edit)
  (define-key map (kbd "5") 'egg-file-ediff)
  (define-key map (kbd "6") 'egg-grep)
  (define-key map (kbd "7") 'egg-file-stage-current-file)
  (define-key map (kbd "8") 'egg-log)
  (define-key map (kbd "9") 'egg-reflog)
  (define-key map (kbd "h") 'egg-file-log)
  (define-key map (kbd "o") 'egg-file-checkout-other-version)
  (define-key map (kbd "s") 'egg-status)
  (define-key map (kbd "u") 'egg-file-cancel-modifications)
  (define-key map (kbd "v") 'egg-next-action)
  (define-key map (kbd "w") 'egg-commit-log-edit)
  (define-key map (kbd "/") 'egg-file-log-pickaxe)
  (define-key map (kbd "=") 'egg-file-diff)
  (define-key map (kbd "~") 'egg-file-version-other-window))


(provide 'fuzzyjump-mode)

;;; fuzzyjump.el ends here
