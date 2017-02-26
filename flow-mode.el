;;; flow-mode.el --- Flow type mode based on web-mode. -*- lexical-binding: t -*-

;; This source code is licensed under the BSD-style license found in
;; the LICENSE file in the root directory of this source tree.

;; Version: 0.1
;; URL: https://github.com/an-sh/flow-mode

;; Package-Requires: ((emacs "25.1") (web-mode "14.1"))

;;; Commentary:

;; Major mode for flowtype.org, derived from web-mode.  Essentially a
;; rewrite of an official flow-for-emacs snippet into a standalone
;; mode with an improved usability.

;;

;;; Code:

(require 'web-mode)
(require 'xref)

;;;###autoload
(define-derived-mode
  flow-mode web-mode "Flow"
  "Flow mode")

(defcustom flow-binary "flow"
  "Flow executable."
  :group 'flow-mode
  :type 'string)

(defcustom flow-jump-other-window nil
  "Jump to definitions in other window."
  :group 'flow-mode
  :type 'boolean)

(defun flow-column-at-pos (position)
  "Column number at position.
POSITION point"
  (save-excursion (goto-char position) (current-column)))

(defun flow-region ()
  "Format region data."
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (format ":%d:%d,%d:%d"
                (line-number-at-pos begin)
                (flow-column-at-pos begin)
                (line-number-at-pos end)
                (flow-column-at-pos end)))
    ""))

(defmacro flow-with-flow (&rest body)
  "With flow.
BODY progn"
  `(progn
     (shell-command-to-string (format "%s start" flow-binary))
     ,@body))

(defmacro flow-region-command (file-sym region-sym &rest body)
  "Flow command on a region.
FILE-SYM symbol
REGION-SYM symbol
BODY progn"
  (declare (indent defun))
  `(flow-with-flow
    (let ((,file-sym (buffer-file-name))
          (,region-sym (flow-region)))
      (switch-to-buffer-other-window "*Shell Command Output*")
      ,@body)))

(defun flow-status ()
  "Initialize flow."
  (interactive)
  (flow-with-flow
   (compile (format "%s status --from emacs; exit 0" flow-binary))
   (switch-to-buffer-other-window "*compilation*")))

(defun flow-suggest ()
  "Fill types."
  (interactive)
  (flow-region-command file region
    (shell-command
     (format "%s suggest %s%s" flow-binary file region))
    (diff-mode)
    (read-only-mode)))

(defun flow-coverage ()
  "Show coverage."
  (interactive)
  (flow-region-command file region
    (shell-command
     (format "%s coverage %s%s" flow-binary file region))
    (compilation-mode)
    (read-only-mode)))

(defun flow-type-at-pos ()
  "Show type at position."
  (interactive)
  (flow-with-flow
   (let* ((file (buffer-file-name))
          (line (line-number-at-pos))
          (col (current-column))
          (buffer (current-buffer))
          (type (shell-command-to-string
                 (format "%s type-at-pos %s %d %d"
                         flow-binary
                         file
                         line
                         (1+ col)))))
     (message "%s" (car (split-string type "\n"))))))

(defun flow-jump-to-definition ()
  "Jump to definition."
  (interactive)
  (flow-with-flow
   (let* ((file (buffer-file-name))
          (line (line-number-at-pos))
          (col (current-column))
          (buffer (current-buffer))
          (location (json-read-from-string
                     (shell-command-to-string
                      (format "%s get-def --json %s %d %d"
                              flow-binary
                              file
                              line
                              (1+ col)))))
          (path (alist-get 'path location))
          (line (alist-get 'line location))
          (offset-in-line (alist-get 'start location)))
     (if (> (length path) 0)
         (progn
           (xref-push-marker-stack)
           (funcall (if flow-jump-other-window #'find-file-other-window #'find-file) path)
           (goto-line line)
           (when (> offset-in-line 0)
             (forward-char (1- offset-in-line))))
       (message "Not found")))))

(defvar flow-mode-map (make-sparse-keymap)
  "Keymap for ‘flow-mode’.")

(define-key flow-mode-map (kbd "M-.") 'flow-jump-to-definition)

(define-key flow-mode-map (kbd "C-c C-c s") 'flow-status)
(define-key flow-mode-map (kbd "C-c C-c c") 'flow-coverage)
(define-key flow-mode-map (kbd "C-c C-c t") 'flow-type-at-pos)
(define-key flow-mode-map (kbd "C-c C-c f") 'flow-suggest)

(define-key flow-mode-map [menu-bar flow-mode]
  (cons "Flow" flow-mode-map))

(define-key flow-mode-map [menu-bar flow-mode flow-mode-s]
  '(menu-item "Flow status" flow-status))

(define-key flow-mode-map [menu-bar flow-mode flow-mode-c]
  '(menu-item "Flow coverage" flow-coverage))

(define-key flow-mode-map [menu-bar flow-mode flow-mode-t]
  '(menu-item "Type at point" flow-type-at-pos))

(define-key flow-mode-map [menu-bar flow-mode flow-mode-f]
  '(menu-item "Type suggestions" flow-suggest))

(add-hook 'kill-emacs-hook
          (lambda ()
            (shell-command (format "%s stop" flow-binary))))

(provide 'flow-mode)
;;; flow-mode.el ends here
