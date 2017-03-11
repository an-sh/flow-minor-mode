;;; flow-mode.el --- Flow type mode based on web-mode. -*- lexical-binding: t -*-

;; This source code is licensed under the BSD-style license found in
;; the LICENSE file in the root directory of this source tree.

;; Version: 0.1
;; URL: https://github.com/an-sh/flow-mode

;; Package-Requires: ((emacs "25.1") (web-mode "14.1"))

;;; Commentary:

;; Minor mode for flowtype.org, derived from web-mode.  Essentially a
;; rewrite of an official flow-for-emacs snippet into a standalone
;; mode with an improved usability.
;;
;; To enable this mode, enable it in your preferred javascript mode's
;; hooks:
;;
;;   (add-hook 'js2-mode-hook 'flow-enable-automatically)
;;
;; This will enable flow-minor-mode for a file only when there is a
;; "// @flow" declaration at the first line. If you wish to enable
;; flow-minor-mode for all javascript files, use this instead:
;;
;;  (add-hook 'js2-hook 'flow-minor-mode)
;;
;;; Code:

(require 'xref)
(require 'json)

(defconst flow-buffer "*Flow Output*")

(defcustom flow-default-binary "flow"
  "Flow executable to use when no project-specific binary is found."
  :group 'flow-minor-mode
  :type 'string)

(defcustom flow-jump-other-window nil
  "Jump to definitions in other window."
  :group 'flow-minor-mode
  :type 'boolean)

(defcustom flow-stop-server-on-exit t
  "Stop flow server when Emacs exits."
  :group 'flow-minor-mode
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

(defun flow-binary ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (flow (and root
                    (expand-file-name "node_modules/.bin/flow"
                                      root))))
    (if (and flow (file-executable-p flow))
        flow
      flow-default-binary)))

(defun flow-cmd (&rest args)
  "Run flow with arguments, outputs to flow buffer.
ARGS"
  (apply #'call-process (flow-binary) nil flow-buffer t args))

(defun flow-cmd-ignore-output (&rest args)
  "Run flow with arguments, ignore output.
ARGS"
  (apply #'call-process (flow-binary) nil nil nil args))

(defun flow-cmd-to-string (&rest args)
  "Run flow with arguments, outputs to string.
ARGS"
  (with-temp-buffer
    (apply #'call-process (flow-binary) nil t nil args)
    (buffer-string)))

(defmacro flow-with-flow (&rest body)
  "With flow.
BODY progn"
  `(progn
     (flow-cmd-ignore-output "start")
     ,@body))

(defmacro flow-region-command (region-sym &rest body)
  "Flow command on a region.
REGION-SYM symbol
BODY progn"
  (declare (indent defun))
  `(flow-with-flow
    (let ((,region-sym (concat (buffer-file-name) (flow-region))))
      (switch-to-buffer-other-window flow-buffer)
      (setf buffer-read-only nil)
      (erase-buffer)
      ,@body)))

(defun flow-status ()
  "Show errors."
  (interactive)
  (flow-region-command region
    (flow-cmd "status" "--from" "emacs")
    (compilation-mode)
    (setf buffer-read-only t)))

(defun flow-suggest ()
  "Fill types."
  (interactive)
  (flow-region-command region
    (flow-cmd "suggest" region)
    (diff-mode)
    (setf buffer-read-only t)))

(defun flow-coverage ()
  "Show coverage."
  (interactive)
  (flow-region-command region
    (message "%s" region)
    (flow-cmd "coverage" region)
    (compilation-mode)
    (setf buffer-read-only t)))

(defun flow-type-at-pos ()
  "Show type at position."
  (interactive)
  (flow-with-flow
   (let* ((file (buffer-file-name))
          (line (number-to-string (line-number-at-pos)))
          (col (number-to-string (1+ (current-column))))
          (type (flow-cmd-to-string "type-at-pos" file line col)))
     (message "%s" (car (split-string type "\n"))))))

(defun flow-jump-to-definition ()
  "Jump to definition."
  (interactive)
  (flow-with-flow
   (let* ((file (buffer-file-name))
          (line (number-to-string (line-number-at-pos)))
          (col (number-to-string (1+ (current-column))))
          (location (json-read-from-string
                     (flow-cmd-to-string "get-def" "--json" file line col)))
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
  "Keymap for ‘flow-minor-mode’.")

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

(defun flow-stop-flow-server ()
  "Stop flow hook."
  (if flow-stop-server-on-exit (ignore-errors (flow-cmd-ignore-output "stop"))))

(add-hook 'kill-emacs-hook 'flow-stop-flow-server t)

;;;###autoload
(define-minor-mode flow-minor-mode
  "Flow mode"
  nil " Flow" flow-mode-map)

(defun flow-tag-present-p ()
  "Return true if the '// @flow' tag is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (or (looking-at "//+[ ]*@flow")
        (looking-at "/\\**[ ]*@flow"))))

;;;###autoload
(defun flow-enable-automatically ()
  (when (flow-tag-present-p)
    (flow-minor-mode +1)))

(provide 'flow-mode)
;;; flow-mode.el ends here
