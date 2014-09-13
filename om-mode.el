;;; om-mode.el --- Insert om component with full lifecycle, and jump
;;; easily between the states.  Also, using abbrev-epand to expand
;;; "om" into a skeleton.

;; Copyright (C) 2014 Daniel Szmulewicz
;; <http://about.me/daniel.szmulewicz>

;; Author: Daniel Szmulewicz <daniel.szmulewicz@gmail.com>

;; Version: 0.8

;;; Commentary:

;;; Legal:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Code:

(define-skeleton insert-om-template
  "Om component skeleton"
  "Name of component: "
  "(defn " str \n
  > "\"Om component for new " str "\""
  \n > "[data owner]"
  \n > "(reify" \n
  '(setq v1 (skeleton-read "Render with state (y/n)?"))
  (when (string= v1 "y")
    (progn (init-state) nil))
  '(indent-according-to-mode)
  "om/IWillMount" \n
  > "(will-mount [_]" \n
  @ - ")" \n >
  "om/IDidMount" \n
  > "(did-mount [_]" \n
  @ - ")" \n >
  "om/IWillUnMount" \n
  > "(will-unmount [_]" \n
  @ - ")" \n >
  "om/IShouldUpdate" \n
  > "(should-update [_ next-props next-state]" \n
  @ - ")" \n >
  "om/IWillReceiveProps" \n
  > "(will-receive-props [_ next-props]" \n
  @ - ")" \n >
  "om/IWillUpdate" \n
  > "(will-update [_ next-props next-state]" \n
  @ - ")" \n >
  "om/IDidUpdate" \n
  > "(did-update [_ prev-props prev-state]" \n
  @ - ")" \n >
  "om/IDisplayName" \n
  > "(display-name [this]" \n
  "\"" str "\")" \n >
  (if (string= v1 "y")
      "om/IRenderState\n(render-state [_ state]"
    "om/IRender\n(render [_]")
  '(indent-according-to-mode)
  \n > @ - ")"
  '(indent-according-to-mode)
  resume: "))")

(define-skeleton init-state
  "init state"
  nil \n "om/IInitState" \n
  > "(init-state [_] {" @ " }"
  \n ")" \n >)

(defvar *skeleton-markers* nil
         "Markers for locations saved in skeleton-positions.")


(defgroup om-mode nil
  "Customization group for `om-mode'."
	:group 'convenience)

(defconst om-mode-keymap (make-sparse-keymap) "Keymap used in palimpsest mode.")

(defcustom om-expand-abbrev "om"
  "This is the abbrev to expand an Om component."
  :group 'om-mode
  :type '(string))

(add-hook 'skeleton-end-hook 'skeleton-end)

(defun skeleton-end ()
  "Called after skeleton insertion and takes over `C-n keybinding."
  (skeleton-make-markers)
  (define-key om-mode-keymap [remap next-line] 'skeleton-next-position)
  (define-key om-mode-keymap [remap previous-line] (lambda () (interactive) (skeleton-next-position 1))))

(defun skeleton-make-markers ()
  "Make markers in skeleton."
  (while *skeleton-markers*
    (set-marker (pop *skeleton-markers*) nil))
  (setq *skeleton-markers*
	(mapcar 'copy-marker (reverse skeleton-positions))))

(defun exit-om-edit ()
  "Reset the variable with Om markers, so that `next line' can be bound to default."
  (interactive)
  (define-key om-mode-keymap [remap next-line] nil)
  (define-key om-mode-keymap [remap previous-line] nil))

(defun skeleton-next-position (&optional reverse)
  "Jump to next position in skeleton.
REVERSE - Jump to previous position in skeleton"
  (interactive "P")
  (let* ((positions (mapcar 'marker-position *skeleton-markers*))
	 (positions (if reverse (reverse positions) positions))
	(comp (if reverse '> '<))
	pos)
    (when positions
      (if (catch 'break
	    (while (setq pos (pop positions))
	      (when (funcall comp (point) pos)
		(throw 'break t))))
	  (goto-char pos)
	(goto-char (marker-position
		    (car *skeleton-markers*)))))))

;; abbrev table for this mode
;;
(defun om-install-abbrevs ()
  "Install default abbrev table for clojure if none is available."
  (if (and (boundp 'clojure-mode-abbrev-table)
 	   (not (equal clojure-mode-abbrev-table (make-abbrev-table))))
      (progn (define-abbrev clojure-mode-abbrev-table om-expand-abbrev "" 'insert-om-template)
	     (setq save-abbrevs 'silently)
	     (message "Clojure abbrevs already exists, om abbrev added to it"))
    (define-abbrev-table 'clojure-mode-abbrev-table om-expand-abbrev "" 'insert-om-template)
    (setq save-abbrevs nil)
    (message "Clojure abbrevs loaded.")))

(unless noninteractive
  (om-install-abbrevs))

;;;###autoload
(define-minor-mode om-mode
  "Toggle om mode.
Interactively with no argument, this command toggles the mode.
to show buffer size and position in mode-line.  You can customize
this minor mode, see option `om-mode'.."
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Om"
  ;; The minor mode bindings.
  :keymap om-mode-keymap
  :global nil
  :group 'om-mode)

(add-hook 'clojure-mode-hook 'om-mode)

(provide 'om-mode)
;;; om-mode.el ends here
