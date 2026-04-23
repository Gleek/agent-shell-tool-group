;;; agent-shell-tool-group.el --- Group consecutive tool calls in agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Umar Ahmad

;; Author: Umar Ahmad
;; URL: https://github.com/Gleek/agent-shell-tool-group
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.1"))
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Groups consecutive completed tool calls in agent-shell buffers
;; under a single collapsible header.
;;
;; Example: Instead of showing each tool call individually:
;;
;;   ▶  done  find  grep -n "foo" bar.el
;;   ▶  done  find  grep -n "baz" qux.el
;;   ▶  done  read  bar.el (10 - 30)
;;
;; They are grouped under:
;;
;;   ▶ Tool Calls (2 x find, read)
;;
;; Expanding the group reveals the individual tool calls.
;;
;; Usage:
;;   (require 'agent-shell-tool-group)
;;   (agent-shell-tool-group-mode 1)

;;; Code:

(require 'agent-shell)
(require 'agent-shell-ui)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'text-property-search)

(defcustom agent-shell-tool-group-min-count 2
  "Minimum number of consecutive completed tool calls to form a group."
  :type 'integer
  :group 'agent-shell)

(defcustom agent-shell-tool-group-label-format "Tool Calls (%s)"
  "Format string for group header.
%s is replaced with a summary like \"3 x find, read\"."
  :type 'string
  :group 'agent-shell)

(defvar-local agent-shell-tool-group--subscription nil
  "Event subscription token for this buffer.")

(defvar-local agent-shell-tool-group--overlays nil
  "List of active group overlays in this buffer.")

(defvar-local agent-shell-tool-group--grouped-ids nil
  "Set of qualified IDs already grouped, to avoid re-grouping.")

;;; Summary label generation

(defun agent-shell-tool-group--make-summary (kinds)
  "Generate summary string from KINDS list.
E.g. (\"find\" \"find\" \"find\" \"read\") => \"3 x find, read\"."
  (let ((counts nil))
    (dolist (kind kinds)
      (if-let ((entry (assoc kind counts)))
          (setcdr entry (1+ (cdr entry)))
        (push (cons kind 1) counts)))
    (setq counts (nreverse counts))
    (mapconcat (lambda (entry)
                 (if (> (cdr entry) 1)
                     (format "%d x %s" (cdr entry) (car entry))
                   (car entry)))
               counts ", ")))

;;; Fragment inspection

(defun agent-shell-tool-group--find-fragment (qualified-id)
  "Find the buffer range of fragment with QUALIFIED-ID.
Returns alist with :start and :end, or nil."
  (save-mark-and-excursion
    (goto-char (point-max))
    (when-let ((match (text-property-search-backward
                       'agent-shell-ui-state nil
                       (lambda (_ state)
                         (equal (map-elt state :qualified-id) qualified-id))
                       t)))
      (list (cons :start (prop-match-beginning match))
            (cons :end (prop-match-end match))))))

(defun agent-shell-tool-group--extract-kind (qualified-id)
  "Extract the tool kind from the label-left text of fragment QUALIFIED-ID.
Returns a string like \"read\", \"find\", \"edit\", etc."
  (when-let* ((range (agent-shell-tool-group--find-fragment qualified-id)))
    (save-mark-and-excursion
      (save-restriction
        (narrow-to-region (map-elt range :start) (map-elt range :end))
        (goto-char (point-min))
        (when-let ((label-range (agent-shell-ui--nearest-range-matching-property
                                 :property 'agent-shell-ui-section :value 'label-left)))
          (let ((text (buffer-substring-no-properties
                       (map-elt label-range :start)
                       (map-elt label-range :end))))
            ;; Label text is like "[ done ] [ read ]" — extract last bracketed word
            (if (string-match "\\[\\s-*\\([a-zA-Z_-]+\\)\\s-*\\]\\s-*$" text)
                (match-string 1 text)
              (car (last (split-string (string-trim text)))))))))))

;;; Overlay-based grouping

(defun agent-shell-tool-group--create-group (qualified-ids kinds)
  "Create a collapsible group overlay spanning QUALIFIED-IDS with KINDS."
  (let ((ranges (delq nil (mapcar #'agent-shell-tool-group--find-fragment
                                  qualified-ids))))
    (when (>= (length ranges) agent-shell-tool-group-min-count)
      (let* ((group-start (apply #'min (mapcar (lambda (r) (map-elt r :start)) ranges)))
             (group-end (save-mark-and-excursion
                          (goto-char (apply #'max (mapcar (lambda (r) (map-elt r :end)) ranges)))
                          (skip-chars-forward " \t\n")
                          (point)))
             (summary (agent-shell-tool-group--make-summary kinds))
             (count (length qualified-ids))
             (ov (make-overlay group-start group-end nil t nil)))
        (overlay-put ov 'agent-shell-tool-group t)
        (overlay-put ov 'agent-shell-tool-group-summary summary)
        (overlay-put ov 'agent-shell-tool-group-count count)
        (overlay-put ov 'evaporate t)
        (agent-shell-tool-group--set-collapsed ov t)
        (push ov agent-shell-tool-group--overlays)
        ov))))

(defun agent-shell-tool-group--make-keymap (ov)
  "Create keymap for toggling group overlay OV."
  (let ((km (make-sparse-keymap))
        (toggle (lambda ()
                  (interactive)
                  (agent-shell-tool-group--toggle ov))))
    (define-key km [mouse-1]
                (lambda (_event) (interactive "e")
                  (agent-shell-tool-group--toggle ov)))
    (define-key km (kbd "RET") toggle)
    (define-key km (kbd "TAB") toggle)
    km))

(defun agent-shell-tool-group--make-header-text (ov collapsed)
  "Build propertized header string for overlay OV.
COLLAPSED controls the indicator character."
  (let* ((summary (overlay-get ov 'agent-shell-tool-group-summary))
         (count (overlay-get ov 'agent-shell-tool-group-count))
         (indicator (if collapsed "▶ " "▼ "))
         (keymap (agent-shell-tool-group--make-keymap ov))
         (label-format (if (display-graphic-p) " %s " "[%s]"))
         (status-badge (agent-shell--add-text-properties
                        (propertize (format label-format "done")
                                    'font-lock-face 'default)
                        'font-lock-face (list 'success '(:inverse-video t))))
         (box-color (face-foreground 'success nil t))
         (count-badge (agent-shell--add-text-properties
                       (propertize (format label-format (format "%d" count))
                                   'font-lock-face 'default)
                       'font-lock-face `((:box (:color ,box-color)))))
         (title (propertize summary 'font-lock-face 'font-lock-doc-markup-face)))
    (propertize (concat indicator status-badge " " count-badge " " title "\n")
                'keymap keymap)))

(defun agent-shell-tool-group--set-collapsed (ov collapsed)
  "Set group overlay OV to COLLAPSED state."
  (let ((header (agent-shell-tool-group--make-header-text ov collapsed)))
    (overlay-put ov 'agent-shell-tool-group-collapsed collapsed)
    (overlay-put ov 'before-string header)
    (overlay-put ov 'invisible collapsed)
    ;; Indent children when expanded to show hierarchy
    (unless collapsed
      (overlay-put ov 'line-prefix "  ")
      (overlay-put ov 'wrap-prefix "  "))
    (when collapsed
      (overlay-put ov 'line-prefix nil)
      (overlay-put ov 'wrap-prefix nil))))

(defun agent-shell-tool-group--toggle (ov)
  "Toggle collapsed state of group overlay OV."
  (when (overlay-buffer ov)
    (let ((collapsed (overlay-get ov 'agent-shell-tool-group-collapsed)))
      (agent-shell-tool-group--set-collapsed ov (not collapsed)))))

;;; Event handling

(defun agent-shell-tool-group--on-event (event)
  "Handle agent-shell EVENT for tool call grouping."
  (let ((event-type (map-elt event :event)))
    (when (memq event-type '(turn-complete prompt-ready))
      (agent-shell-tool-group--scan-and-group))))

(defun agent-shell-tool-group--scan-and-group ()
  "Scan the buffer for consecutive completed tool call fragments and group them."
  (agent-shell-tool-group--scan-and-group-in-buffer (current-buffer))
  (when-let ((viewport-buffer (agent-shell-viewport--buffer
                               :shell-buffer (current-buffer)
                               :existing-only t)))
    (agent-shell-tool-group--scan-and-group-in-buffer viewport-buffer)))

(defun agent-shell-tool-group--scan-and-group-in-buffer (buffer)
  "Scan BUFFER for consecutive tool call fragments and group them."
  (with-current-buffer buffer
    (let ((fragments nil))
      ;; Collect all fragments in buffer order
      (save-excursion
        (goto-char (point-min))
        (let ((match nil))
          (while (setq match (text-property-search-forward
                              'agent-shell-ui-state nil
                              (lambda (_ v) v) t))
            (let* ((pos (prop-match-beginning match))
                   (state (get-text-property pos 'agent-shell-ui-state))
                   (qid (map-elt state :qualified-id))
                   (is-tool-call (and (string-match-p "toolu_" qid)
                                      (not (member qid agent-shell-tool-group--grouped-ids)))))
              (push (cons qid is-tool-call) fragments)))))
      ;; Find consecutive runs of tool call fragments
      (let ((ordered (nreverse fragments))
            (runs nil)
            (current-run nil))
        (dolist (entry ordered)
          (if (cdr entry)
              (push (car entry) current-run)
            (when (>= (length current-run) agent-shell-tool-group-min-count)
              (push (nreverse current-run) runs))
            (setq current-run nil)))
        (when (>= (length current-run) agent-shell-tool-group-min-count)
          (push (nreverse current-run) runs))
        ;; Create groups for each run
        (dolist (run (nreverse runs))
          (let ((kinds (mapcar (lambda (qid)
                                 (or (agent-shell-tool-group--extract-kind qid) "tool"))
                               run)))
            (when (agent-shell-tool-group--create-group run kinds)
              (dolist (qid run)
                (push qid agent-shell-tool-group--grouped-ids)))))))))


;;; Interactive commands

(defun agent-shell-tool-group-ungroup-all ()
  "Remove all tool call groups in the current buffer."
  (interactive)
  (dolist (ov agent-shell-tool-group--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq agent-shell-tool-group--overlays nil))

(defun agent-shell-tool-group-ungroup-at-point ()
  "Remove the tool call group at point."
  (interactive)
  (when-let* ((ov (seq-find (lambda (o)
                              (overlay-get o 'agent-shell-tool-group))
                            (overlays-at (point)))))
    (setq agent-shell-tool-group--overlays
          (delete ov agent-shell-tool-group--overlays))
    (delete-overlay ov)))

;;; Buffer setup

(defun agent-shell-tool-group--setup-buffer ()
  "Set up tool call grouping in the current agent-shell buffer."
  (when (derived-mode-p 'agent-shell-mode)
    (unless agent-shell-tool-group--subscription
      (setq agent-shell-tool-group--subscription
            (agent-shell-subscribe-to
             :shell-buffer (current-buffer)
             :on-event #'agent-shell-tool-group--on-event)))))

(defun agent-shell-tool-group--teardown-buffer ()
  "Remove tool call grouping from the current agent-shell buffer."
  (when agent-shell-tool-group--subscription
    (agent-shell-unsubscribe :subscription agent-shell-tool-group--subscription)
    (setq agent-shell-tool-group--subscription nil))
  (agent-shell-tool-group-ungroup-all)
  (setq agent-shell-tool-group--grouped-ids nil))

;;; Minor mode

;;;###autoload
(define-minor-mode agent-shell-tool-group-mode
  "Group consecutive completed tool calls in agent-shell buffers."
  :global t
  :group 'agent-shell
  (if agent-shell-tool-group-mode
      (progn
        (add-hook 'agent-shell-mode-hook #'agent-shell-tool-group--setup-buffer)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'agent-shell-mode)
              (agent-shell-tool-group--setup-buffer)))))
    (remove-hook 'agent-shell-mode-hook #'agent-shell-tool-group--setup-buffer)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'agent-shell-mode)
          (agent-shell-tool-group--teardown-buffer))))))

(provide 'agent-shell-tool-group)
;;; agent-shell-tool-group.el ends here
