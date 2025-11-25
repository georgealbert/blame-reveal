;;; blame-reveal-recursive.el --- Recursive blame extension -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Recursive blame navigation for blame-reveal.el
;;
;; This extension allows you to navigate through the history of a line
;; by recursively blaming previous revisions.
;;
;; Features:
;; - Recursive blame: jump to parent commit
;; - Blame stack: navigate back through history
;; - Arbitrary revision blame: blame at any commit/branch/tag
;; - Mode line indicator: shows current revision
;;
;; Usage:
;;   b - Recursively blame (go to parent commit)
;;   p/^ - Go back to previous revision
;;   g - Blame at specific revision
;;   r - Reset to HEAD

;;; Code:

(require 'blame-reveal)


;;;; Recursive Blame Data Functions

(defun blame-reveal--get-blame-data-at-revision (revision file &optional range)
  "Get git blame data for FILE at REVISION.
If RANGE is (START-LINE . END-LINE), only blame that range.
Otherwise blame entire file.
REVISION can be a commit hash or 'uncommitted for working tree."
  (let ((blame-data nil))
    (with-temp-buffer
      (let ((exit-code
             (if range
                 ;; Blame specific range
                 (if (eq revision 'uncommitted)
                     (call-process "git" nil t nil "blame" "--porcelain"
                                   "-L" (format "%d,%d" (car range) (cdr range))
                                   file)
                   (call-process "git" nil t nil "blame" "--porcelain"
                                 "-L" (format "%d,%d" (car range) (cdr range))
                                 revision "--" file))
               ;; Blame entire file
               (if (eq revision 'uncommitted)
                   (call-process "git" nil t nil "blame" "--porcelain" file)
                 (call-process "git" nil t nil "blame" "--porcelain"
                               revision "--" file)))))
        (when (zerop exit-code)
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-f0-9]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
            (let ((commit-hash (match-string 1))
                  (line-number (string-to-number (match-string 3))))
              (push (cons line-number commit-hash) blame-data))))))
    (nreverse blame-data)))

(defun blame-reveal--get-commit-short-info (commit-hash)
  "Get short info string for COMMIT-HASH (for display)."
  (with-temp-buffer
    (when (zerop (call-process "git" nil t nil "show"
                               "--no-patch"
                               "--format=%h %s"
                               commit-hash))
      (string-trim (buffer-string)))))


;;;; State Management Functions

(defun blame-reveal--save-current-state ()
  "Save current blame state to stack."
  (push (list :revision blame-reveal--current-revision
              :revision-display blame-reveal--revision-display
              :blame-data blame-reveal--blame-data
              :blame-data-range blame-reveal--blame-data-range
              :commit-info (copy-hash-table blame-reveal--commit-info)
              :color-map (copy-hash-table blame-reveal--color-map)
              :timestamps blame-reveal--timestamps
              :recent-commits blame-reveal--recent-commits
              :window-start (window-start)
              :point (point))
        blame-reveal--blame-stack))

(defun blame-reveal--restore-state (state)
  "Restore blame state from STATE."
  (setq blame-reveal--current-revision (plist-get state :revision))
  (setq blame-reveal--revision-display (plist-get state :revision-display))
  (setq blame-reveal--blame-data (plist-get state :blame-data))
  (setq blame-reveal--blame-data-range (plist-get state :blame-data-range))
  (setq blame-reveal--commit-info (plist-get state :commit-info))
  (setq blame-reveal--color-map (plist-get state :color-map))
  (setq blame-reveal--timestamps (plist-get state :timestamps))
  (setq blame-reveal--recent-commits (plist-get state :recent-commits))

  ;; Re-render
  (blame-reveal--render-visible-region)

  ;; Restore cursor position
  (goto-char (plist-get state :point))
  (set-window-start nil (plist-get state :window-start)))


;;;; Core Recursive Blame Functions

(defun blame-reveal--load-blame-at-revision (revision)
  "Load blame data at REVISION and update display.
Always loads complete file for proper recursive blame navigation."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "No file associated with buffer"))

    (message "Loading blame at %s..."
             (if (eq revision 'uncommitted) "working tree" revision))

    ;; Recursive blame always loads full file (ignore lazy loading)
    ;; This ensures users can navigate anywhere in the file history
    (let ((blame-data (blame-reveal--get-blame-data-at-revision revision file)))
      (if blame-data
          (progn
            ;; Update current state
            (setq blame-reveal--current-revision revision)
            (setq blame-reveal--revision-display
                  (if (eq revision 'uncommitted)
                      "Working Tree"
                    (blame-reveal--get-commit-short-info revision)))

            ;; Reset data structures
            (setq blame-reveal--blame-data blame-data)
            (setq blame-reveal--blame-data-range nil)  ; Full file loaded
            (setq blame-reveal--commit-info (make-hash-table :test 'equal))
            (setq blame-reveal--color-map (make-hash-table :test 'equal))
            (setq blame-reveal--timestamps nil)
            (setq blame-reveal--recent-commits nil)
            (setq blame-reveal--all-commits-loaded nil)

            ;; Load commit info for visible area
            (blame-reveal--load-commits-incrementally)

            ;; Render
            (blame-reveal--render-visible-region)

            (message "Loaded blame at %s (%d lines)"
                     (or blame-reveal--revision-display revision)
                     (length blame-data)))
        (user-error "Failed to get blame data at revision %s" revision)))))


;;;; Interactive Commands

;;;###autoload
(defun blame-reveal-blame-recursively ()
  "Recursively blame the commit before the one at current line.
This 'time travels' to see who modified this line before the current commit.

Example:
  Current: Line 50 modified by Alice (commit abc123)
  Press 'b': Jump to abc123^ to see who modified it before Alice
  Press 'b' again: Continue going back in history

This is useful for:
  - Finding the original author of a line
  - Understanding how code evolved over time
  - Tracking down when a bug was introduced"
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  (let* ((current-block (blame-reveal--get-current-block))
         (commit-hash (car current-block)))

    (unless commit-hash
      (user-error "No commit at current line"))

    (when (blame-reveal--is-uncommitted-p commit-hash)
      (user-error "Cannot recursively blame uncommitted changes"))

    ;; Save current state
    (blame-reveal--save-current-state)

    ;; Jump to parent commit
    (let ((parent-commit (concat commit-hash "^")))
      (condition-case err
          (blame-reveal--load-blame-at-revision parent-commit)
        (error
         ;; Restore state on error
         (let ((state (pop blame-reveal--blame-stack)))
           (when state
             (blame-reveal--restore-state state)))
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun blame-reveal-blame-back ()
  "Go back to previous blame state in the recursion stack.
Press '^' or 'p' to undo recursive blame and return to newer revision."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  (if (null blame-reveal--blame-stack)
      (message "Already at the newest revision")
    (let ((state (pop blame-reveal--blame-stack)))
      (blame-reveal--restore-state state)
      (message "Returned to %s"
               (or (plist-get state :revision-display) "working tree")))))

;;;###autoload
(defun blame-reveal-blame-at-revision (revision)
  "Show blame at a specific REVISION (interactive).
REVISION can be:
  - Commit hash (e.g., abc123)
  - Branch name (e.g., main, develop)
  - Tag name (e.g., v1.0.0)
  - Relative reference (e.g., HEAD~3, main~5)"
  (interactive "sBlame at revision (commit/branch/tag): ")
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  (when (string-empty-p (string-trim revision))
    (user-error "Revision cannot be empty"))

  ;; Verify revision is valid
  (with-temp-buffer
    (unless (zerop (call-process "git" nil t nil "rev-parse" "--verify"
                                 (concat revision "^{commit}")))
      (user-error "Invalid revision: %s" revision)))

  ;; Save current state
  (blame-reveal--save-current-state)

  (condition-case err
      (blame-reveal--load-blame-at-revision revision)
    (error
     ;; Restore state on error
     (let ((state (pop blame-reveal--blame-stack)))
       (when state
         (blame-reveal--restore-state state)))
     (signal (car err) (cdr err)))))

;;;###autoload
(defun blame-reveal-reset-to-head ()
  "Reset blame to HEAD (newest revision), clearing the stack."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  (when blame-reveal--current-revision
    (setq blame-reveal--blame-stack nil)
    (setq blame-reveal--current-revision nil)
    (setq blame-reveal--revision-display nil)
    (setq blame-reveal--auto-days-cache nil)
    (blame-reveal--full-update)
    (message "Reset to HEAD")))


;;;; Utility Functions

;; Helper function for Emacs versions that don't have copy-hash-table
(unless (fboundp 'copy-hash-table)
  (defun copy-hash-table (table)
    "Make a copy of hash TABLE."
    (let ((new-table (make-hash-table
                      :test (hash-table-test table)
                      :size (hash-table-size table))))
      (maphash (lambda (key value)
                 (puthash key value new-table))
               table)
      new-table)))

(provide 'blame-reveal-recursive)
;;; blame-reveal-recursive.el ends here
