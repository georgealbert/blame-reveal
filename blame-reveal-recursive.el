;;; blame-reveal-recursive.el --- Recursive blame extension -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.4
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
;; - Async support: uses same async setting as main package
;; - Smart error detection: distinguishes between initial commit and real errors
;;
;; Usage:
;;   b - Recursively blame (go to parent commit)
;;   p/^ - Go back to previous revision
;;   g - Blame at specific revision
;;   r - Reset to HEAD

;;; Code:

(require 'blame-reveal)


;;;; Async Loading State for Recursive Blame

(defvar-local blame-reveal--recursive-load-process nil
  "Background process for loading blame at specific revision.")

(defvar-local blame-reveal--recursive-load-buffer nil
  "Temporary buffer for recursive load output.")


;;;; Helper Functions

(defun blame-reveal--file-exists-at-revision-p (revision file)
  "Check if FILE exists at REVISION.
Returns t if file exists, nil otherwise."
  (let ((git-root (vc-git-root file))
        (process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat"
                                         process-environment))))
    (when git-root
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root)))
        (with-temp-buffer
          ;; Try to show the file at that revision
          ;; Exit code 0 means file exists
          (zerop (call-process "git" nil t nil "cat-file" "-e"
                               (concat revision ":" relative-file))))))))

(defun blame-reveal--is-initial-commit-p (commit-hash)
  "Check if COMMIT-HASH is an initial commit (has no parent).
Returns t if it's the first commit in the repository."
  (let ((process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat"
                                         process-environment))))
    (with-temp-buffer
      ;; Try to get parent commit
      ;; Exit code 128 means no parent (initial commit)
      (not (zerop (call-process "git" nil t nil "rev-parse"
                                "--verify" "--quiet"
                                (concat commit-hash "^")))))))

(defun blame-reveal--get-base-commit-from-parent-ref (revision)
  "Extract base commit hash from parent reference REVISION.
For example, 'abc123^' returns 'abc123'.
Returns nil if REVISION is not a parent reference."
  (when (string-match "^\\([a-f0-9]+\\)\\^+$" revision)
    (match-string 1 revision)))


;;;; Synchronous Recursive Blame Data Functions

(defun blame-reveal--get-blame-data-at-revision (revision file &optional range)
  "Get git blame data for FILE at REVISION synchronously.
If RANGE is (START-LINE . END-LINE), only blame that range.
Otherwise blame entire file.
REVISION can be a commit hash or 'uncommitted for working tree."
  (let ((git-root (vc-git-root file))
        (blame-data nil))
    (when git-root
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat"
                                             process-environment))))
        (with-temp-buffer
          (let ((exit-code
                 (if range
                     ;; Blame specific range
                     (if (eq revision 'uncommitted)
                         (call-process "git" nil t nil "blame" "--porcelain"
                                       "-L" (format "%d,%d" (car range) (cdr range))
                                       relative-file)
                       (call-process "git" nil t nil "blame" "--porcelain"
                                     "-L" (format "%d,%d" (car range) (cdr range))
                                     revision "--" relative-file))
                   ;; Blame entire file
                   (if (eq revision 'uncommitted)
                       (call-process "git" nil t nil "blame" "--porcelain" relative-file)
                     (call-process "git" nil t nil "blame" "--porcelain"
                                   revision "--" relative-file)))))
            (when (zerop exit-code)
              (setq blame-data (blame-reveal--parse-blame-output (current-buffer))))))))
    blame-data))

(defun blame-reveal--get-commit-short-info (commit-hash)
  "Get short info string for COMMIT-HASH (for display)."
  (let ((process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat"
                                         process-environment))))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "show"
                                 "--no-patch"
                                 "--format=%h %s"
                                 commit-hash))
        (string-trim (buffer-string))))))


;;;; Asynchronous Recursive Blame Data Functions

(defun blame-reveal--load-blame-at-revision-async (revision)
  "Load blame data at REVISION asynchronously and update display.
Always loads complete file for proper recursive blame navigation."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file))))

    (unless git-root
      (user-error "File is not in a git repository"))

    ;; Cancel any existing process
    (when blame-reveal--recursive-load-process
      (delete-process blame-reveal--recursive-load-process)
      (setq blame-reveal--recursive-load-process nil))

    (when blame-reveal--recursive-load-buffer
      (kill-buffer blame-reveal--recursive-load-buffer)
      (setq blame-reveal--recursive-load-buffer nil))

    (let* ((default-directory git-root)
           (relative-file (file-relative-name file git-root))
           (source-buffer (current-buffer))
           (temp-buffer (generate-new-buffer " *blame-recursive-load*"))
           (process-environment (cons "GIT_PAGER=cat"
                                      (cons "PAGER=cat"
                                            process-environment))))

      (setq blame-reveal--recursive-load-buffer temp-buffer)

      (message "Loading blame at %s..."
               (if (eq revision 'uncommitted) "working tree" revision))

      ;; Build command args
      (let ((args (list "blame" "--porcelain")))

        ;; Add revision
        (unless (eq revision 'uncommitted)
          (setq args (append args (list revision "--"))))

        ;; Add file
        (setq args (append args (list relative-file)))

        ;; Start async process
        (setq blame-reveal--recursive-load-process
              (make-process
               :name "blame-recursive-load"
               :buffer temp-buffer
               :command (cons "git" args)
               :sentinel
               `(lambda (proc event)
                  (cond
                   ((string-match-p "finished" event)
                    (when (buffer-live-p ,source-buffer)
                      (with-current-buffer ,source-buffer
                        (blame-reveal--handle-recursive-load-complete
                         ,temp-buffer ,revision ,file))))

                   ((string-match-p "exited abnormally" event)
                    ;; Check why it failed
                    (let* ((base-commit (blame-reveal--get-base-commit-from-parent-ref ,revision))
                           (is-initial (and base-commit
                                            (blame-reveal--is-initial-commit-p base-commit)))
                           (file-exists (and base-commit
                                             (blame-reveal--file-exists-at-revision-p base-commit ,file))))

                      (cond
                       ;; Case 1: Initial commit (no parent)
                       (is-initial
                        (message "Reached initial commit %s - this is the repository's first commit"
                                 (substring base-commit 0 8)))

                       ;; Case 2: File doesn't exist at parent revision
                       ((and base-commit (not file-exists))
                        (message "Reached the commit where this file was first added (%s)"
                                 (substring base-commit 0 8)))

                       ;; Case 3: Real error
                       (t
                        (message "Failed to load blame at revision %s: %s"
                                 ,revision (string-trim event))
                        (when (buffer-live-p ,temp-buffer)
                          (with-current-buffer ,temp-buffer
                            (let ((error-msg (buffer-string)))
                              (unless (string-empty-p error-msg)
                                (message "Git error: %s" error-msg)))))))

                      ;; Restore state in all cases
                      (when (buffer-live-p ,source-buffer)
                        (with-current-buffer ,source-buffer
                          (when blame-reveal--blame-stack
                            (let ((state (pop blame-reveal--blame-stack)))
                              (blame-reveal--restore-state state))))))

                    ;; Cleanup
                    (when (buffer-live-p ,temp-buffer)
                      (kill-buffer ,temp-buffer))
                    (when (buffer-live-p ,source-buffer)
                      (with-current-buffer ,source-buffer
                        (setq blame-reveal--recursive-load-buffer nil)
                        (setq blame-reveal--recursive-load-process nil))))))
               :noquery t))))))

(defun blame-reveal--handle-recursive-load-complete (temp-buffer revision file)
  "Handle completion of recursive blame loading from TEMP-BUFFER."
  (when (buffer-live-p temp-buffer)
    (unwind-protect
        (let ((blame-data (blame-reveal--parse-blame-output temp-buffer)))

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
                (setq blame-reveal--blame-data-range nil)
                (setq blame-reveal--commit-info (make-hash-table :test 'equal))
                (setq blame-reveal--color-map (make-hash-table :test 'equal))
                (setq blame-reveal--timestamps nil)
                (setq blame-reveal--recent-commits nil)
                (setq blame-reveal--all-commits-loaded nil)

                ;; Load commit info for visible area
                (blame-reveal--load-commits-incrementally)

                ;; Smooth transition: fade out old overlays, render new ones
                (blame-reveal--smooth-transition-render)

                (message "Loaded blame at %s (%d lines)"
                         (or blame-reveal--revision-display revision)
                         (length blame-data)))

            ;; No blame data - shouldn't happen if process exited successfully
            ;; but handle it anyway
            (let* ((base-commit (blame-reveal--get-base-commit-from-parent-ref revision))
                   (is-initial (and base-commit
                                    (blame-reveal--is-initial-commit-p base-commit)))
                   (file-exists (and base-commit
                                     (blame-reveal--file-exists-at-revision-p base-commit file))))

              (cond
               (is-initial
                (message "Reached initial commit %s - this is the repository's first commit"
                         (substring base-commit 0 8)))

               ((and base-commit (not file-exists))
                (message "Reached the commit where this file was first added (%s)"
                         (substring base-commit 0 8)))

               (t
                (message "No blame data at revision %s" revision)))

              ;; Restore state
              (when blame-reveal--blame-stack
                (let ((state (pop blame-reveal--blame-stack)))
                  (blame-reveal--restore-state state))))))

      ;; Cleanup
      (kill-buffer temp-buffer)
      (setq blame-reveal--recursive-load-buffer nil)
      (setq blame-reveal--recursive-load-process nil))))

(defun blame-reveal--smooth-transition-render ()
  "Render new blame data by reusing existing overlays when possible.
This minimizes visual disruption during recursive blame."
  (when blame-reveal--blame-data
    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (blocks (blame-reveal--find-block-boundaries
                    blame-reveal--blame-data
                    start-line
                    end-line))
           (existing-overlay-map (make-hash-table :test 'equal))
           (used-overlays (make-hash-table :test 'eq))
           (new-overlays nil))

      ;; Index existing overlays by line number
      (dolist (ov blame-reveal--overlays)
        (when (overlay-buffer ov)
          (let ((pos (overlay-start ov)))
            (when pos
              (let ((line (line-number-at-pos pos)))
                (puthash line ov existing-overlay-map))))))

      ;; Render blocks, reusing overlays when possible
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block)))

          ;; Skip uncommitted changes unless explicitly enabled
          (unless (and (blame-reveal--is-uncommitted-p commit-hash)
                       (not blame-reveal-show-uncommitted-fringe))
            ;; Render permanent fringe for recent commits
            (when (blame-reveal--should-render-commit commit-hash)
              (let ((color (blame-reveal--get-commit-color commit-hash))
                    (block-end (+ block-start block-length -1)))

                ;; Render each line in block
                (let ((render-start (max block-start start-line))
                      (render-end (min block-end end-line)))
                  (dotimes (i (- render-end render-start -1))
                    (let* ((line-num (+ render-start i))
                           (existing-ov (gethash line-num existing-overlay-map)))

                      (if existing-ov
                          ;; Reuse existing overlay, just update its properties
                          (progn
                            (let ((fringe-face (blame-reveal--ensure-fringe-face color)))
                              (overlay-put existing-ov 'blame-reveal-commit commit-hash)
                              (overlay-put existing-ov 'before-string
                                           (propertize "!" 'display
                                                       (list blame-reveal-style
                                                             'blame-reveal-full
                                                             fringe-face))))
                            (puthash existing-ov t used-overlays)
                            (push existing-ov new-overlays))

                        ;; Create new overlay
                        (when-let ((new-ov (blame-reveal--create-fringe-overlay
                                            line-num color commit-hash)))
                          (push new-ov new-overlays)))))))))))

      ;; Delete unused old overlays
      (dolist (ov blame-reveal--overlays)
        (unless (gethash ov used-overlays)
          (when (overlay-buffer ov)
            (delete-overlay ov))))

      ;; Update overlay list
      (setq blame-reveal--overlays new-overlays)

      ;; Re-trigger header update
      (blame-reveal--update-header))))


;;;; Synchronous Recursive Blame Functions

(defun blame-reveal--load-blame-at-revision-sync (revision)
  "Load blame data at REVISION synchronously and update display.
Always loads complete file for proper recursive blame navigation."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "No file associated with buffer"))

    (message "Loading blame at %s..."
             (if (eq revision 'uncommitted) "working tree" revision))

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
            (setq blame-reveal--blame-data-range nil)
            (setq blame-reveal--commit-info (make-hash-table :test 'equal))
            (setq blame-reveal--color-map (make-hash-table :test 'equal))
            (setq blame-reveal--timestamps nil)
            (setq blame-reveal--recent-commits nil)
            (setq blame-reveal--all-commits-loaded nil)

            ;; Load commit info for visible area
            (blame-reveal--load-commits-incrementally)

            ;; Smooth transition render
            (blame-reveal--smooth-transition-render)

            (message "Loaded blame at %s (%d lines)"
                     (or blame-reveal--revision-display revision)
                     (length blame-data)))

        ;; No blame data - determine why
        (let* ((base-commit (blame-reveal--get-base-commit-from-parent-ref revision))
               (is-initial (and base-commit
                                (blame-reveal--is-initial-commit-p base-commit)))
               (file-exists (and base-commit
                                 (blame-reveal--file-exists-at-revision-p base-commit file))))

          (cond
           ;; Case 1: Initial commit (no parent)
           (is-initial
            (user-error "Reached initial commit %s - this is the repository's first commit"
                        (substring base-commit 0 8)))

           ;; Case 2: File doesn't exist at parent revision (file was added in current commit)
           ((and base-commit (not file-exists))
            (user-error "Reached the commit where this file was first added (%s)"
                        (substring base-commit 0 8)))

           ;; Case 3: Real error
           (t
            (user-error "Failed to get blame data at revision %s" revision))))))))


;;;; Unified Interface

(defun blame-reveal--load-blame-at-revision (revision)
  "Load blame data at REVISION using sync or async based on configuration.
Always loads complete file for proper recursive blame navigation."
  (if (blame-reveal--should-use-async-p)
      (blame-reveal--load-blame-at-revision-async revision)
    (blame-reveal--load-blame-at-revision-sync revision)))


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

  ;; Smooth transition render instead of clearing all
  (blame-reveal--smooth-transition-render)

  ;; Restore cursor position
  (goto-char (plist-get state :point))
  (set-window-start nil (plist-get state :window-start)))


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
  (let ((process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat"
                                         process-environment))))
    (with-temp-buffer
      (unless (zerop (call-process "git" nil t nil "rev-parse" "--verify"
                                   (concat revision "^{commit}")))
        (user-error "Invalid revision: %s" revision))))

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

    ;; Cleanup recursive load process
    (when blame-reveal--recursive-load-process
      (delete-process blame-reveal--recursive-load-process)
      (setq blame-reveal--recursive-load-process nil))

    (when blame-reveal--recursive-load-buffer
      (kill-buffer blame-reveal--recursive-load-buffer)
      (setq blame-reveal--recursive-load-buffer nil))

    (blame-reveal--full-update)
    (message "Reset to HEAD")))


;;;; Cleanup Function

(defun blame-reveal--cleanup-recursive-processes ()
  "Cleanup recursive blame async processes and buffers."
  (when blame-reveal--recursive-load-process
    (delete-process blame-reveal--recursive-load-process)
    (setq blame-reveal--recursive-load-process nil))

  (when blame-reveal--recursive-load-buffer
    (kill-buffer blame-reveal--recursive-load-buffer)
    (setq blame-reveal--recursive-load-buffer nil)))

;; Add cleanup to mode disable
(add-hook 'blame-reveal-mode-hook
          (lambda ()
            (unless blame-reveal-mode
              (blame-reveal--cleanup-recursive-processes))))


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
