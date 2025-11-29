;;; blame-reveal-recursive.el --- Recursive blame extension -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.5
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


;;;; Helper Functions

(defun blame-reveal--build-blame-command-args-for-revision (revision start-line end-line relative-file)
  "Build git blame command arguments for specific REVISION.
Similar to `blame-reveal--build-blame-command-args' but for arbitrary revisions."
  (let ((args (list "blame" "--porcelain")))
    ;; Add line range if provided
    (when (and start-line end-line)
      (setq args (append args (list "-L" (format "%d,%d" start-line end-line)))))
    ;; Add revision (unless it's uncommitted)
    (unless (eq revision 'uncommitted)
      (setq args (append args (list revision))))
    ;; Add file at the end
    (append args (list "--" relative-file))))

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
          (zerop (call-process "git" nil t nil "cat-file" "-e"
                               (concat revision ":" relative-file))))))))

(defun blame-reveal--is-initial-commit-p (commit-hash)
  "Check if COMMIT-HASH is an initial commit (has no parent).
Returns t if it's the first commit in the repository."
  (let ((process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat"
                                         process-environment))))
    (with-temp-buffer
      (not (zerop (call-process "git" nil t nil "rev-parse"
                                "--verify" "--quiet"
                                (concat commit-hash "^")))))))

(defun blame-reveal--get-base-commit-from-parent-ref (revision)
  "Extract base commit hash from parent reference REVISION.
For example, 'abc123^' returns 'abc123'.
Returns nil if REVISION is not a parent reference."
  (when (string-match "^\\([a-f0-9]+\\)\\^+$" revision)
    (match-string 1 revision)))

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


;;;; Data Structure Reset

(defun blame-reveal--reset-data-structures (blame-data)
  "Reset all data structures with new BLAME-DATA."
  (setq blame-reveal--blame-data blame-data)
  (setq blame-reveal--blame-data-range nil)
  (setq blame-reveal--commit-info (make-hash-table :test 'equal))
  (setq blame-reveal--color-map (make-hash-table :test 'equal))
  (setq blame-reveal--timestamps nil)
  (setq blame-reveal--recent-commits nil)
  (setq blame-reveal--all-commits-loaded nil))


;;;; Error Handling

(defun blame-reveal--handle-recursive-load-error (revision file)
  "Handle error when loading blame at REVISION for FILE."
  (let* ((base-commit (blame-reveal--get-base-commit-from-parent-ref revision))
         (is-initial (and base-commit
                          (blame-reveal--is-initial-commit-p base-commit)))
         (file-exists (and base-commit
                           (blame-reveal--file-exists-at-revision-p base-commit file))))

    (cond
     (is-initial
      (blame-reveal--state-error
       (format "Reached initial commit %s" (substring base-commit 0 8)))
      (message "Reached initial commit %s - this is the repository's first commit"
               (substring base-commit 0 8)))

     ((and base-commit (not file-exists))
      (blame-reveal--state-error
       (format "File first added at %s" (substring base-commit 0 8)))
      (message "Reached the commit where this file was first added (%s)"
               (substring base-commit 0 8)))

     (t
      (blame-reveal--state-error (format "No blame data at %s" revision))
      (message "No blame data at revision %s" revision)))

    ;; Restore state from stack
    (when blame-reveal--blame-stack
      (let ((state (pop blame-reveal--blame-stack)))
        (blame-reveal--restore-state state)))))


;;;; Asynchronous Recursive Blame

(defun blame-reveal--load-blame-at-revision-async (revision)
  "Load blame data at REVISION asynchronously.
Always loads complete file for proper recursive blame navigation."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file)))
         (source-buffer (current-buffer)))

    (unless git-root
      (user-error "File is not in a git repository"))

    ;; 使用状态机启动
    (unless (blame-reveal--state-start 'recursive 'async
                                       (list :revision revision))
      (user-error "Cannot start recursive blame: state machine busy"))

    (let* ((default-directory git-root)
           (relative-file (file-relative-name file git-root))
           (temp-buffer (generate-new-buffer " *blame-recursive*"))
           (process-environment (cons "GIT_PAGER=cat"
                                      (cons "PAGER=cat"
                                            process-environment)))
           (args (blame-reveal--build-blame-command-args-for-revision
                  revision nil nil relative-file)))

      (message "Git command: git %s" (mapconcat 'identity args " "))

      ;; 设置异步资源到状态机
      (blame-reveal--state-set-async-resources
       (make-process
        :name "blame-recursive"
        :buffer temp-buffer
        :command (cons "git" args)
        :sentinel (blame-reveal--make-async-sentinel
                   source-buffer temp-buffer
                   (lambda (temp-buf)
                     (blame-reveal--handle-recursive-load-complete
                      temp-buf revision file))
                   (lambda ()
                     (blame-reveal--handle-recursive-load-error revision file)))
        :noquery t)
       temp-buffer))))

(defun blame-reveal--handle-recursive-load-complete (temp-buffer revision file)
  "Handle completion of recursive blame loading from TEMP-BUFFER."
  ;; 检查状态机状态
  (unless (and (eq blame-reveal--state-status 'loading)
               (eq blame-reveal--state-operation 'recursive))
    (message "[State] Unexpected recursive complete in state %s/%s"
             blame-reveal--state-status blame-reveal--state-operation)
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))
    (cl-return-from blame-reveal--handle-recursive-load-complete nil))

  (unwind-protect
      (when (buffer-live-p temp-buffer)
        (blame-reveal--state-transition 'processing)

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
                (blame-reveal--reset-data-structures blame-data)

                ;; Load commit info and render
                (blame-reveal--state-transition 'rendering)
                (blame-reveal--load-commits-incrementally)
                (blame-reveal--smooth-transition-render)

                (message "Loaded blame at %s (%d lines)"
                         (or blame-reveal--revision-display revision)
                         (length blame-data))

                ;; 成功完成
                (blame-reveal--state-complete))

            ;; No blame data
            (blame-reveal--state-error "Failed to parse blame data")
            (blame-reveal--handle-recursive-load-error revision file))))

    ;; Cleanup - 清理临时 buffer（状态机会清理 process）
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))))


;;;; Synchronous Recursive Blame

(defun blame-reveal--get-blame-data-at-revision (revision file &optional range)
  "Get git blame data for FILE at REVISION synchronously.
If RANGE is (START-LINE . END-LINE), only blame that range.
Otherwise blame entire file.
REVISION can be a commit hash or 'uncommitted for working tree."
  (let ((git-root (vc-git-root file)))
    (when git-root
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat"
                                             process-environment))))
        (with-temp-buffer
          (let* ((args (blame-reveal--build-blame-command-args-for-revision
                        revision
                        (when range (car range))
                        (when range (cdr range))
                        relative-file))
                 (exit-code (apply #'call-process "git" nil t nil args)))
            (message "Git command: git %s (exit: %d)" (mapconcat 'identity args " ") exit-code)
            (when (not (zerop exit-code))
              (message "Git error output: %s" (buffer-string)))
            (when (zerop exit-code)
              (blame-reveal--parse-blame-output (current-buffer)))))))))

(defun blame-reveal--load-blame-at-revision-sync (revision)
  "Load blame data at REVISION synchronously.
Always loads complete file for proper recursive blame navigation."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "No file associated with buffer"))

    ;; 使用状态机启动
    (unless (blame-reveal--state-start 'recursive 'sync
                                       (list :revision revision))
      (user-error "Cannot start recursive blame: state machine busy"))

    (condition-case err
        (let ((blame-data (blame-reveal--get-blame-data-at-revision revision file)))

          (if blame-data
              (progn
                (blame-reveal--state-transition 'processing)

                ;; Update current state
                (setq blame-reveal--current-revision revision)
                (setq blame-reveal--revision-display
                      (if (eq revision 'uncommitted)
                          "Working Tree"
                        (blame-reveal--get-commit-short-info revision)))

                ;; Reset data structures
                (blame-reveal--reset-data-structures blame-data)

                ;; Load commit info and render
                (blame-reveal--state-transition 'rendering)
                (blame-reveal--load-commits-incrementally)
                (blame-reveal--smooth-transition-render)

                (message "Loaded blame at %s (%d lines)"
                         (or blame-reveal--revision-display revision)
                         (length blame-data))

                ;; 成功完成
                (blame-reveal--state-complete))

            ;; No blame data - determine why
            (let* ((base-commit (blame-reveal--get-base-commit-from-parent-ref revision))
                   (is-initial (and base-commit
                                    (blame-reveal--is-initial-commit-p base-commit)))
                   (file-exists (and base-commit
                                     (blame-reveal--file-exists-at-revision-p base-commit file))))

              (cond
               (is-initial
                (blame-reveal--state-error
                 (format "Reached initial commit %s" (substring base-commit 0 8)))
                (user-error "Reached initial commit %s - this is the repository's first commit"
                            (substring base-commit 0 8)))

               ((and base-commit (not file-exists))
                (blame-reveal--state-error
                 (format "File first added at %s" (substring base-commit 0 8)))
                (user-error "Reached the commit where this file was first added (%s)"
                            (substring base-commit 0 8)))

               (t
                (blame-reveal--state-error
                 (format "No blame data at %s" revision))
                (user-error "Failed to get blame data at revision %s" revision))))))

      (error
       (blame-reveal--state-error (error-message-string err))
       (message "Error during recursive blame: %s" (error-message-string err))
       (signal (car err) (cdr err))))))


;;;; Unified Interface

(defun blame-reveal--load-blame-at-revision (revision)
  "Load blame data at REVISION using sync or async based on configuration.
Always loads complete file for proper recursive blame navigation."
  (if (blame-reveal--should-use-async-p)
      (blame-reveal--load-blame-at-revision-async revision)
    (blame-reveal--load-blame-at-revision-sync revision)))


;;;; Smooth Transition Rendering

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
           (rendered-lines (make-hash-table :test 'eql)))

      ;; Ensure commit info is loaded for visible blocks
      (dolist (block blocks)
        (let ((commit-hash (nth 1 block)))
          (blame-reveal--ensure-commit-info commit-hash)))

      ;; Update recent commits based on what's loaded so far
      (blame-reveal--update-recent-commits)

      ;; Render blocks using unified overlay system
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

                ;; Render each line in visible range
                (let ((render-start (max block-start start-line))
                      (render-end (min block-end end-line)))
                  (dotimes (i (- render-end render-start -1))
                    (let ((line-num (+ render-start i)))
                      ;; Use v2 function for unified management
                      (blame-reveal--create-fringe-overlay
                       line-num color commit-hash)
                      (puthash line-num t rendered-lines)))))))))

      ;; Delete unused overlays (not in rendered-lines)
      (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
        (when (overlay-buffer overlay)
          (let ((line (plist-get (blame-reveal--get-overlay-metadata overlay) :line)))
            (when (and line (not (gethash line rendered-lines)))
              (blame-reveal--unregister-overlay overlay)))))

      ;; Re-trigger header update
      (blame-reveal--update-header))))


;;;; State Management

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

  ;; Smooth transition render
  (blame-reveal--smooth-transition-render)

  ;; Restore cursor position
  (goto-char (plist-get state :point))
  (set-window-start nil (plist-get state :window-start)))


;;;; Interactive Commands

;;;###autoload
(defun blame-reveal-blame-recursively ()
  "Recursively blame the commit before the one at current line.
This 'time travels' to see who modified this line before the current commit."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  ;; 检查状态机是否忙碌
  (when (blame-reveal--state-is-busy-p)
    (user-error "Please wait for current operation to complete"))

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
      (message "Recursive blame: %s -> %s" (substring commit-hash 0 8) parent-commit)
      (condition-case err
          (blame-reveal--load-blame-at-revision parent-commit)
        (error
         ;; Restore state on error (state machine already handled cleanup)
         (message "Error in recursive blame: %s" (error-message-string err))
         (let ((state (pop blame-reveal--blame-stack)))
           (when state
             (blame-reveal--restore-state state)))
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun blame-reveal-blame-back ()
  "Go back to previous blame state in the recursion stack."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  ;; 可以随时返回，即使在加载中（取消当前加载）
  (when (blame-reveal--state-is-busy-p)
    (blame-reveal--state-cancel "user navigated back"))

  (if (null blame-reveal--blame-stack)
      (message "Already at the newest revision")
    (let ((state (pop blame-reveal--blame-stack)))
      (blame-reveal--restore-state state)
      (message "Returned to %s"
               (or (plist-get state :revision-display) "working tree")))))

;;;###autoload
(defun blame-reveal-blame-at-revision (revision)
  "Show blame at a specific REVISION (interactive)."
  (interactive "sBlame at revision (commit/branch/tag): ")
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))

  ;; 检查状态机是否忙碌
  (when (blame-reveal--state-is-busy-p)
    (user-error "Please wait for current operation to complete"))

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

  (message "Blame at revision: %s" revision)
  (condition-case err
      (blame-reveal--load-blame-at-revision revision)
    (error
     ;; Restore state on error
     (message "Error in blame at revision: %s" (error-message-string err))
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

  ;; 取消当前操作（如果有）
  (when (blame-reveal--state-is-busy-p)
    (blame-reveal--state-cancel "reset to HEAD"))

  (cond
   ;; Case 1: In recursive blame mode
   (blame-reveal--current-revision
    (setq blame-reveal--blame-stack nil)
    (setq blame-reveal--current-revision nil)
    (setq blame-reveal--revision-display nil)
    (setq blame-reveal--auto-days-cache nil)
    (blame-reveal--full-update)
    (message "Reset to HEAD"))

   ;; Case 2: Already at HEAD, but has residual stack
   (blame-reveal--blame-stack
    (setq blame-reveal--blame-stack nil)
    (message "Cleared blame stack"))

   ;; Case 3: Already at HEAD, no stack
   (t
    (message "Already at HEAD"))))


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
