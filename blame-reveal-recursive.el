;;; blame-reveal-recursive.el --- Recursive blame navigation -*- lexical-binding: t; -*-

;; Author: Lucius Chen
;; Version: 0.5
;; Package-Requires: ((emacs "27.1"))
;; Keywords: git, vc, convenience

;;; Commentary:
;; Recursive blame navigation extension for blame-reveal.
;;
;; Features:
;; - Navigate through line history by recursively blaming previous revisions
;; - Blame stack for navigating back through history
;; - Arbitrary revision blame (any commit/branch/tag)
;; - Mode line indicator showing current revision
;; - Async support using main package settings
;; - Smart error detection (initial commit vs real errors)
;;
;; Keybindings:
;;   b   - Recursively blame (go to parent commit)
;;   p/^ - Go back to previous revision
;;   g   - Blame at specific revision
;;   r   - Reset to HEAD

;;; Code:

(require 'blame-reveal)

;;; Helper Functions

(defun blame-reveal--build-blame-command-args-for-revision (revision start-line end-line relative-file)
  "Build git blame arguments for REVISION.
START-LINE and END-LINE specify optional range.
RELATIVE-FILE is path relative to git root."
  (let ((args (list "blame" "--porcelain")))
    (when (and start-line end-line)
      (setq args (append args (list "-L" (format "%d,%d" start-line end-line)))))
    (unless (eq revision 'uncommitted)
      (setq args (append args (list revision))))
    (append args (list "--" relative-file))))

(defun blame-reveal--file-exists-at-revision-p (revision file)
  "Check if FILE exists at REVISION."
  (let ((git-root (vc-git-root file))
        (process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat" process-environment))))
    (when git-root
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root)))
        (with-temp-buffer
          (zerop (call-process "git" nil t nil "cat-file" "-e"
                               (concat revision ":" relative-file))))))))

(defun blame-reveal--is-initial-commit-p (commit-hash)
  "Check if COMMIT-HASH is an initial commit (no parent)."
  (let ((process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat" process-environment))))
    (with-temp-buffer
      (not (zerop (call-process "git" nil t nil "rev-parse"
                                "--verify" "--quiet"
                                (concat commit-hash "^")))))))

(defun blame-reveal--get-base-commit-from-parent-ref (revision)
  "Extract base commit hash from parent reference REVISION.
For example, 'abc123^' returns 'abc123'."
  (when (string-match "^\\([a-f0-9]+\\)\\^+$" revision)
    (match-string 1 revision)))

(defun blame-reveal--get-commit-short-info (commit-hash)
  "Get short info string for COMMIT-HASH (for display)."
  (let ((process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat" process-environment))))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "show"
                                 "--no-patch" "--format=%h %s" commit-hash))
        (string-trim (buffer-string))))))

(defun blame-reveal--has-move-copy-info-p (commit-hash)
  "Check if COMMIT-HASH has move/copy metadata available."
  (and blame-reveal--move-copy-metadata
       (hash-table-p blame-reveal--move-copy-metadata)
       (gethash commit-hash blame-reveal--move-copy-metadata)))

(defun blame-reveal--get-previous-location (commit-hash)
  "Get previous file location for COMMIT-HASH if available.
Returns (PREV-FILE . PREV-COMMIT) or nil."
  (when (and blame-reveal--move-copy-metadata
             (hash-table-p blame-reveal--move-copy-metadata))
    (when-let ((metadata (gethash commit-hash blame-reveal--move-copy-metadata)))
      (let ((prev-file (plist-get metadata :previous-file))
            (prev-commit (plist-get metadata :previous-commit)))
        (when (and prev-file prev-commit)
          (cons prev-file prev-commit))))))

(defun blame-reveal--format-previous-info (prev-file prev-commit)
  "Format previous location info for display."
  (format "moved/copied from %s at %s"
          (propertize (file-name-nondirectory prev-file)
                      'face 'font-lock-string-face)
          (propertize (substring prev-commit 0 8)
                      'face 'font-lock-constant-face)))

(defun blame-reveal--handle-possible-move-boundary (commit-hash)
  "Handle case where we might have reached a move/copy boundary."
  (setq blame-reveal--detect-moves t)
  (message "Reloading with move/copy detection...")
  (blame-reveal--full-update))

(defun blame-reveal--handle-boundary-with-previous (commit-hash)
  "Handle boundary when COMMIT-HASH has previous location info.
This means the line was moved/copied and we already have the info."
  (when-let ((prev-loc (blame-reveal--get-previous-location commit-hash)))
    (let* ((prev-file (car prev-loc))
           (prev-commit (cdr prev-loc))
           (msg (format "Line was %s.\nPress 'f' to follow, any other key to stop."
                        (blame-reveal--format-previous-info prev-file prev-commit))))
      (message "%s" msg)
      ;; 设置临时按键绑定
      (let ((follow-key
             (read-char-choice
              (concat msg "\n[f] Follow  [q] Stay: ")
              '(?f ?q))))
        (when (eq follow-key ?f)
          (blame-reveal--follow-to-previous-file prev-file prev-commit))))))

(defun blame-reveal--follow-to-previous-file (file commit)
  "Follow blame to FILE at COMMIT (previous location)."
  (let* ((git-root (vc-git-root (buffer-file-name)))
         (full-path (expand-file-name file git-root)))
    (cond
     ;; 文件仍然存在
     ((file-exists-p full-path)
      (find-file full-path)
      (unless blame-reveal-mode
        (blame-reveal-mode 1))
      (blame-reveal-blame-at-revision commit)
      (message "Following to %s at %s"
               (file-name-nondirectory file)
               (substring commit 0 8)))

     ;; 文件已被删除，但可以查看历史版本
     (t
      (let ((view-anyway
             (y-or-n-p (format "File %s no longer exists. View historical version? "
                               (file-name-nondirectory file)))))
        (if view-anyway
            (blame-reveal--view-file-at-revision file commit git-root)
          (message "Staying at current location")))))))

(defun blame-reveal--view-file-at-revision (file commit git-root)
  "View FILE at COMMIT in a temporary buffer.
GIT-ROOT is the repository root."
  (let* ((default-directory git-root)
         (relative-file (file-relative-name file git-root))
         (buffer-name (format "*%s @ %s*"
                              (file-name-nondirectory file)
                              (substring commit 0 8)))
         (temp-buffer (get-buffer-create buffer-name)))
    (with-current-buffer temp-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (condition-case err
            (if (zerop (call-process "git" nil t nil "show"
                                     (concat commit ":" relative-file)))
                (progn
                  (goto-char (point-min))
                  (set-auto-mode)
                  (view-mode 1)
                  (setq buffer-read-only t)
                  (message "Viewing %s at %s (read-only)"
                           (file-name-nondirectory file)
                           (substring commit 0 8)))
              (error "Failed to retrieve file content from git"))
          (error
           (erase-buffer)
           (insert (format "Error retrieving file: %s\n\n" (error-message-string err)))
           (insert (format "Command: git show %s:%s\n" commit relative-file))
           (insert (format "Working directory: %s\n" default-directory))
           (message "Failed to view file: %s" (error-message-string err))))))
    (pop-to-buffer temp-buffer)))

;;; Data Structure Management

(defun blame-reveal--reset-data-structures (blame-data)
  "Reset all data structures with new BLAME-DATA."
  (setq blame-reveal--blame-data blame-data)
  (setq blame-reveal--blame-data-range nil)
  (setq blame-reveal--commit-info (make-hash-table :test 'equal))
  (setq blame-reveal--color-map (make-hash-table :test 'equal))
  (setq blame-reveal--timestamps nil)
  (setq blame-reveal--recent-commits nil)
  (setq blame-reveal--all-commits-loaded nil))

;;; Error Handling

(defun blame-reveal--should-suggest-move-detection-p ()
  "Check if we should suggest enabling move/copy detection.
Always returns t when move detection is disabled, as we can't know
if there's move/copy history without trying."
  (not blame-reveal--detect-moves))

(defun blame-reveal--handle-recursive-load-error (revision file)
  "Handle error when loading blame at REVISION for FILE."
  (let* ((base-commit (blame-reveal--get-base-commit-from-parent-ref revision))
         (is-initial (and base-commit
                          (blame-reveal--is-initial-commit-p base-commit)))
         (file-exists (and base-commit
                           (blame-reveal--file-exists-at-revision-p base-commit file))))

    ;; 首先恢复到上一个状态
    (condition-case restore-err
        (when blame-reveal--blame-stack
          (let ((state (pop blame-reveal--blame-stack)))
            (blame-reveal--restore-state state)))
      (error
       (message "Failed to restore state: %s. Resetting to HEAD."
                (error-message-string restore-err))
       (blame-reveal-reset-to-head)
       (cl-return-from blame-reveal--handle-recursive-load-error nil)))

    ;; 然后根据情况处理
    (unwind-protect
        (cond
         ;; 情况 1：真正的初始提交
         (is-initial
          (blame-reveal--state-error
           (format "Reached initial commit %s" (substring base-commit 0 8)))
          (message "Reached initial commit %s - this is where the repository started"
                   (substring base-commit 0 8)))

         ;; 情况 2：文件在这个提交被添加
         ((and base-commit (not file-exists))
          (blame-reveal--state-error
           (format "File first added at %s" (substring base-commit 0 8)))
          (message "File was created at commit %s"
                   (substring base-commit 0 8))
          (when (not blame-reveal--detect-moves)
            (message "Tip: Enable move/copy detection (M) to trace across renames")
            (when (y-or-n-p "Enable move/copy detection and retry? ")
              (setq blame-reveal--detect-moves t)
              (blame-reveal--full-update))))

         ;; 情况 3：其他错误
         (t
          (blame-reveal--state-error (format "No blame data at %s" revision))
          (message "No blame data at revision %s" revision)))

      ;; 确保在所有情况下都重置状态
      (run-with-timer 0.2 nil
                      (lambda ()
                        (when (eq blame-reveal--state-status 'error)
                          (setq blame-reveal--state-status 'idle
                                blame-reveal--state-operation nil
                                blame-reveal--state-mode nil
                                blame-reveal--state-metadata nil
                                blame-reveal--process-id nil)))))))

;;; Asynchronous Loading

(defun blame-reveal--load-blame-at-revision-async (revision)
  "Load blame data at REVISION asynchronously."
  (let* ((file (buffer-file-name))
         (git-root (and file (vc-git-root file)))
         (source-buffer (current-buffer)))
    (unless git-root
      (user-error "File is not in a git repository"))
    (unless (blame-reveal--state-start 'recursive 'async
                                       (list :revision revision))
      (user-error "Cannot start recursive blame: state machine busy"))
    (let* ((default-directory git-root)
           (relative-file (file-relative-name file git-root))
           (temp-buffer (generate-new-buffer " *blame-recursive*"))
           (process-environment (cons "GIT_PAGER=cat"
                                      (cons "PAGER=cat" process-environment)))
           (args (blame-reveal--build-blame-command-args-for-revision
                  revision nil nil relative-file)))
      (message "Git command: git %s" (mapconcat 'identity args " "))
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
       temp-buffer)
      ;; Store source file for verification
      (process-put blame-reveal--state-process 'source-file file))))

(defun blame-reveal--handle-recursive-load-complete (temp-buffer revision file)
  "Handle completion of recursive async blame loading from TEMP-BUFFER."
  (unless (and (eq blame-reveal--state-status 'loading)
               (eq blame-reveal--state-operation 'recursive))
    (message "[State] Unexpected recursive complete in state %s/%s"
             blame-reveal--state-status blame-reveal--state-operation)
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))
    (cl-return-from blame-reveal--handle-recursive-load-complete nil))

  (unwind-protect
      (when (buffer-live-p temp-buffer)
        (condition-case err
            (progn
              (blame-reveal--state-transition 'processing)
              (let* ((git-root (vc-git-root file))
                     (relative-file (file-relative-name file git-root))
                     (result (blame-reveal--parse-blame-output
                              temp-buffer
                              relative-file))  ; 传入 relative-file
                     (blame-data (car result))
                     (move-metadata (cdr result)))
                (if blame-data
                    (progn
                      (setq blame-reveal--current-revision revision)
                      (setq blame-reveal--revision-display
                            (if (eq revision 'uncommitted)
                                "Working Tree"
                              (blame-reveal--get-commit-short-info revision)))
                      (blame-reveal--reset-data-structures blame-data)

                      ;; 保存 move/copy metadata
                      (when (hash-table-p move-metadata)
                        (setq blame-reveal--move-copy-metadata move-metadata)
                        (when (> (hash-table-count move-metadata) 0)
                          (message "Detected %d commits with move/copy history"
                                   (hash-table-count move-metadata))))
                      (blame-reveal--state-transition 'rendering)
                      (blame-reveal--load-commits-incrementally)
                      (blame-reveal--smooth-transition-render)
                      (message "Loaded blame at %s (%d lines)"
                               (or blame-reveal--revision-display revision)
                               (length blame-data))
                      (blame-reveal--state-complete))
                  (blame-reveal--state-error "Failed to parse blame data")
                  (blame-reveal--handle-recursive-load-error revision file))))
          (error
           (blame-reveal--state-error (format "Recursive load error: %s" (error-message-string err)))
           (blame-reveal--handle-recursive-load-error revision file))))
    ;; Always clean up temp buffer
    (when (buffer-live-p temp-buffer)
      (kill-buffer temp-buffer))))

;;; Synchronous Loading

(defun blame-reveal--get-blame-data-at-revision (revision file &optional range)
  "Get git blame data for FILE at REVISION synchronously.
If RANGE is (START-LINE . END-LINE), only blame that range.
REVISION can be commit hash or 'uncommitted for working tree.
Returns (BLAME-DATA . MOVE-METADATA)."
  (let ((git-root (vc-git-root file)))
    (when git-root
      (let ((default-directory git-root)
            (relative-file (file-relative-name file git-root))
            (process-environment (cons "GIT_PAGER=cat"
                                       (cons "PAGER=cat" process-environment))))
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
              ;; 返回完整的 (blame-data . metadata)
              (blame-reveal--parse-blame-output
               (current-buffer)
               relative-file))))))))

(defun blame-reveal--load-blame-at-revision-sync (revision)
  "Load blame data at REVISION synchronously.
Always loads complete file for proper recursive blame navigation."
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "No file associated with buffer"))
    (unless (blame-reveal--state-start 'recursive 'sync
                                       (list :revision revision))
      (user-error "Cannot start recursive blame: state machine busy"))
    (condition-case err-outer
        (let* ((result (blame-reveal--get-blame-data-at-revision revision file))
               (blame-data (car result))
               (move-metadata (cdr result)))
          (if blame-data
              (progn
                (blame-reveal--state-transition 'processing)
                (setq blame-reveal--current-revision revision)
                (setq blame-reveal--revision-display
                      (if (eq revision 'uncommitted)
                          "Working Tree"
                        (blame-reveal--get-commit-short-info revision)))
                (blame-reveal--reset-data-structures blame-data)

                ;; 保存 metadata
                (when (hash-table-p move-metadata)
                  (setq blame-reveal--move-copy-metadata move-metadata)
                  (when (> (hash-table-count move-metadata) 0)
                    (message "Detected %d move/copy entries at revision %s"
                             (hash-table-count move-metadata)
                             (if (eq revision 'uncommitted) "working tree"
                               (substring revision 0 8)))))
                (blame-reveal--state-transition 'rendering)
                (blame-reveal--load-commits-incrementally)
                (blame-reveal--smooth-transition-render)
                (message "Loaded blame at %s (%d lines)"
                         (or blame-reveal--revision-display revision)
                         (length blame-data))
                (blame-reveal--state-complete))
            ;; 使用统一的错误处理
            (blame-reveal--handle-recursive-load-error revision file)))
      (error
       (let ((err-data err-outer))
         (blame-reveal--state-error (error-message-string err-data))
         (message "Error during recursive blame: %s" (error-message-string err-data))
         ;; 尝试恢复状态而不是直接 signal
         (condition-case restore-err
             (when blame-reveal--blame-stack
               (let ((state (pop blame-reveal--blame-stack)))
                 (blame-reveal--restore-state state)))
           (error
            (message "Failed to restore after error: %s"
                     (error-message-string restore-err))
            (blame-reveal-reset-to-head)))
         ;; 重新抛出原始错误
         (signal (car err-data) (cdr err-data)))))))

;;; Unified Interface

(defun blame-reveal--load-blame-at-revision (revision)
  "Load blame data at REVISION using sync or async based on configuration."
  (if (blame-reveal--should-use-async-p)
      (blame-reveal--load-blame-at-revision-async revision)
    (blame-reveal--load-blame-at-revision-sync revision)))

;;; Smooth Transition Rendering

(defun blame-reveal--smooth-transition-render ()
  "Render new blame data by reusing existing overlays when possible.
Minimizes visual disruption during recursive blame."
  (when blame-reveal--blame-data
    (let* ((range (blame-reveal--get-visible-line-range))
           (start-line (car range))
           (end-line (cdr range))
           (blocks (blame-reveal--find-block-boundaries
                    blame-reveal--blame-data start-line end-line))
           (rendered-lines (make-hash-table :test 'eql)))
      (dolist (block blocks)
        (let ((commit-hash (nth 1 block)))
          (blame-reveal--ensure-commit-info commit-hash)))
      (blame-reveal--update-recent-commits)
      (dolist (block blocks)
        (let* ((block-start (nth 0 block))
               (commit-hash (nth 1 block))
               (block-length (nth 2 block)))
          (unless (and (blame-reveal--is-uncommitted-p commit-hash)
                       (not blame-reveal-show-uncommitted-fringe))
            (when (blame-reveal--should-render-commit commit-hash)
              (let ((color (blame-reveal--get-commit-color commit-hash))
                    (block-end (+ block-start block-length -1)))
                (let ((render-start (max block-start start-line))
                      (render-end (min block-end end-line)))
                  (dotimes (i (- render-end render-start -1))
                    (let ((line-num (+ render-start i)))
                      (blame-reveal--create-fringe-overlay
                       line-num color commit-hash)
                      (puthash line-num t rendered-lines)))))))))
      (dolist (overlay (blame-reveal--get-overlays-by-type 'fringe))
        (when (overlay-buffer overlay)
          (let ((line (plist-get (blame-reveal--get-overlay-metadata overlay) :line)))
            (when (and line (not (gethash line rendered-lines)))
              (blame-reveal--unregister-overlay overlay)))))
      (blame-reveal--update-header))))

;;; State Management

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
  (blame-reveal--smooth-transition-render)
  (goto-char (plist-get state :point))
  (set-window-start nil (plist-get state :window-start)))

;;; Interactive Commands

;;;###autoload
(defun blame-reveal-blame-recursively ()
  "Recursively blame the commit before the one at current line.
Time travels to see who modified this line before the current commit."
  (interactive)
  (catch 'blame-reveal-exit
    (unless blame-reveal-mode
      (user-error "blame-reveal-mode is not enabled"))
    (when (blame-reveal--state-is-busy-p)
      (user-error "Please wait for current operation to complete"))
    (let* ((current-block (blame-reveal--get-current-block))
           (commit-hash (car current-block)))
      (unless commit-hash
        (user-error "No commit at current line"))
      (when (blame-reveal--is-uncommitted-p commit-hash)
        (user-error "Cannot recursively blame uncommitted changes"))
      (blame-reveal--save-current-state)

      ;; 检查是否已经有 previous 信息（在 M/C 模式下）
      (when-let ((prev-loc (blame-reveal--get-previous-location commit-hash)))
        (let* ((prev-file (car prev-loc))
               (prev-commit (cdr prev-loc))
               (same-file (string= prev-file
                                   (file-relative-name (buffer-file-name)
                                                       (vc-git-root (buffer-file-name))))))
          ;; 如果是不同文件，询问用户
          (unless same-file
            (when (y-or-n-p (format "Line was %s. Follow to that file? "
                                    (blame-reveal--format-previous-info prev-file prev-commit)))
              (blame-reveal--follow-to-previous-file prev-file prev-commit)
              ;; 使用 throw 退出
              (throw 'blame-reveal-exit nil)))))

      (let ((parent-commit (concat commit-hash "^")))
        (message "Recursive blame: %s -> %s" (substring commit-hash 0 8) parent-commit)
        (condition-case err
            (blame-reveal--load-blame-at-revision parent-commit)
          (error
           (message "Error in recursive blame: %s" (error-message-string err))
           (let ((state (pop blame-reveal--blame-stack)))
             (when state
               (blame-reveal--restore-state state)))
           (signal (car err) (cdr err))))))))

;;;###autoload
(defun blame-reveal-blame-back ()
  "Go back to previous blame state in the recursion stack."
  (interactive)
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))
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
  "Show blame at a specific REVISION (commit/branch/tag)."
  (interactive "sBlame at revision (commit/branch/tag): ")
  (unless blame-reveal-mode
    (user-error "blame-reveal-mode is not enabled"))
  (when (blame-reveal--state-is-busy-p)
    (user-error "Please wait for current operation to complete"))
  (when (string-empty-p (string-trim revision))
    (user-error "Revision cannot be empty"))
  ;; Verify revision is valid
  (let ((process-environment (cons "GIT_PAGER=cat"
                                   (cons "PAGER=cat" process-environment))))
    (with-temp-buffer
      (unless (zerop (call-process "git" nil t nil "rev-parse" "--verify"
                                   (concat revision "^{commit}")))
        (user-error "Invalid revision: %s" revision))))
  (blame-reveal--save-current-state)
  (message "Blame at revision: %s" revision)
  (condition-case err
      (blame-reveal--load-blame-at-revision revision)
    (error
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
  (when (blame-reveal--state-is-busy-p)
    (blame-reveal--state-cancel "reset to HEAD"))
  (cond
   (blame-reveal--current-revision
    (setq blame-reveal--blame-stack nil)
    (setq blame-reveal--current-revision nil)
    (setq blame-reveal--revision-display nil)
    (setq blame-reveal--auto-days-cache nil)
    (blame-reveal--full-update)
    (message "Reset to HEAD"))
   (blame-reveal--blame-stack
    (setq blame-reveal--blame-stack nil)
    (message "Cleared blame stack"))
   (t
    (message "Already at HEAD"))))

;;; Utility Functions

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
