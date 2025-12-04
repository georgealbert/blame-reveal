;;; blame-reveal-state.el --- State machine for async operations -*- lexical-binding: t; -*-

;;; Commentary:
;; State machine for managing async loading operations.
;; Coordinates between initial load, expansion, and recursive blame operations.

;;; Code:

(require 'blame-reveal-core)

;;; State Machine Core Functions

(defun blame-reveal--state-can-start-p (operation)
  "Check if OPERATION can start in current state.
Returns (CAN-START-P . REASON)."
  (let ((status blame-reveal--state-status)
        (current-op blame-reveal--state-operation))
    (cond
     ((eq status 'idle)
      '(t . "idle"))
     ((and (eq status 'loading)
           (eq current-op operation))
      `(nil . ,(format "already loading %s" operation)))
     ((and (eq status 'loading)
           (eq current-op 'initial)
           (eq operation 'expansion))
      '(t . "upgrade initial->expansion"))
     ((eq operation 'recursive)
      '(t . "recursive cancels current"))
     (t
      `(nil . ,(format "busy: %s %s" status current-op))))))

(defun blame-reveal--state-start (operation mode &optional metadata)
  "Start OPERATION in MODE with optional METADATA.
Returns t if started successfully."
  (pcase-let ((`(,can-start . ,reason)
               (blame-reveal--state-can-start-p operation)))
    (if can-start
        (progn
          (when (not (eq blame-reveal--state-status 'idle))
            (blame-reveal--state-cancel reason))
          ;; 重要：在生成新 ID 之前，确保旧进程已经清理
          (when blame-reveal--state-process
            (when (process-live-p blame-reveal--state-process)
              (delete-process blame-reveal--state-process))
            (setq blame-reveal--state-process nil))
          ;; 生成新的进程 ID
          (setq blame-reveal--process-id (blame-reveal--generate-process-id))
          (setq blame-reveal--state-status 'loading
                blame-reveal--state-operation operation
                blame-reveal--state-mode mode
                blame-reveal--state-metadata metadata
                blame-reveal--state-process nil
                blame-reveal--state-buffer nil)
          (blame-reveal--start-loading-animation)
          (message "[State] Start: %s (%s) - %s [ID: %s]"
                   operation mode reason blame-reveal--process-id)
          t)
      (message "[State] Cannot start %s: %s" operation reason)
      nil)))

(defun blame-reveal--state-set-async-resources (process buffer)
  "Set async PROCESS and BUFFER for current operation."
  ;; 确保有进程 ID
  (unless blame-reveal--process-id
    (setq blame-reveal--process-id (blame-reveal--generate-process-id)))
  ;; 存储进程 ID 到进程对象
  (when (and process (processp process))
    (process-put process 'blame-reveal-process-id blame-reveal--process-id)
    (process-put process 'source-file (buffer-file-name)))
  (setq blame-reveal--state-process process
        blame-reveal--state-buffer buffer))

(defun blame-reveal--state-verify-process (process)
  "Verify PROCESS belongs to current operation."
  (and (processp process)
       ;; 关键：如果进程已经不在 state-process 中，说明已被替换
       (eq process blame-reveal--state-process)
       (let ((proc-id (process-get process 'blame-reveal-process-id))
             (proc-file (process-get process 'source-file)))
         (and proc-id
              blame-reveal--process-id
              (equal proc-id blame-reveal--process-id)
              (equal proc-file (buffer-file-name))))))

(defun blame-reveal--state-transition (new-status)
  "Transition to NEW-STATUS."
  (let ((old-status blame-reveal--state-status))
    (setq blame-reveal--state-status new-status)
    (message "[State] Transition: %s -> %s (op:%s)"
             old-status new-status blame-reveal--state-operation)))

(defun blame-reveal--state-cleanup-async ()
  "Cleanup async resources (process and buffer)."
  (when blame-reveal--state-process
    (when (process-live-p blame-reveal--state-process)
      (delete-process blame-reveal--state-process))
    (setq blame-reveal--state-process nil))
  (when blame-reveal--state-buffer
    (when (buffer-live-p blame-reveal--state-buffer)
      (kill-buffer blame-reveal--state-buffer))
    (setq blame-reveal--state-buffer nil)))

(defun blame-reveal--state-complete ()
  "Complete current operation successfully.
Ensures cleanup happens even if rendering fails."
  (unwind-protect
      (progn
        (blame-reveal--stop-loading-animation)
        (blame-reveal--state-cleanup-async))
    ;; Always reset state
    (setq blame-reveal--state-status 'idle
          blame-reveal--state-operation nil
          blame-reveal--state-mode nil
          blame-reveal--state-metadata nil
          blame-reveal--process-id nil))
  (message "[State] Complete"))

(defun blame-reveal--state-error (error-msg)
  "Handle error with ERROR-MSG.
Ensures all resources are cleaned up even if cleanup fails.
Uses defensive programming to prevent cascading failures."
  (message "[State] Error: %s" error-msg)

  ;; 第一层：清理异步资源
  (ignore-errors
    (blame-reveal--stop-loading-animation))
  (ignore-errors
    (blame-reveal--state-cleanup-async))

  ;; 第二层：清理 UI 元素
  (ignore-errors
    (when (and (boundp 'blame-reveal--header-overlay)
               blame-reveal--header-overlay
               (overlayp blame-reveal--header-overlay))
      (delete-overlay blame-reveal--header-overlay)
      (setq blame-reveal--header-overlay nil)))

  (ignore-errors
    (when (fboundp 'blame-reveal--clear-sticky-header)
      (blame-reveal--clear-sticky-header)))

  ;; 第三层：清理定时器
  (ignore-errors
    (when (and (boundp 'blame-reveal--temp-overlay-timer)
               blame-reveal--temp-overlay-timer
               (timerp blame-reveal--temp-overlay-timer))
      (cancel-timer blame-reveal--temp-overlay-timer)
      (setq blame-reveal--temp-overlay-timer nil)))

  (ignore-errors
    (when (and (boundp 'blame-reveal--header-update-timer)
               blame-reveal--header-update-timer
               (timerp blame-reveal--header-update-timer))
      (cancel-timer blame-reveal--header-update-timer)
      (setq blame-reveal--header-update-timer nil)))

  ;; 设置错误状态
  (setq blame-reveal--state-status 'error)

  ;; 延迟重置状态（给用户时间看到错误）
  (run-with-timer 0.1 nil
                  (lambda ()
                    (when (and (boundp 'blame-reveal--state-status)
                               (eq blame-reveal--state-status 'error))
                      (setq blame-reveal--state-status 'idle
                            blame-reveal--state-operation nil
                            blame-reveal--state-mode nil
                            blame-reveal--state-metadata nil
                            blame-reveal--process-id nil)))))

(defun blame-reveal--state-cancel (reason)
  "Cancel current operation for REASON.
Ensures all resources are cleaned up."
  (message "[State] Cancel: %s" reason)
  (unwind-protect
      (progn
        (blame-reveal--stop-loading-animation)
        (blame-reveal--state-cleanup-async))
    ;; Ensure cleanup even if errors occur
    (ignore-errors
      (when blame-reveal--header-overlay
        (delete-overlay blame-reveal--header-overlay)
        (setq blame-reveal--header-overlay nil))
      (blame-reveal--clear-sticky-header)
      (when blame-reveal--temp-overlay-timer
        (cancel-timer blame-reveal--temp-overlay-timer)
        (setq blame-reveal--temp-overlay-timer nil))))
  (setq blame-reveal--state-status 'idle
        blame-reveal--state-operation nil
        blame-reveal--state-mode nil
        blame-reveal--state-metadata nil
        blame-reveal--process-id nil))

(defun blame-reveal--state-is-busy-p ()
  "Check if state machine is busy."
  (not (eq blame-reveal--state-status 'idle)))

(provide 'blame-reveal-state)
;;; blame-reveal-state.el ends here
