;;--------------------;;
;; Performance tuning ;;
;;--------------------;;
;; Increase garbage collection threshold
(setq gc-cons-threshold (* 200 1024 1024))      ;; GC threshold 200 MB
(setq gc-cons-percentage 0.2)                   ;; GC percentage

;; Warn for large files >500 MB
(setq large-file-warning-threshold (* 500 1024 1024))

;; Disable expensive bidirectional reordering
(setq bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right)

;; Skip fontification while typing, limit very large buffers
(setq redisplay-skip-fontification-on-input t
      font-lock-maximum-size 2000000)

;; Limit version control backends, file notifications
(setq vc-handled-backends '(Git)
      file-notify-watch-descriptor-max 10000)

;; Improve subprocess read performance
(setq read-process-output-max (* 64 1024 1024)
      process-adaptive-read-buffering nil)
