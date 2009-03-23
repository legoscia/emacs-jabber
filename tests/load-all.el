;; Test that all files can be loaded

(let* ((default-directory (expand-file-name (getenv "top_builddir")))
       (elc-files (file-expand-wildcards "*.elc" t)))
  (dolist (f elc-files)
    (load f nil t)))

;; arch-tag: 509c4808-2e92-11dd-9c8c-000a95c2fcd0
