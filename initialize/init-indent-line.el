(use-package highlight-indent-guides
  :straight t
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top
              highlight-indent-guides-suppress-auto-error t)
  :config
  (with-no-warnings
    ;; Don't display first level of indentation
    (defun my-indent-guides-for-all-but-first-column (level responsive display)
      (unless (< level 1)
        (highlight-indent-guides--highlighter-default level responsive display)))
    (setq highlight-indent-guides-highlighter-function
          #'my-indent-guides-for-all-but-first-column)

    ;; Disable in `macrostep' expanding
    (with-eval-after-load 'macrostep
      (advice-add #'macrostep-expand
                  :after (lambda (&rest _)
                           (when highlight-indent-guides-mode
                             (highlight-indent-guides-mode -1))))
      (advice-add #'macrostep-collapse
                  :after (lambda (&rest _)
                           (when (derived-mode-p 'prog-mode 'yaml-mode)
                             (highlight-indent-guides-mode 1)))))

    ;; Don't display indentations in `swiper'
    ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
    (with-eval-after-load 'ivy
      (defun my-ivy-cleanup-indentation (str)
        "Clean up indentation highlighting in ivy minibuffer."
        (let ((pos 0)
              (next 0)
              (limit (length str))
              (prop 'highlight-indent-guides-prop))
          (while (and pos next)
            (setq next (text-property-not-all pos limit prop nil str))
            (when next
              (setq pos (text-property-any next limit prop nil str))
              (ignore-errors
                (remove-text-properties next pos '(display nil face nil) str))))))
      (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation))))

(provide 'init-indent-line)
