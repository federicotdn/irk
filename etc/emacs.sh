#!/usr/bin/env bash
set -e

make install
emacs --eval "
(progn
  (require 'eglot)

  (add-to-list 'eglot-server-programs
               '(python-mode . (\"irk\" \"lsp\")))
  (add-to-list 'eglot-server-programs
               '(go-mode . (\"irk\" \"lsp\")))
  (add-to-list 'eglot-server-programs
               '(c-mode . (\"irk\" \"lsp\")))
  (add-to-list 'eglot-server-programs
               '(haskell-mode . (\"irk\" \"lsp\")))

  (setq
   eglot-report-progress t
   eglot-ignored-server-capabilities '(:inlayHintProvider :documentOnTypeFormattingProvider)
   eglot-events-buffer-config '(:size 1000000 :format full)
   jsonrpc-default-request-timeout 60)

  (global-set-key (kbd \"C-x C-c\") 'kill-emacs))
"
