#!/usr/bin/env bash
set -e

make install
emacs -l etc/irk.el --eval "
(progn
  (require 'eglot)

  (setq irk-languages '(\"haskell\" \"go\"))

  (irk-enable)
  (irk-eglot-enable)

  (setq
   eglot-report-progress t
   eglot-ignored-server-capabilities '(:inlayHintProvider :documentOnTypeFormattingProvider)
   eglot-events-buffer-config '(:size 1000000 :format full)
   jsonrpc-default-request-timeout 60)

  (global-set-key (kbd \"C-x C-c\") 'kill-emacs))
"
