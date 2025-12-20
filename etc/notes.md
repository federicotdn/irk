# Dev notes

## Eglot debugging

```elisp
(setq eglot-report-progress t
      eglot-events-buffer-config '(:size 1000000 :format full)
      jsonrpc-default-request-timeout 30)
```
