;;; excali-gui-bench.el --- Benchmark backends in a GUI frame  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Run with `make bench'.  Writes the results table to bench.txt.

(setq inhibit-startup-screen t)

(defconst excali-gui-bench--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(run-at-time
 1.5 nil
 (lambda ()
   (let ((out (expand-file-name "bench.txt" excali-gui-bench--root)))
     (condition-case err
         (progn
           (require 'excali)
           (set-frame-size nil 1200 800 t)
           (excali-open (expand-file-name "test/sample.excalidraw" excali-gui-bench--root))
           (delete-other-windows)
           (redisplay t)
           (dolist (scale '(1.0 2.0))
             (setq excali--pixel-scale scale)
             (excali--use-backend excali--backend)
             (excali-bench-backends 60)
             (with-current-buffer "*excali-bench*"
               (append-to-file (point-min) (point-max) out))))
       (error (with-temp-buffer
                (insert (format "ERROR %S\n" err))
                (append-to-file (point-min) (point-max) out))))
     (kill-emacs 0))))

;;; excali-gui-bench.el ends here
