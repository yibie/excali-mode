;;; excal-gui-bench.el --- Benchmark backends in a GUI frame  -*- lexical-binding: t; -*-

;; Run with `make bench'.  Writes the results table to bench.txt.

(setq inhibit-startup-screen t)

(defconst excal-gui-bench--root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))

(run-at-time
 1.5 nil
 (lambda ()
   (let ((out (expand-file-name "bench.txt" excal-gui-bench--root)))
     (condition-case err
         (progn
           (require 'excal)
           (set-frame-size nil 1200 800 t)
           (excal-open (expand-file-name "test/sample.excalidraw" excal-gui-bench--root))
           (delete-other-windows)
           (redisplay t)
           (dolist (scale '(1.0 2.0))
             (setq excal--pixel-scale scale)
             (excal--use-backend excal--backend)
             (excal-bench-backends 60)
             (with-current-buffer "*excal-bench*"
               (append-to-file (point-min) (point-max) out))))
       (error (with-temp-buffer
                (insert (format "ERROR %S\n" err))
                (append-to-file (point-min) (point-max) out))))
     (kill-emacs 0))))

;;; excal-gui-bench.el ends here
