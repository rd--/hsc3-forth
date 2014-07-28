;; This mode is implemented as a derivation of `forth-mode' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.

(require 'forth-mode)
(require 'comint)
(require 'thingatpt)

(defun hsc3-forth-send-string (s)
  (if (comint-check-proc "*forth*")
      (comint-send-string (forth-proc) (concat s "\n"))
    (error "no HSC3-FORTH process running?")))

(defun hsc3-forth-word-at-point (word post q-str)
  "If POST then WORD <POINT>, else <POINT> WORD.  If Q-STR quote <POINT> as string."
  (let* ((p (thing-at-point 'symbol))
         (p-q (if q-str (format "s\" %s\"" p) p)))
    (hsc3-forth-send-string
     (if post (format "%s %s" word p-q) (format "%s %s" p-q word)))))

(defun hsc3-forth-bye () "BYE." (interactive) (hsc3-forth-send-string "bye"))
(defun hsc3-forth-stop () "STOP" (interactive) (hsc3-forth-send-string "stop"))
(defun hsc3-forth-killall () "KILLALL" (interactive) (hsc3-forth-send-string "killall"))
(defun hsc3-forth-help () "s\" <word>\"" (interactive) (hsc3-forth-word-at-point "?" nil t))
(defun hsc3-forth-play () "<word> PLAY" (interactive) (hsc3-forth-word-at-point "play" nil nil))
(defun hsc3-forth-draw () "<word> DRAW" (interactive) (hsc3-forth-word-at-point "draw" nil nil))

(defun hsc3-forth-load-buffer ()
  "INCLUDED"
  (interactive)
  (save-buffer)
  (hsc3-forth-send-string (format "s\" %s\" included" buffer-file-name)))

(defun hsc3-forth-send-line ()
  "Send the current line to the interpreter."
  (interactive)
  (hsc3-forth-send-string (buffer-substring (line-beginning-position) (line-end-position))))

(defun region-string ()
  "Get region as string (no properties)"
  (buffer-substring-no-properties
   (region-beginning)
   (region-end)))

(defun hsc3-forth-send-region ()
  "Send the current region to the interpreter."
  (interactive)
  (hsc3-forth-send-string (region-string)))

(defun hsc3-forth-see-forth ()
 "Show forth output."
 (interactive)
 (run-forth forth-program-name)
 (forth-split))

(defvar hsc3-forth-mode-map nil
  "Forth SuperCollider keymap.")

(defun hsc3-forth-mode-keybindings (map)
  "Forth SuperCollider keybindings."
  (define-key map [?\C-c ?<] 'hsc3-forth-load-buffer)
  (define-key map [?\C-c ?>] 'hsc3-forth-see-forth)
  (define-key map [?\C-c ?\C-a] 'hsc3-forth-play)
  (define-key map [?\C-c ?\C-g] 'hsc3-forth-draw)
  (define-key map [?\C-c ?\C-s] 'hsc3-forth-killall)
  (define-key map [?\C-c ?\C-c] 'hsc3-forth-send-line)
  (define-key map [?\C-c ?\C-d] 'hsc3-forth-send-region)
  (define-key map [?\C-c ?\C-k] 'hsc3-forth-stop)
  (define-key map [?\C-c ?\C-q] 'hsc3-forth-bye)
  (define-key map [?\C-c ?\C-u] 'hsc3-forth-help))

(defun hsc3-forth-mode-menu (map)
  "Forth SuperCollider menu."
  (define-key map [menu-bar hsc3]
    (cons "Forth-SuperCollider" (make-sparse-keymap "Forth-SuperCollider")))
  (define-key map [menu-bar hsc3 help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar hsc3 help ugen]
    '("UGen parameter summary" . hsc3-forth-help))
  (define-key map [menu-bar hsc3 expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar hsc3 expression load-buffer]
    '("Load buffer" . hsc3-forth-load-buffer))
  (define-key map [menu-bar hsc3 expression run-line]
    '("Run line" . hsc3-forth-send-line))
  (define-key map [menu-bar hsc3 expression reset]
    '("Reset scsynth" . hsc3-forth-stop))
  (define-key map [menu-bar hsc3 forth]
    (cons "Forth" (make-sparse-keymap "Forth")))
  (define-key map [menu-bar hsc3 forth quit-forth]
    '("Quit forth" . hsc3-forth-bye))
  (define-key map [menu-bar hsc3 forth see-forth]
    '("See forth" . hsc3-forth-see-forth)))

(if hsc3-forth-mode-map
    ()
  (let ((map (make-sparse-keymap "Forth-SuperCollider")))
    (hsc3-forth-mode-keybindings map)
    (hsc3-forth-mode-menu map)
    (setq hsc3-forth-mode-map map)))

(define-derived-mode
  hsc3-forth-mode
  forth-mode
  "Forth SuperCollider"
  "Major mode for interacting with an inferior forth process."
  (setq forth-program-name "hsc3-forth")
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.fs$" . hsc3-forth-mode))

(provide 'hsc3-forth)
