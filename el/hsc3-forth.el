;;; hsc3-forth.el --- Forth SuperCollider

;;; Commentary:
; This mode is implemented as a derivation of `forth-mode'.
; Indentation and font locking is courtesy `forth-mode'.
; Inter-process communication is courtesy `comint'.
; Symbol at point acquisition is courtesy `thingatpt'.

(require 'forth-mode)
(require 'comint)
(require 'thingatpt)

;;; Code:

(defun hsc3-forth-send (str)
  "Send STR argument to hsc3-forth."
  (if (comint-check-proc forth-process-buffer)
      (comint-send-string (forth-proc) (concat str "\n"))
    (error "HSC3-FORTH not running?")))

(defun hsc3-forth-point (word post q-str)
  "If POST then WORD <point>, else <point> WORD.  If Q-STR quote <point> as string."
  (let* ((p (thing-at-point 'symbol))
         (p-q (if q-str (format "s\" %s\"" p) p)))
    (hsc3-forth-send
     (if post (format "%s %s" word p-q) (format "%s %s" p-q word)))))

(defun hsc3-forth-bye () "Bye." (interactive) (hsc3-forth-send "bye"))
(defun hsc3-forth-stop () "Stop." (interactive) (hsc3-forth-send "stop"))
(defun hsc3-forth-sc3-status () "Sc3-Status." (interactive) (hsc3-forth-send "sc3-status"))
(defun hsc3-forth-killall () "Killall." (interactive) (hsc3-forth-send "killall"))
(defun hsc3-forth-help () "<word> ?." (interactive) (hsc3-forth-point "?" nil t))
(defun hsc3-forth-manual () "<word> Manual." (interactive) (hsc3-forth-point "manual" nil t))
(defun hsc3-forth-dpans () "<word> Dpans." (interactive) (hsc3-forth-point "dpans" nil t))
(defun hsc3-forth-play-word () "<word> Play." (interactive) (hsc3-forth-point "play" nil nil))
(defun hsc3-forth-draw-word () "<word> Draw." (interactive) (hsc3-forth-point "draw" nil nil))
(defun hsc3-forth-pp-word () "<word> Pp." (interactive) (hsc3-forth-point "pp" nil nil))
(defun hsc3-forth-play () "Play." (interactive) (hsc3-forth-send "play"))
(defun hsc3-forth-play-region () "<region> Play." (interactive) (hsc3-forth-send-region) (hsc3-forth-send "play"))
(defun hsc3-forth-play-paragraph () "<paragraph> Play." (interactive) (hsc3-forth-send-paragraph) (hsc3-forth-send "play"))
(defun hsc3-forth-draw () "Draw." (interactive) (hsc3-forth-send "draw"))
(defun hsc3-forth-draw-region () "<region> Draw." (interactive) (hsc3-forth-send-region) (hsc3-forth-send "draw"))
(defun hsc3-forth-draw-paragraph () "<paragraph> Draw." (interactive) (hsc3-forth-send-paragraph) (hsc3-forth-send "draw"))
(defun hsc3-forth-pp () "Pp." (interactive) (hsc3-forth-send "pp"))

(defun hsc3-forth-included ()
  "Included."
  (interactive)
  (save-buffer)
  (hsc3-forth-send (format "s\" %s\" included" buffer-file-name)))

(defun hsc3-forth-send-line ()
  "Send the current line to the interpreter."
  (interactive)
  (hsc3-forth-send
   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun hsc3-forth-send-region ()
  "Send the current region to the interpreter."
  (interactive)
  (hsc3-forth-send
   (buffer-substring-no-properties (region-beginning) (region-end))))

(defun hsc3-forth-set-region-to-paragraph ()
  "Set the mark at the start and point at the end of the current paragraph."
  (interactive)
  (backward-paragraph)
  (push-mark nil t t)
  (forward-paragraph))

(defun hsc3-forth-send-paragraph ()
  "Send the current paragraph to the interpreter."
  (interactive)
  (hsc3-forth-set-region-to-paragraph)
  (hsc3-forth-send-region))

(defun hsc3-forth-see-forth ()
 "Start and see hsc3-forth."
 (interactive)
 (save-selected-window (run-forth forth-program-name))
 (forth-split))

(defun hsc3-forth-interrupt ()
  "Interrupt hsc3-forth."
  (interactive)
  (with-current-buffer forth-process-buffer
    (interrupt-process (get-buffer-process (current-buffer)))))

(defvar hsc3-forth-mode-map nil
  "Forth SuperCollider keymap.")

(defun hsc3-forth-mode-keybindings (map)
  "Add Forth SuperCollider keybindings to MAP."
  (define-key map (kbd "C-c <") 'hsc3-forth-included)
  (define-key map (kbd "C-c >") 'hsc3-forth-see-forth)
  (define-key map (kbd "C-c C-a") 'hsc3-forth-play-paragraph)
  (define-key map (kbd "C-c C-g") 'hsc3-forth-draw-paragraph)
  (define-key map (kbd "C-c C-e") 'hsc3-forth-pp)
  (define-key map (kbd "C-c C-s") 'hsc3-forth-killall)
  (define-key map (kbd "C-c C-c") 'hsc3-forth-send-line)
  (define-key map (kbd "C-c C-d") 'hsc3-forth-send-region)
  (define-key map (kbd "C-c C-k") 'hsc3-forth-stop)
  (define-key map (kbd "C-c C-p") 'hsc3-forth-sc3-status)
  (define-key map (kbd "C-c C-i") 'hsc3-forth-interrupt)
  (define-key map (kbd "C-c C-q") 'hsc3-forth-bye)
  (define-key map (kbd "C-c C-j") 'hsc3-forth-manual)
  (define-key map (kbd "C-c C-f") 'hsc3-forth-dpans)
  (define-key map (kbd "C-c C-u") 'hsc3-forth-help))

(defun hsc3-forth-mode-menu (map)
  "Add Forth SuperCollider menu entries to MAP."
  (define-key map [menu-bar hsc3-forth] (cons "Forth-SuperCollider" map))
  (define-key map [menu-bar hsc3-forth help] '("Help" . hsc3-forth-help))
  (define-key map [menu-bar hsc3-forth manual] '("Manual" . hsc3-forth-manual))
  (define-key map [menu-bar hsc3-forth included] '("Included" . hsc3-forth-included))
  (define-key map [menu-bar hsc3-forth send-line] '("Send line" . hsc3-forth-send-line))
  (define-key map [menu-bar hsc3-forth stop] '("Stop" . hsc3-forth-stop))
  (define-key map [menu-bar hsc3-forth pp] '("Pp" . hsc3-forth-pp))
  (define-key map [menu-bar hsc3-forth draw] '("Draw" . hsc3-forth-draw))
  (define-key map [menu-bar hsc3-forth play] '("Play" . hsc3-forth-play))
  (define-key map [menu-bar hsc3-forth bye] '("Bye" . hsc3-forth-bye))
  (define-key map [menu-bar hsc3-forth see-forth] '("See Forth" . hsc3-forth-see-forth)))

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

;;; hsc3-forth.el ends here
