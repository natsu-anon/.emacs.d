 (defvar python-executable "python"
   "path to python executable to use, defaults to 'python'")

 (defvar chot-script "chot.py"
   "path to chot script to use, defaults to '~/.emacs.d/chot/chot.py'")

(defun chot (&optional url port)
  "View a youtube livestream chat in a seperate buffer--NON-BLOCKING!!!"
  (interactive"surl: \nnport: ")
  (message "chotting on port: %d" port)
  (get-buffer-create "*chot*")
  (display-buffer "*chot*")
  (string-match "v=\\(.\\{11\\}\\)" url)
  (make-process
   :name "chot"
   :buffer (get-buffer "*chot*")
   :command `(,python-executable ,chot-script  ,(match-string 1 url) ,(number-to-string port)))
  (open-network-stream "chot-conn" (get-buffer "*chot*") "localhost" port :type 'plain))

(provide 'chot)
