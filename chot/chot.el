 (defvar python-executable "python"
   "path to python executable to use, defaults to 'python'")

 (defvar chot-script "chot.py"
   "path to chot script to use, defaults to '~/.emacs.d/chot/chot.py'")

(defun chot (&optional target port)
  "View a youtube livestream chat in a seperate buffer--NON-BLOCKING!!!"
  (interactive"starget: \nnport: ")
  (message "chotting on port: %d" port)
  (get-buffer-create "*chot*")
  (display-buffer "*chot*")
  (make-process
   :name "chot"
   :buffer (get-buffer "*chot*")
   :command `(,python-executable ,chot-script  ,target ,(number-to-string port)))
  (open-network-stream "chot-conn" (get-buffer "*chot*") "localhost" port :type 'plain))

(provide 'chot)
