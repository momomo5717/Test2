;;; emms-player-mpv-radiko.el --- emms simple player to play Radiko, etc  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 momomo5717

;; Version 0.1.0
;; URL: https://github.com/momomo5717/emms-player-mpv-radiko


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Usage
;; (require 'emms-player-mpv-radiko)
;; (add-to-list 'emms-player-list 'emms-player-mpv-radiko)
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tq)
(require 'emms-volume)
(require 'emms-radiko-start-simple-process)

(defgroup emms-player-mpv-radiko nil
  "EMMS Player for Radiko"
  :group 'emms)

(defcustom emms-radiko-start-mpv-simple-process-function
  'emms-radiko-start-simple-process
  "The function is used to play radiko."
  :type '(fucntion :tag "Function to start emms-player-simple-process for radiko")
  :group 'emms-player-mpv-radiko)

(define-emms-simple-player mpv-radiko '(streamlist)
  "\\`\\(radiko\\|mms\\|rtmp\\)://\\|"
  "mpv" "--no-terminal")

(defvar emms-player-mpv-radiko--socket nil)

(defun emms-player-mpv-radiko--socket ()
  (setq emms-player-mpv-radiko--socket
   (make-temp-name
    (expand-file-name "mpv-radiko-socket" temporary-file-directory))))

(defvar emms-player-mpv-radiko--tq nil)

;; Utility for tq

(defun emms-player-radiko--tq-create ()
  (tq-create (make-network-process
              :name "emms-player-mpv-radiko-network"
              :family 'local
              :service emms-player-mpv-radiko--socket)))

(defvar emms-player-mpv-radiko--tq-event-buffer
  "*emms-player-mpv-radiko--tq-event*"
  "Buffer for event from mpv.")

(defun emms-player-mpv-radiko--tq-close ()
  (when emms-player-mpv-radiko--tq
    (tq-close emms-player-mpv-radiko--tq)
    (setq emms-player-mpv-radiko--tq nil))
  (when (file-exists-p emms-player-mpv-radiko--socket)
    (delete-file emms-player-mpv-radiko--socket))
  (when (get-buffer emms-player-mpv-radiko--tq-event-buffer)
    (kill-buffer emms-player-mpv-radiko--tq-event-buffer)))

(add-hook 'emms-player-stopped-hook  'emms-player-mpv-radiko--tq-close)
(add-hook 'emms-player-finished-hook 'emms-player-mpv-radiko--tq-close)

(defun emms-player-mpv-radiko--socket-filter (_proc string)
  (emms-player-mpv-radiko--tq-filter
   emms-player-mpv-radiko--tq string))

(defun emms-player-mpv-radiko--tq-filter (tq string)
  "Append STRING to the TQ's buffer; then process the new data. See tq.el."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert string)
        (emms-player-mpv-radiko--tq-process-buffer tq)))))

(defun emms-player-mpv-radiko--tq-process-buffer (tq)
  "Check TQ's buffer at the head of the queue. See tq.el."
  (let ((buffer (tq-buffer tq))
        (buffer-tq-event emms-player-mpv-radiko--tq-event-buffer))
    (while (and (buffer-live-p buffer) (set-buffer buffer)
                (/= 0 (buffer-size)))
      (if (tq-queue-empty tq)
          (let ((buf (get-buffer-create buffer-tq-event)))
            (copy-to-buffer buf (point-min) (point-max))
            (delete-region (point-min) (point))
            (ignore-errors (emms-player-mpv-radiko--tq-event-action)))
        (goto-char (point-min))
        (let ((answer-ls (cl-loop with ls
                          for obj = (ignore-errors (json-read))
                          unless obj return (nreverse ls)
                          when obj do (push obj ls)))
              (fn (tq-queue-head-fn tq))
              (closure (tq-queue-head-closure tq)))
          (delete-region (point-min) (point-max))
          (tq-queue-pop tq)
          (condition-case nil
              (funcall fn closure answer-ls)
            (error nil)))))))

(defun emms-player-mpv-radiko--tq-event-action ()
  (let ((buf (get-buffer emms-player-mpv-radiko--tq-event-buffer))
        ans-ls)
    (when (buffer-live-p  buf)
     (setq ans-ls
           (with-current-buffer buf
             (goto-char (point-min))
             (cl-loop with ls
                      for obj = (ignore-errors (json-read))
                      unless obj return (nreverse ls)
                      when obj do (push obj ls))))
     (cl-loop for ans in ans-ls
              for event-pair = (assoc 'event ans)
              for event      = (cdr event-pair)
              when event-pair do
              (cond
               ((equal event "pause")
                (setq emms-player-paused-p t)
                (run-hooks 'emms-player-paused-hook))
               ((equal event "unpause")
                (setq emms-player-paused-p nil)
                (run-hooks 'emms-player-paused-hook))
               (t nil)))
     (with-current-buffer buf (erase-buffer)))))

(defun emms-player-mpv-radiko--tq-make-command (com &rest params)
  "Build JSON command."
  (concat (json-encode `(("command" . (,com ,@params)))) "\n"))

(defun emms-player-mpv-radiko-tq-clear ()
  (let ((tq emms-player-mpv-radiko--tq))
    (while (not (tq-queue-empty tq))
      (tq-queue-pop tq))))

(defun emms-player-mpv-radiko-playing-p ()
  (eq (assoc-default 'start emms-player-playing-p)
            'emms-player-mpv-radiko-start))

(defun emms-player-mpv-radiko-tq-enqueue
    (com-ls closure fn &optional delay-question)
  "Wrapper function of tq-enqueue."
  (when (emms-player-mpv-radiko-playing-p)
    (tq-enqueue emms-player-mpv-radiko--tq
                (apply 'emms-player-mpv-radiko--tq-make-command com-ls)
                "" closure fn delay-question)))

(defun emms-player-mpv-radiko-tq-successp (ans)
  "Check command response from ANS."
  (if (atom (caar ans)) ;; for decoded JSON obj
      (let ((err-msg (assoc-default 'error ans)))
        (and (stringp err-msg)
             (string= err-msg "success")))
    ;; for decoded JSON obj list
    (let ((err-msg
           (assoc-default 'error (cl-find-if (lambda (obj) (assoc 'error obj))
                                     ans))))
      (and (stringp err-msg)
           (string= err-msg "success")))))

(defun emms-player-mpv-radiko-tq-assoc (key ans)
  "Return the association for KEY in ANS."
  (if (atom (caar ans)) ;; for decoded JSON obj
      (assoc key ans)
    ;; for decoded JSON obj list
    (assoc key
           (cl-find-if
            (lambda (obj) (assoc key obj))
            ans))))

(defun emms-player-mpv-radiko-tq-assoc-v (key ans)
  (cdr (emms-player-mpv-radiko-tq-assoc key ans)))

(defun emms-player-mpv-radiko-tq-data-message (format)
  "Return function to display a data message by FORMAT.
FORMAT must have a format specification to insert data."
  (lambda (_ ans-ls)
    (let ((data (emms-player-mpv-radiko-tq-assoc-v 'data ans-ls)))
      (if (emms-player-mpv-radiko-tq-successp ans-ls)
          (message format data)
        (message format (emms-player-mpv-radiko-tq-assoc-v 'error ans-ls))))))

(defun emms-player-mpv-radiko-tq-error-message (format)
  "Return function to display an error message by FORMAT.
FORMAT must have a format specification to insert error message."
  (lambda (_ ans-ls)
    (let ((err-msg (emms-player-mpv-radiko-tq-assoc-v 'error ans-ls)))
      (if err-msg
          (message format err-msg)
        (message "mpv-radiko nothing error message")))))

;; Override emms-player-mpv-radiko-start

(defvar emms-player-mpv-radiko-specific-source-alist
  '((("^https://www.youtube" t)  .
     (lambda (track-name) (format "--ytdl %s"
                              (shell-quote-argument track-name)))))
  "Alist to convert track-name to readable mpv format.
(((regexp track-type) . function) ...)")

(defun emms-player-mpv-radiko--convert-specific-source (track-name track-type)
  (cl-loop for ((regexp type) . fn) in emms-player-mpv-radiko-specific-source-alist
           when (and (or (eq t type) (eq track-type type))
                     (string-match-p regexp track-name))
           return (funcall fn track-name)))

(defun emms-player-mpv-radiko--track-name-to-mpv-format (track-name track-type)
  "Convert TRACK-NAME to readable mpv format by TRACK-TYPE."
  (cond
   ((emms-player-mpv-radiko--convert-specific-source track-name track-type))
   ((eq track-type 'streamlist)
    (cond
     ((string-match-p "^rtmp://" track-name)
      (format "\"%s live=1\"" track-name))
     (t track-name)))
   ((eq track-type 'playlist)
    (shell-quote-argument (format "--playlist=%s" track-name)))
   (t (shell-quote-argument track-name))))

(defadvice emms-player-mpv-radiko-start (around override activate)
  "Override emms-player-mpv-radiko-start
Defined in define-emms-simple-player macro."
  (let* ((track (ad-get-arg 0))
         (socket (emms-player-mpv-radiko--socket))
         (input-socket (format "--input-unix-socket=%s" socket))
         (track-name (emms-track-name track))
         (track-type (emms-track-type track))
         (mpv-form (emms-player-mpv-radiko--track-name-to-mpv-format
                     track-name track-type))
         (media-title
          (shell-quote-argument
           (format "--media-title=%s"
                   (if (eq track-type 'streamlist)
                       (emms-stream-name (emms-track-get track 'metadata))
                     (file-name-nondirectory track-name)))))
         (process
          (cond
           ((string-match-p "^radiko://" track-name)
            (apply  emms-radiko-start-mpv-simple-process-function
                    (replace-regexp-in-string "^radiko://" "" track-name)
                    emms-player-mpv-radiko-command-name
                    (cons media-title
                     (cons input-socket emms-player-mpv-radiko-parameters))))
           (t (apply 'start-process-shell-command
                     emms-player-simple-process-name
                     nil
                     emms-player-mpv-radiko-command-name
                     (append emms-player-mpv-radiko-parameters
                             (list media-title
                                   input-socket mpv-form)))))))
    (set-process-sentinel process 'emms-player-simple-sentinel)
    (emms-player-mpv-radiko--reset-control track-name)
    (emms-player-started emms-player-mpv-radiko)
    (while (and (eq (process-status process) 'run)
                (not (file-exists-p socket)))
      (sit-for 0.05))
    (setq emms-player-mpv-radiko--tq (emms-player-radiko--tq-create))
    (set-process-filter (tq-process emms-player-mpv-radiko--tq)
                        'emms-player-mpv-radiko--socket-filter)))

;; To control mpv

(defun emms-player-mpv-radiko--volume-change-helper (v ans-ls)
  "Set volume to V in emms-player-mpv-radiko-volume-change."
  (if (emms-player-mpv-radiko-tq-successp ans-ls)
      (let* ((data (emms-player-mpv-radiko-tq-assoc-v 'data ans-ls))
             (vol (round (+ data v)))
             (vol (cond
                   ((< vol 0) 0)
                   ((> vol 100) 100)
                   (t vol))))
        (emms-player-mpv-radiko-tq-enqueue
         (list "set_property" "volume" vol)
         vol
         (lambda (vol ans-ls)
           (if (emms-player-mpv-radiko-tq-successp ans-ls)
               (message "mpv volume : %s" vol)
             (message "mpv volume : error")))))
    (message "mpv volume : error")))

(defun emms-player-mpv-radiko-volume-change (v)
  "Change volume to V for mpv."
  (emms-player-mpv-radiko-tq-clear)
  (emms-player-mpv-radiko-tq-enqueue
   (list "get_property" "volume")
   v 'emms-player-mpv-radiko--volume-change-helper))

(defun emms-player-mpv-radiko-volume-set (v)
  "Set volume to V for mpv."
  (interactive "nmpv volume set : ")
  (emms-player-mpv-radiko-tq-enqueue
   (list "set_property" "volume" v)
   v
   (lambda (v ans-ls)
     (if (emms-player-mpv-radiko-tq-successp ans-ls)
         (message "mpv volume : %s" v)
       (message "mpv volume : error")))))

(defun emms-palyer-mpv-radiko-time-pos-message ()
  "Get time pos in current file for mpv."
  (interactive)
  (emms-player-mpv-radiko-tq-enqueue
   '("get_property" "time-pos")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-mpv-radiko-tq-successp ans-ls)
         (let* ((data (round(emms-player-mpv-radiko-tq-assoc-v 'data ans-ls)))
                (h (/ data 3600))
                (m (/ (- data (* 3600 h)) 60))
                (s (- data (* 60 (+ (* 60 h) m)))))
           (message "mpv time position : %02d:%02d:%02d" h m s))
       (message "mpv time position : error")))))

(defun emms-palyer-mpv-radiko-percent-pos-message ()
  "Get percent pos in current file for mpv."
  (interactive)
  (emms-player-mpv-radiko-tq-enqueue
   '("get_property" "percent-pos")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-mpv-radiko-tq-successp ans-ls)
         (let ((data (emms-player-mpv-radiko-tq-assoc-v 'data ans-ls)))
           (message "mpv time position (%%) : %.2f" data))
       (message "mpv time position (%%) : error")))))

(defun emms-player-mpv-radiko--seek-helper (als ans-ls)
  ;; als  : ((sec . n0) (len . n1))
  ;; data : time-pos
  (if (emms-player-mpv-radiko-tq-successp ans-ls)
      (let* ((sec (assoc-default 'sec als))
             (len (assoc-default 'len als))
             (data  (emms-player-mpv-radiko-tq-assoc-v 'data ans-ls))
             (data+ (+ sec data))
             (next-sec (round (cond
                               ((< data+ 0) 0)
                               ((> data+ len) len)
                               (t data+))))
             (h (/ next-sec 3600))
             (m (/ (- next-sec (* 3600 h)) 60))
             (s (- next-sec (* 60 (+ (* 60 h) m)))))
        (emms-player-mpv-radiko-tq-enqueue
         (list "seek" (number-to-string sec) "relative")
         (format "mpv seek %s : %02d:%02d:%02d" (if (>= sec 0) ">>" "<<") h m s)
         (lambda (form ans-ls)
           (if (emms-player-mpv-radiko-tq-successp ans-ls)
               (message form)
             (message "mpv seek : error")))))
    (message "mpv seek : error")))

(defun emms-player-mpv-radiko--seek-helper-streamline (sec)
  ;; For a track which does not have length property.
  (emms-player-mpv-radiko-tq-enqueue
   (list "seek" (number-to-string sec) "relative")
   nil
   (emms-player-mpv-radiko-tq-error-message
    (format "mpv seek %s %+d : %%s" (if (>= sec 0) ">>" "<<") sec))))

(defun emms-player-mpv-radiko-seek (sec)
  "Seek by SEC for mpv."
  (emms-player-mpv-radiko-tq-clear)
  (emms-player-mpv-radiko-tq-enqueue
   '("get_property" "length")
   sec
   (lambda (sec ans-ls)
     (if (emms-player-mpv-radiko-tq-successp ans-ls)
         (let ((data (emms-player-mpv-radiko-tq-assoc-v 'data ans-ls)))
           (emms-player-mpv-radiko-tq-enqueue
            '("get_property" "time-pos")
            `((sec . ,sec) (len . ,data))
            'emms-player-mpv-radiko--seek-helper))
       (emms-player-mpv-radiko--seek-helper-streamline sec)))))

(defun emms-player-mpv-radiko-seek-to (sec)
  "Seek to SEC for mpv."
  (interactive "nmpv seek to (sec) : ")
  (emms-player-mpv-radiko-tq-enqueue
   (list "seek" (number-to-string sec) "absolute")
   sec
   (lambda (sec ans-ls)
     (if (emms-player-mpv-radiko-tq-successp ans-ls)
         (let* ((sec (round sec))
                (h (/ sec 3600))
                (m (/ (- sec (* 3600 h)) 60))
                (s (- sec (* 60 (+ (* 60 h) m)))))
           (message "mpv seek to : %02d:%02d:%02d" h m s))
       (message "mpv seek to : error")))))

(defun emms-player-mpv-radiko-seek-to-% (per)
  "Seek to PER(percent position) for mpv."
  (interactive "nmpv seek to (%%) : ")
  (setq per (cond ((< per 0) 0) ((> per 100) 100) (t per)))
  (emms-player-mpv-radiko-tq-enqueue
   '("get_property" "length")
   per
   (lambda (per ans-ls)
     (if (emms-player-mpv-radiko-tq-successp ans-ls)
         (let* ((data (emms-player-mpv-radiko-tq-assoc-v 'data ans-ls))
                (pos  (/ (round (* per data)) 100))
                (h (/ pos 3600))
                (m (/ (- pos (* 3600 h)) 60))
                (s (- pos (* 60 (+ (* 60 h) m)))))
           (emms-player-mpv-radiko-tq-enqueue
            (list "seek" (number-to-string per) "absolute-percent")
            (format "mpv seek to %s(%%%%) : %02d:%02d:%02d" per  h m s)
            (lambda (form ans-ls)
              (if (emms-player-mpv-radiko-tq-successp ans-ls)
                  (message form)
                (message "mpv seek to (%%) : error")))))
       (message "mpv seek to (%%) : error")))))

(defun emms-player-mpv-radiko-mute-on ()
  "Mute on for mpv."
  (emms-player-mpv-radiko-tq-enqueue
   '("set_property_string" "mute" "yes")
   nil
   (emms-player-mpv-radiko-tq-error-message "mpv mute on : %s")))

(defun emms-player-mpv-radiko-mute-off ()
  "Mute off for mpv."
  (emms-player-mpv-radiko-tq-enqueue
   '("set_property_string" "mute" "no")
   nil
   (emms-player-mpv-radiko-tq-error-message "mpv mute off : %s")))

(defun emms-player-mpv-radiko-mute ()
  "Cycle mut for mpv"
  (interactive)
  (emms-player-mpv-radiko-tq-clear)
  (emms-player-mpv-radiko-tq-enqueue
   '("cycle" "mute")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-mpv-radiko-tq-successp ans-ls)
         (emms-player-mpv-radiko-tq-enqueue
          '("get_property_string" "mute")
          nil
          (emms-player-mpv-radiko-tq-data-message "mpv mute : %s"))
       (message "mpv mute : error")))))

(defun emms-player-mpv-radiko-pause ()
  "Pause for mpv."
  (emms-player-mpv-radiko-tq-enqueue
   '("set_property_string" "pause" "yes")
   nil
   (emms-player-mpv-radiko-tq-error-message "mpv pause : %s")))

(defun emms-player-mpv-radiko-resume ()
  "Unpause for mpv."
  (emms-player-mpv-radiko-tq-enqueue
   '("set_property_string" "pause" "no")
   nil
   (emms-player-mpv-radiko-tq-error-message "mpv unpause : %s")))

(defmacro emms-player-mpv-radiko--playlist-change-helper (str)
  (let ((n (if (string= str "next") 1  -1)))
    `(progn
       (emms-player-mpv-radiko-tq-clear)
       (emms-player-mpv-radiko-tq-enqueue
        '("get_property" "playlist-pos")
        nil
        (lambda (_ ans-ls)
          (if (emms-player-mpv-radiko-tq-successp ans-ls)
              (let* ((data (emms-player-mpv-radiko-tq-assoc-v 'data ans-ls))
                     (form (format "mpv playlist_%s position %s : %%s"
                                   ,str (+ data ,n))))
                (emms-player-mpv-radiko-tq-enqueue
                 '(,(format "playlist_%s" str))
                 form
                 (lambda (form ans-ls)
                   (if (emms-player-mpv-radiko-tq-successp ans-ls)
                       (message form "success")
                     (message form "error")))))
            (message ,(format "mpv playlist_%s : error" str))))))))

(defun emms-player-mpv-radiko-playlist-next ()
  "Playlist next for mpv."
  (interactive)
  (emms-player-mpv-radiko--playlist-change-helper "next"))

(defun emms-player-mpv-radiko-playlist-prev ()
  "Playlist prev for mpv."
  (interactive)
  (emms-player-mpv-radiko--playlist-change-helper "prev"))

;; Reset control when emms-player-mpv-radiko starts.
(defun emms-player-mpv-radiko--set-default-volume-function ()
  "Reset volume control in emms-player-stopped-hook or emms-player-finished-hook."
  (let ((default-volume-function
          (get 'emms-player-mpv-radiko-volume-change
                       'default-volume-function)))
    (if  (null default-volume-function)
        ;; Defalt value in emms-volume.el
        (setq emms-volume-change-function 'emms-volume-amixer-change)
      (setq emms-volume-change-function default-volume-function)
      (put 'emms-player-mpv-radiko-volume-change
           'default-volume-function nil)))
  (remove-hook 'emms-player-stopped-hook
               'emms-player-mpv-radiko--set-default-volume-function)
  (remove-hook 'emms-player-finished-hook
               'emms-player-mpv-radiko--set-default-volume-function))

(defun emms-player-mpv-radiko--reset-control (track-name)
  "Reset control command not to pause and seek while playing radiko by TRACK-NAME."
  (put 'emms-player-mpv-radiko-volume-change
       'default-volume-function emms-volume-change-function)
  (setq emms-volume-change-function 'emms-player-mpv-radiko-volume-change)
  (add-hook 'emms-player-stopped-hook
            'emms-player-mpv-radiko--set-default-volume-function)
  (add-hook 'emms-player-finished-hook
            'emms-player-mpv-radiko--set-default-volume-function)
  (cond
   ((string-match-p "^radiko://" track-name)
    ;; Can not seek while playing radiko
    (dolist (action '(seek seek-to))
      (emms-player-set emms-player-mpv-radiko
                       action
                       nil))
    (when (member emms-radiko-start-mpv-simple-process-function
                  '(emms-radiko-start-simple-process-rtmpdump
                    emms-radiko-start-simple-process-rtmpdump-while))
      ;; Can not pause while playing radiko with rtmpdump
      ;; Mute inssted of pause
      (emms-player-set emms-player-mpv-radiko
                       'pause
                       'emms-player-mpv-radiko-mute-on)
      (emms-player-set emms-player-mpv-radiko
                       'resume
                       'emms-player-mpv-radiko-mute-off)))
   (t ;; Default
    (emms-player-set emms-player-mpv-radiko
                     'pause
                     'emms-player-mpv-radiko-pause)

    (emms-player-set emms-player-mpv-radiko
                     'resume
                     'emms-player-mpv-radiko-resume)

    (emms-player-set emms-player-mpv-radiko
                     'seek
                     'emms-player-mpv-radiko-seek)

    (emms-player-set emms-player-mpv-radiko
                     'seek-to
                     'emms-player-mpv-radiko-seek-to))))

(provide 'emms-player-mpv-radiko)
;;; emms-player-mpv-radiko.el ends here
