;;; emms-player-mplayer-radiko.el --- emms simple player to play Radiko, etc

;; Copyright (C) 2015 momomo5717

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
;; (require 'emms-player-mplayer-radiko)
;; (add-to-list 'emms-player-list 'emms-player-mplayer-radiko)
;;
;;; Code:

(require 'emms-player-mplayer)
(require 'emms-radiko-start-simple-process)

(defgroup emms-player-mplayer-radiko nil
  "EMMS Player for Radiko"
  :group 'emms)

(defcustom emms-radiko-start-mplayer-simple-process-function 'emms-radiko-start-simple-process-rtmpdump
  "The function is used to play radiko."
  :type '(fucntion :tag "Function to start emms-player-simple-process for radiko")
  :group 'emms-player-mplayer-radiko)

(add-to-list 'emms-radiko-play-ffmpeg-form-params "buffer=3000")

(define-emms-simple-player mplayer-radiko '(streamlist)
  "\\`\\(radiko\\|mms\\|rtmp\\)://"
  "mplayer" "-slave" "-quiet" "-really-quiet")

(defadvice emms-player-mplayer-radiko-start (around override activate)
  "Override emms-player-mplayer-radiko-start
Defined in define-emms-simple-player macro."
  (let* ((track (ad-get-arg 0))
         (track-url (emms-track-name track))
         (stream-name (emms-stream-name (emms-track-get track 'metadata)))
         (track-arg
          (if (string-match-p "^radiko://" track-url)
              (replace-regexp-in-string "^radiko://" "" track-url)
            track-url))
         
         (process
          (cond
           (string-match-p "^radiko://" track-url)
           (apply emms-radiko-start-mplayer-simple-process-function
                  track-arg emms-player-mplayer-radiko-command-name
                  emms-player-mplayer-radiko-parameters)
           (t
            (apply 'start-process-shell-command
                   emms-player-simple-process-name
                   nil
                   emms-player-mplayer-radiko-command-name
                   (append emms-player-mplayer-radiko-parameters
                           (list track-arg)))))))
    (set-process-sentinel process 'emms-player-simple-sentinel))
  (emms-player-started emms-player-mplayer-radiko))

(defun emms-player-mplayer-radiko-mute-on ()
  "Depends on mplayer's -slave mode.
Mute on insted of pause."
  (process-send-string
   emms-player-simple-process-name "mute 1\n"))

(defun emms-player-mplayer-radiko-mute-off ()
  "Depends on mplayer's -slave mode.
Mute off insted of resume."
  (process-send-string
   emms-player-simple-process-name "mute 0\n"))

;; Can not use pause while playing radiko
;; Use mute insted of pause
(emms-player-set emms-player-mplayer-radiko
                 'pause
                 'emms-player-mplayer-radiko-mute-on)

(emms-player-set emms-player-mplayer-radiko
                 'resume
                 'emms-player-mplayer-radiko-mute-off)

(emms-player-set emms-player-mplayer-radiko
                 'seek
                 nil)

(emms-player-set emms-player-mplayer-radiko
                 'seek-to
                 nil)

(provide 'emms-player-mplayer-radiko)
;;; emms-player-mplayer-radiko.el ends here
