;;; emms-radiko-start-simple-process.el --- helper function to play radiko for emms-player-simple-process

;; Copyright (C) 2015 momomo5717

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
;;  Requirements
;;  wget, swfextract (swftools), rtmpdump
;;
;;; Code:

(require 'cl-lib)
(require 'emms-player-simple)

(defvar emms-radiko-playerurl  "http://radiko.jp/player/swf/player_4.1.0.00.swf")
(defvar emms-radiko-playerfile
  (expand-file-name "radiko_player.swf" temporary-file-directory))
(defvar emms-radiko-keyfile
  (expand-file-name "radiko_authkey.png" temporary-file-directory))

(defun emms-radiko--wget-playerfile ()
  "Wget player.swf to emms-radiko-playerfile."
  (unless (file-exists-p emms-radiko-playerfile)
    (unless (zerop (call-process-shell-command
                    (format "wget -q -O %s %s"
                            emms-radiko-playerfile
                            emms-radiko-playerurl)))
      (error "Failed to wget the player.swf"))))

(defun emms-radiko--write-keydata ()
  "Write keydata from emms-radiko-keyfile."
  (unless (file-exists-p emms-radiko-keyfile)
    (unless (zerop (call-process-shell-command
                    (format "swfextract -b 14 %s -o %s"
                            emms-radiko-playerfile
                            emms-radiko-keyfile)))
      (error "Failed to write the keydata"))))

(defun emms-radiko-access-auth1-fms ()
  "Return auth1_fms."
  (shell-command-to-string
   "wget -q \\
     --header=\"pragma: no-cache\" \\
     --header=\"X-Radiko-App: pc_1\" \\
     --header=\"X-Radiko-App-Version: 2.0.1\" \\
     --header=\"X-Radiko-User: test-stream\" \\
     --header=\"X-Radiko-Device: pc\" \\
     --post-data='\\r\\n' \\
     --no-check-certificate \\
     --save-headers \\
     -O - \\
     https://radiko.jp/v2/api/auth1_fms"))

(defun emms-radiko--get-auth1-value (key auth1)
  "Return value of KEY from AUTH1."
  (if (string-match (format "%s: " key) auth1)
      (let ((pos (match-end 0)))
        (if (string-match "\r\n" auth1 pos)
            (substring auth1 pos (match-beginning 0))))
    (error "Failed to get %s value" key)))

(defun emms-radiko--get-authtoken (auth1)
  "Return X-Radiko-Authtoken value from  AUTH1."
  (emms-radiko--get-auth1-value "X-Radiko-AuthToken" auth1))

(defun emms-radiko--get-offset (auth1)
  "Return x-radiko-keyoffset value from  AUTH1."
  (emms-radiko--get-auth1-value "X-Radiko-KeyOffset" auth1))

(defun emms-radiko--get-length (auth1)
  "Return x-radiko-keylength value from  AUTH1."
  (emms-radiko--get-auth1-value "X-Radiko-KeyLength" auth1))

(defun emms-radiko--get-partialkey (keyfile auth1)
  "Return partialkey from KEYFILE, AUTH1."
  (car (split-string
        (shell-command-to-string
         (format "dd if=%s bs=1 skip=%s count=%s 2> /dev/null | base64"
                 keyfile
                 (emms-radiko--get-offset auth1)
                 (emms-radiko--get-length auth1)))
        "\n")))

(defun emms-radiko-access-auth2-fms (auth1)
  "Return auth2_fms from AUTH1."
  (shell-command-to-string
   (format
    "wget -q \\
        --header=\"pragma: no-cache\" \\
        --header=\"X-Radiko-App: pc_1\" \\
        --header=\"X-Radiko-App-Version: 2.0.1\" \\
        --header=\"X-Radiko-User: test-stream\" \\
        --header=\"X-Radiko-Device: pc\" \\
        --header=\"X-Radiko-Authtoken: %s\" \\
        --header=\"X-Radiko-Partialkey: %s\" \\
        --post-data='\\r\\n' \\
        --no-check-certificate \\
        -O - \\
        https://radiko.jp/v2/api/auth2_fms"
    (emms-radiko--get-authtoken auth1)
    (emms-radiko--get-partialkey emms-radiko-keyfile auth1))))

(defun emms-radiko-get-area-id (auth2)
  "Retrun area-id from AUTH2."
  (if (string-match "^.+[0-9][,]" auth2)
      (substring auth2 (match-beginning 0) (1- (match-end 0)))
    (error "Failed to get area-id")))

(defun emms-radiko-wget-stream-url (channel)
  "Return stream-url from CHANNEL.
\"channel\"
=> \"rtmpe://f-radiko.smartstream.ne.jp/channel/_definst_/simul-stream.stream\""
  (with-temp-buffer
    (unless (zerop
             (call-process
              "wget"  nil t nil "-q" "-O" "-"
              (format "http://radiko.jp/v2/station/stream/%s.xml" channel)))
      (error "Failed to wget station/stream/%s.xml" channel))
    (goto-char (point-min))
    (let ((item-b (search-forward "<item>" nil t))
          (item-e (progn (search-forward "</item>" nil t)
                         (match-beginning 0))))
      (if (and item-b item-e)
          (buffer-substring item-b item-e)
        (error "Failed to parce stream url")))))

(defun emms-radiko--split-url (stream-url)
  "Split url to use rtmpdump args from STREAM-URL."
  ;; "rtmpe://f-radiko.smartstream.ne.jp/channel/_definst_/simul-stream.stream"
  ;; => ("rtmpe://f-radiko.smartstream.ne.jp" "channel" "_definst_" "simul-stream.stream") 
  (if (and (string-match "://" stream-url)
           (string-match "/" stream-url (match-end 0)))
      (let ((pos (match-end 0)))
        (cons (substring stream-url 0 (1- pos))
              (split-string (substring stream-url pos) "/" t)))
    (error "Failed to split stream-url")))

;; rtmpdump pipe

(defvar emms-radiko-play-rtmpdump-form-params nil)

(defun emms-radiko-play-rtmpdump-form-arg (channel &optional params)
  "Return rtmpdump args string for CHANNEL."
  (emms-radiko--wget-playerfile)
  (emms-radiko--write-keydata)
  (let* ((url-ls (emms-radiko--split-url
                  (emms-radiko-wget-stream-url channel)))
         (url0 (cl-first url-ls))
         (url1 (format "%s/%s" (cl-second url-ls) (cl-third url-ls)))
         (url2 (cl-fourth url-ls))
         (auth1 (emms-radiko-access-auth1-fms))
         (playerurl  emms-radiko-playerurl)
         (authtoken (emms-radiko--get-authtoken auth1)))
    (emms-radiko-access-auth2-fms auth1)
    (format "-r %s --app %s --playpath %s -W %s -C S: -C S: -C S: -C S:%s --live %s"
            url0 url1 url2 playerurl authtoken
            (mapconcat #'identity params " "))))

(defun emms-radiko-start-simple-process-rtmpdump (channel media-player &rest player-args)
  "Start process for emms-player-simple-process using rtmpdump.
Use rtmpdump pipe.
string string string ... -> #<emms-player-simple-process>"
  (when (string= media-player "mplayer")
   (setq player-args (remove "-slave" player-args)))
  (start-process-shell-command
   emms-player-simple-process-name
   nil
   (format "rtmpdump %s | %s %s -"
           (emms-radiko-play-rtmpdump-form-arg
            channel emms-radiko-play-rtmpdump-form-params)
           media-player (mapconcat #'identity player-args " "))))

(defun emms-radiko-start-simple-process-rtmpdump-while (channel media-player &rest player-args)
  "Start process for emms-player-simple-process using rtmpdump.
Use rtmpdump pipe.
string string string ... -> #<emms-player-simple-process>"
  (when (string= media-player "mplayer")
   (setq player-args (remove "-slave" player-args)))
  (start-process-shell-command
   emms-player-simple-process-name
   nil
   (format "while :; do rtmpdump %s | %s %s - ; done;"
           (emms-radiko-play-rtmpdump-form-arg
            channel emms-radiko-play-rtmpdump-form-params)
           media-player (mapconcat #'identity player-args " "))))

;; play ffpeg://rtpe:// form

(defvar emms-radiko-play-ffmpeg-form-params nil)

(defun emms-radiko-play-ffmpeg-form-arg (channel &optional params)
  "Return \"ffmpeg://rtmpe://...\" ffplay format arg"
  (emms-radiko--wget-playerfile)
  (emms-radiko--write-keydata)
  (let* ((rtmpe-url (emms-radiko-wget-stream-url channel))
        (auth1 (emms-radiko-access-auth1-fms))
        (authtoken (emms-radiko--get-authtoken auth1)))
    (emms-radiko-access-auth2-fms auth1)
    (format "ffmpeg://%s swfUrl=%s swfVfy=1 conn=S:  conn=S:  conn=S:  conn=S:%s live=1 %s "
            rtmpe-url emms-radiko-playerurl authtoken
            (mapconcat #'identity params " "))))

(defun emms-radiko-start-simple-process-ffmpeg (channel media-player &rest player-args)
    "Start process for emms-player-simple-process.
Use ffmpeg://rtmpe:// format arg
string string string ... -> #<emms-player-simple-process>"    
    (start-process-shell-command
     emms-player-simple-process-name
     nil
     (format "%s  %s \"%s\""
             media-player
             (mapconcat #'identity player-args " ")
             (emms-radiko-play-ffmpeg-form-arg
              channel emms-radiko-play-ffmpeg-form-params))))

(defun emms-radiko-start-simple-process-ffmpeg-while (channel media-player &rest player-args)
    "Start process for emms-player-simple-process.
Use ffmpeg://rtmpe:// format arg
string string string ... -> #<emms-player-simple-process>"
    (start-process-shell-command
     emms-player-simple-process-name
     nil
     (format "while :; do %s  %s \"%s\" ; done;"
             media-player
             (mapconcat #'identity player-args " ")
             (emms-radiko-play-ffmpeg-form-arg
              channel emms-radiko-play-ffmpeg-form-params))))

;; play rtmpe:// form

(defvar emms-radiko-play-rtmpe-form-params nil)

(defun emms-radiko-play-rtmpe-form-arg (channel &optional params)
  "Retrun \"rtmpe://...\" ffplay format arg"
  (emms-radiko--wget-playerfile)
  (emms-radiko--write-keydata)
  (let* ((rtmpe-url (emms-radiko-wget-stream-url channel))
        (auth1 (emms-radiko-access-auth1-fms))
        (authtoken (emms-radiko--get-authtoken auth1)))
    (emms-radiko-access-auth2-fms auth1)
    (format "%s swfUrl=%s swfVfy=1 conn=S:  conn=S:  conn=S:  conn=S:%s live=1 %s"
            rtmpe-url emms-radiko-playerurl authtoken
            (mapconcat #'identity params " "))))

(defun emms-radiko-start-simple-process (channel media-player &rest player-args)
    "Start process for emms-player-simple-process.
Use rtmpe:// format arg.
string string string ... -> #<emms-player-simple-process>"
    (start-process-shell-command
     emms-player-simple-process-name
     nil
     (format "%s %s \"%s\""
             media-player
             (mapconcat #'identity player-args " ")
             (emms-radiko-play-rtmpe-form-arg
              channel emms-radiko-play-rtmpe-form-params))))

(provide 'emms-radiko-start-simple-process)
;;; emms-radiko-start-simple-process.el ends here
