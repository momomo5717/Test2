;;; emms-radiko-stream-list.el --- stream lists to play 超!A&G+, らじる★らじる and radiko for emms

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

;; Usage
;; If you want to save the following stream lists to emms-stream-bookmarks-file,
;; eval the folloing example.
;;
;; ;;add stream list you want to play
;; (emms-radiko-stream-add-bookmark-and-save
;;  emms-radiko-stream-list-agqr
;;  (emms-radiko-get-rajiru-stream-list-rajiru 'tokyo)
;;  (emms-radiko-wget-current-area-radiko-stream-list))
;;
;; 
;;; Code:
(require 'emms-streams)
(require 'emms-radiko-start-simple-process)

(defvar emms-radiko-stream-list-agqr
  '(("超!A&G+" "rtmp://fms-base2.mitene.ad.jp/agqr/aandg22" 1 streamlist))
  "超!A&G+ stream list")

(defvar emms-radiko-stream-list-rajiru-sendai
    ;; らじる★らじる
  '(("NHK第1 仙台"
     "mms://a1125.l22063959124.c220639.g.lm.akamaistream.net/D/1125/220639/v0001/reflector:59124"
     1 streamlist)
    ("NHK第2"
     "mms://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 仙台"
     "mms://a18.l22064052017.c220640.g.lm.akamaistream.net/D/18/220640/v0001/reflector:52017"
     1 streamlist))
  "らじる★らじる 仙台 stream list")

(defvar emms-radiko-stream-list-rajiru-tokyo
    ;; らじる★らじる
  '(("NHK第1 東京"
     "mms://a33.l12993146032.c129931.g.lm.akamaistream.net/D/33/129931/v0001/reflector:46032"
     1 streamlist)
    ("NHK第2"
     "mms://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 東京"
     "mms://a52.l12993346051.c129933.g.lm.akamaistream.net/D/52/129933/v0001/reflector:46051"
     1 streamlist))
  "らじる★らじる 東京 stream list")

(defvar emms-radiko-stream-list-rajiru-nagoya
    ;; らじる★らじる
  '(("NHK第1 名古屋"
     "mms://a220.l22063752219.c220637.g.lm.akamaistream.net/D/220/220637/v0001/reflector:52219"    
     1 streamlist)
    ("NHK第2"
     "mms://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 名古屋"
     "mms://a1741.l22063855740.c220638.g.lm.akamaistream.net/D/1741/220638/v0001/reflector:55740"
     1 streamlist))
  "らじる★らじる 名古屋 stream list")

(defvar emms-radiko-stream-list-rajiru-osaka
    ;; らじる★らじる
  '(("NHK第1 大阪"
     "mms://a1532.l22063553531.c220635.g.lm.akamaistream.net/D/1532/220635/v0001/reflector:53531"
     1 streamlist)
    ("NHK第2"
     "mms://a57.l12993246056.c129932.g.lm.akamaistream.net/D/57/129932/v0001/reflector:46056"
     1 streamlist)
    ("NHK-FM 大阪"
     "mms://a884.l22063650883.c220636.g.lm.akamaistream.net/D/884/220636/v0001/reflector:50883"
     1 streamlist))
  "らじる★らじる 大阪 stream list")

(defun emms-radiko-get-rajiru-stream-list (area)
  (cl-case area
    (sendai emms-radiko-stream-list-rajiru-sendai)
    (tokyo  emms-radiko-stream-list-rajiru-tokyo)
    (nagoya emms-radiko-stream-list-rajiru-nagoya)
    (osaka  emms-radiko-stream-list-rajiru-osaka)
    (t nil)))

(defvar emms-radiko-stream-list-radiko
  '(("TBSラジオ"        "radiko://TBS"            1 streamlist)
    ("文化放送"         "radiko://QRR"            1 streamlist)
    ("ニッポン放送"     "radiko://LFR"            1 streamlist)
    ("ラジオNIKKEI第1 " "radiko://RN1"            1 streamlist)
    ("ラジオNIKKEI第2"  "radiko://RN2"            1 streamlist)
    ("InterFM"          "radiko://INT"            1 streamlist)
    ("TOKYO FM"         "radiko://FMT"            1 streamlist)
    ("J-WAVE"           "radiko://FMJ"            1 streamlist)
    ("ラジオ日本"       "radiko://JORF"           1 streamlist)
    ("bayfm78"          "radiko://BAYFM78"        1 streamlist)
    ("NACK5"            "radiko://NACK5"          1 streamlist)
    ("ＦＭヨコハマ"     "radiko://YFM"            1 streamlist)
    ("放送大学"         "radiko://HOUSOU-DAIGAKU" 1 streamlist))
  "Example radiko stream list (area_id=JP13)")

(defun emms-radiko-wget-radiko-stream-list (area-id)
  "Retrun AREA-ID radiko stream list.
string -> stream-list
\(emms-radiko-wget-radiko-stream-list \"JP13\"\) => stream-list-radiko"
  (with-temp-buffer
    (unless (zerop
             (call-process
              "wget" nil t nil "-q" "-O" "-"
              (format "http://radiko.jp/v2/station/list/%s.xml" area-id)))
      (error "failed wget aera station xml"))
    (goto-char (point-min))
    (cl-loop with ls
     unless (search-forward "<id>" nil t) return (nreverse ls) do
     (let ((id-b (match-end 0))
           (id-e (progn (search-forward "</id>" nil t)
                        (match-beginning 0)))
           (name-b (search-forward "<name>" nil t))
           (name-e (progn (search-forward "</name>" nil t)
                          (match-beginning 0))))
       (push (list (buffer-substring name-b name-e)
                   (format "radiko://%s"
                           (buffer-substring id-b id-e))
                   1 'streamlist)
             ls)))))

(defun emms-radiko-wget-current-area-radiko-stream-list () 
  (emms-radiko-wget-radiko-stream-list
   (emms-radiko-get-area-id (emms-radiko-access-auth2-fms
                             (emms-radiko-access-auth1-fms)))))

(defun emms-radiko-stream-add-bookmark (&rest stream-lists)
  (kill-buffer (get-buffer-create emms-stream-buffer-name))
  (set-buffer (get-buffer-create emms-stream-buffer-name))
  (erase-buffer)  
  (emms-stream-mode)
  (setq emms-stream-list
        (append (apply 'append stream-lists)
                emms-stream-list))
  (emms-stream-redisplay)
  (goto-char (point-min)))

(defun emms-radiko-stream-add-bookmark-and-save (&rest stream-lists)
  (apply 'emms-radiko-stream-add-bookmark stream-lists)
  (emms-stream-save-bookmarks-file))

(provide 'emms-radiko-stream-list)
;;; emms-radiko-stream-list.el ends here
