(require 'exwm)
(require 'exwm-randr)

  (setq exwm-workspace-number 5)
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j ;; Buffer list
          ?\C-\ ))

(setq exwm-randr-workspace-output-plist '(1 "HDMI-A-0" 3 "DisplayPort-2"))

(exwm-randr-enable)
(start-process-shell-command "xrandr" "xrandr" "~/.screenlayout/exwm.sh")

;(after! perspective-exwm
;  (setq persp-mode-prefix-key (kbd "M-TAB")))

(setq perspective-exwm-override-initial-name
'((0 . "browser")
        (1 . "dev")
        (2 . "vm")
        (3 . "media")
        (4 . "comms")
        (5 . "system")))

(perspective-exwm-mode)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

(display-time-mode)

(exwm-enable)
