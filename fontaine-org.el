;;; fontaine-org.el --- Org extensions for fontaine -*- lexical-binding: t -*-

;; Copyright (C) 2024 Duncan Britt

;; Author: Duncan Britt <dbru997@gmail.com>
;; Homepage: https://github.com/Duncan-Britt/fontaine-org
;; Keywords: Graphics,images,themes

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29"))

;; The software is provided “as is”, without warranty of any kind, express or implied,
;; including but not limited to the warranties of merchantability, fitness for a particular
;; purpose and noninfringement. in no event shall the authors or copyright holders be liable
;; for any claim, damages or other liability, whether in an action of contract, tort or
;; otherwise, arising from, out of or in connection with the software or the use or other
;; dealings in the software.

;;; Commentary:

;;;

;;; Code:
(require 'fontaine)
(require 'org)

(defconst fontaine-org-heading-faces
  '(org-level-1 org-level-2 org-level-3 org-level-4
    org-level-5 org-level-6 org-level-7 org-level-8)
  "List of org heading faces to be managed by fontaine.")

(defun fontaine-org--add-faces ()
  "Add org heading faces to `fontaine-faces'."
  (setq fontaine-faces
        (append fontaine-faces fontaine-org-heading-faces)))

(defun fontaine-org--apply-heading-fonts (preset)
  "Apply org heading fonts from PRESET configuration."
  (let* ((props (fontaine--get-preset-properties preset))
         (variable-pitch-family (plist-get props :variable-pitch-family)))
    (dolist (face fontaine-org-heading-faces)
      (let* ((face-name (symbol-name face))
             (family-prop (intern (format ":%s-family" face-name)))
             (weight-prop (intern (format ":%s-weight" face-name)))
             (height-prop (intern (format ":%s-height" face-name)))
             (slant-prop (intern (format ":%s-slant" face-name)))
             (width-prop (intern (format ":%s-width" face-name))))
        ;; If no specific family is set for this org heading level,
        ;; use the variable-pitch family
        (fontaine--set-face-attributes
         face
         (or (plist-get props family-prop) variable-pitch-family)
         (plist-get props weight-prop)
         (plist-get props slant-prop)
         (plist-get props height-prop)
         (plist-get props width-prop))))))

(defun fontaine-org--advice-after-set-preset (&rest _)
  "Additional font settings for org-mode after fontaine preset is applied."
  (when fontaine-current-preset
    (fontaine-org--apply-heading-fonts fontaine-current-preset)))

;;;###autoload
(define-minor-mode fontaine-org-mode
  "Add org-mode heading font support to fontaine."
  :global t
  (if fontaine-org-mode
      (progn
        (fontaine-org--add-faces)
        (add-hook 'fontaine-set-preset-hook #'fontaine-org--advice-after-set-preset))
    (setq fontaine-faces (cl-set-difference fontaine-faces fontaine-org-heading-faces))
    (remove-hook 'fontaine-set-preset-hook #'fontaine-org--advice-after-set-preset)))

(provide 'fontaine-org)
;;; fontaine-org.el ends here
