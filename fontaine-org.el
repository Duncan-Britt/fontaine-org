;;; fontaine-org.el --- Org extensions for fontaine -*- lexical-binding: t -*-

;; Copyright (C) 2024 Duncan Britt

;; Author: Duncan Britt
;; Contact: https://github.com/Duncan-Britt/fontaine-org/issues
;; Homepage: https://github.com/Duncan-Britt/fontaine-org
;; Keywords: faces

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; ┌─────────┐
;; │ Summary │
;; └─────────┘
;; Extends `fontaine' to make keywords available within
;; `fontaine-presets' for setting custom font families for Org mode
;; headings.
;; ┌─────────────┐
;; │ Example use │
;; └─────────────┘
;; (setq fontaine-presets
;;       `((Adelle-regular
;;            :default-height 130
;;            :default-family "Iosevka"
;;            :fixed-pitch-family "Iosevka"
;;            :fixed-pitch-height 1.0
;;            :org-level-1-family "Symbola" ;; ┐
;;            :org-level-2-family "Symbola" ;; │
;;            :org-level-3-family "Symbola" ;; │
;;            :org-level-4-family "Symbola" ;; │ <-- Requires `fontaine-org'.
;;            :org-level-5-family "Symbola" ;; │
;;            :org-level-6-family "Symbola" ;; │
;;            :org-level-7-family "Symbola" ;; │
;;            :org-level-8-family "Symbola" ;; ┘
;;            :variable-pitch-family  "Adelle"
;;            :variable-pitch-height 1.0)))
;;
;; ┌─────────┐
;; │ Install │
;; └─────────┘
;; (use-package
;;   :ensure (:host github :repo "Duncan-Britt/fontaine-org")
;;   :after fontaine
;;   :config
;;   (fontaine-org-mode 1))

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

(defun fontaine-org--get-face-spec (face family weight slant height width)
  "Create a face spec for FACE with the given attributes."
  (when (or family weight slant height width)
    `(,face
      ((((type graphic))
        ,@(when family (list :family family))
        ,@(when weight (list :weight weight))
        ,@(when slant (list :slant slant))
        ,@(when height (list :height height))
        ,@(when width (list :width width)))))))

(defun fontaine-org--apply-heading-fonts (preset)
  "Apply org heading fonts from PRESET configuration."
  (let* ((props (fontaine--get-preset-properties preset))
         (variable-pitch-family (plist-get props :variable-pitch-family))
         (custom--inhibit-theme-enable nil)
         (faces nil))
    (dolist (face fontaine-org-heading-faces)
      (let* ((face-name (symbol-name face))
             (family-prop (intern (format ":%s-family" face-name)))
             (weight-prop (intern (format ":%s-weight" face-name)))
             (height-prop (intern (format ":%s-height" face-name)))
             (slant-prop (intern (format ":%s-slant" face-name)))
             (width-prop (intern (format ":%s-width" face-name)))
             (family (or (plist-get props family-prop) variable-pitch-family))
             (weight (plist-get props weight-prop))
             (slant (plist-get props slant-prop))
             (height (plist-get props height-prop))
             (width (plist-get props width-prop))
             (face-spec (fontaine-org--get-face-spec
                         face family weight slant height width)))
        (when face-spec
          (push face-spec faces))))
    (when faces
      (apply 'custom-theme-set-faces 'fontaine faces))))

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
