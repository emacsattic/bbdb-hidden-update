;;; bbdb-hidden-update.el --- Update BBDB records very quietly and automatically

;;; Copyright (C) 2000 Nix

;; Author: Nix
;; Created: 2000-10-26
;; Last modified: 2000-10-28
;; Keywords: mail news
;; Version: $Revision: 1.1 $

;; This file is not part of XEmacs, or GNU Emacs.

;; This library is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2.1, or (at your option)
;; any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this library; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; The `bbdb-notice-hook' allows things to happen whenver a BBDB record
;; is noticed; but if you try to record that fact in the BBDB record itself,
;; you have the problem that the BBDB record is marked as changed in the
;; `bbdb-changed-records' list, and that the modification itself triggers
;; the `bbdb-change-hook', which by default calls `bbdb-timestamp-hook',
;; leading to the BBDB's last-modified timestamp being updated *every
;; single time* the record is noticed, and to `bbdb-changed' listing
;; every record you've seen since the database was last saved; not an
;; especially useful function.

;; (Yes, the docstring for `bbdb-change-hook' states that it is not called
;; for modifications made within the `bbdb-notice-hook'. This is incorrect;
;; maybe it is a bug.)

;; This file fixes this, providing the ability to have some field that is
;; automatically updated whenever a record is noticed, without making a fuss
;; about it, recording it as changed, or calling `bbdb-change-hook' or
;; `bbdb-after-change-hook'. (The .bbdb file is still marked as modified, so
;; that it will still be saved.)

;; This facility should be integrated --- in a neater and less fragile form ---
;; into BBDB itself; but until it is, this file exists to provide the same
;; facilities.

;; This will probably only work with version 2.00 of the BBDB, and may well
;; only work with version 2.00.06.

;; To use:

;; Call the function `bbdb-hidden-update-initialize' to begin hidden
;; updating. Add the names of functions to call to do the updating to the
;; `bbdb-hidden-update-functions' alist. They get passed the record to
;; use.

;; If you want to do some arbitrary thing without running the BBDB
;; change hooks (for instance, to avoid timestamp changes), you can
;; use the `with-bbdb-change-hooks-suppressed' macro.

;; If you just want to work around the buglet with `bbdb-notice-hook',
;; you just need to load the file; there is no need to run
;; `bbdb-hidden-update-initialize' unless you want to use
;; `bbdb-hidden-update-functions' in some way.

;; To do:

;; Parts of this code are very brittle, and highly dependent on BBDB version
;; (because of all the advisements). This will remain true if and until code
;; doing what this does is folded into the BBDB. (Note that this code should
;; *not* be folded in; it jumps through lots of hoops to avoid changing the
;; BBDB's code, and all these hoops can be thrown away.)

;; This might not work with GNU Emacs; I don't currently have a copy so it is
;; hard to test it there, and there may be unintentional dependencies on
;; XEmacs.

;; If you want to use this on GNU Emacs, and it doesn't work, please tell me
;; what is missing or broken for GNU Emacs support so I can fix it, as I
;; would like it to work there as well.

;;; Requirements:

(require 'advice)
(require 'bbdb)

;;; User-configurable variables:

(defvar bbdb-hidden-update-functions '()
  "Functions that should be called whenever a BBDB record is noticed.
These differ from functions on the `bbdb-notice-hook' in that if they
change the record they are modifying, the record is not marked as changed,
and the hooks `bbdb-change-hook' and `bbdb-after-change-hook' are not run.")

;;; Code:

;; Do a hidden update.

(defun bbdb-hidden-update-run-functions (record)
  "Do some BBDB record updates, very quietly.
This calls the functions named in the `bbdb-hidden-update-functions',
allowing them to change the `record' without marking it as changed,
or calling the `bbdb-change-hook' or `bbdb-after-change-hook'."

  ; The horrible frobbing with the `bbdb-changed-records' variable is because it
  ; is local to the .bbdb buffer, and is changed there by the function
  ; `bbdb-changed-records'. Locally rebinding the `bbdb-changed-records'
  ; variable does not work at all; it is not clear why this is.
  ; TODO: fix this.

  (let ((real-bbdb-changed-records (bbdb-with-db-buffer bbdb-changed-records)))
    (bbdb-invoke-hook 'bbdb-hidden-update-functions record)
    (bbdb-with-db-buffer
     (setq bbdb-changed-records real-bbdb-changed-records))))

;; Fix the `bbdb-.*-change-hook' buglet.

;; We have an advisement on `bbdb-invoke-hook' which suppresses execution of
;; hooks `eq' to `bbdb-change-hook' and `bbdb-after-change-hook' if a certain
;; variable is set; there is a macro `with-bbdb-change-hooks-suppressed' which
;; sets this variable; and there is an advisement on
;; `bbdb-annotate-message-sender' (which runs the `bbdb-notice-hook') that
;; forces `bbdb-annotate-message-sender' to be run
;; `with-bbdb-change-hooks-suppressed'.

;; This really does too much; if `bbdb-annotate-message-sender' does things
;; that might call the change hooks without going through the notice hooks,
;; they won't run. But as far as I can tell it does not do such things.

;; This could be done somewhat more simply if we could modify the BBDB source,
;; but we cannot assume that everyone will do that...

(defmacro with-bbdb-change-hooks-suppressed (&rest body)
  "Execute the forms in BODY without running the BBDB change hooks.
If the BBDB changes in a way that would normally run these hooks,
they will not run.
The value returned is the value of the last form in BODY."
  `(let ((bbdb-hidden-update-suppress-change-hooks t))
     ,@body))

;; This uses dynamic scoping, so we shut up the byte-compiler where it matters.

(let ((byte-compile-default-warnings
       (delq 'unused-vars (copy-list byte-compile-default-warnings))))

;; This advice does the actual work.

  (defadvice bbdb-invoke-hook (around bbdb-hidden-update-not-change-hooks
                               activate)
    "Do not call the `bbdb-change-hook' or `bbdb-after-change-hook' if inside
`bbdb-annotate-message-sender'."
    (or (and (boundp 'bbdb-hidden-update-suppress-change-hooks)
             (or (eq hook bbdb-change-hook)
                 (eq hook bbdb-after-change-hook)))
        ad-do-it)))

;; This advice merely suppresses the change hooks.

(defadvice bbdb-annotate-message-sender (around 
bbdb-hidden-update-annotate-message-sender
                                                activate)
  "Notify `bbdb-invoke-hook' not to run the `bbdb-change-hook' or
`bbdb-after-change-hook'."
  (with-bbdb-change-hooks-suppressed ad-do-it))

;; Initialization.

(defun bbdb-hidden-update-initialize ()
  "Prepare to do hidden updates of fields in the BBDB."
  (add-hook 'bbdb-notice-hook 'bbdb-hidden-update-run-functions))

(provide 'bbdb-hidden-update)
