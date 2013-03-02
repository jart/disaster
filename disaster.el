;;; disaster.el --- Disassemble C/C++ code under cursor in Emacs

;; Copyright (C) 2013 Justine Tunney.

;; Author: Justine Tunney <jtunney@gmail.com>
;; Created: 2013-03-02
;; Version: 0.1
;; Keywords: tools
;; URL: https://github.com/jart/disaster

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ![Screenshot](http://i.imgur.com/vl2OD3P.png)
;;
;; Disaster shows you assembly code for the C/C++ file being currently edited
;; in a separate window. If you're using Clang, it'll also hop to the line in
;; the assembly file that corresponds to to the C/C++ text under your cursor.
;;
;; Disaster tries to be smart about detecting the location of your Makefile and
;; will fall back to invoking clang or cc manually if there's no Makefile. For
;; more information, see `disaster'.

;;; Installation:

;; Make sure to place `disaster.el` somewhere in the load-path and add the
;; following lines to your `.emacs` file to enable the `C-c C-d` shortcut to
;; invoke `disaster':
;;
;;     (eval-after-load 'cc-mode
;;       '(progn
;;          (require 'disaster)
;;          (defun my-c-mode-common-hook ()
;;            (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
;;            (define-key c-mode-base-map (kbd "C-c C-d") 'disaster))
;;          (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)))
;;
;; Put these lines in your Makefile so it knows how to compile assembly:
;;
;;     %.S: %.c   ; $(COMPILE.c)   -g -S $(OUTPUT_OPTION) $<
;;     %.S: %.cc  ; $(COMPILE.cc)  -g -S $(OUTPUT_OPTION) $<
;;     %.S: %.cpp ; $(COMPILE.cpp) -g -S $(OUTPUT_OPTION) $<

;;; Code:

(defgroup disaster nil
  "Disassemble C/C++ under cursor (Works best with Clang)."
  :prefix "disaster-"
  :group 'tools)

(defcustom disaster-make-flags "-k"
  "Command line options to pass to make if a Makefile is found."
  :group 'disaster
  :type 'string)

(defcustom disaster-cc (or (getenv "CC")
                           (executable-find "clang")
                           "cc")
  "The command for your C compiler."
  :group 'disaster
  :type 'string)

(defcustom disaster-cxx (or (getenv "CXX")
                            (executable-find "clang++")
                            "c++")
  "The command for your C++ compiler."
  :group 'disaster
  :type 'string)

(defcustom disaster-cflags (or (getenv "CFLAGS")
                               "-S -g -O3 -march=native")
  "Command line options to use when compiling C.
   Be sure to include `-S -g`!"
  :group 'disaster
  :type 'string)

(defcustom disaster-cxxflags (or (getenv "CXXFLAGS")
                                 "-S -g -O3 -march=native")
  "Command line options to use when compiling C++.
   Be sure to include `-S -g`!"
  :group 'disaster
  :type 'string)

(defcustom disaster-buffer-assembler "*Assembler*"
  "Buffer name to use for assembler output."
  :group 'disaster
  :type 'string)

(defcustom disaster-project-root-files
  (list (list ".git/")         ;; Git directory is almost always project root.
        (list "setup.py"       ;; Python apps.
              "package.json")  ;; node.js apps.
        (list "Makefile"))     ;; Makefiles are sometimes in subdirectories.
  "List of lists of files that may indicate software project root
   directory. Sublist are ordered from highest to lowest
   precedence."
  :group 'disaster
  :type '(repeat (repeat string)))

(defvar save-place)

;;;###autoload
(defun disaster (&optional file line)
  "Shows assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a Makefile in this directory? Run `make bufname.S`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.S`.
- Or is this a C file? Run `clang -g -O3 -S -o bufname.S bufname.c`
- Or is this a C++ file? Run `clang++ -g -O3 -S -o bufname.S bufname.c`
- If build failed, display errors in compile-mode.
- If build succeeded, open bufname.S in new window while maintaining focus.
- If clang -g output, jump and highlight corresponding line of assembly code.

For example, if you're editing lol.cc and have a Makefile in your
current directory (or a parent directory), then this function
will run `make -C project-root -k lol.S` to compile the object
to assembly and display it in a popup window with the current
source line focused. If make fails, a standard compile output log
will be displayed.

If FILE and LINE are not specified, the current editing location
is used."
  (interactive)
  (let* ((file (or file (buffer-name)))
         (line (or line (line-number-at-pos)))
         (file-line (format "%s:%d" file line))
         (assembler (get-buffer-create "*Assembler*")))
    (if (not (string-match "\\.c[cp]?p?$" file))
        (message "Not C/C++ non-header file")
      (let* ((asm-file (concat (file-name-sans-extension file) ".S"))
             (make-root (disaster-find-project-root "Makefile" file))
             (command (if make-root
                          (format "make %s -C %s %s"
                                  disaster-make-flags make-root
                                  (file-relative-name asm-file make-root))
                        (if (string-match "\\.c[cp]p?$" file)
                            (format "%s %s -o %s %s"
                                    disaster-cxx disaster-cxxflags
                                    asm-file file)
                          (format "%s %s -o %s %s"
                                  disaster-cc disaster-cflags
                                  asm-file file)))))
        (if (get-buffer asm-file)
            (kill-buffer (get-buffer asm-file)))
        (if (and (eq 0 (shell-command command assembler))
                 (file-exists-p asm-file))
            (let ((buffer (find-file-noselect asm-file)))
              (kill-buffer assembler)
              (with-current-buffer buffer
                ;; saveplace.el will prevent us from hopping to a line.
                (set (make-local-variable 'save-place) nil)
                (hi-lock-face-phrase-buffer file-line)
                (unless (search-forward file-line nil t)
                  (message "Couldn't find corresponding assembly line.")))
              (display-buffer buffer)
              (message command))
          (with-current-buffer assembler
            (save-excursion
              (goto-char 0)
              (insert (concat command "\n")))
            (disaster--helping-hand)
            (compilation-mode)
            (display-buffer assembler)))))))

(defun disaster--find-parent-dirs (&optional file)
  "Returns a list of parent directories with trailing slashes.

For example:

    (disaster--find-parent-dirs \"/home/jart/disaster-disaster.el\")
    => (\"/home/jart/disaster-\" \"/home/jart/\" \"/home/\" \"/\")

FILE defaults to `buffer-name'."
  (let ((res nil)
        (dir (file-name-directory
              (expand-file-name (or file (buffer-name))))))
    (while dir
      (setq res (cons dir res)
            dir (if (string-match "/[^/]+/$" dir)
                    (substring dir 0 (+ 1 (match-beginning 0))))))
    (reverse res)))

(defun disaster--dir-has-file (dir file)
  "Returns t if DIR contains FILE (or any file if FILE is a list).

For example:

    (disaster--dir-has-file \"/home/jart/\" \".bashrc\")
    (disaster--dir-has-file \"/home/jart/\" (list \".bashrc\" \".screenrc\"))"
  (let ((res nil)
        (dir (file-name-as-directory dir))
        (files (if (listp file)
                   file
                 (list file))))
    (while (and files (not res))
      (setq res (file-exists-p (concat dir (car files)))
            files (cdr files)))
    res))

(defun disaster-find-project-root (&optional looks file)
  "General-purpose Heuristic to detect bottom directory of project.

This works by scanning parent directories of FILE (using
`disaster--find-parent-dirs') for certain types of files like a
`.git/` directory or a `Makefile` (which is less preferred).

The canonical structure of LOOKS is a list of lists of files
to look for in each parent directory where sublists are ordered
from highest precedence to lowest.  However you may specify
LOOKS as a single string or a list of strings for your
convenience. If LOOKS is not specified, it'll default to
`disaster-project-root-files'."
  (let ((res nil)
        (looks (if looks
                   (if (listp looks)
                       (if (listp (car looks))
                           looks
                         (list looks))
                     (list (list looks)))
                 disaster-project-root-files))
        (parents (disaster--find-parent-dirs file)))
    (while (and looks (null res))
      (while (and parents (null res))
        (setq res (disaster--dir-has-file (car parents) (car looks))
              parents (cdr parents)))
      (setq looks (cdr looks)))
    res))

(defun disaster--helping-hand ()
  "If inside compile output buffer, check for common errors and provide help!"
  (when (or (save-excursion
              (goto-char 0)
              (search-forward "Nothing to be done" nil t))
            (save-excursion
              (goto-char 0)
              (search-forward "No rule to make target" nil t)))
    (goto-char (max-char))
    (insert "
Oh no.. it didn't work :(\n
Try putting this in your Makefile:\n
%.S: %.c   ; $(COMPILE.c)   -g -S $(OUTPUT_OPTION) $<
%.S: %.cc  ; $(COMPILE.cc)  -g -S $(OUTPUT_OPTION) $<
%.S: %.cpp ; $(COMPILE.cpp) -g -S $(OUTPUT_OPTION) $<\n
These are are generic rules that tell GNU Make how to compile
GNU Gas assembly files for C/C++.\n
It's also STRONGLY recommended that you use Clang so disaster can
jump to the corresponding line of assembly.
")))

(provide 'disaster)

;;; disaster.el ends here
