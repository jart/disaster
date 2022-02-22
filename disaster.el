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

;; ![Screenshot](http://i.imgur.com/kMoN1m6.png)
;;
;; Disaster lets you press `C-c d` to see the compiled assembly code for the
;; C/C++ file you're currently editing. It even jumps to and highlights the
;; line of assembly corresponding to the line beneath your cursor.
;;
;; It works by creating a `.o` file using make (if you have a Makefile) or the
;; default system compiler. It then runs that file through objdump to generate
;; the human-readable assembly.

;;; Installation:

;; Make sure to place `disaster.el` somewhere in the load-path and add the
;; following lines to your `.emacs` file to enable the `C-c d` shortcut to
;; invoke `disaster':
;;
;;     (add-to-list 'load-path "/PATH/TO/DISASTER")
;;     (require 'disaster)
;;     (define-key c-mode-base-map (kbd "C-c d") 'disaster)
;;

;;; Code:

(defgroup disaster nil
  "Disassemble C/C++ under cursor (Works best with Clang)."
  :prefix "disaster-"
  :group 'tools)

(defcustom disaster-make-flags "-k"
  "Command line options to pass to make if a Makefile is found."
  :group 'disaster
  :type 'string)

(defcustom disaster-cc (or (getenv "CC") "cc")
  "The command for your C compiler."
  :group 'disaster
  :type 'string)

(defcustom disaster-cxx (or (getenv "CXX") "c++")
  "The command for your C++ compiler."
  :group 'disaster
  :type 'string)

(defcustom disaster-cflags (or (getenv "CFLAGS")
                               "-march=native")
  "Command line options to use when compiling C."
  :group 'disaster
  :type 'string)

(defcustom disaster-cxxflags (or (getenv "CXXFLAGS")
                                 "-march=native")
  "Command line options to use when compiling C++.!"
  :group 'disaster
  :type 'string)

(defcustom disaster-objdump "objdump -d -M att -Sl --no-show-raw-insn"
  "The command name and flags for running objdump."
  :group 'disaster
  :type 'string)

(defcustom disaster-buffer-compiler "*compilation*"
  "Buffer name to use for assembler output."
  :group 'disaster
  :type 'string)

(defcustom disaster-buffer-assembly "*assembly*"
  "Buffer name to use for objdump assembly output."
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

(defcustom disaster-c++-extensions
  (list "cc" "cpp" "cxx")
  "List of common C++ source file extensions."
  :group 'disaster
  :type '(repeat string))

(defvar save-place)

;;;###autoload
(defvar disaster-find-build-root-functions nil
  "Functions to call to get the build root directory from the project directory.
If nil is returned, the next function will be tried.  If all
functions return nil, the project root directory will be used as
the build directory.")

(defun create-compile-command-make (make-root cwd rel-obj obj-file proj-root rel-file file)
  (if make-root
      ;; if-then
      (if (equal cwd make-root)
          (format "make %s %s" disaster-make-flags (shell-quote-argument rel-obj))
        (format "make %s -C %s %s"
                disaster-make-flags make-root
                rel-obj))
    ;; if-else
    (if (member (file-name-extension file) disaster-c++-extensions)
        (format "%s %s -g -c -o %s %s"
                disaster-cxx disaster-cxxflags
                (shell-quote-argument obj-file) (shell-quote-argument file))
      (format "%s %s -g -c -o %s %s"
              disaster-cc disaster-cflags
              (shell-quote-argument obj-file) (shell-quote-argument file)))))

(defun create-compile-command-cmake (make-root cwd rel-obj obj-file proj-root rel-file)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file (concat make-root "/compile_commands.json"))))
    (dolist (obj json)
      (when (string-equal (gethash "file" obj) (concat proj-root rel-file))
        (return (gethash "command" obj))))))

(defun get-object-file-path-cmake (compile-command)
  (let* ((parts (split-string compile-command " "))
         (break-on-next nil))
    (dolist (part parts)
      (if (string-equal "-o" part)
          (setq break-on-next t)
        (when break-on-next
          (return part))))))

(defun create-compile-command (use-cmake make-root cwd rel-obj obj-file proj-root rel-file file)
  (if use-cmake
      (create-compile-command-cmake make-root cwd rel-obj obj-file proj-root rel-file)
    (create-compile-command-make make-root cwd rel-obj obj-file proj-root rel-file file)))

;;;###autoload
(defun disaster (&optional file line)
  "Shows assembly code for current line of C/C++ file.

Here's the logic path it follows:

- Is there a Makefile in this directory? Run `make bufname.o`.
- Or is there a Makefile in a parent directory? Run `make -C .. bufname.o`.
- Or is this a C file? Run `cc -g -O3 -c -o bufname.o bufname.c`
- Or is this a C++ file? Run `c++ -g -O3 -c -o bufname.o bufname.c`
- If build failed, display errors in compile-mode.
- Run objdump inside a new window while maintaining focus.
- Jump to line matching current line.

If FILE and LINE are not specified, the current editing location
is used."
  (interactive)
  (save-buffer)
  (let* ((file      (or file (file-name-nondirectory (buffer-file-name))))
         (line      (or line (line-number-at-pos)))
         (file-line (format "%s:%d" file line))
         (makebuf   (get-buffer-create disaster-buffer-compiler))
         (asmbuf    (get-buffer-create disaster-buffer-assembly)))
    (if (member (file-name-extension file) disaster-c++-extensions)
        (let* ((cwd       (file-name-directory (expand-file-name (buffer-file-name)))) ;; path to current source file
               (proj-root (disaster-find-project-root nil file)) ;; path to project root
               (use-cmake (file-exists-p (concat proj-root "/compile_commands.json")))
               (make-root (disaster-find-build-root use-cmake proj-root)) ;; path to build root
               (rel-file  (if proj-root     ;; path to source file (relative to project root)
                              (file-relative-name file proj-root)
                            file))
               (rel-obj   (concat (file-name-sans-extension rel-file) ".o")) ;; path to object file (relative to project root)
               (obj-file  (concat make-root rel-obj)) ;; full path to object file (build root!)
               (cc        (create-compile-command use-cmake make-root cwd rel-obj obj-file proj-root rel-file file))
               (dump      (format "%s %s" disaster-objdump
                                  (shell-quote-argument (concat make-root rel-obj))))
               (line-text (buffer-substring-no-properties
                           (point-at-bol)
                           (point-at-eol))))

          (when use-cmake
            (setq tmp      (get-object-file-path-cmake cc)
                  obj-file (concat make-root "/" tmp)
                  cc       (concat "cmake --build " make-root " --target " tmp)
                  dump     (format "%s %s" disaster-objdump
                                   (shell-quote-argument obj-file))))

          (if (and (eq 0 (progn
                           (message (format "Running: %s" cc))
                           (shell-command cc makebuf)))
                   (file-exists-p obj-file))
              (when (eq 0 (progn
                            (message (format "Running: %s" dump))
                            (shell-command dump asmbuf)))
                (kill-buffer makebuf)
                (with-current-buffer asmbuf
                  ;; saveplace.el will prevent us from hopping to a line.
                  (set (make-local-variable 'save-place) nil)
                  (asm-mode)
                  (disaster--shadow-non-assembly-code))
                (let ((oldbuf (current-buffer)))
                  (switch-to-buffer-other-window asmbuf)
                  (goto-char 0)
                  (if (or (search-forward line-text nil t)
                          (search-forward file-line nil t))
                      (progn
                        (recenter)
                        (overlay-put (make-overlay (point-at-bol)
                                                   (1+ (point-at-eol)))
                                     'face 'region))
                    (message "Couldn't find corresponding assembly line."))
                  (switch-to-buffer-other-window oldbuf)))
            (with-current-buffer makebuf
              (save-excursion
                (goto-char 0)
                (insert (concat cc "\n")))
              (compilation-mode)
              (display-buffer makebuf))))
      (message "Not C/C++ non-header file"))))

(defun disaster--shadow-non-assembly-code ()
  "Scans current buffer, which should be in asm-mode, and uses
the standard `shadow' face for lines that don't appear to contain
assembly code."
  (remove-overlays)
  (save-excursion
    (goto-char 0)
    (while (not (eobp))
      (beginning-of-line)
      (if (not (looking-at "[ \t]+[a-f0-9]+:[ \t]+"))
          (let ((eol (save-excursion (end-of-line) (point))))
            (overlay-put (make-overlay (point) eol)
                         'face 'shadow)))
      (forward-line))))

(defun disaster--find-parent-dirs (&optional file)
  "Returns a list of parent directories with trailing slashes.

For example:

    (disaster--find-parent-dirs \"/home/jart/disaster-disaster.el\")
    => (\"/home/jart/disaster-\" \"/home/jart/\" \"/home/\" \"/\")

FILE defaults to `buffer-file-name'."
  (let ((res nil)
        (dir (file-name-directory
              (expand-file-name (or file (buffer-file-name))))))
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
        (parent-dirs (disaster--find-parent-dirs file)))
    (while (and looks (null res))
      (let ((parents parent-dirs))
    (while (and parents (null res))
      (setq res (if (disaster--dir-has-file
             (car parents) (car looks))
            (car parents))
        parents (cdr parents))))
      (setq looks (cdr looks)))
    res))

(defun disaster-find-build-root (use-cmake project-root)
  (if use-cmake
      (progn
        (let* ((json-object-type 'hash-table)
               (json-array-type 'list)
               (json-key-type 'string)
               (json (json-read-file (concat project-root "/compile_commands.json"))))
          (gethash "directory" (first json))))

    (and project-root
         (or (let (build-root
                   (funcs disaster-find-build-root-functions))
               (while (and (null build-root) funcs)
                 (setq build-root (funcall (car funcs) project-root)
                       funcs (cdr funcs)))
               (and build-root
                    (file-name-as-directory build-root)))
             project-root))))

(provide 'disaster)

;;; disaster.el ends here
