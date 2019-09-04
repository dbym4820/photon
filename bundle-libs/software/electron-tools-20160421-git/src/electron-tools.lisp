(in-package :cl-user)
(defpackage electron-tools
  (:use :cl)
  (:export :+download-url+
           :download-url
           :download
           :extract
           :binary-pathname
           :get-release
           :app-directory)
  (:documentation "Tools for Electron."))
(in-package :electron-tools)

;;; Constants

(defparameter +download-url+
  "https://github.com/atom/electron/releases/download/v~A/electron-v~A-~A-~A.zip"
  "A format string for the download URL. Values are: version, version, operating
system, architecture.")

;;; OS-specific utilities

(defun download-url (&key operating-system version architecture)
  "A download link to the specified release."
  (let ((os (case operating-system
              (:linux "linux")
              (:mac "darwin")
              (:windows "win32")))
        (arch (case architecture
                (:64 "x64")
                (:32 "ia32"))))
  (format nil +download-url+ version version os arch)))

(defun download (pathname &key operating-system version architecture)
  "Download a release to a pathname."
  (trivial-download:download (download-url :operating-system operating-system
                                           :version version
                                           :architecture architecture)
                             pathname))

(defun extract (pathname)
  "Extract an Electron snapshot into its containing directory."
  (trivial-extract:extract-zip pathname)
  ;; When on Unix, set the executable bit on the file
  #-(or win32 mswindows)
  (let* ((parent (uiop:pathname-directory-pathname pathname))
         (binary (or (probe-file (binary-pathname parent
                                                  :operating-system :linux))
                     (probe-file (binary-pathname parent
                                                  :operating-system :mac)))))
    (when binary
      (trivial-exe:ensure-executable binary))))

(defun binary-pathname (directory &key operating-system)
  "The pathname to the Electron binary inside the directory it was extracted to."
  (merge-pathnames (case operating-system
                     (:linux #p"electron")
                     (:mac #p"Electron.app/Contents/MacOS/Electron")
                     (:windows #p"electron.exe")
                     (t (error "Unsupported operating system.")))
                   directory))

(defun get-release (directory &key operating-system version architecture)
  "Download an Electron release to the directory."
  (let ((archive (merge-pathnames #p"electron.zip" directory)))
    ;; Download the release to the directory
    (download archive :operating-system operating-system
                      :version version
                      :architecture architecture)
    ;; Extract it
    (extract archive)
    ;; Delete it
    (delete-file archive)
    t))

(defun app-directory (directory &key operating-system)
  "The pathname to the application directory of an Electron release."
  (merge-pathnames (if (eq operating-system :mac)
                       #p"Electron.app/Contents/Resources/default_app/"
                       #p"resources/default_app/")
                   directory))
