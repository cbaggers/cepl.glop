(in-package :cepl.glop)

;;======================================================================
;; api v1

(let ((listeners nil))
  (defun glop-register-listener (func)
    (push func listeners))
  (defun glop-step-v1 (win)
    (loop :for event := (glop:next-event win :blocking nil) :while event :do
       (loop :for listener :in listeners :do
          (funcall listener event)))))

;;----------------------------------------------------------------------

(defvar *core-context* t)

(defun make-glop-context (surface version double-buffer
                          alpha-size depth-size stencil-size buffer-size
                          red-size green-size blue-size)
  (declare (ignore alpha-size depth-size double-buffer
                   stencil-size red-size green-size blue-size buffer-size))
  (destructuring-bind (&optional major minor)
      (when version (cepl.context:split-float-version version))
    (let ((context (if version
                       (glop:create-gl-context
                        surface :major major :minor minor
                        :profile (if *core-context* :core :compat)
                        :make-current t)
                       (search-for-context surface))))
      (assert context ()
              "CEPL.GLOP: Could not find a suitable context for CEPL.
Your machine must support at least GL 3.3")
      context)))

(defun search-for-context (surface)
  (let ((profile (if *core-context* :core :compat))
        context)
    (loop :for (major minor profile) :in `((4 5 ,profile) (4 4 ,profile)
                                           (4 3 ,profile) (4 2 ,profile)
                                           (4 1 ,profile) (4 0 ,profile)
                                           (3 3 ,profile))
       :until context
       :do (handler-case
               (setf context (glop:create-gl-context
                              surface :make-current t
                              :major major :minor minor :profile profile))
             (error ())))
    context))

(defun glop-make-current (context surface)
  (glop:detach-gl-context context)
  (glop:attach-gl-context surface context))

;; X Error of failed request:  GLXBadDrawable
;;   Major opcode of failed request:  155 (GLX)
;;   Minor opcode of failed request:  11 (X_GLXSwapBuffers)
;;   Serial number of failed request:  35
;;   Current serial number in output stream:  36

;;----------------------------------------------------------------------
;; Surface

(defun make-glop-surface (width height title fullscreen
                          no-frame alpha-size depth-size stencil-size
                          red-size green-size blue-size buffer-size
                          double-buffer hidden resizable)
  (declare (ignore no-frame resizable buffer-size))
  (let ((win (make-instance 'glop:window)))
    (glop:open-window win title width height
                      :double-buffer double-buffer
                      :red-size red-size
                      :green-size green-size
                      :blue-size blue-size
                      :alpha-size alpha-size
                      :depth-size depth-size
                      :stencil-buffer (when stencil-size t)
                      :stencil-size stencil-size)
    (if hidden
        (glop:hide-window win)
        (glop:show-window win))
    (glop:set-fullscreen win fullscreen)
    win))

(defun destroy-glop-surface (surface)
  (glop:close-window surface))

(defun glop-surface-size (surface)
  (list (glop:window-width surface) (glop:window-height surface)))

(defun glop-set-surface-size (surface width height)
  (glop:set-geometry surface
                     (glop:window-x surface)
                     (glop:window-y surface)
                     width
                     height)
  surface)

(defun glop-surface-fullscreen-p (surface)
  (glop::window-fullscreen surface))

(defun glop-set-surface-fullscreen (surface state)
  (glop:set-fullscreen surface state))

(defun glop-surface-title (surface)
  (values (glop::window-title surface)))

(defun glop-set-surface-title (surface title)
  (glop:set-window-title surface title))

;;----------------------------------------------------------------------

(defun glop-swap (win)
  (glop:swap-buffers win))

;;----------------------------------------------------------------------

(defclass glop-api (cepl.host:api-1)
  (;;
   (supports-multiple-contexts-p :initform nil)
   ;;
   (supports-multiple-surfaces-p :initform t)
   ;;
   (init-function :initform (lambda (&key &allow-other-keys)))
   ;;
   (shutdown-function :initform (lambda ()))
   ;;
   (make-surface-function :initform #'make-glop-surface)
   ;;
   (destroy-surface-function :initform #'destroy-glop-surface)
   ;;
   (make-context-function :initform #'make-glop-context)
   ;;
   (step-function :initform #'glop-step-v1)
   ;;
   (register-event-callback-function :initform #'glop-register-listener)
   ;;
   (swap-function :initform #'glop-swap)
   ;;
   (surface-size-function :initform #'glop-surface-size)
   ;;
   (make-context-current-function :initform #'glop-make-current)
   ;;
   (set-surface-size-function :initform #'glop-set-surface-size)
   ;;
   (surface-fullscreen-p-function :initform #'glop-surface-fullscreen-p)
   ;;
   (set-surface-fullscreen-function :initform #'glop-set-surface-fullscreen)
   ;;
   (surface-title-function :initform #'glop-surface-title)
   ;;
   (set-surface-title-function :initform #'glop-set-surface-title)))

(register-host 'glop-api)
