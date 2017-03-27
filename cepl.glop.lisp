(in-package :cepl.glop)

;;======================================================================
;; api v1

(let ((listeners nil))
  (defun glop-register-listener (func)
    (push func listeners))
  (defun glop-step-v1 (win)
    (loop :for event := (glop:next-event win :blocking nil) :while event :do
       (progn
         (loop :for listener :in listeners :do
            (funcall listener event))
         (when (typep event 'glop:close-event)
           (cepl:quit))))))

;;----------------------------------------------------------------------

(defun make-glop-context (surface version width height title fullscreen
                          no-frame alpha-size depth-size stencil-size
                          red-size green-size blue-size buffer-size
                          double-buffer hidden resizable)
  (declare (ignore width height title fullscreen no-frame alpha-size depth-size
                   stencil-size red-size green-size blue-size buffer-size
                   double-buffer hidden resizable))
  (destructuring-bind (&optional major minor)
      (when version (cepl.context:split-float-version version))
    (glop:create-gl-context surface :major major :minor minor
                            :make-current t)))

(defun glop-make-current (context surface)
  (glop:attach-gl-context surface context))

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

(defun glop-surface-size (win-handle)
  (list (glop:window-width win-handle) (glop:window-height win-handle)))

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
   (make-context-current-function :initform #'glop-make-current)))

(register-host 'glop-api)
