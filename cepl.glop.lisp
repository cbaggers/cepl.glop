(in-package :cepl.glop)

(defvar %win nil)

(defmethod cepl.host:init (&optional (init-flags :everything))
  (declare (ignore init-flags))
  t)

(defmethod cepl.host:request-context
    (width height title fullscreen
     no-frame alpha-size depth-size stencil-size
     red-size green-size blue-size buffer-size
     double-buffer hidden resizable gl-version)
  "Initializes the backend and returns a list containing: (context window)"
  (destructuring-bind (&optional major minor)
      (when gl-version (cepl.context:split-float-version gl-version))
    (let ((win (glop:create-window
		title width height :major major :minor minor
		:fullscreen fullscreen :double-buffer double-buffer
		:red-size red-size :green-size green-size :blue-size blue-size
		:alpha-size alpha-size :depth-size depth-size
		:stencil-size stencil-size)))
      (setf %win win)
      (setf cl-opengl-bindings::*gl-get-proc-address*
	    #'glop:gl-get-proc-address)
      (list (slot-value win 'glop::gl-context)
	    win))))

(defmethod cepl.host:shutdown ()
  (glop:destroy-window %win))


(defmethod set-primary-thread-and-run (func &rest args)
  (when func
    (error "set-primary-thread-and-run not implemented for glop")))

;;----------------------------------------------------------------------
;; event stub

(defun collect-glop-events (win)
  (loop :for event := (glop:next-event win :blocking nil) :while event
     :do (typecase event (glop:close-event (cepl.host:shutdown)))))

;;----------------------------------------------------------------------

(defun glop-swap (win)
  (glop:swap-buffers win))

;;----------------------------------------------------------------------
;; tell cepl what to use

(set-step-func #'collect-glop-events)
(set-swap-func #'glop-swap)
