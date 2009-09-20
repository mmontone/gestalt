; interaction-mode

(defclass interaction-mode ()
  ((page :documentation "The page to render")
   (style-sheets :documentation "List of css style sheets")
   (default-view-factory :documentation "The default view factory")
   (app :documentation "The application")))

(defmethod renders-ajax ((interaction-mode interaction-mode))
  nil)

(defmethod basic-send-restart ((interaction-mode interaction-mode))
  "Do nothing"
)

(defun get-interaction-mode-class ()
  (if (get-attribute 'interaction-mode *request*)
      (concatenate 'string (get-attribute 'interaction-mode *request*)
		   "-interaction-mode")))


(defmethod init-page ((interaction-mode interaction-mode) wnd)
  (let
      ((view (view wnd)))
    (setf (tag-name view) "form")
    (always-with view
	  (set-attribute :action (make-action-url))
	  (set-attribute :method "post")
	  (set-attribute :enctype "multipart/form-data")
	  (add-variable :app-class (class-of (parent wnd)))
	  (add-variable :bookmark (act-url (url-manager wnd)))
	  (add-variable :base-dir *base-dir*)
	  (add-variable :window (owner-index wnd)))))

(defun add-variable (view name value)
  (let
      ((xml-var (make-instance 'xml-variable :tag "input")))
    (always-with xml-var
	  (set-attribute "type" "hidden")
	  (set-attribute "id" name)
	  (set-attribute "value" value))
    (setf (controller xml-var) true)
    (append-child view xml-var)))

(defmethod initial-page-render ((interaction-mode interaction-mode) wnd)
  ;; Maybe we should use *app* and we are done instead of
  ;; holding the app in objects slots
  (let
      ((interaction-mode (make-instance 'standard-interaction-mode :app (app interaction-mode)))))
  (render-page interaction-mode wnd))

(defmethod set-title ((wnd window) title)
  (setf (slot-value (view wnd) 'title) (to-ajax title)))