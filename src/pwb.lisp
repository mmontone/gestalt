; page-renderer

(defclass page-renderer ()
  ((page :documentation "The page to render")
   (style-sheets :documentation "List of css style sheets")
   (default-view-factory :documentation "The default view factory")
   (app :documentation "The application")))

(defmethod renders-ajax ((page-renderer page-renderer))
  nil)

(defmethod basic-send-restart ((page-renderer page-renderer))
  "Do nothing"
)

(defun get-renderer-class ()
  (if (get-attribute 'renderer *request*)
      (concatenate 'string (get-attribute 'renderer *request*)
		   "-page-renderer")))


(defmethod init-page ((page-renderer page-renderer) wnd)
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

(defmethod initial-page-render ((page-renderer page-renderer) wnd)
  ;; Maybe we should use *app* and we are done instead of
  ;; holding the app in objects slots
  (let
      ((renderer (make-instance 'standard-page-renderer :app (app page-renderer)))))
  (render-page renderer wnd))

(defmethod set-title ((wnd window) title)
  (setf (slot-value (view wnd) 'title) (to-ajax title)))