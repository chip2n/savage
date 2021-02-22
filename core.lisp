(in-package #:savage)

(defvar *svg* nil)
(defparameter *width* nil)
(defparameter *height* nil)

(defun emit-svg (tree)
  (when tree
    (if (stringp tree)
        (format *svg* "~a" tree)
        (let ((tag (string-downcase (car tree)))
              (args (mapcar (lambda (x)
                              (cons (string-downcase (car x)) (cdr x)))
                            (c:group (cadr tree) 2)))
              (body (cddr tree)))
          (format *svg* "<~a ~:{~a='~a' ~}>" tag args)
          (loop for form in body do
            (emit-svg form))
          (format *svg* "</~a>" tag)))))

(defun rect->svg (x y width height fill)
  `(:rect (:x ,x :y ,y :width ,width :height ,height :fill ,fill)))

(defun circle->svg (x y r fill stroke stroke-width &optional dashedp)
  `(:circle (:cx ,x
             :cy ,y
             :r ,r
             :fill ,fill
             :stroke ,stroke
             :stroke-width ,stroke-width
             ,@(when dashedp
                 '(:stroke-dasharray "4,4")))))

(defun line->svg (x1 y1 x2 y2 stroke stroke-width)
  `(:line (:x1 ,x1
           :y1 ,y1
           :x2 ,x2
           :y2 ,y2
           :stroke ,stroke
           :stroke-width ,stroke-width)))

(defun text->svg (s x y fill size)
  `(:text (:x ,x
           :y ,y
           :font-family "Noto Sans"
           :font-size ,size
           :font-weight "bold"
           :text-anchor "middle"
           :fill ,fill
           :dominant-baseline "central")
          ,s))

(defun group->svg (attrs tree)
  (destructuring-bind (&key x y rotation) attrs
    `(:g (:transform ,(format nil "translate(~a ~a) rotate(~a)" x y rotation))
         ,@(mapcar #'node->svg tree))))

(defun node->svg (node)
  (if (listp (car node))
      (group->svg '(:x 0 :y 0 :rotation 0) node)
      (ecase (car node)
        (:rect (apply #'rect->svg (cdr node)))
        (:circle (apply #'circle->svg (cdr node)))
        (:line (apply #'line->svg (cdr node)))
        (:text (apply #'text->svg (cdr node)))
        (:group (group->svg (cadr node) (caddr node))))))

(defun ast->svg (tree)
  (let ((data (mapcar #'node->svg tree)))
    `(:svg (:xmlns "http://www.w3.org/2000/svg"
            :|xmlns:xlink| "http://www.w3.org/1999/xlink"
            :viewBox ,(format nil "0 0 ~a ~a" *width* *height*)
            :version "1.1")
       ,@data)))

(defun compile-svg (width height ast)
  (let ((*width* width)
        (*height* height))
    (->> ast (ast->svg) (emit-svg))))

(defmacro svg (&body body)
  (c:with-interned-symbols (rect circle line text group)
    (alexandria:with-gensyms (cmds)
      `(let ((,cmds nil))
         (macrolet ((rect (&key (x 0) (y 0) width height fill)
                      `(list :rect ,x ,y ,width ,height ,fill))
                    (circle (&key (x 0) (y 0) radius fill stroke (stroke-width 0) dashedp)
                      `(list :circle ,x ,y ,radius ,fill ,stroke ,stroke-width ,dashedp))
                    (line (&key (x1 0) (y1 0) (x2 0) (y2 0) stroke (stroke-width 1))
                      `(list :line ,x1 ,y1 ,x2 ,y2 ,stroke ,stroke-width))
                    (text (s &key (x 0) (y 0) fill (size 18))
                      `(list :text ,s ,x ,y ,fill ,size))
                    (group ((&key (x 0) (y 0) (rotation 0)) &body body)
                      `(list :group (list :x ,x :y ,y :rotation ,rotation) (list ,@body))))
           ,@(mapcar (lambda (form) `(push ,form ,cmds)) body)
           (nreverse ,cmds))))))

(defmacro inset ((dx dy &optional (anchor :center)) &body body)
  (alexandria:with-gensyms (gdx gdy)
    (let ((group-args
            (ecase anchor
              (:center `(:x (/ ,gdx 2) :y (/ ,gdy 2)))
              (:bottom `(:x (/ ,gdx 2) :y ,gdy)))))
      `(let* ((,gdx ,dx)
              (,gdy ,dy)
              (*width* (- *width* ,gdx))
              (*height* (- *height* ,gdy)))
         (group ,group-args ,@body)))))
