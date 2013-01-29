(defpackage :cl-maze 
  (:use :common-lisp)
  (:shadow :room))

(in-package :cl-maze)

(ql:quickload :lispbuilder-sdl)
(ql:quickload :lispbuilder-sdl-gfx)
(ql:quickload :queues.priority-queue)

(defstruct room
  idx
  cluster-id
  adjacency)

;;迷路のデータ構造
(defclass <maze> ()
  ((width :initarg :w :accessor w)
   (height :initarg :h :accessor h)
   (rooms :initarg :rooms :accessor rooms)))

;;部屋の情報を管理するリストをつくる
(defun make-room-list (w h)
  (let ((array (make-array (* w h) :initial-element 0)))
    (loop for idx from 0 upto (1- (* w h))
          do
          (setf (aref array idx)
                (make-room :idx idx :cluster-id idx :adjacency '())))
    array))

;;単純なグリッドの部屋をつくる
(defun make-base-maze (w h)
  (make-instance '<maze>
                 :w w :h h
                 :rooms (make-room-list w h)))

(defmethod get-room ((maze <maze>) idx)
  (aref (rooms maze) idx))

;;全ての部屋がクラスタ0に属していたら終了
#|
| 初期状態ではクラスタ番号 i は i >= 0 を満している
| 部屋と部屋を接続する時には、かならず小さい方のクラスタ番号に属させるようにしている、よって
| 0 n = 0
| n m = if n > m then n else n
| とクラスタ番号が決められていくので、部屋を繋げていくたびに、クラスタ番号は単調減少していく。
| よって最終状態では、クラスタ番号は確実に全て0になっている。
 |#
(defmethod build-finished-p ((maze <maze>))
  (every (lambda (room) (zerop (room-cluster-id room)))
         (rooms maze)))

;;x座標とy座標を計算
(defmethod calc-x-y (idx w)
  (values
    (mod idx w))
    (truncate idx w))

;;隣接した部屋かどうか
(defmethod neighbor-room-p ((maze <maze>) from to)
  (multiple-value-bind (from-x from-y) (calc-x-y from (w maze))
    (multiple-value-bind (to-x to-y) (calc-x-y to (w maze))
      (= (+ (abs (- from-x to-x)) 
            (abs (- from-y to-y))) 1))))

;;隣接した部屋ならば、繋げる
(defmethod connect-room ((maze <maze>) i j)
  (let* ((room-i (get-room maze i))
         (room-j (get-room maze j))
         (id-i (room-cluster-id room-i))
         (id-j (room-cluster-id room-j)))
    (when (and (not (equal id-i id-j)) 
               (neighbor-room-p maze i j))
      ;それぞれの部屋の隣接リストに相手の部屋を追加する
      (pushnew j (room-adjacency room-i))
      (pushnew i (room-adjacency room-j))
      ;小さい方のクラスタ番号を採用
      (if (< id-i id-j)
        (loop for room across (rooms maze)
              when (= (room-cluster-id room) id-j)
              do
              (setf (room-cluster-id room) id-i))
        (loop for room across (rooms maze)
              when (= (room-cluster-id room) id-i)
              do
              (setf (room-cluster-id room) id-j))))))

;;初期迷路を生成し、全ての部屋が同じクラスタに属するまで
;;部屋をつなげていく
(defun build-maze (w h)
  (let ((maze (make-base-maze w h)))
    (loop until (build-finished-p maze)
          do
          (connect-room maze
                        (random (* w h))
                        (random (* w h))))
    maze))

(defun t-or-nil (val)
  (not (not val)))

;;部屋の接続リストを各部屋の壁のリストにする
(defun room-list-to-wall-list (room-list w h)
  (declare (ignore h))
  (loop for r across room-list
        for room-id = (room-idx r)
        for adja = (room-adjacency r)
        collect `(,room-id 
                   ,(list 
                      (t-or-nil (member (- room-id w) adja))
                      (t-or-nil (member (1- room-id) adja))
                      (t-or-nil (member (1+ room-id) adja))
                      (t-or-nil (member (+ room-id w) adja))))))

(defmethod solve ((maze <maze>))
  (let ((U (queues:make-queue :priority-queue 
                              :compare (lambda (r1 r2) (< (cdr r1) (cdr r2)))))
        (V (make-hash-table)))
    ;;Add First node
    (queues:qpush U (cons (aref (rooms maze) 0) 0))
    ;;Add Rest nodes
    (loop for idx from 1 below (* (w maze) (h maze))
          do
          (queues:qpush U (cons (aref (rooms maze) idx) most-positive-fixnum)))
    (loop while (not (zerop (queues:qsize U)))
          do
          (let ((p (queues:qtop U)))
            ;;Remove p from U add to V
            (queues:qpop U)
            ;; roomIdx -> distance
            (setf (gethash (room-idx (car p)) V) (cdr p))
            (loop for adj in (room-adjacency (car p))
                  when #1=(queues:queue-find U (lambda (n) (equalp (car n) (aref (rooms maze) adj))))
                  do
                  (setf (gethash (room-idx (car #2=(queues::node-value #1#))) V)
                        #3=(min (cdr #2#) (1+ (cdr p))))
                  (queues:queue-change U #1# (cons (car #2#) #3#)) )))
    V))

(defmethod display-maze ((maze <maze>) cell-size)
  (let ((wall-list (room-list-to-wall-list (rooms maze)
                                           (w maze)
                                           (h maze)))
        (player-idx 0))
    (sdl:with-init ()
        (sdl:window (* cell-size (+ 2 (w maze))) (* cell-size (+ 2 (h maze))) :title-caption "Maze")
        (sdl:clear-display sdl:*white*)
        (sdl:with-events ()
          (:quit-event () t)
          (:key-down-event (:key key)
           (let ((walls (cadr (nth player-idx wall-list))))
             (cond 
               ((sdl:key= key :sdl-key-up)
                (when (nth 0 walls)
                  (decf player-idx (w maze))))
               ((sdl:key= key :sdl-key-left)
                (when (nth 1 walls)
                  (decf player-idx)))
               ((sdl:key= key :sdl-key-right)
                (when (nth 2 walls)
                  (incf player-idx)))
               ((sdl:key= key :sdl-key-down)
                (when (nth 3 walls)
                  (incf player-idx (w maze)))))))
          (:idle () 
           (sdl:clear-display sdl:*white*)
           ;;Display Map once
           (loop for w in wall-list
                 for id = (car w)
                 for w-list = (cadr w)
                 do
                 (multiple-value-bind (y x) (calc-x-y id (w maze))
                   (let ((x (+ cell-size (* x cell-size)))
                         (y (+ cell-size (* y cell-size))))
                     (let ((f-dif-x `(0 0 ,cell-size 0))
                           (f-dif-y `(0 0 0 ,cell-size))
                           (t-dif-x `(,cell-size 0 ,cell-size ,cell-size))
                           (t-dif-y `(0 ,cell-size ,cell-size ,cell-size)))
                       (loop for wall in w-list
                             for widx from 0
                             for from-x = (+ x (nth widx f-dif-x))
                             for from-y = (+ y (nth widx f-dif-y))
                             for to-x = (+ x (nth widx t-dif-x))
                             for to-y = (+ y (nth widx t-dif-y))
                             when (null wall)
                             do
                             (sdl:draw-line-* from-x from-y to-x to-y :color sdl:*black*))))))
           ;;Display Player
           (multiple-value-bind (y x) (calc-x-y player-idx (w maze))
             (sdl:draw-filled-circle-* 
               (round (+ cell-size (/ cell-size 2) (* x cell-size)))
               (round (+ cell-size (/ cell-size 2) (* y cell-size)))
               (round (/ cell-size 2.5)) :color sdl:*red*))
           (sdl:update-display))))))
