(defmodule main
  (export all))

(defun blocksize () 50)

(defun gridcount () 10)

(defun rect ()
  (random:seed (now))
  (io_lib:format "{\"x\":~p,\"y\":~p,\"w\":~p,\"h\":~p}"
    (list (* (blocksize) (random:uniform (- (gridcount) 1)))
      (* (blocksize) (random:uniform (- (gridcount) 1)))
      (blocksize) (blocksize))))

(defun init ()
  (io_lib:format "{\"blocksize\":~p,\"gridcount\":~p}"
    (list (blocksize) (gridcount))))

(defun start ()
  (yaws:start_embedded "." (list (tuple 'listen (tuple 0 0 0 0)) (tuple 'port 8080))
    (list (tuple 'cache_refresh_secs 0))))
