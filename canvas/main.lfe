(defmodule main
  (export all))

(defun blocksize () 50)

(defun gridcount () 10)

(defun randomColor ()
  (lists:flatten (io_lib:format "rgb(~p,~p,~p)" (list (- (random:uniform 256) 1)
    (- (random:uniform 256) 1) (- (random:uniform 256) 1)))))

(defun rect ()
  (random:seed (now))
  (json2:encode (tuple 'struct (list 
    (tuple "x" (* (blocksize) (random:uniform (- (gridcount) 1))))
    (tuple "y" (* (blocksize) (random:uniform (- (gridcount) 1))))
    (tuple "w" (blocksize))
    (tuple "h" (blocksize))))))

(defun rects ()
  (random:seed (now))
  (json2:encode (list (tuple 'struct (list 
    (tuple "x" (* (blocksize) (- (random:uniform (gridcount)) 1)))
    (tuple "y" (* (blocksize) (- (random:uniform (gridcount)) 1)))
    (tuple "w" (blocksize))
    (tuple "h" (blocksize))
    (tuple "color" (randomColor)))))))

(defun init ()
  (io_lib:format "{\"blocksize\":~p,\"gridcount\":~p}"
    (list (blocksize) (gridcount))))

(defun start ()
  (yaws:start_embedded "." (list (tuple 'listen (tuple 0 0 0 0)) (tuple 'port 8080))
    (list (tuple 'cache_refresh_secs 0))))
