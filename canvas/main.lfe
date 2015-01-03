(defmodule main
  (export all))

(defun blocksize () 50)

(defun gridcount () 10)

(defun rand (n)
  (- (random:uniform n) 1))

(defun randomColor ()
  (lists:flatten (io_lib:format "rgb(~p,~p,~p)"
    (list (rand 256) (rand 256) (rand 256)))))

(defun rects ()
  (random:seed (now))
  (json2:encode (list (tuple 'struct (list 
    (tuple "x" (* (blocksize) (rand (gridcount))))
    (tuple "y" (* (blocksize) (rand (gridcount)))) 
    (tuple "w" (blocksize))
    (tuple "h" (blocksize))
    (tuple "color" (randomColor)))))))

(defun init ()
  (json2:encode (tuple 'struct (list (tuple "blocksize" (blocksize))
    (tuple "gridcount" (gridcount))))))

(defun start ()
  (yaws:start_embedded "." (list (tuple 'listen (tuple 0 0 0 0))
    (tuple 'port 8080)) (list (tuple 'cache_refresh_secs 0))))
