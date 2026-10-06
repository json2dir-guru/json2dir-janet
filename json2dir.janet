#!/usr/bin/env janet
# json2dir: create the directory tree a JSON document describes, in the current directory.
# Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
# Janet core only: hand-written UTF-8 check and JSON parser.
# Representation: object -> table, string -> string, array -> array.

(defn fail [msg] (error (string "json2dir: " msg)))

# --- UTF-8 validation (§3) ---

(defn check-utf8 [b]
  (def n (length b))
  (var i 0)
  (defn cont [k]
    (when (or (>= k n) (not= (band (b k) 0xC0) 0x80)) (fail "input is not valid UTF-8")))
  (while (< i n)
    (def c (b i))
    (cond
      (< c 0x80) (++ i)
      (and (>= c 0xC2) (<= c 0xDF)) (do (cont (+ i 1)) (+= i 2))
      (and (>= c 0xE0) (<= c 0xEF))
      (do (cont (+ i 1)) (cont (+ i 2))
        (def c1 (b (+ i 1)))
        (when (or (and (= c 0xE0) (< c1 0xA0)) (and (= c 0xED) (>= c1 0xA0)))
          (fail "input is not valid UTF-8"))
        (+= i 3))
      (and (>= c 0xF0) (<= c 0xF4))
      (do (cont (+ i 1)) (cont (+ i 2)) (cont (+ i 3))
        (def c1 (b (+ i 1)))
        (when (or (and (= c 0xF0) (< c1 0x90)) (and (= c 0xF4) (>= c1 0x90)))
          (fail "input is not valid UTF-8"))
        (+= i 4))
      (fail "input is not valid UTF-8"))))

# --- JSON parser (§3) ---

(defn parse-json [b]
  (def n (length b))
  (var i 0)
  (defn bad [] (fail (string "input is not valid JSON at byte " i)))
  (defn ws []
    (while (and (< i n) (index-of (b i) [32 9 10 13])) (++ i)))
  (defn expect [c] (if (and (< i n) (= (b i) c)) (++ i) (bad)))
  (defn put-utf8 [buf cp]
    (cond
      (< cp 0x80) (buffer/push-byte buf cp)
      (< cp 0x800) (buffer/push-byte buf (bor 0xC0 (brshift cp 6)) (bor 0x80 (band cp 0x3F)))
      (< cp 0x10000) (buffer/push-byte buf (bor 0xE0 (brshift cp 12))
                                       (bor 0x80 (band (brshift cp 6) 0x3F)) (bor 0x80 (band cp 0x3F)))
      (buffer/push-byte buf (bor 0xF0 (brshift cp 18)) (bor 0x80 (band (brshift cp 12) 0x3F))
                        (bor 0x80 (band (brshift cp 6) 0x3F)) (bor 0x80 (band cp 0x3F)))))
  (defn hex4 []
    (when (> (+ i 4) n) (bad))
    (var v 0)
    (for k 0 4
      (def c (b (+ i k)))
      (def d (cond (and (>= c 48) (<= c 57)) (- c 48)
                   (and (>= c 97) (<= c 102)) (- c 87)
                   (and (>= c 65) (<= c 70)) (- c 55)
                   (bad)))
      (set v (+ (* v 16) d)))
    (+= i 4)
    v)
  (defn pstring []
    (expect 34)
    (def buf @"")
    (var done false)
    (while (not done)
      (when (>= i n) (bad))
      (def c (b i))
      (cond
        (= c 34) (do (++ i) (set done true))
        (< c 0x20) (bad)
        (= c 92)
        (do
          (++ i)
          (when (>= i n) (bad))
          (def e (b i))
          (++ i)
          (case e
            34 (buffer/push-byte buf 34)
            92 (buffer/push-byte buf 92)
            47 (buffer/push-byte buf 47)
            98 (buffer/push-byte buf 8)
            102 (buffer/push-byte buf 12)
            110 (buffer/push-byte buf 10)
            114 (buffer/push-byte buf 13)
            116 (buffer/push-byte buf 9)
            117 (do
                  (def u (hex4))
                  (cond
                    (and (>= u 0xD800) (<= u 0xDBFF))
                    (do
                      (unless (and (< (+ i 1) n) (= (b i) 92) (= (b (+ i 1)) 117))
                        (fail "string contains an unpaired surrogate"))
                      (+= i 2)
                      (def lo (hex4))
                      (unless (and (>= lo 0xDC00) (<= lo 0xDFFF))
                        (fail "string contains an unpaired surrogate"))
                      (put-utf8 buf (+ 0x10000 (blshift (- u 0xD800) 10) (- lo 0xDC00))))
                    (and (>= u 0xDC00) (<= u 0xDFFF))
                    (fail "string contains an unpaired surrogate")
                    (put-utf8 buf u)))
            (bad)))
        (do (buffer/push-byte buf c) (++ i))))
    (string buf))
  (var pvalue nil)
  (defn pobject []
    (expect 123)
    (def t @{})
    (ws)
    (if (and (< i n) (= (b i) 125))
      (++ i)
      (do
        (var more true)
        (while more
          (ws)
          (def k (pstring))
          (ws) (expect 58)
          (put t k (pvalue)) # duplicate names: the last one wins (§4.2)
          (ws)
          (cond
            (and (< i n) (= (b i) 44)) (++ i)
            (and (< i n) (= (b i) 125)) (do (++ i) (set more false))
            (bad)))))
    t)
  (defn parray []
    (expect 91)
    (def a @[])
    (ws)
    (if (and (< i n) (= (b i) 93))
      (++ i)
      (do
        (var more true)
        (while more
          (array/push a (pvalue))
          (ws)
          (cond
            (and (< i n) (= (b i) 44)) (++ i)
            (and (< i n) (= (b i) 93)) (do (++ i) (set more false))
            (bad)))))
    a)
  (defn literal [word]
    (if (= (string (buffer/slice b i (min n (+ i (length word))))) word)
      (do (+= i (length word)) (keyword word))
      (bad)))
  (defn pnumber []
    (def m (peg/match ~(* (? "-") (+ "0" (* (range "19") (any (range "09"))))
                          (? (* "." (some (range "09"))))
                          (? (* (set "eE") (? (set "+-")) (some (range "09"))))
                          ($)) b i))
    (unless m (bad))
    (set i (last m))
    :number)
  (set pvalue
       (fn []
         (ws)
         (when (>= i n) (bad))
         (def c (b i))
         (cond
           (= c 123) (pobject)
           (= c 91) (parray)
           (= c 34) (pstring)
           (= c 116) (literal "true")
           (= c 102) (literal "false")
           (= c 110) (literal "null")
           (or (= c 45) (and (>= c 48) (<= c 57))) (pnumber)
           (bad))))
  (def v (pvalue))
  (ws)
  (when (< i n) (bad))
  v)

# --- validation (§4, §6) ---

(defn check-name [name where]
  (when (or (= name "") (= name ".") (= name "..") (string/find "/" name) (string/find "\0" name))
    (fail (string where ": invalid name " (describe name)))))

(defn validate [v where]
  (cond
    (string? v) nil
    (array? v)
    (do
      (unless (and (= (length v) 2) (string? (v 0)) (string? (v 1)))
        (fail (string where ": an array must be [\"link\", target] or [\"script\", content]")))
      (unless (or (= (v 0) "link") (= (v 0) "script"))
        (fail (string where ": unknown array kind " (describe (v 0)))))
      (when (and (= (v 0) "link") (string/find "\0" (v 1)))
        (fail (string where ": a link target cannot contain NUL"))))
    (table? v)
    (eachp [k x] v
      (def child (if (= where ".") k (string where "/" k)))
      (check-name k child)
      (validate x child))
    (fail (string where ": " v " values are not allowed"))))

# --- applying (§5) ---

(defn clear [p st]
  (when st
    (when (= (st :mode) :directory) (fail (string p ": a directory is in the way")))
    (os/rm p)))

(defn write-file [p content exec]
  # :c :e = O_CREAT|O_EXCL: never write through an entry that appeared after clear
  (def f (os/open p :wce 8r666))
  (defer (:close f)
    (when (> (length content) 0) (:write f content)))
  (when exec
    (os/chmod p (bor (os/lstat p :int-permissions) 8r111))))

(defn apply-tree [dir tree]
  (each name (sort (keys tree))
    (def p (if (= dir ".") name (string dir "/" name)))
    (def v (tree name))
    (def st (os/lstat p))
    (cond
      (string? v) (do (clear p st) (write-file p v false))
      (array? v) (do (clear p st)
                   (if (= (v 0) "link") (os/symlink (v 1) p) (write-file p (v 1) true)))
      (do
        (when (or (nil? st) (not= (st :mode) :directory))
          (when st (os/rm p))
          (os/mkdir p))
        (apply-tree p v)))))

(defn main [& args]
  (when (> (length args) 1)
    (eprint "usage: json2dir < document.json")
    (os/exit 2))
  (def ok
    (try
      (do
        (var input (or (file/read stdin :all) @""))
        (check-utf8 input)
        (when (and (>= (length input) 3) (= (input 0) 0xEF) (= (input 1) 0xBB) (= (input 2) 0xBF))
          (set input (buffer/slice input 3)))
        (def root (parse-json input))
        (unless (table? root) (fail "the root of the document must be an object"))
        (validate root ".")
        (apply-tree "." root)
        true)
      ([err]
        (eprint (if (and (string? err) (string/has-prefix? "json2dir: " err)) err (string "json2dir: " err)))
        false)))
  (os/exit (if ok 0 1)))
