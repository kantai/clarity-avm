;; AVM state
;; version 1 uses 128 bit math :o
(define-constant AVM_STATE_HALTED 0x00)
(define-constant AVM_STATE_ERROR_STOP 0x01)
(define-constant AVM_STATE_EXTENSIVE 0x02)
(define-data-var avm-state (buff 1) AVM_STATE_HALTED)

(define-constant ERR_INSTR_STACK u1111111111111111111)
(define-constant ERR_LARGE_TUPLE_ENTRY u222222)

(define-data-var pc uint u0)
(define-map instr-stack uint { op: (buff 1), nextHash: (buff 32)})
(define-data-var instr-stack-tail uint u0)

(define-map data-stack uint (buff 10240))
;; points to the _next_ filled spot, i.e., data-stack[tail] is empty
(define-data-var data-stack-tail uint u0)
(define-map aux-stack uint (buff 10240))
;; points to the _next_ filled spot, i.e., data-stack[tail] is empty
(define-data-var aux-stack-tail uint u0)
(define-data-var register (buff 10240) 0x00)
(define-constant static 0x00)
(define-data-var avm-gas-remaining uint u0)
(define-data-var error-codepoint uint u0)

;; avm value types are stored in stacks as clarity serializations
;;  avm int => uint
;;  avm 'codepoint' => { pcr: uint }
;;  avm tuple => (list 8 (buff MAX_VALUE_SIZE/8 - 6)) ;; v2 todo: I think we can bound this better
;;  avm buffer => (buff MAX_VALUE_SIZE - 5)

;; error handling: do AVM error handling: set codepoint to error handler
;;  if set, otherwise halt. Call if the stepped op returns an error.
(define-private (handle-error)
  (begin 
    (if (is-eq (var-get error-codepoint) u0)
        (var-set avm-state AVM_STATE_EXTENSIVE)
        (var-set pc (var-get error-codepoint)))
    (ok false)))

(define-private (halt)
    (begin (var-set avm-state AVM_STATE_HALTED)
           (ok false)))

(define-private (deser-tuple (x (buff 10240)))
    (ok (unwrap! (from-consensus-buff (list 8 (buff 1274)) x) (err u2))))
(define-private (deser-int (x (buff 10240)))
    (ok (unwrap! (from-consensus-buff uint x) (err u2))))

(define-private (deser-sint (x (buff 10240)))
    (ok (unwrap! (from-consensus-buff int 
        ;; clarity type cast!
        (concat 0x00 (unwrap-panic (slice x u1 (len x)))))
        (err u2))))

(define-private (deser-codepoint (x (buff 10240)))
    (ok (get pcr (unwrap! (from-consensus-buff { pcr: uint } x) (err u2)))))

(define-private (ser-codepoint (x uint))
    (ok (unwrap! (to-consensus-buff { pcr: x }) (err u500))))

(define-private (pop-data-stack)
  (let ((tail (var-get data-stack-tail)))
    (asserts! (< u0 tail) (err u0))
    (var-set data-stack-tail (- u1 tail))
    (let ((rval (unwrap! (map-get? data-stack (- u1 tail)) (err u1))))
        (map-delete data-stack (- u1 tail))
        (ok rval))))
(define-private (pop-aux-stack)
  (let ((tail (var-get aux-stack-tail)))
    (asserts! (< u0 tail) (err u0))
    (var-set aux-stack-tail (- u1 tail))
    (let ((rval (unwrap! (map-get? aux-stack (- u1 tail)) (err u1))))
        (map-delete aux-stack (- u1 tail))
        (ok rval))))

(define-private (push-data-stack (x (buff 10240)))
  (let ((tail (var-get data-stack-tail)))
    (var-set data-stack-tail (+ u1 tail))
    (ok (map-set data-stack tail x))))
(define-private (push-data-stack-tuple (x (list 8 (buff 1274))))
    (push-data-stack (unwrap! (to-consensus-buff x) (err u500))))
(define-private (push-data-stack-buff (x (buff 10235)))
    (push-data-stack (unwrap! (to-consensus-buff x) (err u500))))
(define-private (push-data-stack-int (x uint))
    (push-data-stack (unwrap! (to-consensus-buff x) (err u500))))
(define-private (push-data-stack-sint (x int))
    (let ((int-ser (unwrap! (to-consensus-buff x) (err u500))))
         (push-data-stack (concat 0x01 (unwrap-panic (slice int-ser u1 (len int-ser)))))))
(define-private (push-aux-stack (x (buff 10240)))
  (let ((tail (var-get aux-stack-tail)))
    (var-set aux-stack-tail (+ u1 tail))
    (map-set aux-stack tail x)))

;; todo: does the byte opcode push an integer onto the stack or a buffer?
(define-private (handle-byte (a uint) (b uint))
 (if (< a u16) ;; should be 32 when using 256 bit integers
     (ok (buff-to-uint-be (unwrap! (element-at (unwrap-panic (to-consensus-buff b)) (+ a u1))
                        (err u10))))
     (err u10)))

;; handled: [1, 4] u [6] u [0x0a] u [0x10, 0x11] u [0x16, 0x18] u [0x1b, 0x1d]
(define-private (handled-by-bin-uint (op (buff 1)))
    (is-some (index-of 0x01020304060a10111617181a1b1c1d op)))
(define-private (handle-bin-uint (op (buff 1)))
 (let (
   (a_val (try! (pop-data-stack)))
   (b_val (try! (pop-data-stack)))
   (a_int (try! (deser-int a_val)))
   (b_int (try! (deser-int b_val)))
   (arithm-res
    (if (is-eq op 0x01) (ok (+ a_int b_int))
    (if (is-eq op 0x02) (ok (* a_int b_int))
    (if (is-eq op 0x03) (ok (- a_int b_int))
    (if (is-eq op 0x04) (ok (/ a_int b_int))
    (if (is-eq op 0x06) (ok (mod a_int b_int))
    (if (is-eq op 0x0a) (ok (pow a_int b_int))
    (if (is-eq op 0x10) (ok (if (< a_int b_int) u1 u0))
    (if (is-eq op 0x11) (ok (if (> a_int b_int) u1 u0))
    (if (is-eq op 0x16) (err u500) ;; todo: bitwise and possible via map?
    (if (is-eq op 0x17) (err u500) ;; todo: bitwise or possible via map?
    (if (is-eq op 0x18) (ok (xor a_int b_int))
    (if (is-eq op 0x1a) (handle-byte a_int b_int)
    (if (is-eq op 0x1b) (err u500) ;; todo: bitwise shifts possible via map?
    (if (is-eq op 0x1c) (err u500) ;; todo: bitwise shifts possible via map?
    (if (is-eq op 0x1d) (err u500) ;; todo: bitwise shifts possible via map?
        (err u0))))))))))))))))))
  (push-data-stack-int (try! arithm-res))))

(define-private (bin-uint (op (buff 1)))
    (if (handled-by-bin-uint op)
        (some (handle-bin-uint op))
        none))

;; handled: [5, 7, 12, 13]
(define-private (handled-by-bin-sint (op (buff 1)))
    (is-some (index-of 0x05071213 op)))
(define-private (handle-bin-sint (op (buff 1)))
 (let (
   (a_val (try! (pop-data-stack)))
   (b_val (try! (pop-data-stack)))
   (a_int (try! (deser-sint a_val)))
   (b_int (try! (deser-sint b_val)))
   (arithm-res
    (if (is-eq op 0x05) (ok (/ a_int b_int))
    (if (is-eq op 0x07) (ok (mod a_int b_int))
    (if (is-eq op 0x12) (ok (if (< a_int b_int) 1 0))
    (if (is-eq op 0x13) (ok (if (> a_int b_int) 1 0))
        (err u0)))))))
  (push-data-stack-sint (try! arithm-res))))

(define-private (bin-sint (op (buff 1)))
    (if (handled-by-bin-sint op)
        (some (handle-bin-sint op))
        none))

(define-private (pop)
  (begin
    (try! (pop-data-stack))
    (ok true)))
(define-private (spush)
  (push-data-stack static))
(define-private (rpush)
  (push-data-stack (var-get register)))
(define-private (rset)
  (ok (var-set register (try! (pop-data-stack)))))
(define-private (jump)
  (ok (begin (var-set pc (try! (deser-codepoint (try! (pop-data-stack))))) false)))
(define-private (cjump)
  (let ((a_val (try! (pop-data-stack)))
        (b_val (try! (pop-data-stack)))
        (a_cp (try! (deser-codepoint a_val)))
        (b_int (try! (deser-int b_val))))
        (if (is-eq b_int u0) 
            (ok true)
            (ok (begin (var-set pc a_cp) false)))))
(define-private (stackempty)
  (if (is-eq u0 (var-get data-stack-tail))
      (push-data-stack-int u1)
      (push-data-stack-int u0)))
(define-private (pcpush)
  (push-data-stack (try! (ser-codepoint (var-get pc)))))
(define-private (auxpush)
  (ok (push-aux-stack (try! (pop-data-stack)))))
(define-private (auxpop)
  (push-data-stack (try! (pop-aux-stack))))
(define-private (auxstackempty)
  (if (is-eq u0 (var-get aux-stack-tail))
      (push-data-stack-int u1)
      (push-data-stack-int u0)))
(define-private (nop) (ok true))
(define-private (errpush)
  (push-data-stack (try! (ser-codepoint (var-get error-codepoint)))))
(define-private (errset)
    (ok (var-set error-codepoint
     (try! (deser-codepoint (try! (pop-data-stack)))))))

(define-private (opmod (is-add-or-mul bool))
 (let (
   (a_val (try! (pop-data-stack)))
   (b_val (try! (pop-data-stack)))
   (c_val (try! (pop-data-stack)))
   (a_int (try! (deser-int a_val)))
   (b_int (try! (deser-int b_val)))
   (c_int (try! (deser-int c_val)))
   (inner_val (if is-add-or-mul (+ a_int b_int) (* a_int b_int))))
   (asserts! (not (is-eq u0 c_int)) (err u0))
   (push-data-stack-int (mod inner_val c_int))))

(define-private (addmod) (opmod true))
(define-private (mulmod) (opmod false))

(define-private (signextend)
    (err u500)) ;; todo: sign extension

;; handler for arithmetic opcodes not handled by bin-int, bin-sint (0x08, 0x09, 0x0b)
(define-private (arithmetic (op (buff 1)))
  (if (is-eq op 0x08) (some (addmod))
  (if (is-eq op 0x09) (some (mulmod))
  (if (is-eq op 0x0b) (some (signextend))
       none))))

(define-private (opeq)
 (let ((a_val (try! (pop-data-stack)))
       (b_val (try! (pop-data-stack))))
    (asserts! (and (>= (len a_val) u1) (is-eq (element-at a_val u0) (element-at b_val u0))) (err u1))
    (push-data-stack-int
        (if (is-eq a_val b_val) u1 u0))))

(define-private (iszero)
 (let (
   (a_val (try! (pop-data-stack)))
   (a_int (try! (deser-int a_val))))
  (push-data-stack-int (if (is-eq u0 a_int) u1 u0))))

(define-private (to-buffer (x uint))
    (unwrap-panic (as-max-len?
        (unwrap-panic (slice (unwrap-panic (to-consensus-buff x)) u1 u17)) u16)))

(define-private (invert-byte (x (buff 1)))
    (unwrap-panic (element-at
        (to-buffer (xor (buff-to-uint-be 0xff) (buff-to-uint-be x))) u15)))

(define-private (concat-len16-buff (a (buff 16)) (b (buff 16)))
    (unwrap-panic (as-max-len? (concat b a) u16)))

(define-private (notop)
 (let ((a-buff (try! (pop-data-stack))))
   (try! (deser-int a-buff)) ;; check that it is an int
   (push-data-stack
       (concat 0x01 
        (fold concat-len16-buff
         (unwrap-panic (as-max-len? (map invert-byte (unwrap-panic (slice a-buff u1 (len a-buff)))) u16)) 0x)))))

;; handler for logic opcodes not handled by bin-int, bin-sint (0x14, 0x15, 0x19)
(define-private (logic (op (buff 1)))
  (if (is-eq op 0x14) (some (opeq))
  (if (is-eq op 0x15) (some (iszero))
  (if (is-eq op 0x19) (some (notop))
  none))))

;; handler for flow control opcodes. if passed an op that isn't in the range, return none
(define-private (flow (op (buff 1)))
  (if (is-eq op 0x30) (some (pop))
  (if (is-eq op 0x31) (some (spush))
  (if (is-eq op 0x32) (some (rpush))
  (if (is-eq op 0x33) (some (rset))
  (if (is-eq op 0x34) (some (jump))
  (if (is-eq op 0x35) (some (cjump))
  (if (is-eq op 0x36) (some (stackempty))
  (if (is-eq op 0x37) (some (pcpush))
  (if (is-eq op 0x38) (some (auxpush))
  (if (is-eq op 0x39) (some (auxpop))
  (if (is-eq op 0x3a) (some (auxstackempty))
  (if (is-eq op 0x3b) (some (nop))
  (if (is-eq op 0x3c) (some (errpush))
  (if (is-eq op 0x3d) (some (errset))
  none)))))))))))))))

(define-private (typeop)
  (let ((a-ser-type 
          (unwrap-panic (element-at (try! (pop-data-stack)) u0))))
    (push-data-stack-int
     (if (is-eq a-ser-type 0x01) u0 ;; int
     (if (is-eq a-ser-type 0x0c) u1 ;; codepoint
     (if (is-eq a-ser-type 0x0b) u3 ;; avm tuple
     u12)))))) ;; else: buffer

(define-private (hashop)
  ;; todo: perform actual avm hash...
  (let ((a-buff (try! (pop-data-stack))))
    (push-data-stack-buff (keccak256 a-buff))))
;; todo: implement these functions
(define-private (ethhash2) (err u500))
(define-private (keccakfop) (err u500))
(define-private (sha256op) (err u500))

(define-private (hashing (op (buff 1)))
  (if (is-eq op 0x20) (some (hashop))
  (if (is-eq op 0x21) (some (typeop))
  (if (is-eq op 0x22) (some (ethhash2))
  (if (is-eq op 0x23) (some (keccakfop))
  (if (is-eq op 0x24) (some (sha256op))
  none))))))

(define-private (tget)
 (let (
   (a (try! (pop-data-stack)))
   (b (try! (pop-data-stack)))
   (a-int (try! (deser-int a)))
   (b-tup (try! (deser-tuple b))))
 ;; push raw serialized value
 (push-data-stack (unwrap! (element-at b-tup a-int) (err u2)))))

(define-private (tset)
 (let (
   (a (try! (pop-data-stack)))
   (b (try! (pop-data-stack)))
   (c (try! (pop-data-stack)))
   (a-int (try! (deser-int a)))
   (b-tup (try! (deser-tuple b))))
 (asserts! (< a-int (len b-tup)) (err u2))
 (push-data-stack-tuple
  (unwrap-panic (as-max-len? (concat
    (append (unwrap-panic (slice b-tup u0 a-int))
            (unwrap! (as-max-len? c u1274) (err ERR_LARGE_TUPLE_ENTRY)))
    (default-to (list) (slice b-tup (+ a-int u1) (len b-tup))
   )) u8)))))

(define-private (tuples (op (buff 1)))
  (if (is-eq op 0x50) (some (tget))
  (if (is-eq op 0x51) (some (tset))
  (if (is-eq op 0x52) (some (err u500))
  (if (is-eq op 0x53) (some (err u500))
  (if (is-eq op 0x54) (some (err u500))
  none))))))


(define-private (cur-op)
  (get op (map-get? instr-stack (var-get pc))))

(define-private (step)
    (let ((op (unwrap! (cur-op) (err ERR_INSTR_STACK))))
        (match (bin-uint op) result result
        (match (bin-sint op) result result
        (match (arithmetic op) result result
        (match (logic op) result result
        (match (flow op) result result        
        (err u404))))))))

(define-private (run-one)
    (match (step)
    ;; op handled ok, determine next pc value
    pop-instr?
    (if pop-instr?
        (let ((cur-pc (var-get pc)))
            (if (is-eq cur-pc u0) (halt) (ok (var-set pc (- cur-pc u1)))))
        (ok true))
    ;; op errored, run error handler
    error-code (handle-error)))