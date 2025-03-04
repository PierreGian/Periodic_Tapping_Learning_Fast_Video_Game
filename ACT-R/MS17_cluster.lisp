;;; Support code for the transfer version of the Space Fortress task

;;;global variables for monitoring state
  (defparameter *thrust* nil) ;has a thrust been issued
  (defparameter *vulnerability* 0); current vulnerability
  (defparameter *missiles* nil); number of missiles present
  (defparameter *points* 0)
  (defparameter *data* nil)
  (defparameter *turn* nil)
  (defparameter *deflate* nil)
  (defparameter *reset* nil)
  (defparameter *last-inc* 0)
  (defparameter *last-shot* 0)

;;; This notes relevant information for purpose of updating the imaginal and game-state module in the two games
(defun record-features-fortress (data)  
;(print data)
(if (equal (getf data :SCREEN-TYPE) "game")
  (let* ((vuln (getf data :vlner)) speed
         (collisions (getf data :COLLISIONS))
         (fortress (getf data :fortress))
         (ship (getf data :ship))
         (keys (getf data :keys))
         (missiles (getf data :MISSILES))
         (points (getf data :pnts)))
    (if (and (numberp *points*) (> points *points*)) (update-slot  'kill))
    (cond ((member :thrust keys) (setf *thrust* t))
          ((and *thrust* ship) (setf *thrust* nil) (setf speed (speed-sf (getf ship :vx) (getf ship :vy)))
            (if (or (< speed 1) (> speed 1.7)) (update-slot 'badspeed) (update-slot 'goodspeed))))
    (cond ((intersection collisions '(:BIG-HEX)) (update-slot 'bighexdeath))
          ((intersection collisions '(:small-HEX))  (update-slot 'smallhexdeath))
          ((intersection collisions '(:shell))  (update-slot 'shelldeath)))
    (if (intersection collisions '(:shell :BIG-HEX :small-HEX))  (update-slot 'death))
    (if (> vuln *vulnerability*) (setf *last-inc* *last-shot*))
    (cond ( (< (length *missiles*) (length missiles))(setf *last-shot* (/ (get-time) 1000.0))))
    ;(if (and (numberp *vulnerability*)(= vuln 0)(member :FORTRESS collisions)) (print *vulnerability*))  
    (if (and (numberp *vulnerability*)(numberp vuln) )
        ;
      (cond ;updating resets
            ((and (= vuln 0) (member :FORTRESS collisions) (< *vulnerability* 11) (getf fortress :alive))
             (setf *reset* t)(setf *deflate* nil) (update-slot 'bad-time) )
            ;reset vulnerability - successful shot
            ((< *vulnerability* vuln) (setf *reset* nil)(setf *deflate* nil)(update-slot 'hit))
            ;updating deflations - removed (equal *last-inc* *last-shot*)
            ((and (> *vulnerability* vuln) (< (- *vulnerability* vuln) 1) (not *deflate*)) (setf *deflate* t) (update-slot 'bad-time))))   
    (if (and (> (length *missiles*) (length missiles)) (not (and (= vuln 0) (member :FORTRESS collisions))) (= vuln *vulnerability*)) (update-slot 'miss))
    (setf *missiles* missiles)
    (setf *points* points)
    (setf *vulnerability* vuln)))
     (game-state-features-fortress data))

(defun outward (x y vx vy)
       (let* ((x1 (- x 355))
              (y1 (- y 315))
              (dist1 (sqrt (+ (* x1 x1) (* y1 y1) )))
              (x2 (- (+ x vx) 355))
              (y2 (- (- y vy) 315))
              (dist2 (sqrt (+ (* x2 x2) (* y2 y2)))))
         (- dist2 dist1)))

(defun game-slot (slot) (if (buffer-read 'game-state) (chunk-slot-value-fct (buffer-read 'game-state) slot)))

(defun game-state-features-fortress (data) 
(setf *data* data)
 (if (car (no-output (buffer-chunk game-state)))
   (cond ((equal (getf data :screen-type) "score" )
          (mod-buffer-chunk 'game-state (list 'x nil 'y nil 'vx nil 'vy nil 'fortress-alive  nil 'ship-alive nil 'VULNERABILITY nil 'outward nil
                         'prop-to-big nil       'deflate nil  'reset nil                   
                    'fortress-angle nil 'orientation nil   'thrust-angle nil 'speed nil 'angle nil  'time-to-outer nil 'status 'game-over)))
         ((getf (getf data :ship) :alive)
          (let* ((ship (getf data :ship))
                 (fortress-alive (if (getf (getf data :fortress) :alive) 'yes 'no))
                 (old-fortress-alive (game-slot 'fortress-alive))
                 (old-x (or (game-slot 'x) 235))
                 (old-y (or (game-slot 'y) 235))
                 (x (getf ship :x))
                 (y (getf ship :y))
                 (angle (getf ship :angle))
                 (orientation (getf ship :orientation))
                 (vel (/ (round (sqrt (+ (sqr (- x old-x)) (sqr (- y old-y)))) .05) 20.0))
                 (fortress-angle (mod (- (angle x (- 630 y) 355 315) 6) 360))
                 (vdir (getf ship :vdir))
                 (vx (getf ship :vx))
                 (vy (getf ship :vy))
                 (dsmall (dist-to-hex x y :radius 40))
                 (dbig (dist-to-hex x y :radius 200))
                 (deflate (if *deflate* (or (game-slot 'deflate) 'new)))
                 (reset (if *reset* (or (game-slot 'reset) 'new)))
                 (vuln (getf data :vlner))
                 (outward (outward x y vx vy))
                 (bighex (aif (getf data :bighex) it 200))
                 (game-change (if (not (equal fortress-alive old-fortress-alive)) 'state-changed 
                                (game-slot 'status)))
                 (time-to-outer  (if  (> outward 0) (travel-time-to-hex vel x y vx vy :radius bighex) 10))
                 (prop-to-big (if  (> outward 0) (/ dsmall (+ dsmall dbig)) 0))
                 )
            (mod-buffer-chunk 'game-state (list 'x x 'y y 'vx vx 'vy vy 'fortress-alive  fortress-alive 'ship-alive 'yes 'VULNERABILITY vuln
                    'fortress-angle fortress-angle 'orientation orientation 'game 'autoorbit 'outward outward 'prop-to-big prop-to-big
                    'deflate deflate  'reset reset
                     'thrust-angle (- vdir angle) 'speed vel 'angle angle 'time-to-outer time-to-outer 'status game-change))))
         (t 
          (mod-buffer-chunk 'game-state (list 'x nil 'y nil 'vx nil 'vy nil 'fortress-alive  (if (getf (getf data :fortress) :alive) 'yes 'no)
                                               'fortress-angle nil 'orientation nil 'VULNERABILITY (getf data :vlner) 'outward nil
                                               'prop-to-big nil  'deflate nil  'reset nil  
                                              'thrust-angle nil 'speed nil 'angle nil 'ship-alive 'no  'time-to-outer nil))))))


(defun update-slot (slot)   
       (schedule-mod-buffer-chunk 'imaginal (list slot 1) 0))

(defun speed-sf (vx vy) (sqrt (+ (* vx vx) (* vy vy)))) 

;calculates the difference between 2 angles dealing with looping at 360
(defun angle-offset (ang1 ang2)
          (let* ((offset (mod (- ang1 ang2) 360)))
            (if (> offset 180) (- 360 offset) offset)))

;what is angle of vector from first to second point?
(defun angle (x1 y1 x2 y2)
  (let* ((run (- x2 x1))
         (rise (- y2 y1))
         (a (if (not (zerop run))  (* (atan (/ rise run)) (/ 180 pi)))))
    (cond ((zerop run) (if (>= rise 0) 90 270))
          ((> run 0) (if (>= rise 0) a (+ 360 a)))
          (t (if (>= rise 0) (+ 180 a) (+ 180 a))))))

(defun safe-div (a b) (if (not (zerop b)) (/ a b) (progn (print 'bad-division) 1000)))

(defun intersection-point (p1 p2 x y vx vy) 
  (let* ((slope1  (safe-div (- vy) vx))
         (inter1 (- y (* x slope1)))
         (x1 (second p1))
         (x2 (second p2))
         (y1 (fourth p1))
         (y2 (fourth p2))
         (slope2 (if (not (equal x1 x2))  (safe-div (- y2 y1) (- x2 x1))))
         (inter2 (if slope2 (- y1 (* x1 slope2))))
         (point (line-intersect (list inter1 slope1)(if slope2 (list inter2 slope2) x2))))
    (if (and point (within (first point) x1  x2) (within (second point) y1  y2) ) point)))

(defun line-intersect (line1 line2)
  (cond ((and (listp line1) (listp line2))
         (let* ((a1 (first line1))
                (b1 (second line1))
                (a2 (first line2))
                (b2 (second line2)))
           (cond ((not (= b1 b2))
                  (let* ((x (safe-div (- a2 a1) (- b1 b2)))
                         (y (+ a1 (* b1 x))))
                    (list x y))))))
        ((listp line1)
         (list line2 (+ (first line1) (* line2 (second line1)))))
        ((listp line2)
         (list line1 (+ (first line2) (* line1 (second line2)))))))

(defun within (a b c)
  (or (and (>= b a) (>= a c)) (and (<= b a) (<= a c))))       


(defun sqr (x) (* x x))
(defun cubic (x) (* x x x))



     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Code to run conditions


 (defparameter *alpha* .2)
 (defparameter *factor* 1)
(defparameter *strategy* 1)
(defparameter *speed* 1)

(defun run-n-learning_FFF (flag speed &optional server)
  (setf *speed* 1)
  (setf *data-hook* 'record-features-fortress)
  (setf *instructions* autoorbit-instructions1)
  (play-sf-games 1 :speed speed :draw flag :condition "autoorbit_fast")
  (do ((i 1 (1+ i))
       (quads (list (quad-lists 1)) (cons (quad-lists (1+ i)) quads)))
      ((= i 15) (reverse quads))
    (play-sf-games 1 :speed speed :draw flag :cont t)))

(defun run-transfer-conds (params )
  (setf *alpha* (first params))
  (setf *factor* (second params))
 (case (third params) 
   (1 (run-n-learning_FFF nil 1000 ))))

(defun orbit-FFF-cluster (n server)
  (run-n-learning_FFF nil 1000 server))

(defun quad-lists (i)
          (mapcar (lambda (x) (list i x)) (no-output (print-tracker-stats))))

(defun noisecalc (time ss) ;;Function to calculate noise
  (let ((toreturn (+ time (act-r-noise ss))))
    (min (* 3 time) (max 0 toreturn))))

(defparameter *data-hook* 'record-features-fortress)


(defparameter starting '(GET-STARTED GET-STARTED-AGAIN GET-STARTED-NEW-GAME START-PLAYING-SPACETRACK START-PLAYING-SPACEFORTRESS START-PLAYING-AGAIN-SPACETRACK 
                                     START-PLAYING-AGAIN-SPACEFORTRESS DETECT-TRIAL-END DETECT-DESTROYED DO-SOMETHING DO-SOMETHING-FAST OBJECT-PRESENT OBJECT-NOT-PRESENT TEST-DIMENSION-SUCCESS 
                                     TEST-DIMENSION-FAIL TEST-DIMENSION-PRESS LONG-AIM AIM-AGAIN SECOND-AIM TAPPING TURN-TO-POINT1 SKIP-TURN START-PRESS-CLOCKWISE PRESS-CLOCKWISE-TAP START-PRESS-COUNTERCLOCKWISE PRESS-COUNTERCLOCKWISE-TAP TAPPPING-D
                                     PRESSING-D FINISH-PRESS-D PRESSING-A FINISH-PRESS-A TAPPING-A CALCULATE-TURN-ANGLE INCREMENT-ANGLE DECREMENT-ANGLE NEW-AIM VULNERABILITY-PRESS?
                                     DELAY-OK PRESS-SPACEBAR DOUBLE-SPACEBAR START-SHOT-TAP DOUBLE-SHOT AIM THRUST-TIME STOPPING-PERIOD))

(defparameter session1 nil)

(defparameter autoorbit-instructions1
  '(
;;common operators
(starter1
  ISA OPERATOR
   PRE  autoorbit
   action adjust-aim
   post delay?)
(kill-it 
      isa operator
      pre kill-it
      action double-l
      post done)
(delay
  ISA OPERATOR
   PRE  delay?
   action  check-delay
   success vulnerability-press?
   fail new-aim)
(vulnerability-press
  ISA OPERATOR
   PRE  vulnerability-press?
   ACTION  test-dimension-press
   arg1 vulnerability
  success kill-it
   fail shoot)
(shoot 
      isa operator
      pre shoot
      action l
      post new-aim)
(aiming-again
      isa operator
      pre new-aim
      action aim-again
      short delay?
      long long-aim
)
(secondAim
      isa operator
      pre long-aim
      action second-aim
      post delay?
)
))


(defparameter *instructions* autoorbit-instructions1)


(defparameter *lower* 10.0)
(defparameter *upper* 30.0)
(defparameter *time-width* 6)

(defun lower-bounds (ticks lower upper) 
  (cond ((> ticks (- upper *time-width*)) nil)
        ((> lower (- upper *time-width*)) nil)
        ((> ticks lower) ticks)
        (t nil)))

(defun upper-bounds (ticks lower upper) 
  (cond ((< ticks (+ lower *time-width*)) nil)
        ((< upper (+ lower 1)) nil)
        ((< ticks upper) ticks)
        (t nil)))

(clear-all)

(check-module-version :tracker "5.3")

(define-model Orbit-player
(setf *thrust* nil *vulnerability* 0 *missiles* nil *points* 0 *deflate* nil *reset* nil)
  (setf *last-inc* 0)

  
  (sgp :er t :esc t :egs .0001 :ol t :lf .05  :ans .01 :rt -1 :pct nil :trace-detail low :v t )
  ;; Temporal module's noise
  (eval `(sgp :sf-data-hook ,*data-hook*))

;; Temporal module's noise
  (sgp :TIME-NOISE .005)
  
 ;; Motor setup, timing, and randomization 
  (sgp :randomize-time 3)
  (sgp :dual-execution-stages t)
  (sgp :MOTOR-FEATURE-PREP-TIME 0.020)
  (sgp :MOTOR-INITIATION-TIME .020)

  ;; visual configuration 
  (sgp :visual-movement-tolerance 2 :do-not-harvest visual)

  (chunk-type gamestate x y orientation vx vy ship-alive status game  ;common
                time-to-outer thrust-angle vulnerability FORTRESS-ANGLE fortress-alive outward prop-to-big speed)   ;space fortess
  (chunk-type goal step state next target aim game thrust-time width ;common
              time-to-outer time-thresh dist-thresh vulnerability last-action outward prop-to-big speed) ;space fortess
  (chunk-type operator pre action arg1 result post  fail success fast slow short long)
  (chunk-type mapping function key)
  (chunk-type tracker rectangle death bad-aim good-aim
              hit miss badspeed goodspeed bighexdeath shelldeath smallhexdeath)

(define-chunks yes no speed adjust offaim turn-to-point test-dimension pressing-d pressing-key pressing-left down target-angle
                      THRUST-TO-TURNTAP-THRUST death press-point stop-angle tapthrust TAPPING-KEY pressing-a
                      CALCULATE-TURN-ANGLE maketurn2 test-speed  start done connected play start-playing-again do-step game-over
                      start-playing current-angle future-angle pressing-thrust tap-key thrust-to-turn ship-speed
                      NOFORTRESS FORTRESS-ALIVE TESTOBJECT DELAY? AIM? START-PERIOD CHECK-DELAY ANGLE fortress-angle autoorbit aim
                      check-aim attended adjusting refining aim-thresh
                             thrust thrust-angle increment-angle decrement-angle TIME-TO-OUTER shooting vulnerability? thrusting
                             double-l state-changed reset hit othet-time-thresh time-thresh bighexdeath test-object prop-to-big
                             kill smallhexdeath miss badspeed goodspeed retrieve-vulnerability adjust-aim)

(add-dm 


(a isa mapping function counterclockwise key a)
(d isa mapping function clockwise key d)
(l isa mapping function shooting key l)
)
(eval (cons 'add-dm *instructions*))


(sgp :ul t  :epl t :egs .05 :alpha .2 :iu 9  )  


(eval `(sgp :alpha ,*alpha*))
(eval `(sgp :initial-temp ,*factor*))
(sgp :tracker-decay-method exponential)
(eval `(sgp :tracker-decay .995))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; General productions for playing a SF-style game

    ;;; This production continues play at the very beginning


  (p get-started
     "Make sure game is running"
     ?game>
       game-state connected
     ?goal>
       buffer empty
     ?imaginal>
       state free
     ?game-state>
       state free
     ==>
     +goal>
         isa goal
         state start-playing
     +imaginal>
         isa tracker
     +game-state>
         isa gamestate
      )

    ;;; This production continues play for another game.
(p get-started-again
     "Make sure game is running"
     ?game>
       game-state connected
    =game-state>
       ship-alive yes 
       game =game
     =goal>
       state game-over
       game =game
==>
    =game-state>
      status nil
     =goal>
       state start-playing-again)

(p get-started-new-game
     "Make sure game is running"
     ?game>
       game-state connected
    =game-state>
       ship-alive yes 
       game =game
     =goal>
       state game-over
      - game =game
     ==>
     =goal>
         isa goal
         state start-playing
     +imaginal>
        isa tracker
     +game-state>
      )

;initiates for first game of auto orbit
 (p start-playing-autoorbit
     "once the ship is visible get going"
     =goal>
       isa goal
       state start-playing  
       - game autoorbit 
    =game-state>
        game autoorbit    
     ==>
!bind! =lower *lower*
!bind! =upper *upper*     
     +tracker>
         control-slot time-period
         good-slot hit
         bad-slot bad-time
         min =lower
         max =upper
         bad-weight -1
         name 1
     +tracker>
         control-slot aim
         good-slot hit
         bad-slot miss
         min -18.0
         max 0.0
         bad-weight -1
         name 1
     +tracker>
         control-slot width
         good-slot hit
         bad-slot miss
         min 5.0
         max 15.0
         bad-weight -1
         name 1
     +tracker>
         control-slot vulnerability
         good-slot kill
         bad-slot reset
         min 9.0
         max 11.0
         bad-weight -1
     +tracker>
         control-slot aim-thresh
         good-slot hit
         bad-slot deflate
         min -10.0
         max 0.0
         bad-weight -1
         name 1
     =goal>
         state play
         step nil
         reinit yes
         game autoorbit
         coef 5
         name 1
         speed 1
    +temporal>
       isa time
     )

  (p start-playing-again-autoorbit
     =goal>
       isa goal
       state start-playing-again
       game autoorbit
       speed =speed
    =game-state>
        game autoorbit 
       ship-alive yes
   !safe-eval! (= =speed *speed*)
     ==>
   -retrieval>
     =goal>
         state play
     )

  (p start-playing-again-autoorbit-new
     =goal>
       isa goal
       state start-playing-again
       game autoorbit
       speed 1
   !safe-eval! (= *speed* 2)
    =game-state>
        game autoorbit 
       ship-alive yes
     ==>
!bind! =lower *lower*
!bind! =upper *upper*
     +tracker>
         control-slot time-period
         good-slot hit
         bad-slot bad-time
         min =lower
         max =upper
         bad-weight -1
         name 3     
   -retrieval>
     =goal>
         state play
         coef 3
         reinit no
         speed 2
     )

  (p start-playing-again-autoorbit-old
     =goal>
       isa goal
       state start-playing-again
       game autoorbit
       speed 2
   !safe-eval! (= *speed* 1)
    =game-state>
        game autoorbit 
       ship-alive yes
     ==>
   -retrieval>
     +tracker>
         control-slot time-period
         name 1
     =goal>
       state play
       reinit yes
       speed 1)


  (spp GET-STARTED :u 100 :reward t)
  (spp start-playing-autoorbit :u 100 :reward t)
  (spp start-playing-again-autoorbit :u 100 :reward t)
  (spp start-playing-again-autoorbit-new :u 100 :reward t)

;;; This production determines game over and requires that information in game-state
 (p detect-trial-end
    "If a trial is clear things out"
    =goal>
      isa goal
    -  state game-over
    =game-state>
      status game-over
    ?manual>
      processor free
==>
    =goal>
      state game-over
      step nil
    +manual>
     isa release-all-fingers
    )

 (spp detect-trial-end :u 10000 :reward t)

    (p do-something
       =goal>
          isa goal
          state play
      ?retrieval>
           state free
       ?MANUAL>
          preparation FREE
       ?MANUAL-left>
          state FREE
       ?MANUAL-right>
          state FREE
    =game-state>
        game =game 
      ==>
        =goal>
           state do-step
           next nil
        +retrieval>
           isa operator
           pre =game
     )

(p initial-aim
    =goal>
       isa goal
       state do-step
    =retrieval>
       action adjust-aim
       post =NEWSTEP
    ==>
    =goal>
       next =NEWSTEP
       state tapping
       step adjusting
)

    (p back-to-aim
       =goal>
          isa goal
          state tapping
          next =NEWSTEP
          aim =aim
          width =width
       ?MANUAL>
          preparation FREE
     =game-state>
          angle =angle
!safe-eval!   (and (< =angle (+ =AIM  =WIDTH)) (> =angle (- =AIM  =WIDTH)))
      ==>
       =goal>
          next nil
          state do-step
          step nil
        +retrieval>
           isa operator
           pre =NEWSTEP
)

(p detect-deflate
    =goal>
       coef =coef
    =temporal>
       ticks =ticks
    =game-state>
       deflate new
     =tracker>
         control-slot time-period
         min =min
         max =max
!eval!  (upper-bounds =ticks =min =max)
==>
!bind! =val  (- =max (/ (- =max (upper-bounds =ticks =min =max)) =coef))
   =game-state>
       deflate attended
 *tracker>
      max =val
)

(spp detect-deflate :u 20 :reward t)

(p detect-reset
    =goal>
       time-period =timing
    =game-state>
       reset new
    =tracker>
       control-slot time-period
       min =min
       max =max
!eval! (lower-bounds =timing =min =max)
==>
!bind! =val (lower-bounds =timing =min =max)
=game-state>
    reset attended
*tracker>
    min =val)

(spp detect-reset :u 20 :reward t)

(p tapping-d
   =goal>
      state tapping-key
   =RETRIEVAL>
       key d
     ==>
   =goal>
     state tapping
   +MANUAL>
       CMD DELAYED-PUNCH
       FINGER index
       HAND LEFT
       delay .12)

(p tapping-a
   =goal>
      state tapping-key
   =RETRIEVAL>
       key a
     ==>
   =goal>
     state tapping
   +MANUAL>
       CMD DELAYED-PUNCH
       FINGER ring
       HAND LEFT
       delay .12)

    (p adjust-back-clockwise
       =goal>
          isa goal
          state tapping
          step adjusting
        - next nil
          aim =aim
          width =width
     =game-state>
          angle =angle
    ?MANUAL-LEFT>
          state free
!SAFE-EVAL! (>= =ANGLE (+ =AIM  =WIDTH))
      ==>
   +retrieval>
      isa mapping
      function clockwise
   =goal>
     state tapping-key
     step refining)

(p adjust-back-clockwise-again
       =goal>
          isa goal
          state tapping
          step refining
        - next nil
          aim =aim
          width =width
     =game-state>
          angle =angle
       ?MANUAL>
          state FREE
!SAFE-EVAL! (>= =ANGLE (+ =AIM  =WIDTH))
    ==>
   +retrieval>
      isa mapping
      function clockwise
   =goal>
     state tapping-key
)

    (p adjust-back-counterclockwise
       =goal>
          isa goal
          state tapping
          step adjusting
          aim =aim
          width =width
        - next nil
     =game-state>
          angle =angle
    ?MANUAL-LEFT>
          state free
!SAFE-EVAL! (<= =ANGLE  (- =AIM  (* 2 =WIDTH)))
      ==>
   +retrieval>
      isa mapping
      function counterclockwise
   =goal>
     state tapping-key
     step refining)

    (p adjust-back-counterclockwise-again
       =goal>
          isa goal
          state tapping
          step refining
          aim =aim
          width =width
        - next nil
     =game-state>
          angle =angle
    ?MANUAL-LEFT>
          state free
!SAFE-EVAL! (<= =ANGLE  (- =AIM (* 2 =WIDTH)))
      ==>
   +retrieval>
      isa mapping
      function counterclockwise
   =goal>
     state tapping-key)


(p press-double-l
   =goal>
      state do-step
  =game-state>
      fortress-alive yes
    =retrieval>
      isa operator
      action double-l
      post =next
     ==>
   =goal>
      state double-l
      next =next
     +manual>
       isa delayed-punch
        hand right
       finger index
        delay .09)


(p double-shot
   =goal>
      state double-l
   ?MANUAL>
       processor FREE
     ==>
     +manual>
       isa delayed-punch
      hand right
      finger index
       delay .09
    +temporal>
       isa time  
     =goal>
        state play
        step nil)

(spp press-double-l :reward 10)

(p reinitialize-tracker-new
       =goal>
          isa goal
          state play
        - step tracker-mod
          reinit no
          next done
          speed 2
     =tracker>
         control-slot time-period
         name 3
         min =min
         max =max
   ==>
       =goal>
          step tracker-mod
          tracker-min =min
          tracker-max =max
      +tracker>
        isa copy-tracker
        control-slot time-period
        name 1
        new-name 2
        reweight :new
        copy-temp t 
)

(p mod-tracker
       =goal>
          isa goal
          state play
          step tracker-mod
          reinit no
          next done
          tracker-min =min
          tracker-max =max
          speed 2
     =tracker>
         control-slot time-period
   ==>
      =goal>
          reinit yes
          step nil
      *tracker>
          min =min
          max =max
)

(p test-dimension-success
   =goal>
      state retrieve-vulnerability
      =dimension =thresh
    =retrieval>
      isa operator
      action test-dimension-press
      arg1 =dimension
      success =newstep
    =game-state>
        =dimension =val     
   !SAFE-EVAL! (> =val =thresh)
     ==>
   =goal>
      state do-step
    +retrieval>
        isa operator
        pre =newstep)

(p test-dimension-fail
   =goal>
      state retrieve-vulnerability
      =dimension =thresh
    =retrieval>
      isa operator
      action test-dimension-press
      arg1 =dimension
      fail =newstep
    =game-state>
        =dimension =val     
   !SAFE-EVAL! (<= =val =thresh)
     ==>
   =goal>
      state do-step
    +retrieval>
        isa operator
        pre =newstep)

(p delay-no-temporal
   =goal>
      state do-step
   =RETRIEVAL>
       ACTION check-delay
       success =newstep      
   ?TEMPORAL>
       buffer empty
     ==>
   =goal>
      state retrieve-vulnerability
  +RETRIEVAL>
       PRE =NEWSTEP)

(p delay-not-ok
   =goal>
      state do-step
      width =width
      aim =aim
    - step process-delay
    - step waiting
      time-period =tick-thresh
   =RETRIEVAL>
      ACTION check-delay
      fail =newstep
    =temporal>
      ticks =ticks
   =game-state>
      angle =angle
!safe-eval!   (or (> =angle (+ =aim (* 2 =width))) (< =angle (- =aim (* 2 =width))))
!safe-eval! (< =ticks =tick-thresh)
==>
  =goal>
     step process-delay
  +RETRIEVAL>
       PRE =NEWSTEP)

(p delay-ok
   =goal>
      state do-step
      time-period =tick-thresh
   =RETRIEVAL>
       ACTION check-delay
       success =newstep      
   =TEMPORAL>
       TICKS =TICKS
   =GAME-STATE>
       ANGLE =ANGLE
!safe-eval! (>= =ticks =tick-thresh)
     ==>
   =goal>
      state retrieve-vulnerability
      step nil
  +RETRIEVAL>
       PRE =NEWSTEP)

(p press-l
   =goal>
      state do-step
  =game-state>
      game =game
    =retrieval>
      action l
      post =NEWSTEP
     ==>
    +temporal>
       isa time
    +RETRIEVAL>
       PRE =NEWSTEP 
     +manual>
       isa delayed-punch
        hand right
       finger index
        delay .12)

(spp press-l :reward 10)

(p short-new-aim
   =goal>
      state do-step
      aim-thresh =threshold
      width =width
      aim =aim
    - step waiting
   =game-state>
      angle =angle
   =retrieval>
      action aim-again
      short =NEWSTEP
!eval! (and (<= =angle =threshold) (> =angle (- =aim (* 2 =width))))
    ==>
   =goal>
      state do-step
      step waiting
  +RETRIEVAL>
       PRE =NEWSTEP
  +vocal>
     cmd subvocalize
     string "wait"
)

(p long-new-aim
   =goal>
      state do-step
      aim-thresh =threshold
      width =width
      aim =aim
   =game-state>
      angle =angle
   =retrieval>
      action aim-again
      long =NEWSTEP
!safe-eval! (or (> =angle =threshold) (<= =angle (- =aim (* 2 =width))))
    ==>
   =goal>
      step long-aim
  +RETRIEVAL>
       PRE =NEWSTEP
)

(p turn-to-point2
   =goal>
      state do-step
      step long-aim
    =retrieval>
      isa operator
      action second-aim
      post =next
    =game-state>
      angle =angle
     ==>
    =goal>
       next =next
       state tapping
       step adjusting)

  (p reinitialize-clockwise
     =goal>
       game =game
       aim =AIM
       width =WIDTH
       step long-aim
     =game-state>
          angle =angle
!SAFE-EVAL! (>= =ANGLE (+ =AIM (* 5 =WIDTH)))
     ?manual> 
       preparation free 
     ==> 
   =goal>
      state do-step
      step nil
   +retrieval>
      isa operator
      pre =game
     +temporal> 
       isa clear)

  (p reinitialize-counterclockwise
     =goal>
       game =game
       aim =AIM
       width =WIDTH
       step long-aim
     =game-state>
          angle =angle
!SAFE-EVAL! (<= =ANGLE  (- =AIM (* 5 =WIDTH)))
     ?manual> 
       preparation free 
     ==> 
   =goal>
      state do-step
      step nil
   +retrieval>
      isa operator
      pre =game
     +temporal> 
       isa clear)

)





