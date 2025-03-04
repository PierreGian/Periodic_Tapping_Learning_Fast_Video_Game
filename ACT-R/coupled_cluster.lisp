(load "periodic-tap_correction.lisp")
(load "actr7.x/extras/blending/blending.lisp")

;;; Support code for the transfer version of the Space Fortress task

;;;global variables for monitoring state
  (defparameter *thrust* nil) ;has a thrust been issued
  (defparameter *vulnerability* 0); current vulnerability
  (defparameter *missiles* nil); number of missiles present
  (defparameter *points* 0)
  (defparameter *data* nil)
  (defparameter *turn* nil)
  (defparameter *deflate* nil)
  (defparameter *balloon-burst* nil)
  (defparameter *reset* nil)
  (defparameter *miss* nil)
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
            ((not (getf (getf data :fortress) :alive)) (setf *balloon-burst* t))
			;reset vulnerability - successful shot
            ((< *vulnerability* vuln) (setf *reset* nil)(setf *deflate* nil)(setf *miss* nil)(update-slot 'hit))
            ;updating deflations - removed (equal *last-inc* *last-shot*)
            ((and (> *vulnerability* vuln) (< (- *vulnerability* vuln) 1) (not *deflate*)) (setf *deflate* t) (update-slot 'bad-time))
			((and (> (length *missiles*) (length missiles)) (not (member :FORTRESS collisions)) (= vuln *vulnerability*)) (setf *miss* t))))  
    (if (and (> (length *missiles*) (length missiles)) (not (member :FORTRESS collisions)) (= vuln *vulnerability*)) (update-slot 'miss))
	(if (and (getf (getf data :fortress) :alive) (= vuln 0)) (setf *balloon-burst* nil))
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
                         'prop-to-big nil       'deflate nil  'reset nil 'miss nil                   
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
				 (miss (if *miss* (or (game-slot 'miss) 'new)))
                 (vuln (getf data :vlner))
                 (outward (outward x y vx vy))
                 (bighex (aif (getf data :bighex) it 200))
                 (game-change (if (not (equal fortress-alive old-fortress-alive)) 'state-changed 
                                (game-slot 'status)))
				 (balloon-burst (if *balloon-burst* (or (game-slot 'balloon-burst) 'yes)))
                 (time-to-outer  (if  (> outward 0) (travel-time-to-hex vel x y vx vy :radius bighex) 10))
                 (prop-to-big (if  (> outward 0) (/ dsmall (+ dsmall dbig)) 0))
                 )
            (mod-buffer-chunk 'game-state (list 'x x 'y y 'vx vx 'vy vy 'fortress-alive  fortress-alive 'ship-alive 'yes 'VULNERABILITY vuln
                    'fortress-angle fortress-angle 'orientation orientation 'game 'autoorbit 'outward outward 'prop-to-big prop-to-big
                    'deflate deflate  'reset reset 'miss miss 'balloon-burst balloon-burst
                     'thrust-angle (- vdir angle) 'speed vel 'angle angle 'time-to-outer time-to-outer 'status game-change))))
         (t 
          (mod-buffer-chunk 'game-state (list 'x nil 'y nil 'vx nil 'vy nil 'fortress-alive  (if (getf (getf data :fortress) :alive) 'yes 'no)
                                               'fortress-angle nil 'orientation nil 'VULNERABILITY (getf data :vlner) 'outward nil
                                               'prop-to-big nil  'deflate nil  'reset nil  'miss nil 'balloon-burst nil
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

(defun decreasing-noise-with-bursts (burst-n) ;;multiply by factor in production
	(let ((ans (get-parameter-value :ans)))
	(act-r-noise (/ ans (+ 1 (log (+ 1 burst-n))))))
)


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
 (defparameter *factor* 1.0)
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
   fast1 aim-turn?
   fast2 period
   slow delay?)
(kill-it 
      isa operator
      pre kill-it
      action double-l
      post done)
;;periodic tapping operators
(start-shots
  ISA OPERATOR
   PRE  period
   action  start-period
   success aim?
   fail autoorbit)
(aiming
  ISA OPERATOR
   PRE  aim?
   action  check-aim
   post vulnerability-periodic?)
(vulnerability-periodic
  ISA OPERATOR
   PRE  vulnerability-periodic?
   ACTION  test-dimension
   arg1 vulnerability
  success stop-shot-tap
   fail aim?)
(stop-period
  ISA OPERATOR
  PRE stop-shot-tap
  action stop-period
  post kill-it
)
;;turn estimation operators
(aiming-turns
  ISA OPERATOR
   PRE  aim-turn?
   action  aim-estimate
   fail aim-turn?
   success period)
;;press-shot operators
(delay
  ISA OPERATOR
   PRE  delay?
   action  check-delay
   success vulnerability-press?
   fast1 autoorbit
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

(defun tick-compute (tick)
  (let ((int-tick (round tick)))
  (if (= int-tick 1) 0.011 (* 1.1 (tick-compute (- int-tick 1)))))
)
(defun tick-to-period (tick)
(let ((int-tick (round tick)))
(if (= int-tick 1) 0.011 (+ (tick-to-period (- int-tick 1)) (tick-compute int-tick))))
)

;;make hash table
(setf tick-ht (make-hash-table))

;;for all elements up to upper bound, compute period equivalent
(loop for x from 1 to *upper* 
do (setf (gethash x tick-ht) (tick-to-period x)))

(defun tick-float-period (a b tick-ht)
  (if (= a 0) (let ((t-length (tick-compute (1+ a)))) (* b t-length))
(let ((cur-tick (gethash a tick-ht)) (t-length (tick-compute (1+ a))))
  (+ (float cur-tick) (* b t-length))))
)
;;(setf (gethash 0 tick-ht) 10)
(defun get-period (tick tick-ht)
  (cond ((< tick 0) (- 0 (get-period (- 0 tick) tick-ht)))
        ((= tick 0) 0)
        ((> tick 0)
           (if (< tick 0) (- 0 (get-period (- 0 tick) tick-ht))
(if (> tick *upper*) (get-period *upper* tick-ht)
(cond
   ;if tick is a float
   ((floatp tick) 
       (let ((integ-tick (truncate tick)))
          (tick-float-period integ-tick (- tick integ-tick) tick-ht)))
   ;if tick is an integer
   ((integerp tick) (gethash tick tick-ht))
   ;if tick is a ratio
   ((ratiop tick) (get-period (float tick) tick-ht))))))))

(defun match-tick-per (period tick)
  (let ((cur-x-per (tick-to-period tick)))
    (if (< period cur-x-per) 
        (let* ((x-tick (- tick 1))(remainder (- period (tick-to-period x-tick))))
          (if (= remainder 0) x-tick 
              (let* ((l-tick (tick-compute tick))(tick-dec (/ remainder l-tick)))
                (+ x-tick tick-dec))))
        (match-tick-per period (+ tick 1)))) 
  )

(defun get-tick (period)
(cond ((or (and (< period (tick-to-period 1))(> period 0)) (= period 0))
        0)
      ((< period 0)
        (- 0 (get-tick (- 0 period))))
      ((> period (tick-to-period *upper*))
       *upper*)
      ((and (>= period (tick-to-period 1))(<= period (tick-to-period *upper*)))
       (match-tick-per period 1)
       )))

(clear-all)
(require-extra "blending")

(define-model Orbit-player
(setf *thrust* nil *vulnerability* 0 *missiles* nil *points* 0 *deflate* nil *reset* nil *miss* nil)
  (setf *last-inc* 0)

  
  (sgp :er t :esc t :ol t :lf .05 :ans .01 :mp 2 :rt -1 :pct nil :trace-detail low :v t)
  ;; Temporal module's noise
  (eval `(sgp :sf-data-hook ,*data-hook*))

;; Temporal module's noise
  (sgp :TIME-NOISE .005)
  
 ;; Motor setup, timing, and randomization 
  (sgp :randomize-time 3)
  (sgp :dual-execution-stages t :tap-motor-noise 0.04)
  (sgp :MOTOR-FEATURE-PREP-TIME 0.020)
  (sgp :MOTOR-INITIATION-TIME .020)

  ;; visual configuration 
  (sgp :visual-movement-tolerance 2 :do-not-harvest visual)

  (chunk-type gamestate x y orientation vx vy ship-alive status game  ;common
                time-to-outer thrust-angle vulnerability FORTRESS-ANGLE fortress-alive outward prop-to-big speed   ;space fortess
				balloon-burst deflate reset) ;auto-orbit
  (chunk-type goal step state next target aim game thrust-time width ;common
              time-to-outer time-thresh dist-thresh vulnerability last-action outward prop-to-big speed ;space fortess
			  coef sts turn-time-max turn-time-min turn-no-hit-count turn-step explore-turn sss-count nb-bursts rotation
			  first-burst shot-tapping vln-track count-track fast-thresh time-period hit-count-thresh turn-tap-time tap-freeze-count aim-thresh) ;auto-orbit
  (chunk-type operator pre action arg1 result post  fail success fast1 fast2 slow short long)
  (chunk-type mapping function key)
  (chunk-type tracker rectangle death bad-aim good-aim
              hit miss badspeed goodspeed bighexdeath shelldeath smallhexdeath)
  (chunk-type strategy-option (strategy-option-slot t))
  (chunk-type outcome payoff strategy burst)

(define-chunks yes no speed adjust offaim turn-to-point test-dimension pressing-d pressing-key pressing-left down target-angle
                      THRUST-TO-TURNTAP-THRUST death press-point stop-angle tapthrust TAPPING-KEY pressing-a
                      CALCULATE-TURN-ANGLE maketurn2 test-speed  start done connected play start-playing-again do-step game-over
                      start-playing current-angle future-angle pressing-thrust tap-key thrust-to-turn ship-speed
                      NOFORTRESS FORTRESS-ALIVE TESTOBJECT DELAY? AIM? START-PERIOD CHECK-DELAY ANGLE fortress-angle autoorbit aim
                      check-aim attended adjusting refining aim-thresh old period aim-turn? vulnerability-periodic? stop-shot-tap vulnerability
					  aim-estimate new-aim vulnerability-press? test-dimension-press long-aim aim-again second-aim
					  OK wait-turn time-period tapping turning reset-timer explore processing turn-tap-time freeze process-delay
					  waiting explore-tap tap-explore
                             thrust thrust-angle increment-angle decrement-angle TIME-TO-OUTER shooting vulnerability? thrusting
                             double-l state-changed reset hit othet-time-thresh time-thresh bighexdeath test-object prop-to-big
                             kill smallhexdeath miss badspeed goodspeed retrieve-vulnerability adjust-aim)

(add-dm 
(a isa mapping function counterclockwise key a)
(d isa mapping function clockwise key d)
(l isa mapping function shooting key l)
)
(add-dm (shoot-shoot-shoot isa strategy-option)
        (shoot-turn-shoot isa strategy-option))
		
;;;Set some chunks ahead of time
(add-dm (instance1 isa outcome payoff -43 strategy shoot-shoot-shoot burst 1)
		(instance2 isa outcome payoff +30 strategy shoot-turn-shoot burst 1))


(eval (cons 'add-dm *instructions*))


(sgp :ul t  :epl t :egs .05 :alpha .2 :iu 9 :TMP 0.01)  
;;;(sgp :noise-hook decreasing-noise-with-practice)

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
	   isa goal
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
	   isa goal 
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
         bad-slot miss
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
     +tracker>
         control-slot hit-count-thresh
         good-slot hit
         bad-slot miss
         min -1.0
         max 4.0
         bad-weight -1
         name 1
     +tracker>
         control-slot tap-freeze-count
         good-slot hit
         bad-slot miss
         min 1
         max 5
         bad-weight -1
         name 1
     =goal>
         state play
         step nil
         count-track 0
         vln-track 0
         game autoorbit
		 shot-tapping no
		 first-burst no
		 rotation OK
		 fast-thresh 18
		 nb-bursts 0
		 payoff 0
		 sss-count 0
		 burst 0
		 explore-turn yes
		 turn-step wait-turn
		 turn-no-hit-count 0
		 turn-time-min =lower
		 turn-time-max =upper
		 sts 0
         coef 5
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
		 rotation OK
     )


  (spp GET-STARTED :u 100 :reward t)
  (spp start-playing-autoorbit :u 100 :reward t)
  (spp start-playing-again-autoorbit :u 100 :reward t)

;;; This production determines game over and requires that information in game-state
;;; Case 1: no periodic tapping
 (p detect-trial-end-no-tap 
    "If a trial is clear things out"
    =goal>
      isa goal
    -  state game-over
    =game-state>
      status game-over
    ?manual-right>
       state free
==>
    =goal>
      state game-over
      step nil
    +manual>
     isa release-all-fingers
    )

;;; Case 2:  periodic tapping is ongoing
 (p detect-trial-end-tap
    "If a trial is clear things out"
    =goal>
      isa goal
    -  state game-over
    =game-state>
      status game-over
    ?manual-right>
       state busy
    ?tap>
       state busy
       hand  right
    ?manual>
       preparation free
==>
    =goal>
      state game-over
      step nil
    +manual> 
      isa stop-tapping
    +manual>
     isa release-all-fingers
    )

 (spp detect-trial-end-no-tap :u 10000 :reward t)
 (spp detect-trial-end-tap :u 10000 :reward t)

;;;START production
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
       ?vocal>
            state free
    =game-state>
        game =game 
      ==>
        =goal>
           state do-step
		   shot-tapping no
           next nil
        +retrieval>
           isa operator
           pre =game
     )

;;;update STS payoff for blending
(p detect-balloon-burst-sts-blending
   =goal>
      isa goal
	  nb-bursts =current-count
	  sts 1
   =game-state>
      balloon-burst yes
!safe-eval! (= (mod =current-count 2) 1)
==>
!bind! =new-count (+ =current-count 1)
!bind! =metric 1
	=game-state>
	  balloon-burst no
	=goal>
	  isa outcome
	  strategy shoot-turn-shoot
	  burst =metric
	  payoff 50
	  nb-bursts =new-count
	+goal> =goal
)

(spp detect-balloon-burst-sts-blending :u 10000 :fixed-utility t)

;;;no blending here (blending only happens every couple balloon bursts)
(p detect-balloon-burst-sts-no-blending
   =goal>
      isa goal
	  nb-bursts =current-count
	  sts 1
   =game-state>
      balloon-burst yes
!safe-eval! (not (= (mod =current-count 2) 1))
==>
!bind! =new-count (+ =current-count 1)
!bind! =metric 1
	=game-state>
	  balloon-burst no
	=goal>
	  isa goal
	  nb-bursts =new-count
)

(spp detect-balloon-burst-sts-no-blending :u 10000 :fixed-utility t)

;;;detect balloon burst default -> favor SSS strategy by default
(p detect-balloon-burst-default
   =goal>
      isa goal
	  nb-bursts =current-count
	  sts 0
   =game-state>
      balloon-burst yes
==>
!bind! =new-count (+ =current-count 1)
	=game-state>
	  balloon-burst no
	=goal>
	  isa outcome
	  strategy shoot-shoot-shoot
	  burst 1
	  nb-bursts =new-count
	+goal> =goal

)

(spp detect-balloon-burst-default :u 10000 :fixed-utility t)

;;need multiple types of initial aim

;;; initial-aim fast period with periodic tapping with anticipation of upcoming shots when "ready"
(p initial-aim-fast-period-shoot
    =goal>
       isa goal
       state do-step
	   first-burst yes
	   explore-turn no
       shot-tapping no
       fast-thresh =thresh
       aim =aim
       width =width
    =retrieval>
       action adjust-aim
       fast2 =NEWSTEP
    =game-state>
       angle =angle
    ?manual>
        preparation FREE
    =tracker>
       control-slot time-period
       min =min
       max =max
!safe-eval! (or (>= =angle (+ =AIM  (* 4 =WIDTH))) (<= =angle (- =AIM  (* 4 =WIDTH))))
!eval! (< (/ (+ =min =max) 2) =thresh)
    ==>
    =goal>
       next =NEWSTEP
       state tapping
       step adjusting
	+vocal>
	   cmd subvocalize
	   string "ready"
	+manual>
	   isa delayed-punch
	   hand right
	   finger index
	   delay .09
	+blending>
	   isa outcome
	   burst 1
)

(p initial-aim-fast-period-no-shoot
    =goal>
       isa goal
       state do-step
	   first-burst yes
	   explore-turn no
       shot-tapping no
       fast-thresh =thresh
       aim =aim
       width =width
    =retrieval>
       action adjust-aim
       fast2 =NEWSTEP
    =tracker>
       control-slot time-period
       min =min
       max =max
     =game-state>
       angle =angle
!safe-eval! (and (< =angle (+ =AIM  (* 4 =WIDTH))) (> =angle (- =AIM  (* 4 =WIDTH))))
!eval! (< (/ (+ =min =max) 2) =thresh)
    ==>
    =goal>
       next =NEWSTEP
       state tapping
       step adjusting
	+vocal>
	   cmd subvocalize
	   string "ready"
	+blending>
	   isa outcome
	   burst 1)

;;; initial-aim fast turns to explore good choices of shot tapping periods
(p initial-aim-fast-turns
    =goal>
       isa goal
       state do-step
	   first-burst yes
	   explore-turn yes
       shot-tapping no
       fast-thresh =thresh
    =retrieval>
       action adjust-aim
       fast1 =NEWSTEP
    =tracker>
       control-slot time-period
       min =min
       max =max
!eval! (< (/ (+ =min =max) 2) =thresh)
    ==>
    =goal>
       next =NEWSTEP
       state tapping
       step adjusting
	   turn-step turning
    +temporal>
       isa time  
)

;;; need to finish exploring with slow strategy if first double shot attempt has not been reached yet
(p explore-fast-aim-slow
    =goal>
       isa goal
       state do-step
	   first-burst no
       shot-tapping no
       fast-thresh =thresh
    =retrieval>
       action adjust-aim
       slow =NEWSTEP
    =tracker>
       control-slot time-period
       min =min
       max =max
!eval! (< (/ (+ =min =max) 2) =thresh)
    ==>
    =goal>
       next =NEWSTEP
       state tapping
       step adjusting
)

;;; initial slow mechanism exploration
(p initial-aim-slow
    =goal>
       isa goal
       state do-step
       shot-tapping no
       fast-thresh =thresh
    =retrieval>
       action adjust-aim
       slow =NEWSTEP
    =tracker>
       control-slot time-period
       min =min
       max =max
!eval! (>= (/ (+ =min =max) 2) =thresh)
    ==>
    =goal>
       next =NEWSTEP
       state tapping
       step adjusting
)

;;; ship's aim is aligned to balloon
    (p back-to-aim
       =goal>
          isa goal
          state tapping
		  shot-tapping no
		- turn-step turning
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
		  rotation OK
		  burst 0
        +retrieval>
           isa operator
           pre =NEWSTEP
)

(spp back-to-aim :reward t)

;;; detect a deflation when there is no periodic tapping
(p detect-deflate-no-tap
    =goal>
	   isa goal
       coef =coef
    =temporal>
       ticks =ticks
    =game-state>
       deflate new
    ?manual-right>
          state free
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

(spp detect-deflate-no-tap :u 20)

;;; detect a reset when there is no periodic tapping
(p detect-reset-no-tap
    =goal>
	   isa goal
       time-period =timing
    =game-state>
       reset new
    ?manual-right>
       state free
    =tracker>
       control-slot time-period
       min =min
       max =max
!eval! (lower-bounds =timing =min =max)
==>
!bind! =val (+ =min (/ (- (lower-bounds =timing =min =max) =min) 5))
=game-state>
    reset attended
*tracker>
    min =val)

(spp detect-reset-no-tap :u 20)

(p tapping-d
   =goal>
      isa goal
      state tapping-key
   =RETRIEVAL>
       key d
     ==>
   =goal>
     state tapping
   =RETRIEVAL>
   +MANUAL>
       CMD DELAYED-PUNCH
       FINGER index
       HAND LEFT
       delay .12)

(p tapping-a
   =goal>
      isa goal
      state tapping-key
   =RETRIEVAL>
       key a
     ==>
   =goal>
     state tapping
   =RETRIEVAL>
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
          shot-tapping no
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
     step refining
	 turn-step wait-turn)

(p adjust-back-clockwise-again
       =goal>
          isa goal
          state tapping
          step refining
          shot-tapping no
        - next nil
          aim =aim
          width =width
	 =RETRIEVAL>
		key d
     =game-state>
          angle =angle
       ?MANUAL>
          state FREE
!SAFE-EVAL! (>= =ANGLE (+ =AIM  =WIDTH))
    ==>
   =RETRIEVAL>
   =goal>
     state tapping-key
	 turn-step reset-timer
)

(p adjust-back-clockwise-switch
       =goal>
          isa goal
          state tapping
          step refining
          shot-tapping no
        - next nil
          aim =aim
          width =width
	 =RETRIEVAL>
		key a
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
	 turn-step reset-timer
)


    (p adjust-back-counterclockwise
       =goal>
          isa goal
          state tapping
          step adjusting
          aim =aim
          width =width
          shot-tapping no
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
     step refining
	 turn-step wait-turn)

    (p adjust-back-counterclockwise-again
       =goal>
          isa goal
          state tapping
          step refining
          aim =aim
          width =width
          shot-tapping no
        - next nil
	 =RETRIEVAL>
		key a
     =game-state>
          angle =angle
    ?MANUAL-LEFT>
          state free
!SAFE-EVAL! (<= =ANGLE  (- =AIM (* 2 =WIDTH)))
      ==>
   =RETRIEVAL>
   =goal>
     state tapping-key
	 turn-step reset-timer)

	 (p adjust-back-counterclockwise-switch
       =goal>
          isa goal
          state tapping
          step refining
          shot-tapping no
        - next nil
          aim =aim
          width =width
	 =RETRIEVAL>
		key d
     =game-state>
          angle =angle
       ?MANUAL>
          state FREE
!SAFE-EVAL! (<= =ANGLE  (- =AIM (* 2 =WIDTH)))
    ==>
   +retrieval>
      isa mapping
      function counterclockwise
   =goal>
     state tapping-key
	 turn-step reset-timer
)

;;; double shot productions
(p press-double-l
   =goal>
      isa goal
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
    +temporal>
       isa time 
     +manual>
       isa delayed-punch
        hand right
       finger index
        delay .09)

(spp press-double-l :reward 50)

(p double-shot-lock
   =goal>
      isa goal
      state double-l
	  nb-bursts =current-count
   ?MANUAL>
       processor FREE
   =temporal>
       ticks =ticks
!safe-eval! (<= =ticks (max 5 (- 18 (floor (/ =current-count 5)))))
     ==>
     +manual>
       isa delayed-punch
      hand right
      finger index
       delay .09)

(p double-shot-new-cycle
   =goal>
      isa goal
      state double-l
	  nb-bursts =current-count
   ?MANUAL>
       processor FREE
   =temporal>
       ticks =ticks
!safe-eval! (> =ticks (max 5 (- 18 (floor (/ =current-count 5)))))
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
		first-burst yes
        step nil)


;;Fast turn estimate productions -> figure out how long it takes to execute turns

(p need-turn-again
   =goal>
      isa goal
      state do-step
	  shot-tapping no
	  time-period =this-period
	  turn-step wait-turn
	  turn-time-min =min
	  turn-time-max =max
	  count-track =count
      width =width
      aim =aim
   =temporal>
       ticks =ticks
    ?MANUAL-RIGHT>
       state free
    ?manual>
       preparation free
   =retrieval>
      action aim-estimate
      fail =NEWSTEP
!safe-eval! (and (< =count 10) (> =count 0))
    ==>
!bind! =new-count (+ =count 1)
!bind! =new-min (min =min =ticks)
!bind! =new-max (max =max =ticks)
!bind! =period (get-period =this-period tick-ht)
   =goal>
      state do-step
	  shot-tapping explore
	  turn-step turning
	  turn-time-min =new-min
	  turn-time-max =new-max
	  count-track =new-count
	  next =NEWSTEP
    +temporal>
       isa time
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period	   
)

(p stop-shots-for-turn
   =goal>
      isa goal
      state do-step
	  shot-tapping explore
      width =width
      aim =aim
   =game-state>
      angle =angle
     ?tap>
       state busy
       hand right
    ?manual> 
      preparation free
!safe-eval! (>= =angle (+ =aim =width))
    ==>
   =goal>
      state tapping
	  step adjusting
	  shot-tapping no
   +manual> 
     isa stop-tapping	
)

(p need-turn-again-first-count
   =goal>
      isa goal
      state do-step
	  shot-tapping no
	  time-period =this-period
	  turn-step wait-turn
	  count-track =count
      width =width
      aim =aim
   =temporal>
       ticks =ticks
    ?MANUAL-RIGHT>
       state free
    ?manual>
       preparation free
   =retrieval>
      action aim-estimate
      fail =NEWSTEP
!safe-eval! (= =count 0)
    ==>
!bind! =new-count (+ =count 1)
!bind! =new-min =ticks
!bind! =new-max =ticks
!bind! =period (get-period =this-period tick-ht)
   =goal>
      state do-step
	  shot-tapping explore
	  turn-step turning
	  turn-time-min =new-min
	  turn-time-max =new-max
	  count-track =new-count
	  next =NEWSTEP
    +temporal>
       isa time  
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period	
)

(p need-turn-again-no-timer
   =goal>
      isa goal
      state do-step
	  shot-tapping no
	  time-period =this-period
	  turn-step reset-timer
	  count-track =count
      width =width
      aim =aim
   =temporal>
    ?MANUAL-RIGHT>
       state free
    ?manual>
       preparation free
   =retrieval>
      action aim-estimate
      fail =NEWSTEP
!safe-eval! (< =count 10)
    ==>
!bind! =period (get-period =this-period tick-ht)
   =goal>
      state do-step
	  turn-step turning
	  shot-tapping explore
	  next =NEWSTEP
    +temporal>
       isa time
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period	   
)

(p last-turn-first-speed
   =goal>
      isa goal
      state do-step
	  speed 1
	  shot-tapping no
	  turn-step wait-turn
	  count-track =count
	  turn-time-min =min
	  turn-time-max =max
      width =width
      aim =aim
   =game-state>
      angle =angle
   =retrieval>
      action aim-estimate
      success =NEWSTEP
!safe-eval! (>= =count 10)
    ==>
!bind! =new-min (- =min 1)
!bind! =new-max (+ =max 1)
   =goal>
      isa goal
      state tapping
	  step adjusting
	  turn-step turning
	  explore-turn no
	  count-track 0
	  next =NEWSTEP
	+tracker>
         control-slot turn-tap-time
         good-slot hit
         bad-slot miss
         min =new-min
         max =new-max
         bad-weight -1
         name 1
    +temporal>
       isa time
	+blending>
	   isa outcome
	   burst 1
)

;;Fast FFF productions: detecting resets when periodic tapping is turned on
(p detect-reset-tracker-tap
    =goal>
	   isa goal
       shot-tapping yes
	 - state processing
    =tap>
       period =cur-period
    =game-state>
       reset new
       game =game
    ?manual-right>
       state busy
    ?tap>
       state busy
       hand  right
    =tracker>
       control-slot turn-tap-time
       min =min
       max =max
!eval! (lower-bounds (get-tick =cur-period) =min =max)
==>
!bind! =val (+ =min (/ (- (lower-bounds (get-tick =cur-period) =min =max) =min) 3.5))
=game-state>
    reset attended
   =goal>
      shot-tapping no
      state do-step
   +retrieval>
      isa operator
      pre =game
   +manual> 
     isa stop-tapping
*tracker>
    min =val)

(spp detect-reset-tracker-tap :u 20)

;;;back-up production for deflations, not used a lot in practice
(p detect-deflate-tracker-tap
    =goal>
	   isa goal
       shot-tapping yes
	 - state processing
	=temporal>
       ticks =ticks
    =game-state>
       deflate new
     =tracker>
         control-slot turn-tap-time
         min =min
         max =max
!eval!  (upper-bounds =ticks =min =max)
!eval!  (>= =ticks *lower*)
==>
!bind! =val  (- =max (/ (- =max (upper-bounds =ticks =min =max)) 2))
   =game-state>
       deflate attended
 *tracker>
      max =val
)

(spp detect-deflate-tracker-tap :u 20)

;;; misses are used to adjust timing of shots based on ship's angle
(p detect-miss-clockwise
    =goal>
	   isa goal
       shot-tapping yes
	 - state processing
       width =width
	   turn-no-hit-count =no-hit-count
	   hit-count-thresh =count-thresh
    =game-state>
	   angle =angle
	   game =game
     ?tap>
       state busy
       hand right
       correcting free
       tapped t
     ?manual>
       preparation free
     =tracker>
         control-slot turn-tap-time
         min =min
         max =max
!SAFE-EVAL!  (> =ANGLE =width)
!SAFE-EVAL!  (> =no-hit-count =count-thresh)
==>
!bind! =val (- =max (* 0.25 (- =max =min)))
   =goal>
       shot-tapping no
       state do-step
	   turn-no-hit-count 0
	*tracker>
		max =val
   +retrieval>
       isa operator
       pre =game
   +manual> 
     isa stop-tapping
   +temporal> 
     isa clear
)

(spp detect-miss-clockwise :u 30)

(p detect-miss-counterclockwise
    =goal>
	   isa goal
       shot-tapping yes
	 - state processing
       width =width
	   turn-no-hit-count =no-hit-count
	   hit-count-thresh =count-thresh
    =game-state>
	   angle =angle
	   game =game
     ?tap>
       state busy
       hand right
       correcting free
       tapped t
     ?manual>
       preparation free
     =tracker>
         control-slot turn-tap-time
         min =min
         max =max
!SAFE-EVAL!  (< =ANGLE  (- 0 (* 1.5 =width)))
!SAFE-EVAL!  (> =no-hit-count =count-thresh)
==>
!bind! =val (+ =min (* 0.1 (- =max =min)))
   =goal>
       shot-tapping no
       state do-step
	   turn-no-hit-count 0
	*tracker>
		min =val
   +retrieval>
       isa operator
       pre =game
   +manual> 
     isa stop-tapping
   +temporal> 
     isa clear
)

(spp detect-miss-counterclockwise :u 30)

(p check-aim-tap
   =goal>
          isa goal
          state tapping
          aim =aim
          width =width
          shot-tapping yes
          next =next
    =game-state>
       angle =ANGLE
!safe-eval!   (and (< =angle (+ =AIM  (* 2 =WIDTH))) (> =angle (- =AIM (* 2 =WIDTH))))
     ==>
        +retrieval>
           isa operator
           pre =next
   =goal>
      state retrieve-vulnerability)

;;; first outcome after blending: STS
  (p start-shoot-turn-shoot
   =goal>
      isa goal
      state do-step
	  turn-tap-time =this-period
	  nb-bursts =current-count
   =game-state>
      vulnerability =val
   =RETRIEVAL>
       ACTION start-period
       success =newstep
   =blending>
      isa outcome
	  payoff =payoff
    ?MANUAL-RIGHT>
       state free
    ?manual>
       preparation free
    =tracker>
       control-slot turn-tap-time
       min =min
       max =max
!safe-eval! (>= (+ =payoff (* 500 (decreasing-noise-with-bursts =current-count))) 0)
     ==>
!bind! =period (get-period =this-period tick-ht)
     =goal>
       shot-tapping yes
	   vln-track =val
       count-track 0
	   sts 1
	 @blending>
     +temporal> 
       isa time
     +RETRIEVAL>
       PRE =NEWSTEP
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period)

  (p start-shot-tap-tracker
   =goal>
      isa goal
      state do-step
	  turn-tap-time =this-period
   =game-state>
      vulnerability =val
   =RETRIEVAL>
       ACTION start-period
       success =newstep
   ?blending>
      state error
    ?MANUAL-RIGHT>
       state free
    ?manual>
       preparation free
    =tracker>
       control-slot turn-tap-time
       min =min
       max =max
     ==>
!bind! =period (get-period =this-period tick-ht)
!bind! =to-do 1
     =goal>
       shot-tapping yes
	   vln-track =val
       count-track 0
	   sts =to-do
     +temporal> 
       isa time
     +RETRIEVAL>
       PRE =NEWSTEP
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period)
	   
	(spp start-shot-tap-tracker :u 9.0)

;;; Second outcome after blending: SSS -> "freeze" and execute series of shots  in a row
  (p start-shoot-shoot-shoot
   =goal>
      isa goal
      state do-step
	  time-period =this-period
	  sss-count =shot-count
	  nb-bursts =current-count
   =RETRIEVAL>
       ACTION start-period
       fail =next
    =blending>
      isa outcome
	  payoff =payoff
    ?MANUAL-RIGHT>
       state free
    ?manual>
       preparation free
!safe-eval! (< (+ =payoff (* 500 (decreasing-noise-with-bursts =current-count))) 0)
     ==>
!bind! =period (get-period =this-period tick-ht)
!bind! =new-shot (+ =shot-count (/ 1 4))
     =goal>
       shot-tapping freeze
	   state tapping
       count-track 0
	   next =next
	   sss-count =new-shot
	   sts 0
	 @blending>
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period)
	   
  (p start-shot-tap-freeze
   =goal>
      isa goal
      state do-step
	  time-period =this-period
   =RETRIEVAL>
       ACTION start-period
       fail =next
   ?blending>
      state error
    ?MANUAL-RIGHT>
       state free
    ?manual>
       preparation free
     ==>
!bind! =period (get-period =this-period tick-ht)
!bind! =to-do 0
     =goal>
       shot-tapping freeze
	   state tapping
       count-track 0
	   next =next
	   sts =to-do
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period)
	
	(spp start-shot-tap-freeze :u 9.0)
	
	   
	(p turn-right-freeze
	 =goal>
	   isa goal
	   state tapping
	   shot-tapping freeze
	   count-track =this-count
    ?MANUAL-RIGHT>
      state busy
    ?tap>
       state busy
       hand  right
    =tap>
      count =count
!safe-eval!  (> =count =this-count)
      ==>
!bind! =new-count (+ =this-count 4)  
    +retrieval>
      isa mapping
      function clockwise
    =goal>
     state tapping-key
	 step refining
	 turn-step wait-turn
	 count-track =new-count
	)
	   
	(p end-shot-tap-freeze-fail
       =goal>
          isa goal
          state tapping
          shot-tapping freeze
		  aim-thresh =threshold
		  sss-count =this-count
		  width =width
		  aim =aim
		  tap-freeze-count =count-min
		  next =next
     =game-state>
        angle =angle
    ?MANUAL-RIGHT>
      state busy
    ?tap>
       state busy
       hand  right
  =tap>
      count =count
!safe-eval! (< =this-count 1.0)
!eval!  (> =count =count-min)
      ==>
   =goal>
      shot-tapping no
      state do-step
	  count-track 0
   +retrieval>
      isa operator
      pre =next
   +manual> 
     isa stop-tapping
)

	(p end-shot-tap-freeze-success
       =goal>
          isa goal
          state tapping
          shot-tapping freeze
		  aim-thresh =threshold
		  sss-count =this-count
		  nb-bursts =current-count
		  width =width
		  aim =aim
		  tap-freeze-count =count-min
		  next =next
     =game-state>
        angle =angle
    ?MANUAL-RIGHT>
      state busy
    ?tap>
       state busy
       hand  right
  =tap>
      count =count
!safe-eval! (>= =this-count 1.0)
!eval!  (> =count =count-min)
      ==>
!bind! =new-payoff (+ -10 (floor (/ =current-count 2)))
   =goal>
      isa outcome
      shot-tapping no
	  count-track 0
      state do-step
	  sss-count 0
	  payoff =new-payoff
	  strategy shoot-shoot-shoot
	  burst 1
   +goal> =goal
   +retrieval>
      isa operator
      pre =next
   +manual> 
     isa stop-tapping
)

;;;turning productions for fast periodic tapping, depend on periodic "tap" buffer
    (p turn-clockwise-hit
       =goal>
          isa goal
          state do-step
          shot-tapping yes
		  vln-track =new-vln
          count-track =curcount
    =retrieval>
      isa operator
      action check-aim
      post =next
     =game-state>
          vulnerability =val
    ?MANUAL-LEFT>
      state free
    ?MANUAL-RIGHT>
      state busy
    ?tap>
       state busy
       hand  right
  =tap>
      count =count
!eval!  (> =count =curcount)
!eval!  (= =new-vln =val)
      ==>
!bind! =vln-up (+ =val 1)
   +retrieval>
      isa mapping
      function clockwise
   =goal>
     state tapping-key
	 turn-no-hit-count 0
     count-track =count
	 vln-track =vln-up
     next =next
   =tap>
   +temporal>
     isa time
)

    (p turn-clockwise-no-hit
       =goal>
          isa goal
          state do-step
          shot-tapping yes
		  vln-track =new-vln
          count-track =curcount
		  turn-no-hit-count =no-hit-count
    =retrieval>
      isa operator
      action check-aim
      post =next
     =game-state>
      vulnerability =val
    ?MANUAL-LEFT>
      state free
    ?MANUAL-RIGHT>
      state busy
    ?tap>
       state busy
       hand  right
  =tap>
      count =count
!eval!  (> =count =curcount)
!eval!  (or (not (= =new-vln =val)) (= =val 0))
      ==>
!bind! =vln-up (+ =val 1)
!bind! =new-count (+ =no-hit-count 1)
   +retrieval>
      isa mapping
      function clockwise
   =goal>
     state tapping-key
	 turn-no-hit-count =new-count
     count-track =count
	 vln-track =vln-up
     next =next
   =tap>
)

;;;feedback is processed, need to stop tapping
(p detect-feedback-stop-shot-taps
   =goal>
      isa goal
      state processing
   =game-state>
	  game =game
    =temporal>
	   ticks =ticks
    ?manual-right>
       state busy
    ?tap>
       state busy
       hand  right
	?manual>
       preparation free
!eval!  (> =ticks 22)
==> 
   =goal>
      state do-step
	  shot-tapping no
   +retrieval>
      isa operator
      pre =game
   +manual> 
     isa stop-tapping
)

;;; vulnerability is high enough, can execute double shot	  
(p test-dimension-success
   =goal>
      isa goal
      state retrieve-vulnerability
      shot-tapping yes
      =dimension =thresh
    =retrieval>
      isa operator
      action test-dimension
      arg1 =dimension
      success =newstep
    =game-state>
        =dimension =val     
   !SAFE-EVAL! (> =val =thresh)
     ==>
   =goal>
      state do-step
      shot-tapping no
    +retrieval>
        isa operator
        pre =newstep)

(p stopping-period
   =goal>
      isa goal
      state do-step
      shot-tapping no
    =retrieval>
      isa operator
      action stop-period
      post =newstep
     ?tap>
       state busy
       hand right
    ?manual> 
      preparation free 
   ==>
     +manual> 
       isa stop-tapping
     +temporal> 
       isa clear
     +retrieval>
        isa operator
        pre =newstep

)

;;; vulnerability is not high enough, keep tapping
(p test-dimension-fail
   =goal>
      isa goal
      state retrieve-vulnerability
      shot-tapping yes
      =dimension =thresh
    =retrieval>
      isa operator
      action test-dimension
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

;;;aim is way off, need to adjust aim
  (p stop-shot-tap-clockwise
     =goal>
	   isa goal
     - shot-tapping no
	 - shot-tapping freeze
	 - state processing
       game =game
       aim =AIM
       width =WIDTH
     =game-state>
          angle =angle
!SAFE-EVAL! (>= =ANGLE (+ =AIM (* 5 =WIDTH)))
     ?tap>
       state busy
       hand right
    ?manual> 
      preparation free 
     ==> 
   =goal>
	  count-track 0
      state processing
	  turn-step attended
     +temporal> 
       isa time)

  (p stop-shot-tap-counterclockwise
     =goal>
	   isa goal
     - shot-tapping no
	 - shot-tapping freeze
	 - state processing
       game =game
       aim =AIM
       width =WIDTH
     =game-state>
          angle =angle
!SAFE-EVAL! (<= =ANGLE  (- =AIM (* 5 =WIDTH)))
     ?tap>
       state busy
       hand right
    ?manual> 
      preparation free  
     ==> 
   =goal>
	  count-track 0
      state processing
	  turn-step attended
     +temporal> 
       isa time)

  (p readjust-counterclockwise-no-tap
     =goal>
	   isa goal
       shot-tapping no
	   rotation OK
	 - turn-step attended
       game =game
       aim =AIM
       width =WIDTH
	   fast-thresh =thresh
     =game-state>
          angle =angle
    =tracker>
       control-slot time-period
       min =min
       max =max
!eval! (< (/ (+ =min =max) 2) =thresh)
!SAFE-EVAL! (<= =ANGLE  (- =AIM (* 6 =WIDTH)))
     ?manual> 
       preparation free 
     ==> 
   =goal>
	  count-track 0
      state do-step
	  rotation attended
   +retrieval>
      isa operator
      pre =game
     +temporal> 
       isa clear)	

  (p readjust-clockwise-no-tap
     =goal>
	   isa goal
       shot-tapping no
	   rotation OK
	 - turn-step attended
       game =game
       aim =AIM
       width =WIDTH
	   fast-thresh =thresh
     =game-state>
          angle =angle
    =tracker>
       control-slot time-period
       min =min
       max =max
!eval! (< (/ (+ =min =max) 2) =thresh)
!SAFE-EVAL! (>= =ANGLE (+ =AIM (* 8 =WIDTH)))
     ?manual> 
       preparation free 
     ==> 
   =goal>
	  count-track 0
      state do-step
	  rotation attended
   +retrieval>
      isa operator
      pre =game
     +temporal> 
       isa clear)	   

;;;Productions for slow speeds

(p test-dimension-success-slow
   =goal>
      isa goal
      state retrieve-vulnerability
	  shot-tapping no
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

(p test-dimension-fail-slow
   =goal>
      isa goal
      state retrieve-vulnerability
	  shot-tapping no
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
      isa goal
      state do-step
	  shot-tapping no
   =RETRIEVAL>
       ACTION check-delay
       success =newstep      
   ?TEMPORAL>
       buffer empty
   =GAME-STATE>
      fortress-alive yes
     ==>
   =goal>
      state retrieve-vulnerability
  +RETRIEVAL>
       PRE =NEWSTEP)

(p delay-not-ok
   =goal>
      isa goal
      state do-step
	  shot-tapping no
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

(p delay-ok-slow-time
   =goal>
      isa goal
      state do-step
	  shot-tapping no
      time-period =tick-thresh
   =RETRIEVAL>
       ACTION check-delay
       success =newstep      
   =TEMPORAL>
       TICKS =TICKS
   =GAME-STATE>
       ANGLE =ANGLE
!safe-eval! (> =tick-thresh 12)
!safe-eval! (>= =ticks =tick-thresh)
     ==>
   =goal>
      state retrieve-vulnerability
      step nil
  +RETRIEVAL>
       PRE =NEWSTEP)
	   
(p delay-ok-fast-time
   =goal>
      isa goal
      state do-step
	  shot-tapping no
      time-period =tick-thresh
   =RETRIEVAL>
       ACTION check-delay
       fast1 =newstep      
   =TEMPORAL>
       TICKS =TICKS
   =GAME-STATE>
       ANGLE =ANGLE
!safe-eval! (<= =tick-thresh 12)
!safe-eval! (>= =ticks =tick-thresh)
     ==>
!bind! =period (get-period =tick-thresh tick-ht)
   =goal>
      state explore-tap
      shot-tapping tap-explore
      count-track 0
      next =NEWSTEP
     +manual> 
       isa periodic-tap
       hand right
       finger index 
       period =period)
	   
(p fast-time-end
   =goal>
      isa goal
      state explore-tap
      shot-tapping tap-explore
      tap-freeze-count =count-min
      next =new-step
      aim =AIM
      width =WIDTH
     =game-state>
          angle =angle
     ?tap>
       state busy
       hand right
    ?manual> 
      preparation free
	  =tap>
      count =count
!SAFE-EVAL! (or (<= =ANGLE  (- =AIM (* 5 =WIDTH))) (>= =ANGLE (+ =AIM (* 5 =WIDTH))) (> =count =count-min))	  
     ==> 
   =goal>
	  count-track 0
      state do-step
	  shot-tapping no
      step nil
	  turn-step attended
   +manual> 
     isa stop-tapping
   +retrieval>
      isa operator
      pre =new-step
   +temporal> 
       isa clear
)

(p press-l
   =goal>
      isa goal
      state do-step
	  shot-tapping no
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

(p short-new-aim
   =goal>
      isa goal
      state do-step
	  shot-tapping no
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
      isa goal
      state do-step
	  shot-tapping no
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
      isa goal
      state do-step
	  shot-tapping no
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
	   isa goal
       game =game
       aim =AIM
       width =WIDTH
	   shot-tapping no
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
	   isa goal
       game =game
       aim =AIM
       width =WIDTH
	   shot-tapping no
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





