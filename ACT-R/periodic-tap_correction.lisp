;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2020 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : periodic-tap_correction.lisp
;;; Version     : 1.0a12
;;; 
;;; Description : Add a motor action that repeatedly punches a finger.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2020.04.10 Dan [1.0a1]
;;;             : * Initial creation.
;;; 2020.04.14 Dan [1.0a2]
;;;             : * Adjusted the timing on the release to take out the extra
;;;             :   burst cost since a punch action doesn't have that extra
;;;             :   cost for an immediate down and up.
;;;             : * Updated start-tap and finish-tap to match the current 
;;;             :   finger-loc usage.
;;;             : * Fixed a bug in finish-tap because it didn't convert the 
;;;             :   init time to ms.
;;; 2020.05.21 Pierre [1.0a3]
;;;             : *Created an alternative motor noise function - motor-time-noise
;;;             :  which utilizes the noisecalc function and the tap-module-noise
;;;             :  parameter
;;; 2020.06.04 Pierre [1.0a4]
;;;             :  :key-release-time changed to :key-tap-release
;;;             :  Default value set to 0.14
;;; 2020.06.11 Pierre [1.0a5]
;;;             :  Creation of tap module request mechanism
;;; 2020.08.13 Pierre & Dan [1.0a7]
;;;             :  noisecalc adjustment
;;;             :  "handle-tap-request" schedule-event-now modification
;;;             :  add "key-release" in parameters of defstruct tap-module
;;;             :  chunk-type fix
;;;             :  key-release-time parameter added
;;;             :  change key-release time in functions
;;;             :  remove update-tap-chunk, remove mod-buffer-request
;;; 2021.03.11 Dan [1.0a8]
;;;             : * Changed the module name to tap and made all of the 
;;;             :   events consistent with respect to that since some were
;;;             :   using periodic-tap and some were using tap.
;;;             : * Convert the stop action into a real style so it has the
;;;             :   preparation and initiation costs with 0 execution cost.
;;;             : * Make the tap corrections a new action that inherits from
;;;             :   the tap action and is basically a combination of a stop and
;;;             :   a subsequent new tap.
;;;             : * Changed the module's lock to be recursive so that the
;;;             :   motor-time-noise function can be called safely when called
;;;             :   regardless of whether the lock is currently held or not.
;;;             : * Removed the :tap-count parameter since it doesn't seem like
;;;             :   something that would be set in sgp and it's value doesn't
;;;             :   appear to be used in a way that would make that useful.
;;;             : * Commented out some parameters that aren't currently being
;;;             :   used.  If they're added back in they need to also return the
;;;             :   correct value from the param function as well as set it.
;;;             : * Took some unnecessary tests out of motor-time-noise since
;;;             :   the parameter setting already handles a default value and
;;;             :   forces it to be a number.
;;;             : * Seems like the tap buffer shouldn't get strict harvested
;;;             :   so added a reset function to handle that.
;;;             : * Set the tap count to 0 when creating a new one instead of
;;;             :   when stopping an ongoing one.
;;;             : * Made the duration the key is held down randomized with the
;;;             :   normal motor noise.
;;;             : * Record the time of the last down action to make it easier
;;;             :   to schedule the next one (insted of having to do the math
;;;             :   from the components of the action).
;;;             : * Adjusted the "point of no return" for a stop to also include
;;;             :   the exec time since that's included in the next-down time.
;;;             : * To be safe, don't assume the name of the motor module since
;;;             :   the extended module might not have the same name.
;;;             : * Changed the name of the noise parameter from :noise-motor-time
;;;             :   to :tap-motor-noise, and allow it to be set to nil so that
;;;             :   one can have a noiseless period (for testing/debugging 
;;;             :   purposes).
;;;             : * Fixed the calculation for how long to hold the key down 
;;;             :   because it should have been the max of the :key-tap-release
;;;             :   setting and the keyboard's release time :key-release-time
;;;             :   and it needs to have the initial press delay included so 
;;;             :   that one gets a time that matches :key-tap-release.
;;;             : * Have the state query of the tap buffer return t or nil to
;;;             :   indicate whether there is a finger tapping or not, and
;;;             :   provide hand and finger queries as well that will return t
;;;             :   if the indicated hand or finger is currently involved in
;;;             :   a tapping action, and use the tap-module hand and finger
;;;             :   slots to hold that info for simplicity.
;;; 2021.03.29 Dan [1.0a8]
;;;             : * Use the tap-module-correction-time in scheduling the update
;;;             :   since there is no execution time.
;;; 2021.04.07 Pierre [1.0a8]
;;;             : * Switch motor-time-noise to a percentage instead of a fixed time
;;;             : * [Dan] The default parameter values were also changed for the
;;;             :   :key-tap-release and :tap-correction-time parameters.
;;; 2021.04.16 Dan [1.0a10]
;;;             : * The 2021.03.29 updated had actually updated to a9...
;;;             : * Change the tap correction so that it will directly change
;;;             :   the timing on the next motor action when the update happens
;;;             :   since previously there was a lag since the tap didn't 
;;;             :   process the update until after the next press.
;;;             :   Recomputes the noise for the next tap based on the new period,
;;;             :   but might want to reconsider that since at least some of the
;;;             :   period has already passed.
;;;             : * Commented out the printing in the motor-time-noise function,
;;;             :   and removed the unneeded progn since a let block is an
;;;             :   implicit progn.
;;;             : * Added a safety check in finish-tap to avoid trying to
;;;             :   schedule an event in the past.
;;; 2021.04.19 Dan [1.0a11]
;;;             : * Added a correcting query that's busy during a correction,
;;;             :   and tapped which is t after the first tap of a tap request
;;;             :   or after a correction has started.
;;; 2021.04.21 Dan [1.0a12]
;;;             : * If the correction is delayed because of pending execution
;;;             :   it needs to cause an implicit stop after the next keyup to
;;;             :   avoid a problem with short periods/long press durations 
;;;             :   that could keep motor constantly "busy" (next down is 
;;;             :   happening within the init time of the previous up) and 
;;;             :   prevent the correction from every getting a chance to 
;;;             :   execute.
;;; 2021.04.30 Dan 
;;;             : * When a correction hits the unexpected state likely triggered
;;;             :   by a stop make sure to clear tap-module-pending-correction
;;;             :   otherwise it could get stuck.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; Add three new motor module requests.  One which starts a motor action to 
;;; repeatedly punch a finger at a specific interval, one that stops it, and
;;; one that will adjust the period for an ongoing one.
;;;
;;; Only one such tapping action can be ongoing at a time even though it 
;;; requires the dual execution stages of the motor module.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; New parameter:
;;;
;;;  :tap-period    positive number, with a default of .5
;;;
;;;    Specifies the time in seconds between the finger taps if a period
;;;    is not provided in the request
;;;
;;;  :tap-correction-time  positive number, with a default of .25
;;;
;;;    Time in seconds for the execution of a tap-correction motor action
;;;
;;;  :key-tap-release     positive number, default of .14
;;;    
;;;    Time in seconds for the tap actions to hold down the key
;;;
;;; Manual requests: 
;;; 
;;; isa periodic-tap
;;;    hand [left | right]
;;;    finger [index | middle | ring | pinkie | thumb]
;;;    {period <time in seconds between taps>}
;;;
;;;  Repeatedly schedules finger down and up actions with period time between
;;;  the down actions them until it is stopped.  The time between the down and
;;;  up is determined based on the :key-tap-release parameter
;;;  
;;; isa stop-tapping
;;;
;;;  Stops an ongoing periodic-tap action.  If the next tap is going to happen
;;;  within the default motor init-time then it won't stop that one from 
;;;  occurring (it has passed the point of no return)
;;;
;;; isa period-correction
;;;    correction <time in seconds to adjust current period>
;;;
;;;  Changes the period for the currently ongoing periodic-tap action
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; Requires using the extra extended motor actions and checking to make sure 
;;; that there're multiple execution stages so it doesn't lockout the whole 
;;; motor module when one starts.
;;;
;;; Fitting this into a style by overriding the finish-event scheduling, and
;;; using the extended motor action punch as the basis for the style.
;;; 
;;; Adding a new module to hold the control information so that it's safe for
;;; use with multiple models and provides an easy way to grab all the other 
;;; parameter values needed without having to check them everytime.  
;;; Also include appropriate locking so that it's truly safe for use (no 
;;; possible threading issues with parameter settings).
;;;
;;; The time between actions are currently randomized with motor-time-noise
;;; which generates a normal distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Check that it's the appripriate ACT-R

(written-for-act-r-version "7.17")

;;; Make sure that extended motor actions are loaded.
(require-extra "extended-motor-actions")

;;; Create a structure for the module to hold the info

(defstruct tap-module (lock (bt:make-recursive-lock)) dual-enabled default-period default-correction rate
  current-period count hand finger press release burst init next-down next-up stop noise mvmt key-release
  correction-time last-down-time pending-correction busy tap-start)

;;; The necessary module interface commands

(defun create-tap-module (name)
  (declare (ignore name))
  (make-tap-module))

(defun reset-tap-module (module)
  (bt:with-recursive-lock-held ((tap-module-lock module))
    (chunk-type tap count period)
    (setf (tap-module-next-down module) nil)
    (setf (tap-module-next-up module) nil)
    (setf (tap-module-stop module) nil)
    (setf (tap-module-pending-correction module) nil)
    (setf (tap-module-last-down-time module) nil)
    (setf (tap-module-busy module) nil)
    (setf (tap-module-hand module) nil)
    (setf (tap-module-finger module) nil)
    (setf (tap-module-tap-start module) nil)))

(defun secondary-reset-tap-module (module)
  (declare (ignore module))
  (sgp :do-not-harvest tap))


#| Not used because a period correction request goes to manual now

;;function to make a request to tap module
(defun tap-request (module buffer request)
  (declare (ignore buffer))
  (bt:with-recursive-lock-held ((tap-module-lock module))  
    (let ((new-per (verify-single-explicit-value request 'correction 'tap 'correction-request)))
      (if (not (numberp new-per))
          (print-warning "Invalid period correction request made to the tap module.")
          (progn
            (incf (period (tap-module-mvmt module)) new-per)
            (let ((count (tap-module-count module))
                  (updated-period (period (tap-module-mvmt module))))
              (schedule-event-now 'create-new-buffer-chunk :module 'tap
                                  :priority -100 :params (list 'tap (list 'period updated-period 'count count))
                                  :details "NEW TAP BUFFER CHUNK"))
            )))))

|#

(defun tap-query (tap buffer-name slot value)
  (bt:with-recursive-lock-held ((tap-module-lock tap))
    (let ((busy (tap-module-busy tap)))
      (case slot
        (state
         (case value
           (busy busy)
           (free (not busy))
           (error nil)
           (t 
            (print-warning "Unknown query state ~s to ~s buffer" value buffer-name))))
        (correcting
         (case value
           (free (null (tap-module-pending-correction tap)))
           (busy (tap-module-pending-correction tap))
           (t
            (print-warning "Unknown query correcting ~s to ~s buffer" value buffer-name))))
        (tapped
         (if (tornil value)
             (if value
                 (and busy 
                      (numberp (tap-module-tap-start tap))
                      (numberp (tap-module-last-down-time tap))
                      (> (tap-module-last-down-time tap) (tap-module-tap-start tap))
                      t)
               (and busy 
                    (not 
                     (and 
                      (numberp (tap-module-tap-start tap))
                      (numberp (tap-module-last-down-time tap))
                      (> (tap-module-last-down-time tap) (tap-module-tap-start tap))))
                    t))
           (print-warning "Unknown query tapped ~s to ~s buffer" value buffer-name)))
        (hand
         (case value
           (left (and busy (eq (tap-module-hand tap) 'left) t))
           (right (and busy (eq (tap-module-hand tap) 'right) t))
           (t 
            (print-warning "Unknown query hand ~s to ~s buffer" value buffer-name))))
        (finger
         (case value
           (index (and busy (eq (tap-module-finger tap) 'index) t))
           (middle (and busy (eq (tap-module-finger tap) 'middle) t))
           (ring (and busy (eq (tap-module-finger tap) 'ring) t))
           (pinkie (and busy (eq (tap-module-finger tap) 'pinkie) t))
           (thumb (and busy (eq (tap-module-finger tap) 'thumb) t))
           (t 
            (print-warning "Unknown query finger ~s to ~s buffer" value buffer-name))))
        (t 
         (print-warning "Unknown query ~s ~s to the ~s buffer" slot value buffer-name))))))

(defun print-tap-buffer-status ()
  (let ((tap (get-module tap)))
    (bt:with-recursive-lock-held ((tap-module-lock tap))
      (let ((busy (tap-module-busy tap))) 
        (with-output-to-string (s)
          (format s "  hand                  : ~S~%"
            (and busy (tap-module-hand tap)))
          (format s "  finger                : ~S~%"
            (and busy (tap-module-finger tap)))
          (format s "  correcting free       : ~S~%"
            (null (tap-module-pending-correction tap)))
          (format s "  correcting busy       : ~S~%"
            (tap-module-pending-correction tap))
          (format s "  tapped t              : ~S~%"
            (and busy 
                 (numberp (tap-module-tap-start tap))
                 (numberp (tap-module-last-down-time tap))
                 (> (tap-module-last-down-time tap) (tap-module-tap-start tap))
                 t))
          (format s "  tapped nil            : ~S~%"
            (and busy 
                 (not 
                  (and 
                   (numberp (tap-module-tap-start tap))
                   (numberp (tap-module-last-down-time tap))
                   (> (tap-module-last-down-time tap) (tap-module-tap-start tap))))
                 t)))))))
   
   
(defun params-tap-module (module param)
  (bt:with-recursive-lock-held ((tap-module-lock module))
    (cond ((consp param)
           (case (car param)
             (:dual-execution-stages
              (setf (tap-module-dual-enabled module) (cdr param)))
             (:key-closure-time
              (setf (tap-module-press module) (cdr param)))
             (:key-tap-release
              (setf (tap-module-release module) (cdr param)))
             (:key-release-time 
              (setf (tap-module-key-release module) (cdr param)))
             (:motor-burst-time
              (setf (tap-module-burst module) (cdr param)))
             (:motor-initiation-time
              (setf (tap-module-init module) (cdr param)))
             (:tap-motor-noise
              (setf (tap-module-noise module) (cdr param)))
             (:tap-period
              (setf (tap-module-default-period module) (cdr param)))
             
             ;; These weren't being used so don't implement them until
             ;; they are, and they need to be added to the returned
             ;; values check below as well if they are implmeneted.
             ;(:tap-correction
             ; (setf (tap-module-default-correction module) (cdr param)))
             ;(:exp-rate
             ; (setf (tap-module-rate module) (cdr param)))
             
             (:tap-correction-time
              (setf (tap-module-correction-time module) (cdr param)))))
          (t
           (case param
             (:tap-motor-noise
              (tap-module-noise module))
             (:key-tap-release
              (tap-module-release module))
             (:tap-period
              (tap-module-default-period module))
             (:tap-correction-time
              (tap-module-correction-time module)))))))


(define-module-fct 'tap '((tap nil nil (hand finger correcting tapped) print-tap-buffer-status))
  (list
   (define-parameter :dual-execution-stages :owner nil)
   (define-parameter :key-closure-time :owner nil)
   (define-parameter :key-release-time :owner nil)
   (define-parameter :motor-burst-time :owner nil)
   (define-parameter :motor-initiation-time :owner nil)
   
   
   ;; not currently used 
   ; (define-parameter :exp-rate :valid-test 'posnum :default-value 0.6)
   ; (define-parameter :tap-correction :valid-test 'numberp  :default-value 0.0
   ;  :warning "a positive number" :documentation "Default period error correction.")
   
   (define-parameter :tap-motor-noise :valid-test 'posnumornil :default-value .1
     :warning "a positive number or nil" :documentation "S value for the noise distribution used in tap timing")
   (define-parameter :key-tap-release :valid-test 'posnum :default-value 0.1 
     :warning "a positive number" :documentation "Default tap hold time.")
   (define-parameter :tap-period :valid-test 'posnum  :default-value .5
     :warning "a positive number" :documentation "Default period of a periodic-tap action in seconds.")
   (define-parameter :tap-correction-time :valid-test 'posnum  :default-value .01
     :warning "a positive number" :documentation "Execution time for a tap correction action in seconds."))
  :version "1.0a12"
  :documentation "Support for a new periodic tapping motor action"
  :creation 'create-tap-module
  :query 'tap-query
  ;; no request needed now :request 'tap-request
  :reset '(reset-tap-module secondary-reset-tap-module)
  :params 'params-tap-module)

;;; This is the code that actually implements the action.

;;; Making it into a style which doesn't schedule a finish action
;;; by overriding the queue-finish-event method (not recommended).
;;; Using the punch action as a base and assume the finger ends in
;;; the up position when it ends (the default for a punch so don't
;;; actually have to deal with that).
;;;
;;; Using the two exec times a little differently.  They're
;;; the delays from the "action" not the preparation.  So the timing
;;; for the first action needs to be set to the init cost, and the
;;; rest to the randomized period value.

(defclass periodic-tap (punch)
  ((tap-module :accessor tap-module :initarg :tap-module)
   (period :accessor period :initarg :period :initform .5)))

(defmethod compute-exec-time ((mtr-mod dual-execution-motor-module) (self periodic-tap))
  (let ((tap (tap-module self)))
    (bt:with-recursive-lock-held ((tap-module-lock tap))
      (tap-module-press tap))))

(defmethod compute-second-exec-time ((mtr-mod dual-execution-motor-module) (self periodic-tap))
  (let ((tap (tap-module self)))
    (bt:with-recursive-lock-held ((tap-module-lock tap))
      (+ (tap-module-press tap) (max (tap-module-key-release tap) (tap-module-release tap))))))
  
;; just don't provide one

(defmethod compute-finish-time ((mtr-mod dual-execution-motor-module) (self periodic-tap))
  0)

;; and skip the finish event

(defmethod queue-finish-event ((module dual-execution-motor-module) (self periodic-tap))
  )

(defmethod queue-output-events ((module dual-execution-motor-module) (self periodic-tap))
  (bt:with-recursive-lock-held ((tap-module-lock (tap-module self)))
    
    ;; for consistency in the extended motor module store the new hand position
    ;; eventhough it's not changing because it contains the finger up as a reference
    ;; for when the action is complete.
    
    (schedule-event-relative (seconds->ms (tap-module-init (tap-module self))) 'set-hand-position 
                             :time-in-ms t :module (my-name module) :output nil
                             :destination (my-name module) 
                             :params (list (hand self) (updated-pos self)))

    ;;initializing tap-module mvmt
    (setf (tap-module-mvmt (tap-module self)) self)
    
    ;; to be safe clear previous state
    
    (setf (tap-module-stop (tap-module self)) nil)
    (setf (tap-module-next-up (tap-module self)) nil)
    
    ;; The start-tap action is responsible for scheduling the down and up actions.
    
    (setf (tap-module-next-down (tap-module self))
      (schedule-event-relative (seconds->ms (tap-module-init (tap-module self))) 'start-tap :time-in-ms t 
                               :module (my-name module) :output nil
                               :destination 'tap :params (list module self)))))

(defun noisecalc (time ss) ;;Function to calculate noise
  (if ss
      (let ((toreturn (+ time (* time (act-r-noise ss)))))
        (min (* 3 time) (max 0 toreturn)))
    time))

;;attempt to write motor time function
(defun motor-time-noise (time)
  (if (numberp time)
      (if (zerop time)
          time
        (let ((tap (get-module tap)))
          (if (not tap)
              (print-warning "No tap module available.")
            (bt:with-recursive-lock-held ((tap-module-lock tap))
              ;; ss is guaranteed to be a positive number
              ;; from the parameter setting so no need to test
              ;; it any further here
              (let ((toReturn (noisecalc time (tap-module-noise tap))))
              		;(format t "~a is new time ~%" toReturn)
              		toReturn
                )))))
    (progn
      (print-warning "Invalid value passed to motor-time-noise: ~S" time)
      time)))

(defun start-tap (tap motor mvmt)
  (bt:with-recursive-lock-held ((tap-module-lock tap))
    (setf (tap-module-next-down tap) nil)
    
    (incf (tap-module-count tap))
    (schedule-mod-buffer-chunk 'tap (list 'count (tap-module-count tap)) 0 :module 'tap)
    (setf (tap-module-last-down-time tap) (mp-time))
    
    (schedule-event-relative (seconds->ms (exec-time mvmt))
                             'set-finger-down :time-in-ms t :destination (my-name motor) 
                             :module (my-name motor) :output nil
                             :params (list (style-name mvmt) (hand mvmt) (finger mvmt) (finger-loc (updated-pos mvmt) (finger mvmt))))
    
    (setf (tap-module-next-up tap)
      (schedule-event-relative (seconds->ms (randomize-time (exec2-time mvmt))) 'finish-tap :time-in-ms t 
                               :module (my-name motor) :output nil
                               :destination 'tap :params (list motor mvmt)))
    ))

(defun finish-tap (tap motor mvmt)
  (bt:with-recursive-lock-held ((tap-module-lock tap))
    
    (when (tap-module-next-up tap)
      (schedule-event-now 'set-finger-up :destination (my-name motor) :module (my-name motor) 
                          :output nil
                          :params (list (style-name mvmt) (hand mvmt) (finger mvmt) (finger-loc (updated-pos mvmt) (finger mvmt)))))
    
    (setf (tap-module-next-up tap) nil)

    (cond ((eq (tap-module-stop tap) :pause)
           ;; delay for a correction
           )
          ((tap-module-stop tap) 
           ;; any other stop reason
           (setf (tap-module-stop tap) nil)
           (setf (tap-module-busy tap) nil)
           (setf (tap-module-hand tap) nil)
           (setf (tap-module-finger tap) nil)
           (setf (tap-module-mvmt tap) nil) ;; get rid of the current movement
           ;; schedule a finish event for init time from now
           (schedule-event-relative (seconds->ms (tap-module-init tap)) 'finish-movement-dual 
                                    :destination (my-name motor)
                                    :time-in-ms t :params (list mvmt) :module (my-name motor) 
                                    :details (concatenate 'string (symbol-name 'finish-movement) " " (princ-to-string (request-time mvmt))
                                               " style " (symbol-name (type-of mvmt)) " hand " (symbol-name (hand mvmt)))))
          (t
           (setf (tap-module-next-down tap)
             (schedule-event (seconds->ms (max (mp-time) (+ (tap-module-last-down-time tap) (motor-time-noise (period mvmt)))))
                             'start-tap :time-in-ms t :destination 'tap :module (my-name motor) :output nil
                             :params (list motor mvmt)))))))

;;; Process the request with lots of error checking

(defun handle-tap-request (mtr-mod request)
  (let ((tap (get-module tap)))
    (if (not tap)
        (print-warning "No tap module available for a periodic-tap request.")
      (bt:with-recursive-lock-held ((tap-module-lock tap))
        (if (or (tap-module-next-up tap) (tap-module-next-down tap))
            (print-warning "Only one periodic-tap request allowed at a time.")
          (if (null (tap-module-dual-enabled tap))
              (print-warning "Dual-execution-stages parameter must be t to use a periodic-tap")
           
            ;; use some internal functions to process the request
            (let ((hand (verify-single-explicit-value request 'hand 'tap 'periodic-tap))
                  (finger (verify-single-explicit-value request 'finger 'tap 'periodic-tap))
                  ;; may not be a value so have to check directly
                  (period (if (slot-in-chunk-spec-p request 'period)
                              (let ((spec (chunk-spec-slot-spec request 'period)))
                                (cond ((not (= (length spec) 1))
                                       (print-warning "Multiple period values provided for periodic-tap action."))
                                      ((not (posnum (slot-spec-value (first spec))))
                                       (print-warning "Period value must be a positive number in periodic-tap action but given ~s"
                                                      (slot-spec-value (first spec))))
                                      (t (slot-spec-value (first spec)))))
                            (tap-module-default-period tap))))
              (cond ((not (find hand '(left right)))
                     (print-warning "Invalid hand ~s specified for periodic-tap action." hand))
                    ((not (find finger '(index middle ring pinkie thumb)))
                     (print-warning "Invalid finger ~s specified for periodic-tap action." hand))
                    ((check-jam mtr-mod)
                     )
                    (t ;; all good so clear the count and send it to the motor module
                     (setf (tap-module-count tap) 0)
                     (setf (tap-module-busy tap) t)
                     (setf (tap-module-hand tap) hand)
                     (setf (tap-module-finger tap) finger)
                     (setf (tap-module-tap-start tap) (mp-time))
                     (prepare-movement mtr-mod 
                                       (make-instance 'periodic-tap
                                         :hand hand 
                                         :finger finger 
                                         :period period
                                         :tap-module tap
                                         :request-spec request))
                     (schedule-event-now 'create-new-buffer-chunk :module 'tap
                                         :priority -100 :params (list 'tap (list 'period period 'count 0))
                                         ;; do this for consistency with other buffer chunk creation actions
                                         :details (concatenate 'string (symbol-name 'create-new-buffer-chunk) " " (symbol-name 'tap))))))))))))

(extend-manual-requests (periodic-tap hand finger period) handle-tap-request)


;;; The stop request simply checks to see if there's one ongoing and if so
;;; either stops it now (if it's not past the initiation) or sets the stop flag
;;; so that it stops automatically after the current one completes.
;;;
;;; Implemented as a style now for better motor integration, but need to
;;; explicitly handle initiation since it can't enter the execution 
;;; queue with the tapping already there.

(defclass stop-tapping (dual-hand-movement-style)
  ((hand :accessor hand :initarg :hand)
   (tap-module :accessor tap-module :initarg :tap-module))
  (:default-initargs    ;; since there's no real execution we don't
                        ;; want to enter the execution queue
                        ;; and get stuck because of the tapping
                        ;; so just stop after prep. which means it
                        ;; needs to schedule the actions during the
                        ;; prep timing since normal execution won't happen
      :exec-immediate-p nil))

(defun handle-stop-tap-request (mtr-mod request)
  (declare (ignore mtr-mod request))
  (let ((tap (get-module tap)))
    (if (not tap)
        (print-warning "No tap module available for a stop-tapping request.")
      ;; Pay the cost of prep. even if there's no action to stop
      (bt:with-recursive-lock-held ((tap-module-lock tap))
        (let ((hand (aif (tap-module-mvmt tap)
                        (hand it)
                         'right)))
          (prepare-movement mtr-mod 
                            (make-instance 'stop-tapping
                              :hand hand
                              :tap-module tap
                              :request-spec request)))))))



;; to be safe define a feat-differences method because there
;; isn't one for the base movement class and a model might
;; accidentally call them back-to-back which would lead to an
;; error

(defmethod feat-differences ((s1 stop-tapping) (s2 stop-tapping))
  (declare (ignorable s1 s2))
  0)

;; Since it's not being executed, we need to handle the activity in the
;; feature preparation itself, which can be done with an around
;; method to use default prep timing and then add our events

(defmethod compute-prep-time :around ((mtr-mod dual-execution-motor-module) (self stop-tapping))
  (let ((prep-time (call-next-method))
        (tap (tap-module self)))
    (when tap
      (bt:with-recursive-lock-held ((tap-module-lock tap))
        (let ((init-time (tap-module-init tap))) ;; default init time
          
          ;; Indicate initiation complete in the trace
          (schedule-event-relative (seconds->ms (+ prep-time init-time))
                                   nil
                                   :priority 0
                                   :module (my-name mtr-mod) 
                                   :time-in-ms t 
                                   :details (concatenate 'string (symbol-name 'initiation-complete) " " 
                                              (princ-to-string (request-time self))
                                              " style " (symbol-name (type-of self)) 
                                              " hand " (symbol-name (hand self))))
          
          ;; Schedule the event to stop the tapping and display
          ;; it as the finish event since there's no real execution
          
          (schedule-event-relative (seconds->ms (+ prep-time init-time))
                                   'stop-tapping-action
                                   :params (list tap mtr-mod)
                                   :priority -1
                                   :module (my-name mtr-mod) 
                                   :time-in-ms t 
                                   :details (concatenate 'string (symbol-name 'finish-movement) " " 
                                              (princ-to-string (request-time self))
                                              " style " (symbol-name (type-of self)) 
                                              " hand " (symbol-name (hand self)))))))
    prep-time))

    
(defun stop-tapping-action (tap motor)
  (bt:with-recursive-lock-held ((tap-module-lock tap))
    (if (not (or (tap-module-next-up tap) (tap-module-next-down tap)))
        (print-warning "No periodic-tap request is occurring to stop.")
      
      (if (and (tap-module-next-down tap)
               (> (- (evt-mstime (tap-module-next-down tap)) (mp-time-ms))
                  (seconds->ms (+ (tap-module-init tap) (exec-time (tap-module-mvmt tap))))))
          ;; current hasn't passed the point of no return yet
          ;; so clear the buffer and stop it now
          (let ((mvmt (tap-module-mvmt tap)))
            (schedule-clear-buffer 'tap 0 :module 'tap)
            (schedule-event-relative (seconds->ms (tap-module-init tap)) 'finish-movement-dual 
                                     :destination (my-name motor)
                                     :time-in-ms t :params (list mvmt) :module (my-name motor) 
                                     :details (concatenate 'string (symbol-name 'finish-movement) " " (princ-to-string (request-time mvmt))
                                                " style " (symbol-name (type-of mvmt)) " hand " (symbol-name (hand mvmt))))
            (delete-event (tap-module-next-down tap))
            (setf (tap-module-next-down tap) nil)
            (setf (tap-module-mvmt tap) nil))
          
          ;; set the stop flag so the action stops itself after the current tap
          (setf (tap-module-stop tap) t)))))
    
(extend-manual-requests (stop-tapping) handle-stop-tap-request)


;;; The correction request checks to see if there's a tap ongoing and if so
;;; schedules the update depending on where the current one is at relative
;;; to the initiation of the next press.
;;;
;;; Currently, it only updates when the current tap is not ongoing i.e.
;;; between the initiation and finger up.  Which may mean another tap
;;; at the current period before the update. [It causes an explicit
;;; pause in the tapping when there's a conflict so that a short tap
;;; period and/or long tap durations don't lock out the correction 
;;; with the up happening less than an initiation from the next down.]
;;; 
;;;
;;; Records that there's an ongoing correction in the tap module as
;;; a safety check since it's possible that the previous correction was
;;; delayed because of the ongoing action and a new correction could be
;;; issued before that one happens.  If that does occur, then it ignores
;;; the new correction for now because it gets ugly to try and undo the
;;; one that's already got events scheduled.

(defclass tap-correction (periodic-tap)
  ()
  (:default-initargs    ;; Like the stop, since there's no real execution 
                        ;; don't want to enter the execution queue
                        ;; and get stuck because of the tapping
      :exec-immediate-p nil))

(defun handle-correction-tap-request (mtr-mod request)
  (let ((tap (get-module tap)))
    (if (not tap)
        (print-warning "No tap module available for a tap-correction request.")
      (bt:with-recursive-lock-held ((tap-module-lock tap))
        (if (tap-module-pending-correction tap)
            (print-warning "Only one period-correction can occur at a time. Ignoring new request.")
          (let ((period (verify-single-explicit-value request 'correction 'tap 'period-correction))
                (mvmt (tap-module-mvmt tap)))
            (if (not (numberp period))
                (print-warning "Period value must be a number in period-correction action but given ~s" period)
              (progn
                (setf (tap-module-pending-correction tap) t)
                (setf (tap-module-tap-start tap) nil)
                (prepare-movement mtr-mod 
                                  (make-instance 'tap-correction
                                    :hand (hand mvmt)
                                    :finger (finger mvmt)
                                    :period period
                                    :tap-module tap
                                    :request-spec request))))))))))
    

;; Since it's not be executed, we need to handle the activity in the
;; feature preparation itself, which can be done with an around
;; method to use default prep timing and then schedule our event

(defmethod compute-prep-time :around ((mtr-mod dual-execution-motor-module) (self tap-correction))
  (let ((prep-time (call-next-method))
        (tap (tap-module self)))
    (when tap
      (bt:with-recursive-lock-held ((tap-module-lock tap))
        (let ((init-time (tap-module-init tap))) ;; default init time
          
          ;; Indicate initiation complete in the trace
          ;; and call the event that'll check whether it's safe to
          ;; update yet.
          
          (schedule-event-relative (seconds->ms (+ prep-time init-time (tap-module-correction-time tap)))
                                   'check-tap-update
                                   :params (list mtr-mod self)
                                   :priority 0
                                   :module (my-name mtr-mod) 
                                   :time-in-ms t 
                                   :details (concatenate 'string (symbol-name 'initiation-complete) " " 
                                              (princ-to-string (request-time self))
                                              " style " (symbol-name (type-of self)) 
                                              " hand " (symbol-name (hand self)))))))
    prep-time))


(defun check-tap-update (mtr-mod tap-correction)
  (let ((tap (tap-module tap-correction)))
    (bt:with-recursive-lock-held ((tap-module-lock tap))
      (if (and (tap-module-next-down tap)
               (< (mp-time) (- (evt-time (tap-module-next-down tap))
                               (tap-module-init tap)
                               (exec-time (tap-module-mvmt tap)))))
          ;; The finger is up and we haven't hit the
          ;; point of no return update now
          (update-tap-period mtr-mod tap-correction)
        (if (or (tap-module-next-down tap)
                (tap-module-next-up tap))
            (progn
              ;; we are delayed so set the pause and schedule the
              ;; next check after the next motor module action
              (setf (tap-module-stop tap) :pause)
              (schedule-event-after-module (my-name mtr-mod) 'check-tap-update
                                           :params (list mtr-mod tap-correction)
                                           :output nil))
          (if (eq (tap-module-stop tap) :pause)
              ;; call the update now
              (update-tap-period mtr-mod tap-correction)
            (progn
              (print-warning "Unexpected tap correction state, possibly because of a stop during a correction. Correction terminated.")
              (setf (tap-module-pending-correction tap) nil)
              (schedule-event-now nil
                              :priority -1
                              :module (my-name mtr-mod)
                              :details (concatenate 'string (symbol-name 'finish-movement) " " 
                                              (princ-to-string (request-time tap-correction))
                                              " style " (symbol-name (type-of tap-correction)) 
                                              " hand " (symbol-name (hand tap-correction)))))))))))

(defun update-tap-period (mtr-mod tap-correction)
  (let ((tap (tap-module tap-correction)))
    (when tap
      (bt:with-recursive-lock-held ((tap-module-lock tap))
        
        (setf (tap-module-pending-correction tap) nil)
        
        (let ((new-period (max 0 (+ (period (tap-module-mvmt tap)) (period tap-correction)))))
          (setf (period (tap-module-mvmt tap)) new-period)
          (schedule-mod-buffer-chunk 'tap (list 'period new-period) 0 :module 'tap)
          (setf (tap-module-tap-start tap) (mp-time))
        
          (if (eq (tap-module-stop tap) :pause) 
              ;; it was paused so call finish 
              ;; to schedule the next one normally
              (progn
                (setf (tap-module-stop tap) nil)
                (finish-tap tap mtr-mod (tap-module-mvmt tap)))
            (progn
              ;; Actually update the ongoing tap now instead of waiting for the
              ;; tap action to respond to the change which results in a lag of
              ;; one tap.
              ;; Recomputing the noise on the whole period for now, but maybe it should just be on
              ;; the delta between now and when the action will occur or just add the
              ;; correction since the previous time was aready randomized.
          
          
              (let ((current-event-params (evt-params (tap-module-next-down tap)))
                    (updated-time (max (mp-time)
                                       (+ (tap-module-last-down-time tap)
                                          (motor-time-noise new-period)))))
            
                (delete-event (tap-module-next-down tap))
            
                (setf (tap-module-next-down tap)
                  
                  (schedule-event (seconds->ms updated-time)
                                  'start-tap :time-in-ms t :destination 'tap :module (my-name mtr-mod) :output nil
                                  :params current-event-params)))))
          
          (schedule-event-now nil
                              :priority -1
                              :module (my-name mtr-mod)
                              :details (concatenate 'string (symbol-name 'finish-movement) " " 
                                              (princ-to-string (request-time tap-correction))
                                              " style " (symbol-name (type-of tap-correction)) 
                                              " hand " (symbol-name (hand tap-correction)))))))))

    
(extend-manual-requests (period-correction correction) handle-correction-tap-request)


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
