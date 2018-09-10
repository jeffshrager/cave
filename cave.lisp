;;; To Do List:
;;;   Can be closed loops
;;;   Don't allow to leave via forest until you defeat the dragon

;; (load "cave.lisp")
(declaim (optimize (debug 3)))
(setq *print-circle* t)


(defvar *player* nil)
(defstruct player health inventory)

(defparameter *door-images* '("Cloud" "Diamond" "Glass" "Jupiter" "Mountain" "Orb" "Sword" "Tiger" "Woman"))

(defparameter *n-door-images* (length *door-images*))

(defstruct door symbol room image)

(defparameter *door-symbols* '(:a :b :c :d))

(defstruct room name doors mob weapon)

(defvar *cave* "This is setup by init-cave.")

(defvar *weapons* nil)
(defvar *mobs* nil)
(defvar *dragon* nil)
(load "objects.lisp")
(defvar *nweapons* (length *weapons*))
(defvar *nmobs* (length *mobs*))

(defun init-cave (&aux (n-rooms 25))
  (setf *cave* nil)
  ;; The player always begins in the last room (which will end up being the first entry in *cave*)
  ;; First we make the rooms, then the doors.
  (loop for n below n-rooms
	do (push (make-room :name (format nil "Room-~a" n)) *cave*))
  ;; Put in at least one door in each room.
  (loop for from-room in *cave*
	when (< (length (room-doors from-room)) 4)
	do 
	(let* ((to-room (loop as to-room = (nth (random n-rooms) *cave*)
			      as kill from 1 by 1
			      until (< (length (room-doors to-room)) 4)
			      finally (return to-room)))
	       (free-from-door (loop for symbol in *door-symbols*
				     until (null (find symbol (room-doors from-room) :key #'door-symbol))
				     finally (return symbol)))
	       (free-to-door (loop for symbol in *door-symbols*
				   until (null (find symbol (room-doors to-room) :key #'door-symbol))
				   finally (return symbol))))
	  (push (make-door :symbol free-from-door :room to-room :image (free-room-door-image from-room))
		(room-doors from-room))
	  (push (make-door :symbol free-to-door :room from-room :image (free-room-door-image from-room)) 
		(room-doors to-room))
	  ))
  ;; Assign a weapon and a mob to each room (Except the starting room has no mob)
  (loop for room in (cdr *cave*)
	as mob = (copy-list (nth (random *nmobs*) *mobs*))
	as hp = (getf mob :hp)
	as weapon = (copy-list (nth (random *nweapons*) *weapons*))
	do 
	(setf (room-weapon room) weapon)
	(setf (getf mob :hp) (+ (car hp) (random (cdr hp))))
	(setf (room-mob room) mob)
	)
  ;; Add the exit in a random room, and assign the dragon to defend it
  (let ((exit-room (nth (1+ (random (1- (length *cave*)))) *cave*))) ;; Make sure it's not the start room!
    (push (make-door :symbol :x :image "Forest" :room (make-room :name "Exit")) (room-doors exit-room))
    (setf (room-mob exit-room) (copy-list *dragon*))
    )
  ;; Finally, create the player
  (setf *player*
	(make-player :health (+ 20 (random 20))
		     :inventory (list (copy-list (nth (random *nweapons*) *weapons*)))))
  )
		      
(defun free-room-door-image (room)
  (loop with current-images = (mapcar #'door-image (room-doors room))
	as image = (nth (random *n-door-images*) *door-images*)
	until (not (member image current-images :Test #'string-equal))
	finally (return image)))
		   
(defparameter *help-string*
 "Type the name of the image over a door you want to walk through. 
 (You only need the first letter.)
 Special actions:
   /weapon = attack (You only need the first letter of the weapon,
                     and if you press / again without a weapon name or letter,
                     it will attack with the same weapon as last time.)
   [ = retreat (goes back into the room you came from (NOT YET IMPLMENTED))
 "
 )

(defun play ()
  (init-cave)
  (case 
   (prog ((room (first *cave*)) mob action door doors last-weapon)
	 top
	 ;; If the player got out...
	 (if (string-equal "Exit" (room-name room))
	     (return :win))
	 ;; If the player is dead...
	 (when (<= (player-health *player*) 0)
	   (return :lose))
	 ;; If the mob a dead collect the weapon...
	 (setf mob (room-mob room))
	 (when (and mob (<= (getf mob :hp) 0))
	   (format t "You killed the ~a!" (getf mob :name))
	   (let ((weapon (room-weapon room)))
	     (format t "You collected the ~a!" (getf weapon :name))
	     (add-inventory-weapon weapon) ;; makes a new one or increments the power of any you have
	     (setf (room-weapon room) nil)
	     (setf (room-mob room) nil)
	     (setf mob nil)))
	 ;; Describe the room and roll...
	 (setf doors (room-doors room))
	 (describe-room room)
	 ;; Mob (if any) attack?
	 (when (and mob (zerop (random 2)))
	   (format t "The ~a attacks you!" (getf mob :name))
	   (decf (player-health *player*) (getf mob :power))
	   (if (<= (player-health *player*)) (go top)))
	 (format t "You have health ~a!" (player-health *player*))
	 ;; Person action...
	 (setf action (read-line))
	 (when (zerop (length action))
	   (format t "Hunh???")
	   (go top))
	 (when (char-equal #\/ (aref action 0))
	   (if mob
	       (wield-weapon (if (= 1 (length action))
				 last-weapon 
			       (setf last-weapon
				     (loop with wchar = (aref action 1)
					   for weapon in (player-inventory *player*)
					   as ichar = (aref (getf weapon :name) 0)
					   when (char-equal wchar ichar)
					   do (return weapon))))
			     mob)
	     (format t "There's nothing here to attack!~%"))
	   (go top))
	 (when (string-equal "?" action)
	   (format t *help-string*)
	   (go top))
	 (setf door (loop for door in doors
			  as image = (door-image door) 
			  when (char-equal (aref action 0) (aref image 0))
			  do (return door)))
	 ;; Move, and incr. my health
	 (if door
	     (progn (setf room (door-room door))
		    (incf (player-health *player*)))
	   (format t "I can't understand ~a~%" action))
	 (go top))
   (:win (format t "YOU WON!"))
   (:lose (format t "Oh well, you died!")))
  )

(defun add-inventory-weapon (w)
  (or 
   (loop with wname = (getf w :name)
	 for i in (player-inventory *player*)
	 when (string-equal (getf i :name) wname)
	 do 
	 (incf (getf i :power) (getf w :power))
	 (return t))
   (push w (player-inventory *player*))))

(DEFUN wield-weapon (weapon mob)
  (if (null weapon)
      (format t "You don't have weapon!")
    (progn
      (format t (getf weapon :attack-phrase) (getf mob :name))
      (format t "~%")
      (if (zerop (random 2))
	  (progn
	    (format t "Hit!~%")
	    (decf (getf mob :hp) (getf weapon :power)))
	(format t "Miss!")))))

(defun show-cave ()
  (loop for room in *cave*
	do (describe-room room)))

(defun describe-room (room)
  (format t "~%You are in room ~a, with health ~a.~%" (room-name room) (player-health *player*))
  (loop for door in (room-doors room)
	do (format t "There is a door with a picture of a ~a.~%" (door-image door)))
  (when (room-weapon room) (format t "There is a ~a laying here.~%" (getf (room-weapon room) :name)))
  (when (room-mob room) (format t "Watch out! There is a ~a here with hp ~a.~%" 
				(getf (room-mob room) :name)
				(getf (room-mob room) :hp)
				))
  (loop for weapon in (player-inventory *player*)
	do (format t "You are holding a ~a with power ~a.~%" 
		   (getf weapon :name) 
		   (getf weapon :power)))
  (format t "================================~%What do you want to do? (Type ? for help.) ")
  )
