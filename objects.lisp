(setq *mobs* 
      '(
	(
	 :type :mob
	 :name "skeleton"
	 :attack-phrase "The skeleton shoots an arrow at you!"
	 :hp (5 . 10)
	 :power 3
	 )

	(
	 :type :mob
	 :name "zombie"
	 :attack-phrase "The zombie attacks you!!"
	 :hp (1 . 5)
	 :power 1
	 )
	))

(setq *weapons* 
      '(
	(
	 :type :weapon
	 :name "knife"
	 :attack-phrase "You attempt to knife the ~a!"
	 :power 1
	 ) 
	(
	 :type :weapon
	 :name "sword"
	 :attack-phrase "You swing your sword at the ~a!"
	 :power 3
	 ) 

	(
	 :type :weapon
	 :name "bow"
	 :attack-phrase "You fire an arrow at the ~a!"
	 :power 2
	 ) 
	(
	 :type :weapon
	 :name "grenade launcher"
	 :attack-phrase "You launch a grenade at the ~a!"
	 :power 5
	 ) 
	))

(setq *dragon* 
      '(
	:type :mob
	      :name "dragon"
	      :attack-phrase "The dragon attacks you!"
	      :hp 50
	      :power 10
	      ))

