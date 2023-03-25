(require 'cl-lib)
(require 'dash)
(require 'org-ml)

(define-minor-mode org-cthulhu-mode
  "Starts off a minor mode for handling Call of Cthulhu
scenarios. "
  :init-value nil
  :lighter " Org-cthulhu"
  :after-hook (if org-fragtog-mode
                  (add-hook 'after-save-hook 'cthulhu-personnages-liste-update nil t)
                (remove-hook 'after-save-hook 'cthulhu-personnages-liste-update t)))

(defvar-local cthulhu-personnages-liste nil)

(defconst cthulhu-sections
  '("Objets"
   "Personnages"
   "Monstres"
   "Lieux"
   "Événements"
   "Documentation"))

(defconst cthulhu-outcome
  '(("Maladresse" . 5)
    ("Échec" . 4)
    ("Réussite normale" . 3)
    ("Réussite majeure" . 2)
    ("Réussite extrême" . 1)
    ("Réussite critique" . 0)))

(defconst cthulhu-specials
  '("Impact"
     "Carrure"))

(defvar cthulhu-weapons-list 
  '(("Arbalète" "Arcs" "1D8+2" t 50 0.5 nil 96)
    ("Mains nues" "Corps à corps" "1D3 + Imp" nil nil 1 nil nil)
    ("Hache" "Haches" "1D8+2" t nil 1 nil nil)
    ("Luger" "Armes de poing" "1D10" t 15 1 8 99)
    ("IMI Desert Eagle" "Armes de poing" "3D10+2D6+3+Imp/2" t 15 1 7 94)
    ("Torche" "Corps à corps" "1D6+Feu" nil 1 nil nil)
    ("Automatique cal. 38" "Armes de poing" "1D10" t 15 1 '(6 5) 99))
  "Nom Compétence Dégats Empalement Portée Cadence Capacité Panne")

(defconst cthulhu-fight-rows
  '("Points de vie" 
    "Protection"  
    "Attaques" 
    "Coups rendus" 
    "Blessure grave" 
    "Mourant"
    "Inconscient"
    "Folie"))         

(defconst cthulhu-fight-choices
  '("Esquiver"
    "Rendre les coups"
    "Manœuvre"))

(defun cthulhu--get-tagged-subtree (taglist)
  "Détermine la liste les enfants des nœuds dont la liste des tags est taglist "
  (apply #'-concat (-map #'org-ml-get-children 
 (--filter (eq (org-ml-get-type it) 'headline) (org-ml-match '((:tags taglist)) (org-ml-parse-subtrees 'all))))))

(defun cthulhu--get-name (personnage)
  (org-ml-get-property :raw-value personnage))

(defun cthulhu--get-characters-name ()
  (--map (cthulhu--get-name it) cthulhu-personnages-liste))

(defun cthulhu-personnages-liste-update ()
    (setq cthulhu-personnages-liste (--filter (eq (org-ml-get-type it) 'headline)
                                              (cthulhu--get-tagged-subtree '("personnage")))))


;; https://github.com/10sr/switch-buffer-functions-el/pull/7
;;(add-to-list window-buffer-change-functions 'cthulhu-personnages-liste-update)

(defun org-cthulhu--get-document-sections ()
  (--map (org-ml-get-property :raw-value it) (org-ml-parse-subtrees 'all)))

(defun org-cthulhu--add-new-subsection (section subtitle content)
  (save-excursion
    (goto-char 0)
    (re-search-forward (s-lex-format "^* ${section}"))
    (org-insert-subheading (point))
    (insert subtitle)
    (org-newline-and-indent)
    (insert content)
    (org-id-get-create)))

(defun cthulhu-new-thing-and-make-link (type)
  (interactive "P")
  (let* ((type (if (not type) (completing-read "Sélection ? " (org-cthulhu--get-document-sections))
		 type))
	 (thing (read-from-minibuffer (s-lex-format "${type} à ajouter ? ")))
	 (desc (read-from-minibuffer "Description "))
	 (id (org-cthulhu--add-new-subsection type thing desc)))
    (org-insert-link nil (concat "id:" id) thing)))

(defun cthulhu--make-adders (cthulhu-sections)
  (--map (let* ((type it)
                (fun-name (concat "cthulhu-new-" type "-and-make-link")))
           (defalias (intern fun-name) `(lambda () (interactive) (cthulhu-new-thing-and-make-link ,type))))
         cthulhu-sections))

(cthulhu--make-adders cthulhu-sections)

(defun cthulhu-select-character (&optional personnage-name prompt-info)
  "Renvoie un personnage du buffer courant"
  (let ((personnage-name (if (not personnage-name)
                             (completing-read (concat "Sélectionner un personnage: " prompt-info) (cthulhu--get-characters-name) nil t)
                           personnage-name)))
    (car (--filter (string= (cthulhu--get-name it) personnage-name) cthulhu-personnages-liste))))

(defun cthulhu-get-tables (personnage)
  "Récupère la section des tables d'un personnage donné"
  (car (--filter (string= (org-ml-get-property :raw-value it) "Tables") (org-ml-get-children personnage))))

(defun cthulhu--get-table-column (table n)
  (let ((rows-num (length (org-ml-get-children table))))
    (loop for i from 0 to (- rows-num 1)
          collect (org-ml-get-children (org-ml-table-get-cell i n table)))))

(defun cthulhu--get-character-info (personnage)
  (let* (
         (carac-tables (org-ml-match '(:any * table) personnage))
         (carac (-map (lambda (table) (-map #'car (cthulhu--get-table-column table 0))) carac-tables))
         (carac-name (apply #'-concat carac))
         (carac (-map (lambda (table) (-map #'car (cthulhu--get-table-column table 1))) carac-tables))
         (carac-value (apply #'-concat carac))
         (normalize-str (lambda (str) (if str (substring-no-properties str) ""))))
    (--zip-with (cons (funcall normalize-str it) (funcall normalize-str other)) carac-name carac-value)))

(defun get-carac (personnage carac-name)
  "carac : carac-name [str] . carac-value [str]"
  (assoc carac-name (cthulhu--get-character-info personnage)))

(defun carac-to-string (carac)
  (let ((carac-name (car carac))
        (carac-value (cdr carac)))
    (if (member carac-name cthulhu-specials)
        (format "%s : %s" carac-name carac-value)
      (format "%s : %s [maj. %s/ext. %s]"
              carac-name
              (carac-value carac)
              (cthulhu-majeur (carac-value carac))
              (cthulhu-extreme (carac-value carac))))))

(defun carac-value (carac)
  "A number representing the caracteristic value
If the caracteristic is a dice to roll, roll it. "
  (cond
   ((member (car carac) cthulhu-specials) (cthulhu-roll-from-string (cdr carac)))
   (t (string-to-number (cdr carac)))))

(defun get-carac-value (personnage carac-name)
  (carac-value (get-carac personnage carac-name)))

(defun cthulhu-select-carac (&optional personnage)
  (interactive)
  (let* ((personnage (if (not personnage)
                         (cthulhu-select-character)
                       personnage))
         (info (cthulhu--get-character-info personnage))
         (carac (assoc (completing-read "Sélectionner une  caractéristique: " info nil t) info)))
    (message (carac-to-string carac))
    carac))

;; (cthulhu-select-carac (cthulhu-select-character personnages-subtrees))
;; (get-carac-value (cthulhu-select-character personnages-subtrees) "POU")

(defun cthulhu-set-carac (personnage carac calc-new-carac)
  (let* ((tables (cthulhu-get-tables personnage))
         (beg (org-ml-get-property :begin tables))
         (end (org-ml-get-property :end tables))
         (val (get-carac-value personnage carac))
         (v (apply calc-new-carac (list val)))
         (m (cthulhu-majeur v))
         (e (cthulhu-extreme v))
         (new-line (s-lex-format "| ${carac} | ${v} | ${m} | ${e} |")))
    (save-excursion
      (goto-char beg)
      (re-search-forward (s-lex-format "^.*${carac}.*$"))
      (replace-match new-line)
      (org-table-align)
      (save-buffer))
    v))

(defun cthulhu-select-set-carac (new-value)
  (interactive "PValeur de la caractéristique ? ")
  (let* ((personnage (cthulhu-select-character))
         (carac (completing-read "Sélectionner une  caractéristique: " (cthulhu--get-character-info personnage) nil t))
         (old-value (get-carac-value personnage carac))
         (mod (if (not mod)
                  (string-to-number (read-from-minibuffer "Valeur de la caractéristique : "))
                mod))
         (calc-new-carac (lambda (val) new-value)))
    (cthulhu-set-carac personnage carac calc-new-carac)
    (message (format "%s : %s -> %s" carac old-value new-value))))

(defun cthulhu-select-mod-carac (mod)
  (interactive "PModification à apporter ? ")
  (let* ((personnage (cthulhu-select-character))
         (carac (completing-read "Sélectionner une  caractéristique: " (cthulhu--get-character-info personnage) nil t))
         (mod (if (not mod)
                  (string-to-number (read-from-minibuffer "Modification à apporter : "))
                mod))
         (old-value (get-carac-value personnage carac))
         (calc-new-carac (lambda (val) (+ val mod)))
         (new-value (cthulhu-set-carac personnage carac calc-new-carac)))
    (message (format "%s : %s -> %s" carac old-value new-value))))

(defun cthulhu-impact (fortai)
  (cond
   ((< fortai 64) "-2")
   ((< fortai 84) "-1")
   ((< fortai 124) "0")
   ((< fortai 164) "1D4")
   ((< fortai 204) "1D6")
   ((< fortai 284) "2D6")
   ((< fortai 364) "3D6")
   ((< fortai 444) "4D6")
   ((< fortai 524) "5D6")
   (t "6D6")))

(defun cthulhu-carrure (fortai)
  (cond
   ((< fortai 64) "-2")
   ((< fortai 84) "-1")
   ((< fortai 124) "0")
   ((< fortai 164) "1")
   ((< fortai 204) "2")
   ((< fortai 284) "3")
   ((< fortai 364) "4")
   ((< fortai 444) "5")
   ((< fortai 524) "6")
   (t "100")))

(defun cthulhu-majeur (carac)
  (round (/ carac 2)))
(defun cthulhu-extreme (carac)
  (round (/ carac 5)))

(defun cthulhu--ask-success-type (&optional prompt)
  (cdr (assoc (completing-read (concat "Type de réussite " prompt) cthulhu-outcome) cthulhu-outcome)))

(defun cthulhu--outcomen-to-string (outn)
  (car (rassq outn cthulhu-outcome)))

(defun cthulhu-roll-success (roll comp)
  (cond
   ((= roll 100) 5)
   ((and (< comp 50) (> roll 95)) 5)
   ((> roll comp) 4)
   ((<= roll 1) 0)
   ((<= roll (cthulhu-extreme comp)) 1)
   ((<= roll (cthulhu-majeur comp)) 2)
   ((<= roll comp) 3)))

(defun cthulhu--roll100 (&optional modif)
  "Lance un dé 100 avec des dés bonus/malus "
  (if (not modif)
      (random 100)
  (let* ((choose-dice-fun (if (< 0 modif) #'min #'max))
         (dizaines-chiffre (apply choose-dice-fun
                                  (loop for i below (1+ (abs modif))
                                        collect (random 10))))
         (unités-chiffre (random 10)))
    (+ unités-chiffre (* dizaines-chiffre 10)))))

(defun cthulhu-roll (Ds F &optional modif)
  "Renvoie une liste de lancés de dés. "
  (if (= F 100)
      (loop for i below Ds
	    collect (cthulhu--roll100 modif))
    (loop for i below Ds
	  collect (1+ (random F)))))

(defun cthulhu-roll-from-string (str)
  "str est de la forme xDx ou bien x (constante)"
  (interactive "MLancer ? ")
  (cond
   ((string-match "\\([[:digit:]]\\)D\\([[:digit:]]\\{0,2\\}\\)" str) (apply #'+ (cthulhu-roll (string-to-number (match-string 1 str))
                                                                                            (string-to-number (match-string 2 str)))) )
   (t (string-to-number str))))

(defun cthulhu-roll-max-from-string (str)
  (interactive "MLancer ? ")
  (cond
   ((string-match "\\([[:digit:]]\\)D\\([[:digit:]]\\{0,2\\}\\)" str) (+ (* (string-to-number (match-string 1 str))
                                                                                 (string-to-number (match-string 2 str)))) )
   (t (string-to-number str))))

(defun cthulhu-select-roll-carac (&optional modif)
  (interactive)
  (let* ((perso (cthulhu-select-character))
         (carac (cthulhu-select-carac perso))
         (roll (cthulhu--roll100 modif))
         (out (cthulhu-roll-success roll (carac-value carac))))
    (message (format "%s Roll %d : %s" (carac-to-string carac) roll (cthulhu--outcomen-to-string out)))))


(defun cthulhu--opposed-roll (perso1 perso2 carac1 carac2 roll1 roll2)
  (let ((out1 (cthulhu-roll-success roll1 (carac-value carac1)))
        (out2 (cthulhu-roll-success roll2 (carac-value carac2))))
    (cond
     ((or (and (>= out1 4) (>= out2 4)) (= out1 out2)) (if (> (carac-value carac1) (carac-value carac2))
                                                           perso1
                                                         perso2))
      ((< out1 out2) perso1)
      ((< out2 out1) perso2))))


(defun cthulhu-select-opposed-roll ()
  (interactive)
  (let* ((perso1 (cthulhu-select-character nil "(1/2)"))
         (perso2 (cthulhu-select-character nil "(2/2)"))
         (carac1 (cthulhu-select-carac perso1))
         (carac2 (cthulhu-select-carac perso2))
         (roll1 (string-to-number (read-from-minibuffer (format "Roll ? (%s)" (cthulhu--get-name perso1)))))
         (roll2 (string-to-number (read-from-minibuffer (format "Roll ? (%s)" (cthulhu--get-name perso2)))))
         (winner (cthulhu--opposed-roll perso1 perso2 carac1 carac2 roll1 roll2)))
    (message (format "OPPOSITION\n%s %s %d (%s)\n%s %s %d (%s)\nGAGNANT : %s" 
		     (cthulhu--get-name perso1)
		     (carac-to-string carac1)
		     roll1
		     (cthulhu--outcomen-to-string (cthulhu-roll-success roll1 (carac-value carac1)))
		     (cthulhu--get-name perso2)
		     (carac-to-string carac2)
		     roll2
		     (cthulhu--outcomen-to-string (cthulhu-roll-success roll2 (carac-value carac2)))
		     (cthulhu--get-name winner)))))

(defun cthulhu--failurep (weapon roll)
  (> (nth 8 weapon) roll))

(defun cthulhu--range-weaponp (weapon)
  "Renvoie la portée d'une arme si celle-ci est une arme à distance,
sinon. "
  (nth 5 weapon))

(defun cthulhu--impale-weaponp (weapon)
  "Détermine si l'arme weapon est capable de dégâts d'empalement."
  (nth 4 weapon))

(defun cthulhu-weapon-to-string (weapon)
  (let ((name (nth 0 weapon))
        (degats (nth 2 weapon))
        (empalement (nth 3 weapon)))
    (format "%s (%s) %s" name degats (if empalement "[E]" " "))))

(defun cthulhu-select-weapon ()
  (interactive)
  (let* ((weapon-name (completing-read "Arme ? " cthulhu-weapons-list))
         (weapon (assoc weapon-name cthulhu-weapons-list)))
    (message (cthulhu-weapon-to-string weapon))
    weapon))
;; (weapon-to-string (select-weapon))

(defun cthulhu-weapon-get-damage (weapon imp)
  (let ((rolls (split-string (nth 2 weapon) "+"))
        (imp-damage (cthulhu-roll-from-string imp)))
    (apply #'+
           (--map (if (string-match "Imp" it)
                      (string-to-number (calc-eval (replace-match (number-to-string imp-damage) nil nil it)))
                    (cthulhu-roll-from-string it))
                  rolls))))

(defun cthulhu-weapon-get-max-damage (weapon imp)
  (let ((cthulhu-rolls (split-string (nth 2 weapon) "+"))
        (imp-damage (cthulhu-roll-max-from-string imp)))
    (apply #'+
           (--map (if (string-match "Imp" it)
                      (string-to-number (calc-eval (replace-match (number-to-string imp-damage) nil nil it)))
                    (cthulhu-roll-max-from-string it))
                  cthulhu-rolls))))

(defun cthulhu-weapon-get-impalement-damage (weapon imp)
  (+ (cthulhu-weapon-get-damage weapon imp)
     (cthulhu-weapon-get-max-damage weapon imp)))

(defun cthulhu--compute (personnage)
  (let ((imp (cthulhu-impact (+ (get-carac-value personnage "FOR") (get-carac-value personnage "TAI"))))
        (carr (cthulhu-carrure (+ (get-carac-value personnage "FOR") (get-carac-value personnage "TAI"))))
        (esq (/ (get-carac-value personnage "DEX") 2))
        (san (get-carac-value personnage "POU"))
        (langue (get-carac-value personnage "EDU"))
        (pm (/ (get-carac-value personnage "POU") 5))
        (langue (get-carac-value personnage "EDU"))
        (pdv (/ (+ (get-carac-value personnage "CON") (get-carac-value personnage "TAI")) 10)))
    `(("Impact" . ,(format "| -- Impact | %s |  |  |" imp))
      ("Carrure" . ,(format "| -- Carrure | %s |  |  |" carr))
      ("Esquive" . ,(format "| Esquive | %s | %s | %s |" esq (cthulhu-majeur esq) (cthulhu-extreme esq)))
      ("Langue maternelle" . ,(format "| Langue maternelle | %s | %s | %s |" langue (cthulhu-majeur langue) (cthulhu-extreme langue)))
      ("Santé mentale" . ,(format "| Santé mentale | %s | %s | %s |" san (cthulhu-majeur san) (cthulhu-extreme san)))
      ("Points de magie" . ,(format "| Points de magie | %s |  |  |" pm))
      ("Points de vie" . ,(format "| Points de vie | %s |  |  |" pdv))
      )))

(defun cthulhu-init-tables (personnage)
  (let* ((tables (cthulhu-get-tables personnage))
         (beg (org-ml-get-property :begin tables))
         (end (org-ml-get-property :end tables))
         (pdv (/ (+ (get-carac-value personnage "CON") (get-carac-value personnage "TAI")) 10))
         (new-line (s-lex-format "| Points de vie | ${pdv} |  |  |")))
    (save-excursion
      (cl-loop for (carac-name . new-line) in (cthulhu--compute personnage) do
               (goto-char beg)
               (re-search-forward (s-lex-format "^.*${carac-name}.*$"))
               (replace-match new-line)
               (org-table-align)))))

(defun cthulhu-select-init ()
  (interactive)
  (cthulhu-init-tables (cthulhu-select-character)))

(defun cthulhu-fight-get-current-fighters ()
  (let* ((table (org-ml-parse-this-table-row))
         (row (org-table-current-line))
         (col (org-table-current-column))
         (maxcol (length (org-ml-get-children table)))
         (maxrow (1+ (length cthulhu-fight-rows))))
    (save-excursion 
      (org-table-goto-column 1)
      (org-table-goto-line 1)
      (cl-loop for i from 2 to maxcol
               do (org-table-goto-column i)
               collect (string-trim (substring-no-properties (org-table-get-field)))))))

(defun cthulhu--build-column (rows personnage-name)
  (cons personnage-name
        (loop for row-name in rows
              collect (let ((carac (get-carac (cthulhu-select-character personnage-name) row-name)))
                        (if (not carac)
                            ""
                          (number-to-string (carac-value carac)))))))

(defun cthulhu-fight-new-fight (fighters-list)
  "Créer un tableau afin de suivre le combat entre les participants
présents dans fighters-list."
  (let* ((fighters-listo (--sort (> (get-carac-value it "DEX") (get-carac-value other "DEX")) fighters-list))
         (fighters-name (--map (cthulhu--get-name it) fighters-listo)))
    (cons (cons "Personnages" cthulhu-fight-rows)
          (loop for fname in fighters-name
                collect (cthulhu--build-column cthulhu-fight-rows fname)))))

(defun cthulhu-fight-new-fight-select-insert (&optional num)
  "Sélectionne num participants et créé le tableau correspondant
Par défaut insère tous les personnages disponibles.
Sélectionne [argument préfixe] personnages si présent. " 
  (interactive "P")
  (let* ((num (if (not num)
                  (length cthulhu-personnages-liste)
                num))
         (characters-names (cthulhu--get-characters-name))
         (selection (if (= num (length cthulhu-personnages-liste))
                        cthulhu-personnages-liste
                      (loop for i from 1 to num
                            with sel = nil
                            do
                            (let ((p (completing-read (format "Personnage ? %s/%s " i num) characters-names)))
                              (setq characters-names (--remove (string= p it) characters-names))
                              (push (cthulhu-select-character p) sel))
                            finally return sel))))
    (save-excursion 
      (insert (org-ml-to-trimmed-string (apply #'org-ml-build-table!
                                               (cthulhu-fight-new-fight selection)))))
    (org-table-transpose-table-at-point)
    (org-table-insert-hline)
    (org-table-insert-hline t)))

(defun cthulhu-fight-inflict-major-wound (victime-name)
  "Ajoute un marqueur de blessure majeure sur la victime-name"
  (let* ((party (cthulhu-fight-get-current-fighters))
         (col (+ 2 (position victime-name party :test #'string=)))
         (row (+ 2 (position "Blessure grave" cthulhu-fight-rows :test #'string=))))
    (save-excursion
      (org-table-goto-line row)
      (org-table-goto-column col)
      (org-table-blank-field)
      (insert "t")
      (org-table-align))))

(defun cthulhu-fight-fight-back-dodge (victime-name)
  "Incrémente le compteur de ripostes au CaC de la victime-name"
  (let* ((party (cthulhu-fight-get-current-fighters))
         (col (+ 2 (position victime-name party :test #'string=)))
         (row (+ 2 (position "Coups rendus" cthulhu-fight-rows :test #'string=))))
    (save-excursion
      (org-table-goto-line row)
      (org-table-goto-column col)
      (insert (number-to-string (1+ (string-to-number (org-table-blank-field)))))
      (org-table-align))))

;; Remplacer "18" par : (get-carac-value victime "Points de vie") 
;; Une vie de -100 est un état mort
(defun cthulhu-fight-inflict-damage (victime-name &optional rollstr)
  "Met à jour les pdvs et les états de la victime-name en lui infligeant un montant de dégâts"
  (let* ((party (cthulhu-fight-get-current-fighters))
         ;; (victime-name (completing-read "Cible de l'attaque : " party))
         (col (+ 2 (position victime-name party :test #'string=)))
         (row (+ 2 (position "Points de vie" cthulhu-fight-rows :test #'string=)))
         (degats (eval-minibuffer (concat "Dégâts infligés : " rollstr))))
    (save-excursion
      (org-table-goto-line row)
      (org-table-goto-column col)
      (let* ((current-health (string-to-number (org-table-blank-field)))
             (new-health (- current-health degats)))
        (cond
         ((> degats (get-carac-value (cthulhu-select-character victime-name) "Points de vie"))
          (setq new-health -100))
         ((> degats (/ (get-carac-value (cthulhu-select-character victime-name) "Points de vie") 2))
          (cthulhu-fight-inflict-major-wound victime-name)))
        (insert (number-to-string new-health)))
      (org-table-align))))

(defun cthulhu-fight-attack-brawl ()
  (interactive)
  (let* ((attaquant-name (string-trim (substring-no-properties (org-table-get-field))))
         (victime-name (completing-read "Choix de la victime ? " (cthulhu-fight-get-current-fighters)))
         (def (completing-read "Choix de la riposte ? " cthulhu-fight-choices))
         (carac "Corps à corps")
         (acarac (get-carac (cthulhu-select-character attaquant-name) carac))
         (vcarac (get-carac (cthulhu-select-character victime-name) carac)))
    (cond
     ((string= def "Esquiver") (dodge-fun
                                victime-name
                                (cthulhu-roll-success (cthulhu-ask-roll-result attaquant-name acarac) (carac-value acarac))
                                (cthulhu-roll-success (cthulhu-ask-roll-result victime-name vcarac) (carac-value vcarac))
                                ))
     ((string= def "Rendre les coups") (fight-back-fun
                                        victime-name
                                        (cthulhu-roll-success (cthulhu-ask-roll-result attaquant-name acarac) (carac-value acarac))
                                        (cthulhu-roll-success (cthulhu-ask-roll-result victime-name vcarac) (carac-value vcarac))))
     (t nil))))

(defun cthulhu-ask-roll-result (perso-name carac)
  (eval-minibuffer (format "Lancer pour %s. %s "
			   perso-name
			   (carac-to-string carac))))

(defun dodge-fun (victime-name asuccess vsuccess)
  (if (< asuccess vsuccess) (cthulhu-fight-inflict-damage victime-name))
  (cthulhu-fight-fight-back-dodge victime-name))

(defun fight-back-fun (victime-name asuccess vsuccess)
  (if (<= asuccess vsuccess) (cthulhu-fight-inflict-damage victime-name))
  (cthulhu-fight-fight-back-dodge victime-name))
