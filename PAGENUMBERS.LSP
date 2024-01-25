;;; =========================================================================
;;; File     : PAGENUMBERS.LSP
;;; Author   : PPBH
;;; Date     : July 27, 2023
;;; Purpose  : Sets page number and total page number.  
;;; =========================================================================

(defun c:reo 
       (
        /
        ;;; local functions
        layoutfield
        layoutfield:layout
        owner
        objectid
        startundo
        endundo
        acdoc
        ;;; local variables
        ctr
        i
        entblk
        entpblk
        ent
		blklist
		blkname
		blk$
        )
  
	(defun acdoc nil
		(vla-get-activedocument (vlax-get-acad-object))
	)
	
	(defun startundo ( doc )
		(endundo doc)
		(vla-startundomark doc)
	)

	(defun endundo ( doc )
		(while (= 8 (logand 8 (getvar 'undoctl)))
			(vla-endundomark doc)
		)
	)
	
	(defun layoutfield ( fld ent )
		(apply fld (list (vlax-ename->vla-object ent)))
		(if (= "ATTRIB" (cdr (assoc 0 (entget ent))))
			(vla-regen (acdoc) acactiveviewport)
		)
		(princ)
	)
	
	(defun layoutfield:layout ( obj )
		(if (and (vlax-property-available-p obj 'islayout) (= :vlax-true (vla-get-islayout obj)))
			(vla-get-layout obj)
			(layoutfield:layout (owner obj))
		)
	)

	(defun owner ( obj )
		(if (vlax-method-applicable-p obj 'ownerid32)
			(vla-objectidtoobject32 (acdoc) (vla-get-ownerid32 obj))
			(vla-objectidtoobject   (acdoc) (vla-get-ownerid   obj))
		)
	)

	(defun objectid ( obj )
		(if (vlax-method-applicable-p (vla-get-utility (acdoc)) 'getobjectidstring)
			(vla-getobjectidstring (vla-get-utility (acdoc)) obj :vlax-false)
			(itoa (vla-get-objectid obj))
		)
	)
	
	(startundo (acdoc))
  
  (setq
        blkss 
			(ssget "_x" 
				(list 
					(cons 0 "INSERT")
					;(cons 2 "TH-Template")
				)
			)
		blklist (list "TH-Template" "TH_HUVERET")
    	ctr 0
	)
  (repeat (setq i (sslength blkss))
		(setq 
			entblk (ssname blkss (setq i (1- i)))
			entpblk (entget entblk)
			ent entblk
			blkname (cdr (assoc 2 entpblk))
		)
		(if (member (strcase blkname) (mapcar 'strcase blklist))
			(progn
				(setq ctr (1+ ctr))
				(while (/= (cdr (assoc 0 (setq entdata (entget (setq ent (entnext ent)))))) "SEQEND")
					(cond
						(
							(and 
								(= "ATTRIB" (cdr (assoc 0 entdata)))
								(= "PAGE" (cdr (assoc 2 entdata)))
							)
							(layoutfield
								'(lambda ( obj )
										(vla-put-textstring obj
											(strcat
												"%<\\AcObjProp Object(%<\\_ObjId "
												(objectid (layoutfield:layout obj))
												">%).TabOrder>%"
											)
										)
									)
								ent
							)
						)
						(
							(and 
								(= "ATTRIB" (cdr (assoc 0 entdata)))
								(= "TOTAL_PAGES" (cdr (assoc 2 entdata)))
								(layoutfield
									'(lambda ( obj )
											(vla-put-textstring obj
												(strcat
													"%<\\AcExpr %<\\AcObjProp Object(%<\\_ObjId "
													(objectid (vla-get-layouts (acdoc)))
													">%).Count>%-1 >%"
												)
											)
										)
									ent 
								)
							)
							
						)
					)
				
				);;; end while
			)
		);;; end if
  	)
  (setq
		blk$
		(if 
			(or
				(> ctr 1)
				(= ctr 0)
			)
			" titleblocks "
			" titleblock "
		)
	)
	(princ (strcat "\n" (itoa ctr) blk$ "modified."))
	(endundo (acdoc))
	(princ)
)