;;; =========================================================================
;;; File     : PAGENUMBERS.LSP
;;; Purpose  : Sets page number and total page number.  
;;; =========================================================================

(defun c:reo 
       (
        / layoutfield layoutfield:layout owner objectid startundo endundo acdoc
        ctr i entblk entpblk ent blklist blkname blk$
        )
  
  ;; Function to get the active document
  (defun acdoc nil
    (vla-get-activedocument (vlax-get-acad-object))
    )

  ;; Function to start undo mark
  (defun startundo ( doc )
    (endundo doc)
    (vla-startundomark doc)
    )

  ;; Function to end undo mark
  (defun endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
      (vla-endundomark doc)
      )
    )

  ;; Function to update attributes for a layout field
  (defun layoutfield ( fld ent )
    (apply fld (list (vlax-ename->vla-object ent)))
    (if (= "ATTRIB" (cdr (assoc 0 (entget ent))))
        (progn
          (vla-regen (acdoc) acactiveviewport)
          (setq ctr (1+ ctr)) ;; Increment the counter
        )
    )
    (princ)
    )

  ;; Function to get the layout associated with an object
  (defun layoutfield:layout ( obj )
    (if (and (vlax-property-available-p obj 'islayout) (= :vlax-true (vla-get-islayout obj)))
        (vla-get-layout obj)
        (layoutfield:layout (owner obj))
      )
    )

  ;; Function to get the owner of an object
  (defun owner ( obj )
    (if (vlax-method-applicable-p obj 'ownerid32)
        (vla-objectidtoobject32 (acdoc) (vla-get-ownerid32 obj))
        (vla-objectidtoobject   (acdoc) (vla-get-ownerid   obj))
      )
    )

  ;; Function to get the object ID of an object
  (defun objectid ( obj )
    (if (vlax-method-applicable-p (vla-get-utility (acdoc)) 'getobjectidstring)
        (vla-getobjectidstring (vla-get-utility (acdoc)) obj :vlax-false)
        (itoa (vla-get-objectid obj))
      )
    )
  
  ;; Start undo operation
  (startundo (acdoc))

  ;; Reset the counter to 0
  (setq ctr 0)

  ;; ... (Existing code remains unchanged)

  ;; Set the block suffix based on the counter value
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
  
  ;; Display a message indicating the modification
  (princ (strcat "\n" (itoa ctr) blk$ "modified."))

  ;; End undo operation
  (endundo (acdoc))

  ;; End the function
  (princ)
  )