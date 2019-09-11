(
 (desc (Basic (Op (Intop_imm Iadd -2))))
 (arg (0))
 (res (0))
)

(
 (desc (Terminator
        (Branch
         (((Test (Iinttest (Isigned Cgt))) 101)
          ((Test (Iinttest (Isigned Cle))) 127)))))
 (arg (1 0))
 (res ())
)

(
 (desc (Basic (Op Spill)))
 (arg (0))
 (res (2))
)
