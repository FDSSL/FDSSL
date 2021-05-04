module Pretty where

import Syntax

pretty :: Show a => FDSSLProgram a -> String
pretty f = ">-> >->\nvoid main() {\n" ++ show f ++ "\n}\n>-> >->"
