structure Types =
struct

  type unique = unit ref

  datatype ty =
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	        | NAME of Symbol.symbol * ty option ref
	        | UNIT

  fun print_ty ty = (case ty of
    RECORD (a,b) => print "record\n"
  | NIL => print "nil\n"
  | INT => print "int\n"
  | STRING => print "string\n"
  | ARRAY (a,b) => print "array\n"
  | NAME (a,b) => print "name\n"
  | UNIT => print "unit\n"
  )

end
