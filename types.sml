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
          | BOTTOM

  fun name ty = (case ty of
    RECORD (a,b) => "record"
  | NIL => "nil"
  | INT => "int"
  | STRING => "string"
  | ARRAY (a,b) => "array"
  | NAME (a,b) => ("name " ^ Symbol.name(a))
  | UNIT => "unit"
  | BOTTOM => "bottom"
  )

end
