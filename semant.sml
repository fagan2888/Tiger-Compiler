structure A = Absyn
structure S = Symbol
structure T = Types
structure E = Env

signature SEMANT =
sig
  val transProg : A.exp -> unit
end

structure Semant : SEMANT =
struct
  structure Translate = struct type exp = unit end
  type expty = {exp: Translate.exp, ty: T.ty}
  type venv = E.enventry S.table
  type tenv = T.ty S.table

  val depth = ref 0

  (* TODO: function mutual recursion *)
  (* TODO: polymorphic type inference *)

  fun transExp (venv, tenv) =
    let
      fun trexp (A.VarExp(var)) = trvar var
        | trexp (A.NilExp) = {exp=(), ty=T.NIL}
        | trexp (A.IntExp(int)) = {exp=(), ty=T.INT}
				| trexp (A.StringExp(string,pos)) = {exp=(), ty=T.STRING}
        | trexp (A.CallExp{func,args,pos}) = check_func (func,args,pos)
        | trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.MinusOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.TimesOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.DivideOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.EqOp,right,pos}) = check_comparision (left, right, pos)
				| trexp (A.OpExp{left,oper=A.NeqOp,right,pos}) = check_comparision (left, right, pos)
				| trexp (A.OpExp{left,oper=A.LtOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.GtOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.LeOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.GeOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp = (), ty=T.INT})
        | trexp (A.RecordExp{fields,typ,pos}) = check_record (fields,typ,pos)
        | trexp (A.SeqExp(expseq)) = check_sequence (expseq)
        | trexp (A.AssignExp{var,exp,pos}) = check_assign (var,exp,pos)
        | trexp (A.IfExp{test=exp1,then'=exp2,else'=exp3,pos=pos}) = check_if (exp1,exp2,exp3,pos)
        | trexp (A.WhileExp{test=exp1,body=exp2,pos=pos}) = check_while (exp1,exp2,pos)
        | trexp (A.ForExp{var=var,escape=bool,lo=exp1,hi=exp2,body=exp3,pos=pos}) = check_for (var,exp1,exp2,exp3,pos)
        | trexp (A.BreakExp(pos)) = check_break(pos)
        | trexp (A.LetExp{decs=decs,body=expseq,pos=pos}) = check_let (decs,expseq,pos)
        | trexp (A.ArrayExp{typ=typ,size=exp1,init=exp2,pos=pos}) = check_array (typ,exp1,exp2,pos)

      and transDecs (venv,tenv,[]) = {venv=venv,tenv=tenv}
        | transDecs (venv,tenv,dec::decs) =
          let
            val {venv=new_venv,tenv=new_tenv} = transDec (venv,tenv,dec)
            (* TODO: circular linked types - maybe in actual_type function keep already visited and throw error if already visited *)
          in
            transDecs (new_venv,new_tenv,decs)
          end

      and transDec (venv,tenv,A.TypeDec(tydecs)) = {venv=venv,tenv=(recursive_type_body ((recursive_type_dec (tenv,tydecs)),tydecs))}
        | transDec (venv,tenv,A.VarDec{name,escape,typ,init,pos}) = {venv=create_var(venv,tenv,name,escape,typ,init,pos),tenv=tenv}
        | transDec (venv,tenv,A.FunctionDec({name,params,body,pos,result=SOME(rt,pos')}::fundecs)) =
					let
            val SOME(result_ty) = S.look(tenv,rt)
						fun transparam {name,escape,typ,pos} =
							case S.look(tenv,typ) of
                SOME t => {name=name,ty=t}
							  | _  => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); {name=name,ty=T.BOTTOM})
						val params' = map transparam params
						val venv' = S.enter(venv,name, E.FunEntry{formals=map #ty params', result=result_ty})
						fun enterparam ({name,ty},venv) = S.enter(venv,name, E.VarEntry{ty=ty})
						val venv'' = foldl enterparam venv' params'
            val  {exp=_,ty=body_ty} = transExp(venv'',tenv) body;
            val _ = if types_equal (body_ty,result_ty) then () else ErrorMsg.error pos ("declared function type and expression do not match")
					in
						transDec(venv',tenv, A.FunctionDec(fundecs))
					end
        | transDec (venv,tenv,A.FunctionDec({name,params,body,pos,result=NONE}::fundecs)) =
          let
            fun transparam {name,escape,typ,pos} =
              case S.look(tenv,typ) of
                SOME t => {name=name,ty=t}
                | _  => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); {name=name,ty=T.BOTTOM})
            val params' = map transparam params
            fun enterparam ({name,ty},venv) = S.enter(venv,name, E.VarEntry{ty=ty})
            val venv' = S.enter(venv,name, E.FunEntry{formals=map #ty params', result=T.BOTTOM}) (* IMPROVE: Use bottom type *)
            val venv'' = foldl enterparam venv' params'
            val  {exp=_,ty=body_ty} = transExp(venv'',tenv) body;
          in
            transDec(venv',tenv, A.FunctionDec(fundecs))
          end
        | transDec (venv,tenv,A.FunctionDec[]) = {venv=venv,tenv=tenv}

      and recursive_type_dec (tenv,[]) = tenv
        | recursive_type_dec (tenv,({name,ty,pos}::tydecs)) = (case S.look(tenv,name) of
            SOME t => (ErrorMsg.error pos ("type already exists: " ^ S.name(name)); recursive_type_dec(tenv,tydecs))
            | NONE => S.enter(recursive_type_dec(tenv,tydecs),name,T.NAME(name, ref NONE)))

      and recursive_type_body (tenv,[]) = tenv
        | recursive_type_body (tenv,({name,ty,pos}::tydecs)) =
          ((case S.look(tenv,name) of
            SOME(Types.NAME(n, typ)) => (typ := SOME(transTy(tenv, ty, pos)); ())
            | _ => ErrorMsg.error pos ("type does not exists: " ^ S.name(name)));
          recursive_type_body (tenv, tydecs))

      and types_equal (t1,t2) = (case t1 of
        T.RECORD(l,u) => if t2=T.NIL then true else t1=t2
        | T.NIL => (case t2 of T.RECORD(l,u) => true | _ => t1=t2)
        | T.BOTTOM => true
        | _ => (case t2 of T.BOTTOM => true | _ => t1=t2))

      and transTy (tenv,ty,pos) = (case ty of
          A.NameTy(s,p) => (case S.look(tenv,s) of
            SOME t => t
            | NONE => (ErrorMsg.error pos ("undefined type: " ^ S.name(s)); T.BOTTOM))
        | A.RecordTy(fieldlist) => T.RECORD(create_fields fieldlist, ref ())
        | A.ArrayTy(s,p) => T.ARRAY(create_array (tenv,s,p),ref ()))

      and create_fields [] = []
        | create_fields ({name,escape,typ,pos}::fieldlist) = (case S.look(tenv, typ) of
            SOME ty => (name,ty)::(create_fields fieldlist)
          | _ => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); (create_fields fieldlist)))

      and create_array (tenv,typ,pos) = (case S.look(tenv,typ) of
          SOME ty => actual_ty (ty,pos)
        | _ => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); T.BOTTOM))

      and create_var (venv,tenv,name,escape,typ,init,pos) =
        let
          val  {exp=_,ty=ty} = transExp (venv,tenv) init
        in
          ((case typ of
              SOME (sym,p) => if (types_equal ((actual_ty((get_type (tenv,sym,p)), pos)),ty)) then () else ErrorMsg.error pos ("declared type " ^ (T.name (actual_ty ( (get_type (tenv,sym,p)), pos))) ^ " and expression " ^ (T.name ty) ^" do not match")
            | NONE => ());
          S.enter(venv,name,E.VarEntry{ty=ty}))
        end

      and get_type (tenv,sym,pos) = (case S.look(tenv,sym) of
            SOME ty => ty
          | NONE => (ErrorMsg.error pos ("undefined type: " ^ S.name(sym)); T.BOTTOM))

      and trvar (A.SimpleVar(id,pos)) = check_simple_var (id,pos)
        | trvar (A.FieldVar(v,id,pos)) = check_field_var (v,id,pos)
        | trvar (A.SubscriptVar(v,exp,pos)) = check_subscript_var (v,exp,pos)

      and check_simple_var (id,pos) = (case S.look(venv, id) of
          SOME (E.VarEntry{ty}) => {exp=(), ty=actual_ty (ty,pos)}
        | _ => (ErrorMsg.error pos ("undefined variable: " ^ S.name(id)); {exp=(), ty=T.BOTTOM}))

      and check_field_var (var,id,pos) =
        let
          val {exp=_,ty=ty} = trvar var
        in
          (case actual_ty (ty,pos) of
              T.RECORD(fieldlist,_) => fields_contain_sym (pos,fieldlist,id)
            | T.BOTTOM => {exp=(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("variable not a record"); {exp=(), ty=T.BOTTOM})) (* IMPROVE: error message *)
        end

      and fields_contain_sym (pos,[],sym) = (ErrorMsg.error pos ("undefined record field : " ^ S.name(sym)); {exp=(), ty=T.BOTTOM})
        | fields_contain_sym (pos,(id,ty)::fieldlist,sym) = if sym=id then {exp=(), ty=ty} else fields_contain_sym (pos,fieldlist,sym)

      and check_subscript_var (var,exp,pos) =
        let
          val _ = check_int(trexp exp,pos)
          val {exp=_,ty=ty} = trvar var
        in
          (case actual_ty (ty,pos) of
              T.ARRAY(ty,_) => {exp=(), ty=ty}
            | T.BOTTOM => {exp=(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("variable not an array"); {exp=(), ty=T.BOTTOM})) (* IMPROVE: error message *)
        end

      and actual_ty (ty,pos) =
        let
          fun check_ty typ = (case typ of
            T.NAME(s, t) => (case !t of
              NONE => (ErrorMsg.error pos ("undefined type: " ^ T.name(typ)); T.BOTTOM)
              | SOME t => check_ty t)
            | _ => typ)
        in
          check_ty ty
        end

      and check_func (func,args,pos) =  (case S.look(venv, func) of
          SOME (E.FunEntry{formals=tylist, result=ty}) => check_args(args,tylist,ty,pos)
        | _ => (ErrorMsg.error pos ("undefined function: " ^ S.name(func)); {exp=(), ty=T.BOTTOM}))

      and check_args (args,tylist,ty,pos) =
        let
          fun check_arg_types ((exp::exps),(t::tys)) =
            let
              val {exp=_,ty=typ} = trexp exp
            in
              if types_equal(typ,t) then check_arg_types (exps,tys) else (ErrorMsg.error pos ("argument type does not match declared type"); false)
            end
          | check_arg_types ([],[]) = true
          | check_arg_types (_,_) = false
          val return_ty = if List.length(tylist)=List.length(args) then (if check_arg_types(args,tylist) then ty else T.BOTTOM) else (ErrorMsg.error pos ("incorrect number of arguments"); T.BOTTOM)
        in
          {exp=(), ty=return_ty}
        end

      and check_record (fields,typ,pos) = (case S.look(tenv, typ) of
          SOME t => (case actual_ty (t,pos) of
            T.RECORD(typelist,_) => check_field_types (fields, typelist, pos, typ)
            | T.BOTTOM => {exp=(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("not record type: " ^ S.name(typ)); {exp=(), ty=T.BOTTOM}))
        | NONE => (ErrorMsg.error pos ("undefined record: " ^ S.name(typ)); {exp=(), ty=T.BOTTOM}))

      and check_field_types (fields, types, pos, record_ty) =
        let
          val return_ty = if List.length(fields)=List.length(types) then get_type (tenv,record_ty,pos) else (ErrorMsg.error pos ("incorrect number of record fields"); T.UNIT) (* IMPROVE: error message, check duplicates and num *)
          fun compare_field_types ((sym,exp,pos)::fields, types) = if type_exists (sym,exp,types) then compare_field_types (fields, types) else false
            | compare_field_types ([], types) = true
          and type_exists (sym,exp,(s,t)::types) = if (S.name sym)=(S.name s) then (if exp_matches (exp,t) then true else false) else type_exists (sym,exp,types)
            | type_exists (sym,exp,[]) = (ErrorMsg.error pos ("record field does not exist"); false) (* IMPROVE: error message *)
          and exp_matches (exp,ty) =
            let
              val {exp=_,ty=typ} = trexp exp
            in
              if types_equal(ty,typ) then true else (ErrorMsg.error pos ("record field type does not match expression"); false) (* IMPROVE: error message *)
            end
        in
          if compare_field_types (fields, types) then {exp=(), ty=return_ty} else {exp=(), ty=T.BOTTOM}
        end

      and check_sequence [] = {exp=(), ty=Types.UNIT}
        | check_sequence ((exp, _)::nil) = trexp exp
        | check_sequence ((exp, _)::expseq) = (trexp exp; check_sequence (expseq))

      and check_assign (var,exp,pos) =
        let
          val {exp=_,ty=tyl} = trvar var
          val {exp=_,ty=tyr} = trexp exp
        in
          if types_equal (tyl,tyr)
          then {exp=(), ty=T.UNIT}
          else (ErrorMsg.error pos ("variable and expresion of different type"); {exp=(), ty=T.BOTTOM}) (* IMPROVE: error message *)
        end

      and check_if (exp1,exp2,exp3,pos) =
        (check_int (trexp exp1,pos);
        (case exp3 of
            SOME exp =>
              let
                val {exp=_,ty=ty2} = trexp exp2
                val {exp=_,ty=ty3} = trexp exp
              in
                if types_equal(ty2,ty3) then {exp=(), ty=ty2} else (ErrorMsg.error pos ("types of then - else differ");{exp=(), ty=T.BOTTOM}) (* IMPROVE: what if the type ty2 is a subtype (want supertype)*)
              end
          | NONE => (check_unit (trexp exp2,pos); {exp=(), ty=T.UNIT})))

      and check_while (exp1,exp2,pos) =
        (depth := !depth + 1;
         check_int (trexp exp1,pos);
         check_unit (trexp exp2,pos);
         depth := !depth - 1;
         {exp=(), ty=T.UNIT})

      and check_for (var,exp1,exp2,exp3,pos) =
        (depth := !depth + 1;
         check_int (trexp exp1,pos);
         check_int (trexp exp2,pos);
         check_unit (transExp(S.enter(venv,var,E.VarEntry{ty=T.INT}),tenv) exp3,pos);
         depth := !depth - 1;
         {exp=(), ty=T.UNIT})

      and check_break (pos) =
        (if !depth>0 then () else ErrorMsg.error pos ("break must be inside loop");
         {exp=(), ty=T.UNIT})

      and check_let (decs,expseq,pos) =
        let
          val curr_depth = depth
          val _ = depth := 0
          val {venv=new_venv,tenv=new_tenv} = transDecs (venv,tenv,decs)
          val _ = depth := !curr_depth

        in
          transExp (new_venv,new_tenv) expseq
        end

      and check_array (typ,exp1,exp2,pos) = (case S.look(tenv, typ) of
          SOME t => (case actual_ty (t,pos) of
            T.ARRAY(ty,_) => check_array_init (t,ty,exp1,exp2,pos)
            | T.BOTTOM => {exp=(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("not array type: " ^ S.name(typ)); {exp=(), ty=T.BOTTOM}))
        | NONE => (ErrorMsg.error pos ("undefined array: " ^ S.name(typ)); {exp=(), ty=T.BOTTOM}))

      and check_array_init (t,ty,exp1,exp2,pos) =
        let
          val {exp=_,ty=typ} = trexp exp2
          val return_ty = if types_equal (typ,ty) then t else (ErrorMsg.error pos ("array initialization does not match type"); T.BOTTOM)
        in
          (check_int (trexp exp1,pos);
           {exp=(), ty=return_ty})
        end

      and check_int ({exp=_,ty=T.INT},_) = ()
        | check_int ({exp=_,ty=T.BOTTOM},_) = ()
      	| check_int ({exp=_,ty=_},pos) = ErrorMsg.error pos "integer argument expected"

      and check_unit ({exp=_,ty=T.UNIT},_) = ()
        | check_unit ({exp=_,ty=T.BOTTOM},_) = ()
        | check_unit ({exp=_,ty=_},pos) = ErrorMsg.error pos "unit argument expected"

      and check_comparision (exp1,exp2,pos) =
        let
          val {exp=_,ty=ty1} = trexp exp1
          val {exp=_,ty=ty2} = trexp exp2
        in
          if types_equal (ty1,ty2) then {exp=(),ty=ty1} else {exp=(),ty=T.BOTTOM} (* IMPROVE: what if the type ty1 is a subtype (want supertype)*)
        end
    in
      trexp
    end

  fun transProg exp =
    let
      val expty = (transExp (E.base_venv, E.base_tenv) exp)
    in
      ()
    end

end
