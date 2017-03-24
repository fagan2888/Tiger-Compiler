signature SEMANT =
sig
  val transProg : Absyn.exp -> unit
end

structure Semant : SEMANT =
struct
  structure A = Absyn
  structure S = Symbol
  structure T = Types
  structure E = Env
  structure R = Translate

  type expty = {exp: R.exp, ty: T.ty}
  type venv = E.enventry S.table
  type tenv = T.ty S.table

  val depth = ref 0

  (* TODO: polymorphic type inference *)

  fun transExp (venv, tenv, break, level) =
    let
      fun trexp (A.VarExp(var)) = trvar var
        | trexp (A.NilExp) = {exp=R.nilExp(), ty=T.NIL}
        | trexp (A.IntExp(int)) = {exp=R.intExp(int), ty=T.INT}
				| trexp (A.StringExp(string,pos)) = {exp=R.stringExp(string), ty=T.STRING}
        | trexp (A.CallExp{func,args,pos}) = check_func (func,args,pos)
        | trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.PlusOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.MinusOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.MinusOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.TimesOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.TimesOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.DivideOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.DivideOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.EqOp,right,pos}) = check_comparision (A.EqOp, left, right, pos)
				| trexp (A.OpExp{left,oper=A.NeqOp,right,pos}) = check_comparision (A.NeqOp, left, right, pos)
				| trexp (A.OpExp{left,oper=A.LtOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.LtOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.GtOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.GtOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.LeOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.LeOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
				| trexp (A.OpExp{left,oper=A.GeOp,right,pos}) = (check_int(trexp left, pos); check_int(trexp right, pos); {exp=R.opExp(A.GeOp,#exp (trexp left),#exp (trexp right)), ty=T.INT})
        | trexp (A.RecordExp{fields,typ,pos}) = check_record (fields,typ,pos)
        | trexp (A.SeqExp(expseq)) = check_sequence (expseq)
        | trexp (A.AssignExp{var,exp,pos}) = check_assign (var,exp,pos)
        | trexp (A.IfExp{test=exp1,then'=exp2,else'=exp3,pos=pos}) = check_if (exp1,exp2,exp3,pos)
        | trexp (A.WhileExp{test=exp1,body=exp2,pos=pos}) = check_while (exp1,exp2,pos)
        | trexp (A.ForExp{var=var,escape=bool,lo=exp1,hi=exp2,body=exp3,pos=pos}) = check_for (var,exp1,exp2,exp3,pos)
        | trexp (A.BreakExp(pos)) = check_break(pos)
        | trexp (A.LetExp{decs=decs,body=expseq,pos=pos}) = check_let (decs,expseq,pos)
        | trexp (A.ArrayExp{typ=typ,size=exp1,init=exp2,pos=pos}) = check_array (typ,exp1,exp2,pos)

      and transDecs (venv,tenv,[],expseq) = {venv=venv,tenv=tenv,expseq=expseq}
        | transDecs (venv,tenv,dec::decs,expseq) =
          let
            val {venv=new_venv,tenv=new_tenv,expseq=new_expseq} = transDec (venv,tenv,dec,expseq)
          in
            transDecs (new_venv,new_tenv,decs,new_expseq)
          end

      and transDec (venv,tenv,A.TypeDec(tydecs),expseq) =
          let
            val tenv' = (recursive_type_body ((recursive_type_dec (tenv,[],tydecs)),tydecs))
            val _ = check_cycles (tenv', tydecs)
          in
            {venv=venv,tenv=tenv',expseq=expseq}
          end
        | transDec (venv,tenv,A.VarDec{name,escape,typ,init,pos},expseq) = create_var(venv,tenv,name,escape,typ,init,pos,expseq)
        | transDec (venv,tenv,A.FunctionDec(fundecs),expseq) = {venv=(recursive_func_body ((recursive_func_dec (venv,tenv,[],fundecs)),tenv,fundecs)),tenv=tenv,expseq=expseq}

      and recursive_type_dec (tenv,list,[]) = tenv
        | recursive_type_dec (tenv,list,({name,ty,pos}::tydecs)) = (type_declared (name,list,pos); S.enter(recursive_type_dec(tenv,name::list,tydecs),name,T.NAME(name, ref NONE)))

      and type_declared (n,[],_) = ()
        | type_declared (n, t::list,pos) = if S.name(n)=S.name(t) then ErrorMsg.error pos ("recursive types with same name: " ^ S.name(n)) else type_declared (n,list,pos)

      and recursive_type_body (tenv,[]) = tenv
        | recursive_type_body (tenv,({name,ty,pos}::tydecs)) =
          ((case S.look(tenv,name) of
            SOME(Types.NAME(n, typ)) => (typ := SOME(transTy(tenv, ty, pos)); ())
            | _ => ErrorMsg.error pos ("type does not exists: " ^ S.name(name)));
          recursive_type_body (tenv, tydecs))

      and recursive_func_dec (venv, tenv, list, []) = venv
        | recursive_func_dec (venv, tenv, list, ({name,params,body,pos,result}::fundecs)) =
          let
            val result_ty = (case result of
              SOME (rt, pos') => (case S.look(tenv,rt) of SOME ty => ty | NONE => (ErrorMsg.error pos ("type does not exists: " ^ S.name(rt)); T.BOTTOM))
              | NONE => T.BOTTOM)
            fun transparam {name,escape,typ,pos} =
              case S.look(tenv,typ) of
                SOME t => {name=name,escape=escape,ty=t}
                | _  => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); {name=name,escape=escape,ty=T.BOTTOM})
            val params' = map transparam params
            val label = Temp.newlabel()
            fun determine_escape {name, escape, ty}  = !escape
            val escape = (map determine_escape params')
            val newlevel = R.newLevel{parent=level,name=label,formals=escape}
            val venv' = S.enter(venv, name, E.FunEntry{level=newlevel, label=label, formals=map #ty params', result=result_ty})
            val _ = func_declared (name,list,pos)
          in
            recursive_func_dec(venv', tenv, name::list, fundecs)
          end

      and func_declared (n,[],_) = ()
        | func_declared (n, t::list,pos) = if S.name(n)=S.name(t) then ErrorMsg.error pos ("recursive functions with same name: " ^ S.name(n)) else func_declared (n,list,pos)


      and recursive_func_body (venv,tenv,[]) = venv
        | recursive_func_body (venv,tenv,({name,params,body,pos,result}::fundecs)) =
          let
            fun transparam {name,escape,typ,pos} =
              case S.look(tenv,typ) of
                SOME t => {name=name,ty=t}
                | _  => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); {name=name,ty=T.BOTTOM})
            val params' = map transparam params
            val access = R.allocLocal level false
            fun enterparam ({name,ty},venv) = S.enter(venv,name, E.VarEntry{access=access,ty=ty})
            val venv'' = foldl enterparam venv params'
            val (rt,lvl) = case S.look(venv,name) of
              SOME (E.FunEntry{level=lvl,label=lbl,formals=f,result=return_ty}) => (return_ty,lvl)
              | _ => (ErrorMsg.error pos ("function does not exists: " ^ S.name(name)); (T.BOTTOM,R.outermost))
            val  {exp=exp,ty=body_ty} = transExp(venv'',tenv, break, lvl) body
            val _ = if types_equal (actual_ty (body_ty,pos),actual_ty (rt,pos)) then () else ErrorMsg.error pos ("declared function type and expression do not match")
          in
            R.procEntryExit {level=lvl, body=exp};
            recursive_func_body (venv, tenv,fundecs)
          end

      and types_equal (t1,t2) = (case t1 of
        T.RECORD(l,u) => if t2=T.NIL then true else t1=t2
        | T.NIL => (case t2 of T.RECORD(l,u) => true | _ => t1=t2)
        | T.BOTTOM => true
        | _ => (case t2 of T.BOTTOM => true | _ => t1=t2))

      and transTy (tenv,ty,pos) = (case ty of
          A.NameTy(s,p) => (case S.look(tenv,s) of
            SOME t => t
            | NONE => (ErrorMsg.error pos ("undefined type: " ^ S.name(s)); T.BOTTOM))
        | A.RecordTy(fieldlist) => T.RECORD(create_fields (tenv, fieldlist), ref ())
        | A.ArrayTy(s,p) => T.ARRAY(create_array (tenv,s,p),ref ()))

      and create_fields (tenv,[]) = []
        | create_fields (tenv, ({name,escape,typ,pos}::fieldlist)) = (case S.look(tenv, typ) of
            SOME ty => (name,ty)::(create_fields (tenv,fieldlist))
          | _ => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); (create_fields (tenv,fieldlist))))

      and create_array (tenv,typ,pos) = (case S.look(tenv,typ) of
          SOME ty => actual_ty (ty,pos)
        | _ => (ErrorMsg.error pos ("undefined type: " ^ S.name(typ)); T.BOTTOM))

      and create_var (venv,tenv,name,escape,typ,init,pos,expseq) =
        let
          val  {exp=exp,ty=ty} = transExp (venv,tenv,break,level) init
          val access = R.allocLocal level (!escape)
          val new_expseq = (R.assignExp(R.simpleVar(access, level),exp))::expseq
        in
          ((case typ of
              SOME (sym,p) => if types_equal (actual_ty ((get_type (tenv,sym,p)),p),actual_ty (ty,pos)) then () else ErrorMsg.error pos ("declared type " ^ (T.name (get_type (tenv,sym,p))) ^ " and expression " ^ (T.name ty) ^" do not match")
            | NONE => (if ty=T.NIL then ErrorMsg.error pos ("initializing nil expressions not constrained by record type") else () ));
          {venv=S.enter(venv,name,E.VarEntry{access=access,ty=ty}),tenv=tenv,expseq=new_expseq})
        end

      and get_type (tenv,sym,pos) = (case S.look(tenv,sym) of
            SOME ty => ty
          | NONE => (ErrorMsg.error pos ("undefined type: " ^ S.name(sym)); T.BOTTOM))

      and trvar (A.SimpleVar(id,pos)) = check_simple_var (id,pos)
        | trvar (A.FieldVar(v,id,pos)) = check_field_var (v,id,pos)
        | trvar (A.SubscriptVar(v,exp,pos)) = check_subscript_var (v,exp,pos)

      and check_simple_var (id,pos) = (case S.look(venv, id) of
          SOME (E.VarEntry{access,ty}) => {exp=R.simpleVar(access,level), ty=actual_ty (ty,pos)}
        | _ => (ErrorMsg.error pos ("undefined variable: " ^ S.name(id)); {exp=R.nilExp(), ty=T.BOTTOM}))

      and check_field_var (var,id,pos) =
        let
          val {exp=_,ty=ty} = trvar var
        in
          (case actual_ty (ty,pos) of
              T.RECORD(fieldlist,_) => fields_contain_sym (pos,fieldlist,id)
            | T.BOTTOM => {exp=R.nilExp(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("variable not a record"); {exp=R.nilExp(), ty=T.BOTTOM})) (* IMPROVE: error message *)
        end

      and fields_contain_sym (pos,[],sym) = (ErrorMsg.error pos ("undefined record field : " ^ S.name(sym)); {exp=R.nilExp(), ty=T.BOTTOM})
        | fields_contain_sym (pos,(id,ty)::fieldlist,sym) = if sym=id then {exp=(R.nilExp())(*TODO*), ty=ty} else fields_contain_sym (pos,fieldlist,sym)

      and check_subscript_var (var,exp,pos) =
        let
          val _ = check_int(trexp exp,pos)
          val {exp=_,ty=ty} = trvar var
        in
          (case actual_ty (ty,pos) of
              T.ARRAY(ty,_) => {exp=R.nilExp() (*TODO*), ty=ty}
            | T.BOTTOM => {exp=R.nilExp(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("variable not an array"); {exp=R.nilExp(), ty=T.BOTTOM})) (* IMPROVE: error message *)
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
          SOME (E.FunEntry{level=lvl,label=lbl,formals=tylist, result=ty}) => check_args(lbl,args,tylist,ty,pos)
        | _ => (ErrorMsg.error pos ("undefined function: " ^ S.name(func)); {exp=R.nilExp(), ty=T.BOTTOM}))

      and check_args (lbl,args,tylist,ty,pos) =
        let
          fun check_arg_types ((exp::exps),(t::tys)) =
            let
              val {exp=_,ty=typ} = trexp exp
            in
              if types_equal(actual_ty (typ,pos),actual_ty (t,pos)) then check_arg_types (exps,tys) else (ErrorMsg.error pos ("argument type does not match declared type"); false)
            end
          | check_arg_types ([],[]) = true
          | check_arg_types (_,_) = false
          val return_ty = if List.length(tylist)=List.length(args) then (if check_arg_types(args,tylist) then ty else T.BOTTOM) else (ErrorMsg.error pos ("incorrect number of arguments"); T.BOTTOM)
          fun map_args arg = #exp (trexp arg)
        in
          {exp=R.callExp(lbl,(map map_args args)), ty=return_ty}
        end

      and check_record (fields,typ,pos) = (case S.look(tenv, typ) of
          SOME t => (case actual_ty (t,pos) of
            T.RECORD(typelist,_) => check_field_types (fields, typelist, pos, typ)
            | T.BOTTOM => {exp=R.nilExp(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("not record type: " ^ S.name(typ)); {exp=R.nilExp(), ty=T.BOTTOM}))
        | NONE => (ErrorMsg.error pos ("undefined record: " ^ S.name(typ)); {exp=R.nilExp(), ty=T.BOTTOM}))

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
              if types_equal(actual_ty (ty,pos),actual_ty (typ,pos)) then true else (ErrorMsg.error pos ("record field type does not match expression"); false) (* IMPROVE: error message *)
            end
        in
          if compare_field_types (fields, types) then {exp=(R.nilExp()) (*TODO*), ty=return_ty} else {exp=R.nilExp(), ty=T.BOTTOM}
        end

      and check_sequence (expseq) =
          let
            fun map_expseq (exp,pos) = #exp (trexp exp)
            fun trseq [] = {exp=R.nilExp(), ty=Types.UNIT}
              | trseq ((exp, _)::nil) = {exp=R.seqExp(map map_expseq expseq), ty=(#ty (trexp exp))}
              | trseq ((exp, _)::exps) = (trexp exp; trseq (exps))
          in
            trseq (expseq)
          end

      and check_assign (var,exp,pos) =
        let
          val {exp=v,ty=tyl} = trvar var
          val {exp=e,ty=tyr} = trexp exp
        in
          if types_equal (actual_ty (tyl,pos),actual_ty (tyr,pos))
          then {exp=R.assignExp(v,e), ty=T.UNIT}
          else (ErrorMsg.error pos ("variable and expresion of different type"); {exp=R.nilExp(), ty=T.BOTTOM}) (* IMPROVE: error message *)
        end

      and check_if (exp1,exp2,exp3,pos) =
        (check_int (trexp exp1,pos);
        (case exp3 of
            SOME exp =>
              let
                val {exp=e2,ty=ty2} = trexp exp2
                val {exp=e3,ty=ty3} = trexp exp
              in
                if types_equal(actual_ty (ty2,pos),actual_ty (ty3,pos)) then {exp=R.ifThenElseExp(#exp (trexp exp1),e2,e3), ty=ty2} else (ErrorMsg.error pos ("types of then - else differ");{exp=R.nilExp(), ty=T.BOTTOM}) (* IMPROVE: what if the type ty2 is a subtype (want supertype)*)
              end
          | NONE => (check_unit (trexp exp2,pos); {exp=R.ifThenExp(#exp (trexp exp1),#exp (trexp exp2)), ty=T.UNIT})))

      and check_while (exp1,exp2,pos) =
        let
          val break_label = Temp.newlabel()
        in
          (depth := !depth + 1;
          check_int (trexp exp1,pos);
          check_unit (transExp(venv,tenv,break_label,level) exp2,pos);
          depth := !depth - 1;
          {exp=R.whileExp(break_label,#exp (trexp exp1),#exp (trexp exp2)), ty=T.UNIT})
        end

      and check_for (var,exp1,exp2,exp3,pos) =
        let
          val break_label = Temp.newlabel()
          val access = R.allocLocal level false
          val i = R.simpleVar(access, level)
        in
          (depth := !depth + 1;
          check_int (trexp exp1,pos);
          check_int (trexp exp2,pos);
          check_unit (transExp(S.enter(venv,var,E.VarEntry{access=access,ty=T.INT}),tenv,break_label,level) exp3,pos);
          depth := !depth - 1;
          {exp=R.forExp(break_label,i,#exp (trexp exp1),#exp (trexp exp2),#exp (trexp exp3)), ty=T.UNIT})
        end

      and check_break (pos) =
        (if !depth>0 then () else ErrorMsg.error pos ("break must be inside loop");
         {exp=R.breakExp(break), ty=T.UNIT})

      and check_let (decs,expseq,pos) =
        let
          val curr_depth = depth
          val _ = depth := 0
          val {venv=new_venv,tenv=new_tenv,expseq=decs_expseq} = transDecs (venv,tenv,decs,[])
          val expty = transExp (new_venv,new_tenv,break,level) expseq
          val _ = depth := !curr_depth
        in
          {exp=R.seqExp(decs_expseq @ [#exp expty]), ty=(#ty expty)}
        end

      and check_array (typ,exp1,exp2,pos) = (case S.look(tenv, typ) of
          SOME t => (case actual_ty (t,pos) of
            T.ARRAY(ty,_) => check_array_init (t,ty,exp1,exp2,pos)
            | T.BOTTOM => {exp=R.nilExp(), ty=T.BOTTOM}
            | _ => (ErrorMsg.error pos ("not array type: " ^ S.name(typ)); {exp=R.nilExp(), ty=T.BOTTOM}))
        | NONE => (ErrorMsg.error pos ("undefined array: " ^ S.name(typ)); {exp=R.nilExp(), ty=T.BOTTOM}))

      and check_array_init (t,ty,exp1,exp2,pos) =
        let
          val {exp=_,ty=typ} = trexp exp2
          val return_ty = if types_equal (actual_ty (typ,pos),actual_ty (ty,pos)) then t else (ErrorMsg.error pos ("array initialization does not match type"); T.BOTTOM)
        in
          (check_int (trexp exp1,pos);
           {exp=R.nilExp() (*TODO*), ty=return_ty})
        end

      and check_cycles (tenv, []) = ()
        | check_cycles (tenv, ({name,ty,pos}::tydecs)) =
          let
            val SOME(t) = S.look(tenv,name)
            fun typ_visited (t, []) = false
              | typ_visited (t, ty::tylist) = if S.name(t) = S.name(ty) then true else typ_visited (t, tylist)
            fun cycle (typ, list) = (case typ of
              T.NAME(s, topt) => if typ_visited (s, list) then ErrorMsg.error pos ("type cycle: " ^ S.name(s)) else (case !topt of SOME t => cycle (t,s::list)| NONE => ()) (* if no add to list and go deeper*)
              | _ => ())
            val _ = cycle (t,[])
          in
            check_cycles (tenv, tydecs)
          end

      and check_int ({exp=_,ty=T.INT},_) = ()
        | check_int ({exp=_,ty=T.BOTTOM},_) = ()
      	| check_int ({exp=_,ty=t},pos) = ErrorMsg.error pos ("integer argument expected")

      and check_unit ({exp=_,ty=T.UNIT},_) = ()
        | check_unit ({exp=_,ty=T.BOTTOM},_) = ()
        | check_unit ({exp=_,ty=_},pos) = ErrorMsg.error pos "unit argument expected"

      and check_comparision (oper,exp1,exp2,pos) =
        let
          val {exp=e1,ty=ty1} = trexp exp1
          val {exp=e2,ty=ty2} = trexp exp2
        in
          if types_equal (actual_ty (ty1,pos),actual_ty (ty2,pos)) then {exp=R.opExp(oper,e1,e2),ty=T.INT} else (ErrorMsg.error pos ("types do not match"); {exp=R.nilExp(), ty=T.BOTTOM}) (* IMPROVE: what if the type ty1 is a subtype (want supertype)*)
        end
    in
      trexp
    end

  fun transProg exp =
    let
      val level = R.newLevel{parent=R.outermost,name=Temp.newlabel(),formals=[]}
      val expty = (transExp (E.base_venv, E.base_tenv, Temp.newlabel(), level) exp)
      val tree = R.unNx (#exp expty)
      val _ = Printtree.printtree (TextIO.stdOut,tree)
    in
      R.procEntryExit {level=level, body=(#exp expty)}
    end

end
