structure MipsFrame : FRAME =
struct

datatype access = InFrame of int
								| InReg of Temp.temp

type frame = {name : Temp.label, formals: access list, locals: int ref}

fun newFrame {name:Temp.label,formals:bool list} =
	let val locAlloc = ref 0
			val numRegs = ref 0
			fun transFormals (true::formals) =
				let in
						locAlloc := !locAlloc+1;
						InFrame((!locAlloc)*(~4))::transFormals(formals)
				end
				| transFormals (false::formals) =
					let in
							numRegs := !numRegs+1;
							if !numRegs<4
							then (InReg(Temp.newtemp())::transFormals(formals))
							else (locAlloc := !locAlloc+1; InFrame((!locAlloc)*(~4))::transFormals(formals))
					end
			| transFormals [] = []

	in
			{name=name,formals=transFormals(formals),locals=locAlloc}:frame
	end


fun name (a:frame) = #name a

fun formals (a:frame) = #formals a

fun allocLocal (a:frame)(true) =
	let in
			(#locals a) := !(#locals a) + 1;
			InFrame((!(#locals a))*(~4))
	end
	| allocLocal (a:frame)(false) = InReg(Temp.newtemp())

val wordsize = 4 (* bytes *)

fun exp acc exp = case acc of
	InFrame(num) => Tree.MEM(Tree.BINOP(Tree.PLUS, exp, Tree.CONST(num)))
	| InReg(tmp) => Tree.TEMP(tmp)

fun externalCall (s,args) =
	Tree.CALL(Tree.NAME(Temp.namedlabel(s)), args)

type register = string

fun makeRegs 0 = []
	| makeRegs n = Temp.newtemp()::makeRegs(n-1)

val specialregs = makeRegs(5) (* $rv,$fp,$sp,$ra,$0 *)
val argregs = makeRegs(4) (* $a0-$a3 *)
val calleesaves = makeRegs(8) (* $s0-$s7 *)
val callersaves = makeRegs (10) (* $t0-$t9 *)
val registers = ["v1","fp","sp","ra","0","a0","a1","a2","a3","s0","s1","s2","s3","s4","s5","s6","s7","t0","t1","t2","t3","t4","t5","t6","t7","t8","t9"]

fun make_temp_map ((temp,name), map) = Temp.Map.insert(map,temp,name)
val tempMap = foldl make_temp_map Temp.Map.empty (ListPair.zip(specialregs@argregs@calleesaves@callersaves,registers))

fun tempString temp =
	case Temp.Map.find(tempMap,temp) of
			SOME(register) => register
		| NONE => Temp.makestring temp

val FP = List.nth(specialregs,1)
val RV = List.nth(specialregs,0)
val RA = List.nth(specialregs,3)
val SP = List.nth(specialregs,2)

fun procEntryExit1 ({name,formals,locals},body) = body

fun procEntryExit2 ({name,formals,locals},body) =
	let
		val frameoff = ref 8
		fun arg_reg (_,[]) = []
			| arg_reg (n, (InReg(temp)::list)) = if n<4
				then A.OPER{assem="mv `d0, `s0\n", src=[List.nth(argregs,n)], dst=[temp], jump=NONE}::arg_reg(n+1,list) (* from a0-a3 to temp: move *)
				else A.OPER{assem="lw `d0, " ^ (Int.toString (8+4*(!locals+n-4))) ^ "(`s0)\n", src=[FP], dst=[temp], jump=NONE}::arg_reg(n+1,list) (* from stack to temp : lw *)
			| arg_reg (n, (InFrame(num)::list)) =
				if n<4
				then (* from a0-a3 to frame: sw *)
				  let
						val sw = A.OPER{assem="sw `s1, " ^ (Int.toString (!frameoff)) ^ "(`s0)\n", src=[SP, List.nth(argregs,n)], dst=[], jump=NONE}
						val _ = frameoff := !frameoff+4;
				  in
						if Symbol.name name="main" then arg_reg(n+1,list) else sw::arg_reg(n+1,list)
				  end
				else (* from stack to frame: lw sw *)
					let
						val r = Temp.newtemp()
						val lw = A.OPER{assem="lw `d0, " ^ (Int.toString (0-num)) ^ "(`s0)\n", src=[FP], dst=[r], jump=NONE}
						val sw = A.OPER{assem="sw `s1, " ^ (Int.toString (!frameoff)) ^ "(`s0)\n", src=[SP, r], dst=[], jump=NONE}
						val _ = frameoff := !frameoff+4;
					in
						if Symbol.name name="main" then arg_reg(n+1,list) else lw::sw::arg_reg(n+1,list)
					end
		val args = arg_reg(0,formals)
	in
		args @ body @ [Assem.OPER{assem="", src=specialregs @ calleesaves, dst=[],jump=SOME[]}]
	end

fun procEntryExit3 ({name=name, formals=formals, locals=locals}:frame, body : Assem.instr list) =
	let
		val lab = [A.LABEL{assem=(Symbol.name name) ^ ":\n", lab=name}] (* method label *)
		val spdown = [A.OPER{assem="addi `d0, `s0, -" ^ (Int.toString (4*(!locals+12))) ^ "\n", src=[SP], dst=[SP], jump=NONE}](* move sp by wordsize*(locals+1ra+1fp+10t)*)
		val swra = [A.OPER{assem="sw `s1, 0(`s0)\n", src=[SP, RA], dst=[], jump=NONE}] (* sw $ra, 0($sp) *)
		val swfp = [A.OPER{assem="sw `s1, " ^ (Int.toString wordsize) ^ "(`s0)\n", src=[SP, FP], dst=[], jump=NONE}] (* sw $fp, wordsize($sp) *)

		fun save_tregs (_,[]) = []
			| save_tregs (n,treg::list) = A.OPER{assem="sw `s1, " ^ (Int.toString (8+4*(!locals+n))) ^ "(`s0)\n", src=[SP, treg], dst=[], jump=NONE}::save_tregs (n+1,list)
		val scallee = save_tregs (0, callersaves) (* sw $t0-t9, 8+4*locals...48+4*locals($sp) *)

		(* body - see below *)

		val lwra = [A.OPER{assem="lw `d0, 0(`s0)\n", src=[SP], dst=[RA], jump=NONE}] (* lw $ra, 0($sp) *)
		val lwfp = [A.OPER{assem="lw `d0, " ^ (Int.toString wordsize) ^ "(`s0)\n", src=[SP], dst=[FP], jump=NONE}] (* lw $fp, wordsize($sp) *)

		fun load_tregs (_,[]) = []
			| load_tregs (n,treg::list) = A.OPER{assem="lw `d0, " ^ (Int.toString (8+4*(!locals+n))) ^ "(`s0)\n", src=[SP], dst=[treg], jump=NONE}::load_tregs (n+1,list)
		val lcallee = load_tregs (0, callersaves) (* lw $t0-t9, 8+4*locals...48+4*locals($sp) *)

		val spup = [A.OPER{assem="addi `d0, `s0, " ^ (Int.toString (4*(!locals+12))) ^ "\n", src=[SP], dst=[SP], jump=NONE}] (* move sp by wordsize*(locals+1ra+1fp+8s)*)
		val jrra = [Assem.OPER{assem="jr `d0\n", src=[],dst=[RA],jump=NONE}] (* jr $ra *)
		val body' = if Symbol.name name="main" then lab @ body @ jrra else lab @ spdown @ swra @ swfp @ scallee @ body @ lwra @ lwfp @ lcallee @ spup @ jrra
	in
		{prolog = "PROCEDURE " ^ Symbol.name name ^ "\n", body = body', epilog = "END " ^ Symbol.name name ^ "\n"}
	end

datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string


end
