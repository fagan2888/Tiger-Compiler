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

val tempMap = Temp.Map.empty

fun tempString temp =
	case Temp.Map.find(tempMap,temp) of
			SOME(register) => register
		| NONE => Temp.makestring temp

fun makeRegs (0,[]) = []
	| makeRegs (n,(name::names)) =
		let
				val r = Temp.newtemp();
		in
				Temp.Map.insert(tempMap,r,name);
				r::(makeRegs(n-1, names))
		end
	| makeRegs (n,[]) =
		let
				val r = Temp.newtemp();
		in
				Temp.Map.insert(tempMap,r,NONE);
				r::(makeRegs(n-1,[]))
		end


val specialregs = makeRegs(5,["rv":register,"fp":register,"sp":register,"ra":register,"r0":register]) (* $rv,$fp,$sp,$ra,$0 *)
val argregs = makeRegs(4,[]) (* $a0-$a3 *)
val calleesaves = makeRegs(8,[]) (* $s0-$s7 *)
val callersaves = makeRegs (10,[]) (* $t0-$t9 *)

val FP = List.nth(specialregs,1)
val RV = List.nth(specialregs,0)
val RA = List.nth(specialregs,3)
val SP = List.nth(specialregs,2)

(* TODO *)
fun procEntryExit1 ({name,formals,locals},body) = body

fun procEntryExit2 (frame,body) = body @ [Assem.OPER{assem="", src=specialregs @ calleesaves, dst=[],jump=SOME[]}]

fun procEntryExit3 ({name=name, formals=formals, locals=locals}:frame, body : Assem.instr list) =
	let
		val lab = [A.LABEL{assem=(Symbol.name name) ^ ":\n", lab=name}] (* method label *)
		val spdown = [A.OPER{assem="addi `d0, `s0, -" ^ (Int.toString (4*(!locals+12))) ^ "\n", src=[SP], dst=[SP], jump=NONE}](* move sp by wordsize*(locals+1ra+1fp+10t)*)
		val swra = [A.OPER{assem="sw `s1, 0(`s0)\n", src=[SP, RA], dst=[], jump=NONE}] (* sw $ra, 0($sp) *)
		val swfp = [A.OPER{assem="sw `s1, " ^ (Int.toString wordsize) ^ "(`s0)\n", src=[SP, FP], dst=[], jump=NONE}] (* sw $fp, wordsize($sp) *)
		(* TODO: sw $t0-t9, 8...48($sp) *)
		(* TODO: move $a_ to where used: from a0-a3 to temp: move, from a0-a3 to frame: sw, from stack to temp: lw, from stack to frame: lw sw *)
		fun arg_reg (_,[]) = []
			| arg_reg (n, (InReg(temp)::list)) = A.OPER{assem="mv `s0, $a" ^ (Int.toString n) ^ "\n", src=[temp], dst=[], jump=NONE}::arg_reg(n+1,list)
			| arg_reg (n, (InFrame(num)::list)) = arg_reg(n+1,list) (* TODO *)
		val args = arg_reg(0,formals)
		(* body - see below *)
		val lwra = [A.OPER{assem="lw `d0, 0(`s0)\n", src=[SP], dst=[RA], jump=NONE}] (* lw $ra, 0($sp) *)
		val lwfp = [A.OPER{assem="lw `d0, " ^ (Int.toString wordsize) ^ "(`s0)\n", src=[SP], dst=[FP], jump=NONE}] (* lw $fp, wordsize($sp) *)
		(* TODO: lw $t0-t9, x($sp) *)
		(* TODO: move v_ into used regs *)
		val spup = [A.OPER{assem="addi `d0, `s0, " ^ (Int.toString (4*(!locals+12))) ^ "\n", src=[SP], dst=[SP], jump=NONE}] (* move sp by wordsize*(locals+1ra+1fp+8s)*)
		val jrra = [Assem.OPER{assem="jr `d0\n", src=calleesaves,dst=[RA],jump=NONE}] (* jr $ra *)
		val body' = if Symbol.name name="tiger_main" then lab @ body @ jrra else lab @ spdown @ swra @ swfp @ args @ body @ lwra @ lwfp @ spup @ jrra
	in
		{prolog = "PROCEDURE " ^ Symbol.name name ^ "\n", body = body', epilog = "END " ^ Symbol.name name ^ "\n"}
	end

datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string


end
