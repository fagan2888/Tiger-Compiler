functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
val S = Symbol.symbol


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\178\000\005\000\178\000\007\000\178\000\009\000\178\000\
\\011\000\178\000\013\000\178\000\015\000\037\000\016\000\036\000\
\\018\000\035\000\019\000\034\000\027\000\178\000\028\000\178\000\
\\030\000\178\000\031\000\178\000\032\000\178\000\037\000\178\000\
\\038\000\178\000\041\000\178\000\042\000\178\000\044\000\178\000\
\\045\000\178\000\000\000\
\\001\000\001\000\179\000\005\000\179\000\007\000\179\000\009\000\179\000\
\\011\000\179\000\013\000\179\000\015\000\037\000\016\000\036\000\
\\018\000\035\000\019\000\034\000\027\000\179\000\028\000\179\000\
\\030\000\179\000\031\000\179\000\032\000\179\000\037\000\179\000\
\\038\000\179\000\041\000\179\000\042\000\179\000\044\000\179\000\
\\045\000\179\000\000\000\
\\001\000\001\000\180\000\005\000\180\000\007\000\180\000\009\000\180\000\
\\011\000\180\000\013\000\180\000\015\000\037\000\016\000\036\000\
\\018\000\035\000\019\000\034\000\027\000\180\000\028\000\180\000\
\\030\000\180\000\031\000\180\000\032\000\180\000\037\000\180\000\
\\038\000\180\000\041\000\180\000\042\000\180\000\044\000\180\000\
\\045\000\180\000\000\000\
\\001\000\001\000\181\000\005\000\181\000\007\000\181\000\009\000\181\000\
\\011\000\181\000\013\000\181\000\015\000\037\000\016\000\036\000\
\\018\000\035\000\019\000\034\000\027\000\181\000\028\000\181\000\
\\030\000\181\000\031\000\181\000\032\000\181\000\037\000\181\000\
\\038\000\181\000\041\000\181\000\042\000\181\000\044\000\181\000\
\\045\000\181\000\000\000\
\\001\000\001\000\182\000\005\000\182\000\007\000\182\000\009\000\182\000\
\\011\000\182\000\013\000\182\000\015\000\037\000\016\000\036\000\
\\018\000\035\000\019\000\034\000\027\000\182\000\028\000\182\000\
\\030\000\182\000\031\000\182\000\032\000\182\000\037\000\182\000\
\\038\000\182\000\041\000\182\000\042\000\182\000\044\000\182\000\
\\045\000\182\000\000\000\
\\001\000\001\000\183\000\005\000\183\000\007\000\183\000\009\000\183\000\
\\011\000\183\000\013\000\183\000\015\000\037\000\016\000\036\000\
\\018\000\035\000\019\000\034\000\027\000\183\000\028\000\183\000\
\\030\000\183\000\031\000\183\000\032\000\183\000\037\000\183\000\
\\038\000\183\000\041\000\183\000\042\000\183\000\044\000\183\000\
\\045\000\183\000\000\000\
\\001\000\002\000\022\000\003\000\021\000\004\000\020\000\008\000\019\000\
\\009\000\087\000\016\000\018\000\033\000\017\000\034\000\016\000\
\\036\000\015\000\039\000\014\000\040\000\013\000\043\000\012\000\000\000\
\\001\000\002\000\022\000\003\000\021\000\004\000\020\000\008\000\019\000\
\\016\000\018\000\033\000\017\000\034\000\016\000\036\000\015\000\
\\039\000\014\000\040\000\013\000\043\000\012\000\000\000\
\\001\000\002\000\046\000\000\000\
\\001\000\002\000\056\000\000\000\
\\001\000\002\000\072\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\074\000\000\000\
\\001\000\002\000\083\000\000\000\
\\001\000\002\000\083\000\013\000\082\000\000\000\
\\001\000\002\000\107\000\012\000\106\000\035\000\105\000\000\000\
\\001\000\002\000\109\000\000\000\
\\001\000\002\000\128\000\000\000\
\\001\000\002\000\133\000\000\000\
\\001\000\002\000\137\000\000\000\
\\001\000\002\000\143\000\000\000\
\\001\000\002\000\147\000\000\000\
\\001\000\006\000\092\000\029\000\091\000\000\000\
\\001\000\006\000\123\000\000\000\
\\001\000\006\000\132\000\020\000\131\000\000\000\
\\001\000\006\000\145\000\000\000\
\\001\000\008\000\093\000\000\000\
\\001\000\009\000\078\000\000\000\
\\001\000\009\000\101\000\000\000\
\\001\000\009\000\122\000\000\000\
\\001\000\011\000\088\000\015\000\037\000\016\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\027\000\027\000\
\\028\000\026\000\000\000\
\\001\000\011\000\100\000\015\000\037\000\016\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\027\000\027\000\
\\028\000\026\000\000\000\
\\001\000\013\000\098\000\000\000\
\\001\000\013\000\129\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\
\\037\000\077\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\
\\041\000\112\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\
\\042\000\076\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\
\\042\000\134\000\000\000\
\\001\000\020\000\090\000\000\000\
\\001\000\020\000\099\000\000\000\
\\001\000\020\000\142\000\000\000\
\\001\000\029\000\075\000\000\000\
\\001\000\029\000\121\000\000\000\
\\001\000\044\000\071\000\000\000\
\\001\000\045\000\103\000\000\000\
\\001\000\046\000\119\000\000\000\
\\150\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\\151\000\010\000\025\000\014\000\024\000\029\000\023\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\008\000\054\000\010\000\053\000\012\000\052\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\046\000\116\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\005\000\102\000\015\000\037\000\016\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\027\000\027\000\
\\028\000\026\000\000\000\
\\173\000\018\000\035\000\019\000\034\000\000\000\
\\174\000\018\000\035\000\019\000\034\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\184\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\000\000\
\\185\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\000\000\
\\186\000\000\000\
\\187\000\007\000\080\000\015\000\037\000\016\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\027\000\027\000\
\\028\000\026\000\000\000\
\\188\000\002\000\022\000\003\000\021\000\004\000\020\000\008\000\019\000\
\\016\000\018\000\033\000\017\000\034\000\016\000\036\000\015\000\
\\039\000\014\000\040\000\013\000\043\000\012\000\000\000\
\\189\000\007\000\080\000\015\000\037\000\016\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\027\000\027\000\
\\028\000\026\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\005\000\126\000\015\000\037\000\016\000\036\000\018\000\035\000\
\\019\000\034\000\020\000\033\000\021\000\032\000\022\000\031\000\
\\023\000\030\000\024\000\029\000\025\000\028\000\027\000\027\000\
\\028\000\026\000\000\000\
\\194\000\000\000\
\\195\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\
\\038\000\113\000\000\000\
\\196\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\\197\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\\198\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\030\000\045\000\031\000\044\000\032\000\043\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\032\000\043\000\000\000\
\\206\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\002\000\111\000\000\000\
\\211\000\005\000\139\000\000\000\
\\212\000\000\000\
\\213\000\005\000\139\000\000\000\
\\214\000\000\000\
\\215\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\\216\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\\217\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\
\\030\000\045\000\000\000\
\\218\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\
\\030\000\045\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\\221\000\015\000\037\000\016\000\036\000\018\000\035\000\019\000\034\000\
\\020\000\033\000\021\000\032\000\022\000\031\000\023\000\030\000\
\\024\000\029\000\025\000\028\000\027\000\027\000\028\000\026\000\000\000\
\"
val actionRowNumbers =
"\008\000\060\000\058\000\057\000\
\\054\000\061\000\053\000\052\000\
\\048\000\047\000\092\000\009\000\
\\008\000\008\000\049\000\059\000\
\\008\000\079\000\051\000\050\000\
\\062\000\008\000\010\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\008\000\008\000\008\000\008\000\
\\095\000\094\000\093\000\092\000\
\\044\000\011\000\012\000\013\000\
\\042\000\037\000\035\000\074\000\
\\028\000\078\000\015\000\008\000\
\\007\000\056\000\063\000\031\000\
\\075\000\076\000\001\000\002\000\
\\003\000\004\000\005\000\006\000\
\\073\000\072\000\071\000\070\000\
\\091\000\079\000\039\000\023\000\
\\027\000\008\000\008\000\008\000\
\\055\000\077\000\008\000\033\000\
\\083\000\040\000\032\000\029\000\
\\069\000\067\000\064\000\045\000\
\\016\000\008\000\017\000\101\000\
\\036\000\088\000\086\000\080\000\
\\082\000\008\000\065\000\066\000\
\\008\000\090\000\096\000\046\000\
\\101\000\098\000\106\000\043\000\
\\030\000\024\000\008\000\008\000\
\\081\000\084\000\008\000\068\000\
\\097\000\018\000\034\000\008\000\
\\025\000\019\000\038\000\087\000\
\\014\000\112\000\100\000\099\000\
\\107\000\008\000\020\000\102\000\
\\008\000\085\000\108\000\041\000\
\\103\000\021\000\089\000\110\000\
\\008\000\026\000\109\000\022\000\
\\111\000\104\000\105\000\000\000"
val gotoT =
"\
\\001\000\147\000\010\000\009\000\011\000\008\000\014\000\007\000\
\\016\000\006\000\017\000\005\000\018\000\004\000\020\000\003\000\
\\021\000\002\000\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\040\000\003\000\039\000\004\000\038\000\005\000\037\000\
\\006\000\036\000\000\000\
\\000\000\
\\010\000\045\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\046\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\010\000\047\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\049\000\011\000\008\000\012\000\048\000\014\000\007\000\
\\016\000\006\000\017\000\005\000\018\000\004\000\020\000\003\000\
\\021\000\002\000\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\053\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\010\000\055\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\056\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\057\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\058\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\059\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\060\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\061\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\062\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\063\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\064\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\065\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\066\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\067\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\068\000\003\000\039\000\004\000\038\000\005\000\037\000\
\\006\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\077\000\000\000\
\\019\000\079\000\000\000\
\\010\000\082\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\084\000\011\000\008\000\014\000\007\000\015\000\083\000\
\\016\000\006\000\017\000\005\000\018\000\004\000\020\000\003\000\
\\021\000\002\000\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\049\000\011\000\008\000\012\000\087\000\014\000\007\000\
\\016\000\006\000\017\000\005\000\018\000\004\000\020\000\003\000\
\\021\000\002\000\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\092\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\093\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\094\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\010\000\095\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\102\000\000\000\
\\010\000\106\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\008\000\108\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\112\000\000\000\
\\000\000\
\\010\000\113\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\010\000\084\000\011\000\008\000\014\000\007\000\015\000\115\000\
\\016\000\006\000\017\000\005\000\018\000\004\000\020\000\003\000\
\\021\000\002\000\022\000\001\000\000\000\
\\000\000\
\\004\000\116\000\000\000\
\\000\000\
\\008\000\118\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\122\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\010\000\123\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\010\000\125\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\128\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\019\000\133\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\134\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\009\000\136\000\000\000\
\\010\000\138\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\006\000\139\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\142\000\011\000\008\000\014\000\007\000\016\000\006\000\
\\017\000\005\000\018\000\004\000\020\000\003\000\021\000\002\000\
\\022\000\001\000\000\000\
\\000\000\
\\006\000\144\000\000\000\
\\000\000\
\\000\000\
\\009\000\146\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 148
val numrules = 72
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | letstm of unit ->  (A.exp)
 | loop of unit ->  (A.exp) | ifstm of unit ->  (A.exp)
 | recordinputs of unit ->  ( ( A.symbol * A.exp * A.pos )  list)
 | recordcreate of unit ->  (A.exp) | arraycreate of unit ->  (A.exp)
 | operation of unit ->  (A.exp) | funinputs of unit ->  (A.exp list)
 | funcall of unit ->  (A.exp)
 | expseq2 of unit ->  ( ( A.exp * A.pos )  list)
 | expseq of unit ->  (A.exp) | lvalue of unit ->  (A.var)
 | exp of unit ->  (A.exp) | tyfields2 of unit ->  (A.field list)
 | tyfields of unit ->  (A.field list) | ty of unit ->  (A.ty)
 | fundec of unit ->  (A.fundec list) | vardec of unit ->  (A.dec)
 | tydec of unit ->  ({ name:A.symbol,ty:A.ty,pos:A.pos }  list)
 | dec of unit ->  (A.dec) | decs of unit ->  (A.dec list)
 | program of unit ->  (A.exp)
end
type svalue = MlyValue.svalue
type result = A.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 38) => true | (T 39) => true | (T 40) => true | (T 32) => true
 | (T 42) => true | (T 43) => true | (T 44) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 34) => true | (T 35) => true
 | (T 36) => true | (T 37) => true | (T 41) => true | (T 45) => true
 | (T 33) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 36))::
(nil
,nil
 $$ (T 37))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "UMINUS"
  | (T 17) => "TIMES"
  | (T 18) => "DIVIDE"
  | (T 19) => "EQ"
  | (T 20) => "NEQ"
  | (T 21) => "LT"
  | (T 22) => "LE"
  | (T 23) => "GT"
  | (T 24) => "GE"
  | (T 25) => "EQUALS"
  | (T 26) => "AND"
  | (T 27) => "OR"
  | (T 28) => "ASSIGN"
  | (T 29) => "FUNCTION"
  | (T 30) => "VAR"
  | (T 31) => "TYPE"
  | (T 32) => "BREAK"
  | (T 33) => "NIL"
  | (T 34) => "ARRAY"
  | (T 35) => "IF"
  | (T 36) => "THEN"
  | (T 37) => "ELSE"
  | (T 38) => "WHILE"
  | (T 39) => "FOR"
  | (T 40) => "TO"
  | (T 41) => "DO"
  | (T 42) => "LET"
  | (T 43) => "IN"
  | (T 44) => "END"
  | (T 45) => "OF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.program (fn _ => let val  (exp as exp1
) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.lvalue lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
lvalue as lvalue1) = lvalue1 ()
 in (A.VarExp(lvalue))
end)
 in ( LrTable.NT 9, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (A.NilExp))
 in ( LrTable.NT 9, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (A.IntExp(INT))
end)
 in ( LrTable.NT 9, ( result, INT1left, INT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.STRING STRING1, (STRINGleft as STRING1left),
 STRING1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (STRING as STRING1) = STRING1 ()
 in (A.StringExp(STRING,STRINGleft))
end)
 in ( LrTable.NT 9, ( result, STRING1left, STRING1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.funcall funcall1, funcall1left, 
funcall1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (funcall as funcall1) = funcall1 ()
 in (funcall)
end)
 in ( LrTable.NT 9, ( result, funcall1left, funcall1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.operation operation1, operation1left, 
operation1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (operation as operation1) = operation1 ()
 in (operation)
end)
 in ( LrTable.NT 9, ( result, operation1left, operation1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.recordcreate recordcreate1, 
recordcreate1left, recordcreate1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => let val  (recordcreate as recordcreate1
) = recordcreate1 ()
 in (recordcreate)
end)
 in ( LrTable.NT 9, ( result, recordcreate1left, recordcreate1right), 
rest671)
end
|  ( 8, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expseq 
expseq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val 
 result = MlyValue.exp (fn _ => let val  (expseq as expseq1) = expseq1
 ()
 in (expseq)
end)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.AssignExp({var=lvalue,exp=exp,pos=lvalueleft}))
end)
 in ( LrTable.NT 9, ( result, lvalue1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ifstm ifstm1, ifstm1left, ifstm1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (ifstm
 as ifstm1) = ifstm1 ()
 in (ifstm)
end)
 in ( LrTable.NT 9, ( result, ifstm1left, ifstm1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.loop loop1, loop1left, loop1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (loop
 as loop1) = loop1 ()
 in (loop)
end)
 in ( LrTable.NT 9, ( result, loop1left, loop1right), rest671)
end
|  ( 12, ( ( _, ( _, (BREAKleft as BREAK1left), BREAK1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => (
A.BreakExp(BREAKleft)))
 in ( LrTable.NT 9, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.letstm letstm1, letstm1left, letstm1right))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  (
letstm as letstm1) = letstm1 ()
 in (letstm)
end)
 in ( LrTable.NT 9, ( result, letstm1left, letstm1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.arraycreate arraycreate1, arraycreate1left,
 arraycreate1right)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (arraycreate as arraycreate1) = arraycreate1 ()
 in (arraycreate)
end)
 in ( LrTable.NT 9, ( result, arraycreate1left, arraycreate1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 in (A.SimpleVar(S(ID),IDleft))
end)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.lvalue lvalue1, (lvalueleft as lvalue1left), _)) :: rest671))
 => let val  result = MlyValue.lvalue (fn _ => let val  (lvalue as 
lvalue1) = lvalue1 ()
 val  (ID as ID1) = ID1 ()
 in (A.FieldVar(lvalue,S(ID),lvalueleft))
end)
 in ( LrTable.NT 10, ( result, lvalue1left, ID1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.lvalue lvalue1, (lvalueleft as 
lvalue1left), _)) :: rest671)) => let val  result = MlyValue.lvalue
 (fn _ => let val  (lvalue as lvalue1) = lvalue1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(lvalue,exp,lvalueleft))
end)
 in ( LrTable.NT 10, ( result, lvalue1left, RBRACK1right), rest671)

end
|  ( 18, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: 
rest671)) => let val  result = MlyValue.lvalue (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (A.SubscriptVar(A.SimpleVar(S(ID),IDleft),exp,IDleft))
end)
 in ( LrTable.NT 10, ( result, ID1left, RBRACK1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.funinputs 
funinputs1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as ID1left)
, _)) :: rest671)) => let val  result = MlyValue.funcall (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (funinputs as funinputs1) = funinputs1 ()
 in (A.CallExp({func=S(ID),args=funinputs,pos=IDleft}))
end)
 in ( LrTable.NT 13, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.funcall (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.CallExp({func=S(ID),args=[],pos=IDleft}))
end)
 in ( LrTable.NT 13, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.funinputs funinputs1, _, funinputs1right))
 :: _ :: ( _, ( MlyValue.exp exp1, exp1left, _)) :: rest671)) => let
 val  result = MlyValue.funinputs (fn _ => let val  (exp as exp1) = 
exp1 ()
 val  (funinputs as funinputs1) = funinputs1 ()
 in (exp::funinputs)
end)
 in ( LrTable.NT 14, ( result, exp1left, funinputs1right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.funinputs (fn _ => let val  (exp as 
exp1) = exp1 ()
 in (exp::[])
end)
 in ( LrTable.NT 14, ( result, exp1left, exp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.PlusOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.MinusOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.TimesOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.DivideOp,right=exp2,pos=exp1left}))
end
)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, (
MINUSleft as MINUS1left), _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  (exp as exp1) = exp1 ()
 in (
A.OpExp({left=A.IntExp(0),oper=A.MinusOp,right=exp,pos=MINUSleft}))

end)
 in ( LrTable.NT 15, ( result, MINUS1left, exp1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.GeOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.GtOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.LeOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.LtOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.NeqOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.OpExp({left=exp1,oper=A.EqOp,right=exp2,pos=exp1left}))
end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test=exp1,then'=A.IntExp(1),else'=SOME(exp2),pos=exp1left}))

end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.operation (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (
A.IfExp({test=exp1,then'=exp2,else'=SOME(A.IntExp(0)),pos=exp1left}))

end)
 in ( LrTable.NT 15, ( result, exp1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.expseq2 expseq21, _, expseq21right)) :: ( _
, ( MlyValue.exp exp1, (expleft as exp1left), _)) :: rest671)) => let
 val  result = MlyValue.expseq (fn _ => let val  (exp as exp1) = exp1
 ()
 val  (expseq2 as expseq21) = expseq21 ()
 in (A.SeqExp((exp,expleft)::expseq2))
end)
 in ( LrTable.NT 11, ( result, exp1left, expseq21right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.exp exp1, (expleft as exp1left), exp1right)
) :: rest671)) => let val  result = MlyValue.expseq (fn _ => let val 
 (exp as exp1) = exp1 ()
 in (A.SeqExp([(exp,expleft)]))
end)
 in ( LrTable.NT 11, ( result, exp1left, exp1right), rest671)
end
|  ( 38, ( rest671)) => let val  result = MlyValue.expseq (fn _ => (
A.SeqExp([])))
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 39, ( ( _, ( MlyValue.exp exp1, expleft, exp1right)) :: ( _, ( _,
 SEMICOLON1left, _)) :: rest671)) => let val  result = 
MlyValue.expseq2 (fn _ => let val  (exp as exp1) = exp1 ()
 in ((exp,expleft)::[])
end)
 in ( LrTable.NT 12, ( result, SEMICOLON1left, exp1right), rest671)

end
|  ( 40, ( ( _, ( MlyValue.expseq2 expseq21, _, expseq21right)) :: ( _
, ( MlyValue.exp exp1, expleft, _)) :: ( _, ( _, SEMICOLON1left, _))
 :: rest671)) => let val  result = MlyValue.expseq2 (fn _ => let val 
 (exp as exp1) = exp1 ()
 val  (expseq2 as expseq21) = expseq21 ()
 in ((exp,expleft)::expseq2)
end)
 in ( LrTable.NT 12, ( result, SEMICOLON1left, expseq21right), rest671
)
end
|  ( 41, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.recordinputs
 recordinputs1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft as 
ID1left), _)) :: rest671)) => let val  result = MlyValue.recordcreate
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (recordinputs as recordinputs1) = recordinputs1 ()
 in (A.RecordExp({fields=recordinputs,typ=S(ID),pos=IDleft}))
end)
 in ( LrTable.NT 17, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 (IDleft as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.recordcreate (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.RecordExp({fields=[],typ=S(ID),pos=IDleft}))
end)
 in ( LrTable.NT 17, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let val  
result = MlyValue.recordinputs (fn _ => let val  (ID as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in ((S(ID),exp,IDleft)::[])
end)
 in ( LrTable.NT 18, ( result, ID1left, exp1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.recordinputs recordinputs1, _, 
recordinputs1right)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: _ :: 
( _, ( MlyValue.ID ID1, (IDleft as ID1left), _)) :: rest671)) => let
 val  result = MlyValue.recordinputs (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (exp as exp1) = exp1 ()
 val  (recordinputs as recordinputs1) = recordinputs1 ()
 in ((S(ID),exp,IDleft)::recordinputs)
end)
 in ( LrTable.NT 18, ( result, ID1left, recordinputs1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (IFleft as IF1left), _)) :: 
rest671)) => let val  result = MlyValue.ifstm (fn _ => let val  exp1 =
 exp1 ()
 val  exp2 = exp2 ()
 in (A.IfExp({test=exp1,then'=exp2,else'=NONE,pos=IFleft}))
end)
 in ( LrTable.NT 19, ( result, IF1left, exp2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, (IFleft as IF1left), _)) :: rest671)) => let val  result = 
MlyValue.ifstm (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.IfExp({test=exp1,then'=exp2,else'=SOME(exp3),pos=IFleft}))
end)
 in ( LrTable.NT 19, ( result, IF1left, exp3right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, (WHILEleft as WHILE1left), _))
 :: rest671)) => let val  result = MlyValue.loop (fn _ => let val  
exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.WhileExp({test=exp1,body=exp2,pos=WHILEleft}))
end)
 in ( LrTable.NT 20, ( result, WHILE1left, exp2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FORleft as FOR1left
), _)) :: rest671)) => let val  result = MlyValue.loop (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (
A.ForExp({var=S(ID),escape=ref true,lo=exp1,hi=exp2,body=exp3,pos=FORleft})
)
end)
 in ( LrTable.NT 20, ( result, FOR1left, exp3right), rest671)
end
|  ( 49, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.expseq expseq1,
 _, _)) :: _ :: ( _, ( MlyValue.decs decs1, _, _)) :: ( _, ( _, (
LETleft as LET1left), _)) :: rest671)) => let val  result = 
MlyValue.letstm (fn _ => let val  (decs as decs1) = decs1 ()
 val  (expseq as expseq1) = expseq1 ()
 in (A.LetExp({decs=decs,body=expseq,pos=LETleft}))
end)
 in ( LrTable.NT 21, ( result, LET1left, END1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.decs decs1, _, decs1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decs (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decs as decs1) = decs1 ()
 in (dec::decs)
end)
 in ( LrTable.NT 1, ( result, dec1left, decs1right), rest671)
end
|  ( 51, ( rest671)) => let val  result = MlyValue.decs (fn _ => ([]))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 52, ( ( _, ( MlyValue.tydec tydec1, tydec1left, tydec1right)) :: 
rest671)) => let val  result = MlyValue.dec (fn _ => let val  (tydec
 as tydec1) = tydec1 ()
 in (A.TypeDec(tydec))
end)
 in ( LrTable.NT 2, ( result, tydec1left, tydec1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.vardec vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
vardec as vardec1) = vardec1 ()
 in (vardec)
end)
 in ( LrTable.NT 2, ( result, vardec1left, vardec1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
fundec as fundec1) = fundec1 ()
 in (A.FunctionDec(fundec))
end)
 in ( LrTable.NT 2, ( result, fundec1left, fundec1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.ty ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (TYPEleft as TYPE1left), _)) :: 
rest671)) => let val  result = MlyValue.tydec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 in ({name=S(ID),ty=ty,pos=TYPEleft}::[])
end)
 in ( LrTable.NT 3, ( result, TYPE1left, ty1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.tydec tydec1, _, tydec1right)) :: ( _, ( 
MlyValue.ty ty1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, (TYPEleft as TYPE1left), _)) :: rest671)) => let val  result = 
MlyValue.tydec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (ty as ty1) = ty1 ()
 val  (tydec as tydec1) = tydec1 ()
 in ({name=S(ID),ty=ty,pos=TYPEleft}::tydec)
end)
 in ( LrTable.NT 3, ( result, TYPE1left, tydec1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), ID1right)) :: 
rest671)) => let val  result = MlyValue.ty (fn _ => let val  (ID as 
ID1) = ID1 ()
 in (A.NameTy(S(ID), IDleft))
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 58, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ty (fn _ => let val  (tyfields as tyfields1) =
 tyfields1 ()
 in (A.RecordTy(tyfields))
end)
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, (
ARRAYleft as ARRAY1left), _)) :: rest671)) => let val  result = 
MlyValue.ty (fn _ => let val  (ID as ID1) = ID1 ()
 in (A.ArrayTy(S(ID),ARRAYleft))
end)
 in ( LrTable.NT 6, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 60, ( rest671)) => let val  result = MlyValue.tyfields (fn _ => (
[]))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.tyfields (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ({name=S(ID1),escape=ref true,typ=S(ID2),pos=ID1left}::[])
end)
 in ( LrTable.NT 7, ( result, ID1left, ID2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.tyfields2 tyfields21, _, tyfields21right))
 :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.tyfields (fn _
 => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (tyfields2 as tyfields21) = tyfields21 ()
 in ({name=S(ID1),escape=ref true,typ=S(ID2),pos=ID1left}::tyfields2)

end)
 in ( LrTable.NT 7, ( result, ID1left, tyfields21right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)
) => let val  result = MlyValue.tyfields2 (fn _ => let val  ID1 = ID1
 ()
 val  ID2 = ID2 ()
 in ({name=S(ID1),escape=ref true,typ=S(ID2),pos=ID1left}::[])
end)
 in ( LrTable.NT 8, ( result, COMMA1left, ID2right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.tyfields2 tyfields21, _, tyfields21right))
 :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  
result = MlyValue.tyfields2 (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (tyfields2 as tyfields21) = tyfields21 ()
 in ({name=S(ID1),escape=ref true,typ=S(ID2),pos=ID1left}::tyfields2)

end)
 in ( LrTable.NT 8, ( result, COMMA1left, tyfields21right), rest671)

end
|  ( 65, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, (VARleft as VAR1left), _)) :: 
rest671)) => let val  result = MlyValue.vardec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name=S(ID),escape=ref true,typ=NONE,init=exp,pos=VARleft}))

end)
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, (VARleft as VAR1left), _)) :: rest671)) => let val  
result = MlyValue.vardec (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
A.VarDec({name=S(ID1),escape=ref true,typ=SOME(S(ID2),ID2left),init=exp,pos=VARleft})
)
end)
 in ( LrTable.NT 4, ( result, VAR1left, exp1right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: _ :: ( _, 
( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left), _)) :: rest671))
 => let val  result = MlyValue.fundec (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 in (
{name=S(ID),params=tyfields,result=NONE,body=exp,pos=FUNCTIONleft}::[]
)
end)
 in ( LrTable.NT 5, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, ID2left, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 in (
{name=S(ID1),params=tyfields,result=SOME(S(ID2),ID2left),body=exp,pos=FUNCTIONleft}::[]
)
end)
 in ( LrTable.NT 5, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.exp exp1, _, _)) :: _ :: _ :: ( _, ( MlyValue.tyfields 
tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _,
 (FUNCTIONleft as FUNCTION1left), _)) :: rest671)) => let val  result
 = MlyValue.fundec (fn _ => let val  (ID as ID1) = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  (exp as exp1) = exp1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (
{name=S(ID),params=tyfields,result=NONE,body=exp,pos=FUNCTIONleft}::fundec
)
end)
 in ( LrTable.NT 5, ( result, FUNCTION1left, fundec1right), rest671)

end
|  ( 70, ( ( _, ( MlyValue.fundec fundec1, _, fundec1right)) :: ( _, (
 MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, ID2left, _)
) :: _ :: _ :: ( _, ( MlyValue.tyfields tyfields1, _, _)) :: _ :: ( _,
 ( MlyValue.ID ID1, _, _)) :: ( _, ( _, (FUNCTIONleft as FUNCTION1left
), _)) :: rest671)) => let val  result = MlyValue.fundec (fn _ => let
 val  ID1 = ID1 ()
 val  (tyfields as tyfields1) = tyfields1 ()
 val  ID2 = ID2 ()
 val  (exp as exp1) = exp1 ()
 val  (fundec as fundec1) = fundec1 ()
 in (
{name=S(ID1),params=tyfields,result=SOME(S(ID2),ID2left),body=exp,pos=FUNCTIONleft}::fundec
)
end)
 in ( LrTable.NT 5, ( result, FUNCTION1left, fundec1right), rest671)

end
|  ( 71, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: _ :: ( _, 
( MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, (IDleft
 as ID1left), _)) :: rest671)) => let val  result = 
MlyValue.arraycreate (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.ArrayExp({typ=S(ID),size=exp1,init=exp2,pos=IDleft}))
end)
 in ( LrTable.NT 16, ( result, ID1left, exp2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
end
end
