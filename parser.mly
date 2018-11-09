%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN RPAREN
%token PLUS MINUS TIMES EQUAL LESS MORE LESSEQUAL MOREEQUAL NOTEQUAL IF THEN ELSE LET IN FUN ARROW
%token <int> NUMBER
%token <string> VAR
/* これは、数字には int 型の値が伴うことを示している */
%token TRUE FALSE
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
%nonassoc ARROW
%nonassoc IN
%nonassoc ELSE
%left EQUAL LESS NOTEQUAL LESSEQUAL MOREEQUAL MORE
%left PLUS MINUS
%left TIMES
%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
	{ Syntax.Number ($1) }
| VAR
	{ Syntax.Var ($1)}
| TRUE
	{ Syntax.Bool (true) }
| FALSE
	{ Syntax.Bool (false) }
| LPAREN expr RPAREN
	{ $2 }

expr:
| FUN VAR ARROW expr
	{Syntax.Fun ($2,$4)}
| expr simple_expr
	{Syntax.App ($1 , $2)}
| LET VAR EQUAL expr IN expr
	{ Syntax.Let ($2, $4, $6)}
| simple_expr
	{ $1 }
| expr PLUS expr
	{ Syntax.Op ($1, Syntax.Plus, $3) }
| expr MINUS expr
	{ Syntax.Op ($1, Syntax.Minus, $3) }
| expr TIMES expr
	{ Syntax.Op ($1, Syntax.Times, $3) }
| expr EQUAL expr
	{ Syntax.Op ($1, Syntax.Equal, $3) }
| expr LESS expr
	{ Syntax.Op ($1, Syntax.Less, $3) }
| expr MORE expr
	{ Syntax.Op ($3, Syntax.Less, $1) }
| expr LESSEQUAL expr
	{ Syntax.Op ($1, Syntax.Lessequal, $3) }
| expr MOREEQUAL expr
	{ Syntax.Op ($3, Syntax.Lessequal, $1) }
| expr NOTEQUAL expr
	{ Syntax.Op ($3, Syntax.Notequal, $1) }
| IF expr THEN expr ELSE expr
	{ Syntax.If ($2, $4, $6)}
| MINUS expr %prec UNARY
	{ Syntax.Op (Syntax.Number (0), Syntax.Minus, $2) }
