%{
%}

%token <int>    INTEGER
%token <string> CONSTANT
%token <string> VARIABLE
%token          EOF LPAREN RPAREN LBRACKET RBRACKET COLON RULE_COND SUBTRACT NOT_EQ ADD MULT EQ RTARROW COMMA DOT APOSTROPHE SEMICOLON 

%start filename
%type <string> filename

%%
filename:
  LBRACKET APOSTROPHE CONSTANT DOT CONSTANT APOSTROPHE RBRACKET DOT                         { $3 ^ "." ^ $5 }
;

%%
