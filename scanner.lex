%option noyywrap
%option yylineno

%{
    #include <string.h>
    #include "nodes.hpp"
    #include "output.hpp"
    #include "visitor.hpp"
    #include "parser.tab.h"
    using namespace std;
    using namespace ast;
%}

whitespace      ([\r\n\t ])
digit           ([0-9])
letter          ([a-zA-Z])
digitletter     ([0-9a-zA-Z])

%%

void                                return T_VOID;
int                                 return T_INT;
byte                                return T_BYTE;
bool                                return T_BOOL;
and                                 return AND;
or                                  return OR;
not                                 return NOT;
true                                { yylval = make_shared<ast::Bool>(true);  return TRUE; }
false                               { yylval = make_shared<ast::Bool>(false); return FALSE; }
return                              return RETURN;
if                                  return IF;
else                                return ELSE;
while                               return WHILE;
break                               return BREAK;
continue                            return CONTINUE;
;                                   return SC;
,                                   return COMMA;
\(                                  return LPAREN;
\)                                  return RPAREN;
\{                                  return LBRACE;
\}                                  return RBRACE;
\[                                  return LBRACK;
\]                                  return RBRACK;
>=                                  return T_GE;
[<]=                                return T_LE;
==                                  return T_EQ;
!=                                  return T_NE;
=                                   return ASSIGN;
\+                                  return PLUS;
\-                                  return MINUS;
\*                                  return STAR;
\/                                  return SLASH;
>                                   return T_GT;
[<]                                 return T_LT;
{letter}{digitletter}*              { yylval = make_shared<ast::ID>(yytext);  return T_ID; }
0|([1-9]+{digit}*)                  { yylval = make_shared<ast::Num>(yytext);  return NUM; }
(0|([1-9]+{digit}*))b               { yylval = make_shared<ast::NumB>(yytext); return NUM_B; }
\"([^\n\r\"\\]|\\[rnt"\\])+\"       { yylval = make_shared<ast::String>(yytext); return T_STRING; }
\/\/[^\n\r]*[\r|\n|\r\n]?           ;
{whitespace}                        ;
.                                   { output::errorLex(yylineno); exit(0); }

%%
