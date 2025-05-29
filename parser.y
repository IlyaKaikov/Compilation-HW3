%{
#include <memory>
#include <cstring>
#include "nodes.hpp"
#include "output.hpp"

extern int yylineno;
extern int yylex();
extern char *yytext;
void yyerror(const char*);

std::shared_ptr<ast::Node> program;

template<typename T>
std::shared_ptr<T> as(const std::shared_ptr<ast::Node>& p) {
    return std::dynamic_pointer_cast<T>(p);
}

using namespace ast;
%}

%token T_VOID T_INT T_BYTE T_BOOL TRUE FALSE RETURN IF ELSE WHILE BREAK CONTINUE
%token SC COMMA LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK ASSIGN
%token PLUS MINUS STAR SLASH
%token T_LT T_GT T_LE T_GE T_EQ T_NE
%token AND OR NOT
%token T_ID NUM NUM_B T_STRING

%type Program Funcs FuncDecl RetType Formals FormalsList FormalDecl Statements Statement Call Exp ExpList Type

%start Program

%left OR
%left AND
%nonassoc T_EQ T_NE
%nonassoc T_LT T_GT T_LE T_GE
%left  PLUS MINUS
%left  STAR SLASH
%right NOT
%right UMINUS
%right CAST
%left LBRACK
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%%
Program
    : Funcs { program = $1; }
    ;

Funcs
    : /* empty */ { $$ = std::make_shared<Funcs>(); }
    | FuncDecl Funcs { auto list = std::dynamic_pointer_cast<Funcs>($2); list->push_front(std::dynamic_pointer_cast<FuncDecl>($1)); $$ = list; }
    ;

FuncDecl
    : RetType T_ID LPAREN Formals RPAREN LBRACE Statements RBRACE
        {
            $$ = std::make_shared<FuncDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1),
                std::dynamic_pointer_cast<Formals>($4),
                std::dynamic_pointer_cast<Statements>($7)
            );
        }
    ;

RetType
    : Type { $$ = $1; }
    | T_VOID { $$ = std::make_shared<PrimitiveType>(BuiltInType::VOID); }
    ;

Formals
    : /* empty */ { $$ = std::make_shared<Formals>(); }
    | FormalsList { $$ = $1; }
    ;

FormalsList
    : FormalDecl { $$ = std::make_shared<Formals>(std::dynamic_pointer_cast<Formal>($1)); }
    | FormalDecl COMMA FormalsList
        {
            auto list = std::dynamic_pointer_cast<Formals>($3);
            list->push_front(std::dynamic_pointer_cast<Formal>($1));
            $$ = list;
        }
    ;

FormalDecl
    : Type T_ID
        {
            $$ = std::make_shared<Formal>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1)
            );
        }
    ;

Statements
    : Statement { $$ = std::make_shared<Statements>(std::dynamic_pointer_cast<Statement>($1)); }
    | Statements Statement
        {
            auto list = std::dynamic_pointer_cast<Statements>($1);
            list->push_back(std::dynamic_pointer_cast<Statement>($2));
            $$ = list;
        }
    ;

Statement
    : LBRACE Statements RBRACE { $$ = $2; }
    | Type T_ID SC
        {
            $$ = std::make_shared<VarDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1),
                nullptr
            );
        }
    | Type T_ID ASSIGN Exp SC
        {
            $$ = std::make_shared<VarDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::dynamic_pointer_cast<Type>($1),
                std::dynamic_pointer_cast<Exp>($4)
            );
        }
    | T_ID ASSIGN Exp SC
        {
            $$ = std::make_shared<Assign>(
                std::dynamic_pointer_cast<ID>($1),
                std::dynamic_pointer_cast<Exp>($3)
            );
        }
    | T_ID LBRACK Exp RBRACK ASSIGN Exp SC
        {
            $$ = std::make_shared<ArrayAssign>(
                std::dynamic_pointer_cast<ID>($1),
                std::dynamic_pointer_cast<Exp>($6),
                std::dynamic_pointer_cast<Exp>($3)
            );
        }
    | Type T_ID LBRACK Exp RBRACK SC
        {
            $$ = std::make_shared<VarDecl>(
                std::dynamic_pointer_cast<ID>($2),
                std::make_shared<ArrayType>(
                    std::dynamic_pointer_cast<PrimitiveType>($1)->type,
                    std::dynamic_pointer_cast<Exp>($4)
                ),
                nullptr
            );
        }
    | Call SC { $$ = $1; }
    | RETURN SC { $$ = std::make_shared<Return>(nullptr); }
    | RETURN Exp SC { $$ = std::make_shared<Return>(std::dynamic_pointer_cast<Exp>($2)); }
    | IF LPAREN Exp RPAREN Statement %prec LOWER_THAN_ELSE
        {
            $$ = std::make_shared<If>(
                std::dynamic_pointer_cast<Exp>($3),
                std::dynamic_pointer_cast<Statement>($5),
                nullptr
            );
        }
    | IF LPAREN Exp RPAREN Statement ELSE Statement
        {
            $$ = std::make_shared<If>(
                std::dynamic_pointer_cast<Exp>($3),
                std::dynamic_pointer_cast<Statement>($5),
                std::dynamic_pointer_cast<Statement>($7)
            );
        }
    | WHILE LPAREN Exp RPAREN Statement
        {
            $$ = std::make_shared<While>(
                std::dynamic_pointer_cast<Exp>($3),
                std::dynamic_pointer_cast<Statement>($5)
            );
        }
    | BREAK SC { $$ = std::make_shared<Break>(); }
    | CONTINUE SC { $$ = std::make_shared<Continue>(); }
    ;

Call
    : T_ID LPAREN ExpList RPAREN
        { $$ = std::make_shared<Call>(std::dynamic_pointer_cast<ID>($1), std::dynamic_pointer_cast<ExpList>($3)); }
    | T_ID LPAREN RPAREN
        { $$ = std::make_shared<Call>(std::dynamic_pointer_cast<ID>($1)); }
    ;

ExpList
    : Exp { $$ = std::make_shared<ExpList>(std::dynamic_pointer_cast<Exp>($1)); }
    | Exp COMMA ExpList
        {
            auto list = std::dynamic_pointer_cast<ExpList>($3);
            list->push_front(std::dynamic_pointer_cast<Exp>($1));
            $$ = list;
        }
    ;

Type
    : T_INT { $$ = std::make_shared<PrimitiveType>(BuiltInType::INT); }
    | T_BYTE { $$ = std::make_shared<PrimitiveType>(BuiltInType::BYTE); }
    | T_BOOL { $$ = std::make_shared<PrimitiveType>(BuiltInType::BOOL); }
    ;

Exp
    : LPAREN Exp RPAREN              { $$ = $2; }
    | T_ID LBRACK Exp RBRACK         { $$ = std::make_shared<ArrayDereference>(as<ID>($1), as<Exp>($3)); }
    | Exp PLUS  Exp                  { $$ = std::make_shared<BinOp>(as<Exp>($1), as<Exp>($3), BinOpType::ADD ); }
    | Exp MINUS Exp                  { $$ = std::make_shared<BinOp>(as<Exp>($1), as<Exp>($3), BinOpType::SUB ); }
    | Exp STAR  Exp                  { $$ = std::make_shared<BinOp>(as<Exp>($1), as<Exp>($3), BinOpType::MUL ); }
    | Exp SLASH Exp                  { $$ = std::make_shared<BinOp>(as<Exp>($1), as<Exp>($3), BinOpType::DIV ); }
    | Exp AND   Exp                  { $$ = std::make_shared<And>(as<Exp>($1), as<Exp>($3)); }
    | Exp OR    Exp                  { $$ = std::make_shared<Or> (as<Exp>($1), as<Exp>($3)); }
    | Exp T_LT Exp                   { $$ = std::make_shared<RelOp>(as<Exp>($1), as<Exp>($3), RelOpType::LT); }
    | Exp T_GT Exp                   { $$ = std::make_shared<RelOp>(as<Exp>($1), as<Exp>($3), RelOpType::GT); }
    | Exp T_LE Exp                   { $$ = std::make_shared<RelOp>(as<Exp>($1), as<Exp>($3), RelOpType::LE); }
    | Exp T_GE Exp                   { $$ = std::make_shared<RelOp>(as<Exp>($1), as<Exp>($3), RelOpType::GE); }
    | Exp T_EQ Exp                   { $$ = std::make_shared<RelOp>(as<Exp>($1), as<Exp>($3), RelOpType::EQ); }
    | Exp T_NE Exp                   { $$ = std::make_shared<RelOp>(as<Exp>($1), as<Exp>($3), RelOpType::NE); }
    | T_ID                           { $$ = $1; }
    | Call                           { $$ = $1; }
    | NUM                            { $$ = $1; }
    | NUM_B                          { $$ = $1; }
    | T_STRING                       { $$ = $1; }
    | TRUE                           { $$ = std::make_shared<Bool>(true);  }
    | FALSE                          { $$ = std::make_shared<Bool>(false); }
    | NOT  Exp                       { $$ = std::make_shared<Not>(as<Exp>($2)); }
    | MINUS Exp          %prec UMINUS
                                     { $$ = std::make_shared<BinOp>(
                                             std::make_shared<Num>("0"),
                                             as<Exp>($2),
                                             BinOpType::SUB); }
    | LPAREN Type RPAREN Exp %prec CAST
                                     { $$ = std::make_shared<Cast>(as<Exp>($4),
                                             std::dynamic_pointer_cast<PrimitiveType>($2)); }
    ;

%%

void yyerror(const char*) {
    output::errorSyn(yylineno);
}
