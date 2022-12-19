%require  "3.8"
%language "C++"
%skeleton "lalr1.cc"

%defines "parser.h"
%output "parser.cpp"

%locations
%define api.location.file "location.h"

%define api.parser.class {Parser}
%define api.namespace {cheasle}
%define api.value.type variant
%define api.token.constructor

%define parse.error verbose


%code requires{
#include <cheasle/driver.h>

namespace cheasle { class Lexer; }
}

%parse-param {Lexer& lexer}
%param {Driver* driver}

%code{
  #include "lexer.h"
  #undef yylex
  #define yylex lexer.lex
}

%define api.token.prefix {TOK_}
%token <std::string> IDENTIFIER "identifier"
%token <Value> VALUE "value"
%token EOF 0 "end of file"
%token IF THEN ELSE WHILE DO CONST LET DEF END
%token <BuiltInFunctionId> BUILTIN;

%nonassoc <BinaryLogicalOperator> BLOP
%right '='
%left '+' '-'
%left '*' '/'
%nonassoc '|' UMINUS

%nterm <AST> exp stmt block
%nterm <std::vector<AST>> explist
%nterm <std::vector<std::string>> arglist
%type <int> '+' '-' '*' '/'

%start start

%%
start: block EOF { driver->setAST(std::move($1)); }
  | start error EOF { yyerrok; }
;

block: /* nothing */ { $$ = AST::make<Block>(std::vector<AST>{}, std::move(@$)); }
   | block stmt { 
      $1.cast<Block>()->nodes().emplace_back(std::move($2));
      $$ = std::move($1);
  }
;

stmt: IF exp THEN block ELSE block END  { $$ = AST::make<IfExpression>(std::move($2), std::move($4), std::move($6), std::move(@$)); }
   | WHILE exp DO block END             { $$ = AST::make<WhileExpression>(std::move($2), std::move($4), std::move(@$)); }
   | DEF IDENTIFIER '(' arglist ')' '=' block END { $$ = AST::make<FunctionDefinition>(std::move($2), std::move($7), std::move($4), std::move(@$)); }
   | CONST IDENTIFIER '=' stmt   { $$ = AST::make<VariableDefinition>(std::move($2), true, std::move($4), std::move(@$)); }
   | LET IDENTIFIER '=' stmt   { $$ = AST::make<VariableDefinition>(std::move($2), false, std::move($4), std::move(@$)); }
   | IDENTIFIER '=' stmt   { $$ = AST::make<AssignmentExpression>(std::move($1), std::move($3), std::move(@$)); }
   | exp ';'
;

exp: exp BLOP exp         { $$ = AST::make<BinaryLogicalExpression>(std::move($1), std::move($3), $2, std::move(@$)); }
   | exp '+' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Add, std::move(@$)); }
   | exp '-' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Subtract, std::move(@$));}
   | exp '*' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Multiply, std::move(@$)); }
   | exp '/' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Divide, std::move(@$)); }
   | '|' exp '|'            { $$ = AST::make<UnaryExpression>(std::move($2), UnaryOperator::Abs, std::move(@$)); }
   |'(' exp ')'           { $$ = std::move($2); }
   | '-' exp %prec UMINUS { $$ = AST::make<UnaryExpression>(std::move($2), UnaryOperator::Minus, std::move(@$)); }
   | VALUE               { $$ = AST::make<ConstantValue>(std::move($1), std::move(@$)); }
   | IDENTIFIER           { $$ = AST::make<NameReference>(std::move($1), std::move(@$)); }
   | BUILTIN '(' explist ')' { $$ = AST::make<BuiltInFunction>(std::move($1), std::move($3), std::move(@$)); }
   | IDENTIFIER '(' explist ')' { $$ = AST::make<FunctionCall>(std::move($1), std::move($3), std::move(@$)); }
;

explist: exp { $$ = std::vector<AST>{std::move($1)}; }
 | explist ',' exp  { $1.emplace_back(std::move($3));
                      $$ = std::move($1); }
;

arglist: IDENTIFIER { $$ = std::vector<std::string>{std::move($1)}; }
 | arglist ',' IDENTIFIER  { $1.emplace_back(std::move($3));
                      $$ = std::move($1); }
;
%%

void cheasle::Parser::error(const location& loc, const std::string& msg)
{
  driver->getErrors().append("parser", msg, loc);
  if (lexer.size() == 0)      // if token is unknown (no match)
    lexer.matcher().winput(); // skip character
}
