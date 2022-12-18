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
%define api.value.automove

%define parse.error verbose


%code requires{
#include <cheasle/driver.h>

  namespace cheasle {
    class Lexer;
  }
}

%parse-param {Lexer& lexer}
%param {Driver* driver}

%code{
  #include "lexer.h"
  #undef yylex
  #define yylex lexer.lex
}

%define api.token.prefix {TOK_}
%token <std::string> IDENTIFIER "identifier"  // This defines TOK_IDENTIFIER
%token <double> NUMBER "number"               // This defines TOK_NUMBER
%token EOF 0 "end of file"                    // This defines TOK_EOF with value 0
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

%start calclist

%%

stmt: IF exp THEN block ELSE block END  { $$ = AST::make<IfExpression>(std::move($2), std::move($4), std::move($6)); }
   | WHILE exp DO block END             { $$ = AST::make<WhileExpression>(std::move($2), std::move($4)); }
   | DEF IDENTIFIER '(' arglist ')' '=' block END { $$ = AST::make<FunctionDefinition>($2, $7, $4);}
   | CONST IDENTIFIER '=' stmt   { $$ = AST::make<VariableDefinition>($2, true, $4); }
   | LET IDENTIFIER '=' stmt   { $$ = AST::make<VariableDefinition>($2, false, $4); }
   | IDENTIFIER '=' stmt   { $$ = AST::make<AssignmentExpression>($1, $3); }
   | exp ';'
;

block: /* nothing */ { $$ = AST::make<Block>(std::vector<AST>{}); }
   | block stmt { 
                        $1.cast<Block>()->nodes().emplace_back(std::move($2));
                        $$ = $1;
                    }
;

exp: exp BLOP exp         { $$ = AST::make<BinaryLogicalExpression>(std::move($1), std::move($3), $2); }
   | exp '+' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Add); }
   | exp '-' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Subtract);}
   | exp '*' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Multiply); }
   | exp '/' exp          { $$ = AST::make<BinaryExpression>(std::move($1), std::move($3), BinaryOperator::Divide); }
   | '|' exp '|'            { $$ = AST::make<UnaryExpression>($2, UnaryOperator::Abs); }
   |'(' exp ')'           { $$ = $2; }
   | '-' exp %prec UMINUS { $$ = AST::make<UnaryExpression>($2, UnaryOperator::Minus); }
   | NUMBER               { $$ = AST::make<Number>($1); }
   | IDENTIFIER           { $$ = AST::make<NameReference>($1); }
   | BUILTIN '(' explist ')' { $$ = AST::make<BuiltInFunction>($1, $3); }
   | IDENTIFIER '(' explist ')' { $$ = AST::make<FunctionCall>($1, $3); }
;

explist: exp { $$ = std::vector<AST>{std::move($1)}; }
 | explist ',' exp  { $1.emplace_back(std::move($3));
                      $$ = $1; }

arglist: IDENTIFIER { $$ = std::vector<std::string>{std::move($1)}; }
 | arglist ',' IDENTIFIER  { $1.emplace_back(std::move($3));
                      $$ = $1; }

calclist: /* nothing */
  | calclist block EOF {
     driver->setAST($2);
    }

  | calclist error EOF { yyerrok; printf("> "); }
 ;

%%

void cheasle::Parser::error(const location& loc, const std::string& msg)
{
  std::cerr << loc << ": " << msg << std::endl;
  if (lexer.size() == 0)      // if token is unknown (no match)
    lexer.matcher().winput(); // skip character
}
