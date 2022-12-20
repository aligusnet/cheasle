%top{
#include <cheasle/driver.h>
#include "parser.h" 
#include "location.h"
}

%option c++ noyywrap nodefault
%option outfile="lexer.cpp" header-file="lexer.h"
%option bison-complete
%option bison-cc-namespace=cheasle
%option bison-cc-parser=Parser
%option bison-locations

%option exception="cheasle::Parser::syntax_error(location(), \"Unknown token.\")"

%option namespace=cheasle
%option lexer=Lexer
%option params="Driver* driver"

float -?([0-9]*[.])?[0-9]+

%%
"//".*  
[ \t\n]  /* ignore whitespace */ 
 /* single character ops */
"+" |
"-" |
"*" |
"/" |
"=" |
"|" |
"," |
";" |
":" |
"{" |
"}" |
"(" |
")"     { return cheasle::Parser::symbol_type(chr(), location()); }

 /* comparison ops, all are a CMP token */
">"     { return cheasle::Parser::make_BLOP(BinaryLogicalOperator::GT, location()); }
"<"     { return cheasle::Parser::make_BLOP(BinaryLogicalOperator::LT, location()); }
"!="    { return cheasle::Parser::make_BLOP(BinaryLogicalOperator::NE, location()); }
"=="    { return cheasle::Parser::make_BLOP(BinaryLogicalOperator::EQ, location()); }
">="    { return cheasle::Parser::make_BLOP(BinaryLogicalOperator::GE, location()); }
"<="    { return cheasle::Parser::make_BLOP(BinaryLogicalOperator::LE, location()); }

{float} { return cheasle::Parser::make_VALUE(atof(text()), location()); }
"true"  { return cheasle::Parser::make_VALUE(true, location()); }
"false" { return cheasle::Parser::make_VALUE(false, location()); }

"bool" { return cheasle::Parser::make_VALUE_TYPE(ValueType::Boolean, location()); }
"double" { return cheasle::Parser::make_VALUE_TYPE(ValueType::Double, location()); }

 /* keywords */

"if"    { return cheasle::Parser::make_IF(location()); }
"else"  { return cheasle::Parser::make_ELSE(location()); }
"while" { return cheasle::Parser::make_WHILE(location()); }
"const"   { return cheasle::Parser::make_CONST(location()); }
"let"   { return cheasle::Parser::make_LET(location()); }
"def"   { return cheasle::Parser::make_DEF(location()); }

 /* built-in functions */
"sqrt"  { return cheasle::Parser::make_BUILTIN(BuiltInFunctionId::Sqrt, location()); }
"exp"   { return cheasle::Parser::make_BUILTIN(BuiltInFunctionId::Exp, location()); }
"log"   { return cheasle::Parser::make_BUILTIN(BuiltInFunctionId::Log, location()); }
"print" { return cheasle::Parser::make_BUILTIN(BuiltInFunctionId::Print, location()); }

 /* names */
[a-zA-Z][a-zA-Z0-9]*  { return cheasle::Parser::make_IDENTIFIER(text(), location()); }

<<EOF>> {return cheasle::Parser::make_EOF(location()); }
%%
