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

/* equality operators */
"!="    { return cheasle::Parser::make_EQ(EqualityOperator::NE, location()); }
"=="    { return cheasle::Parser::make_EQ(EqualityOperator::EQ, location()); }

/* comparison operators */
">"     { return cheasle::Parser::make_CMP(ComparisonOperator::GT, location()); }
"<"     { return cheasle::Parser::make_CMP(ComparisonOperator::LT, location()); }
">="    { return cheasle::Parser::make_CMP(ComparisonOperator::GE, location()); }
"<="    { return cheasle::Parser::make_CMP(ComparisonOperator::LE, location()); }

/* logical operations */
"and"   { return cheasle::Parser::make_AND(location()); }
"or"   { return cheasle::Parser::make_OR(location()); }
"not"   { return cheasle::Parser::make_NOT(location()); }

/* values */
{float} { return cheasle::Parser::make_VALUE(atof(text()), location()); }
"true"  { return cheasle::Parser::make_VALUE(true, location()); }
"false" { return cheasle::Parser::make_VALUE(false, location()); }

/* data types */
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
"printd" { return cheasle::Parser::make_BUILTIN(BuiltInFunctionId::Printd, location()); }
"printb" { return cheasle::Parser::make_BUILTIN(BuiltInFunctionId::Printb, location()); }

/* names */
[a-zA-Z][a-zA-Z0-9]*  { return cheasle::Parser::make_IDENTIFIER(text(), location()); }

<<EOF>> {return cheasle::Parser::make_EOF(location()); }
%%
