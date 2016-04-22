%{
%}

%token ABSTRACT
%token ADD_LET
%token AND_LET
%token BOOLEAN
%token BREAK
%token BYTE
%token CASE
%token CATCH
%token CHAR
%token CLASS
%token CONST
%token CONTINUE
%token DEBUGGER
%token DECREMENT
%token DEFAULT
%token DELETE
%token DIV_LET
%token DO
%token DOUBLE
%token ELSE
%token ENUM
%token EQ
%token EQUAL
%token EXPORT
%token EXTENDS
%token FALSE
%token FINAL
%token FINALLY
%token FLOAT
%token FOR
%token FUNCTION
%token GOTO
%token GRATER_EQUAL
%token IDENTIFIER
%token IDENTIFIER_NAME
%token IF
%token IMPLEMENTS
%token IMPORT
%token IN
%token INCREMENT
%token INSTANCEOF
%token INT
%token INTERFACE
%token LESS_EQUAL
%token LOGICAL_AND
%token LOGICAL_OR
%token LONG
%token MOD_LET
%token MUL_LET
%token NATIVE
%token NEW
%token NOT_EQ
%token NOT_EQUAL
%token NOT_LET
%token NULL
%token NUMERIC_LITERAL
%token OR_LET
%token PACKAGE
%token PRIVATE
%token PROTECTED
%token PUBLIC
%token REGEXP_LITERAL
%token RETURN
%token SHIFT_LEFT
%token SHIFT_LEFT_LET
%token SHIFT_RIGHT
%token SHIFT_RIGHT_LET
%token SHORT
%token STATIC
%token STRING_LITERAL
%token SUB_LET
%token SUPER
%token SWITCH
%token SYNCHRONIZED
%token THIS
%token THROW
%token THROWS
%token TRANSIENT
%token TRUE
%token TRY
%token TYPEOF
%token U_SHIFT_RIGHT
%token U_SHIFT_RIGHT_LET
%token VAR
%token VOID
%token VOLATILE
%token WHILE
%token WITH
%token OBJECTBRACK
%token FUNCTIONEXPR
%token DOT3
%token YIELD
%token ARROW
%token ARROWPAREN
%token OF

%start Program

%%

Program
  : SourceElements
  ;

FunctionDeclaration
  : FUNCTION IDENTIFIER '(' FormalParameterList_opt ')' '{' FunctionBody '}'
  ;

FunctionExpression
  : FUNCTIONEXPR Identifier_opt '(' FormalParameterList_opt ')' '{' FunctionBody '}'
  ;

FormalParameterList
  : IDENTIFIER
  | FormalParameterList ',' IDENTIFIER
  ;

FunctionBody
  :
  | SourceElements
  ;

SourceElements
  : SourceElement
  | SourceElements SourceElement
  ;

SourceElement
  : Statement
  | FunctionDeclaration
  | ClassDeclaration
  | ImportDeclaration
  ;

ImportDeclaration
  : IMPORT ModuleSpecifier ';' 
  ;

ModuleSpecifier
  : STRING_LITERAL
  ;

ClassDeclaration
  : CLASS BindingIdentifier ClassTail
  ;

ClassTail
  : ClassHeritage_opt '{' ClassBody_opt '}'
  ;

ClassHeritage_opt
  :
  | ClassHeritage
  ;

ClassHeritage
  : EXTENDS LeftHandSideExpression
  ;

ClassBody_opt
  :
  | ClassBody
  ;

ClassBody
  : ClassElementList
  ;

ClassElementList
  : ClassElement
  | ClassElementList ClassElement
  ;

ClassElement
  : MethodDefinition
  | STATIC MethodDefinition
  | ';'
  ;

MethodDefinition
  : PropertyName '(' StrictFormalParameters ')' '{' FunctionBody '}'
  ;

StrictFormalParameters
  :  FormalParameters
  ;
FormalParameters
  : FormalParameterList 
  | FormalParameter
  ;
// GeneratorMethod[?Yield]
// get PropertyName[?Yield] ( ) { FunctionBody }
// set PropertyName[?Yield] ( PropertySetParameterList ) { FunctionBody }
// PropertySetParameterList :

FormalParameter
  : BindingElement
  ;

BindingPattern
  : ObjectBindingPattern
  | ArrayBindingPattern
  ;

ObjectBindingPattern
  : '{' '}'
  | '{' BindingPropertyList '}'
  ;

BindingPropertyList
  : BindingProperty
  | BindingPropertyList ',' BindingProperty
  ;

BindingProperty
  : SingleNameBinding
  | PropertyName ':' BindingElement
  ;

PropertyName
  : LiteralPropertyName
  // | ComputedPropertyName
  ;
LiteralPropertyName
  : IDENTIFIER_NAME
  | STRING_LITERAL
  | NUMERIC_LITERAL
  ;

BindingElement
  : SingleNameBinding
  | BindingPattern ',' Initializer_opt
  ;

SingleNameBinding
  : BindingIdentifier Initializer_opt
  ;

ArrayBindingPattern
  : '[' Elision_opt BindingRestElement_opt ']'
  | '[' BindingElementList ']'
  | '[' BindingElementList ',' Elision_opt BindingRestElement_opt ']'
  ;

BindingRestElement_opt
  :
  | BindingRestElement
  ;

BindingRestElement
  : DOT3 BindingIdentifier
  ;

BindingElementList
  : BindingElisionElement
  | BindingElementList ',' BindingElisionElement
  ;

BindingElisionElement
  : Elision_opt BindingElement
  ;

FormalParameterList_opt
  :
  | FormalParameterList
  ;

Identifier_opt
  :
  | IDENTIFIER
  ;

Statement
  : Block
  | VariableStatement
  | EmptyStatement
  | ExpressionStatement
  | IfStatement
  | IterationStatement
  | ContinueStatement
  | BreakStatement
  | ReturnStatement
  | WithStatement
  | LabelledStatement
  | SwitchStatement
  | ThrowStatement
  | TryStatement
  ;

Block
  : '{' '}'
  | '{' StatementList '}'
  ;

StatementList
  : Statement
  | StatementList Statement
  ;

VariableStatement
  : VAR VariableDeclarationList ';'
  ;

VariableDeclarationList
  : VariableDeclaration
  | VariableDeclarationList ',' VariableDeclaration
  ;

VariableDeclarationListNoIn
  : VariableDeclarationNoIn
  | VariableDeclarationListNoIn ',' VariableDeclarationNoIn
  ;

VariableDeclaration
  : IDENTIFIER Initializer_opt
  ;

VariableDeclarationNoIn
  : IDENTIFIER InitializerNoIn_opt
  ;

Initializer
  : '=' AssignmentExpression
  ;

InitializerNoIn
  : '=' AssignmentExpressionNoIn
  ;

EmptyStatement
  : ';'
  ;

ExpressionStatement
  : Expression ';'
  ;

IfStatement
  : IF '(' Expression ')' Statement ELSE Statement
  | IF '(' Expression ')' Statement
  ;

IterationStatement
  : DO Statement WHILE '(' Expression ')' ';'
  | WHILE '(' Expression ')' Statement
  | FOR '(' ExpressionNoIn_opt ';' Expression_opt ';' Expression_opt ')' Statement
  | FOR '(' VAR VariableDeclarationListNoIn ';' Expression_opt ';' Expression_opt ')' Statement
  | FOR '(' LeftHandSideExpression IN Expression ')' Statement
  | FOR '(' VAR VariableDeclarationNoIn IN Expression ')' Statement
  | FOR '(' LeftHandSideExpression OF AssignmentExpression ')' Statement
  ;

ContinueStatement
  : CONTINUE Identifier_opt ';'
  ;

BreakStatement
  : BREAK Identifier_opt ';'
  ;

ReturnStatement
  : RETURN Expression_opt ';'
  ;

WithStatement
  : WITH '(' Expression ')' Statement
  ;

SwitchStatement
  : SWITCH '(' Expression ')' CaseBlock
  ;

CaseBlock
  : '{' CaseClauses_opt '}'
  | '{' CaseClauses_opt DefaultClause CaseClauses_opt '}'
  ;

CaseClauses
  : CaseClause
  | CaseClauses CaseClause
  ;

CaseClause
  : CASE Expression ':' StatementList_opt
  ;

DefaultClause
  : DEFAULT ':' StatementList_opt
  ;

LabelledStatement
  : IDENTIFIER ':' Statement
  ;

ThrowStatement
  : THROW Expression ';'
  ;

TryStatement
  : TRY Block catch
  | TRY Block finally
  | TRY Block catch finally
  ;

catch
  : CATCH '(' IDENTIFIER ')' Block
  ;

finally
  : FINALLY Block
  ;

StatementList_opt
  :
  | StatementList
  ;

Initializer_opt
  :
  | Initializer
  ;

InitializerNoIn_opt
  :
  | InitializerNoIn
  ;

CaseClauses_opt
  :
  | CaseClauses
  ;

Literal
  : NullLiteral
  | BooleanLiteral
  | NUMERIC_LITERAL
  | STRING_LITERAL
  | REGEXP_LITERAL
  ;

NullLiteral
  : NULL
  ;

BooleanLiteral
  : TRUE
  | FALSE
  ;

PrimaryExpression
  : THIS
  | IDENTIFIER
  | Literal
  | ArrayLiteral
  | ObjectLiteral
  | '(' Expression ')'
  ;

ArrayLiteral
  : '[' Elision_opt ']'
  | '[' ElementList ']'
  | '[' ElementList ',' Elision_opt ']'
  ;

ElementList
  : Elision_opt AssignmentExpression
  | ElementList ',' Elision_opt AssignmentExpression
  ;

Elision
  : ','
  | Elision ','
  ;

ObjectLiteral
  : OBJECTBRACK PropertyNameAndValueList_opt '}'
  ;

PropertyNameAndValueList_opt
  :
  | PropertyNameAndValueList
  ;

PropertyNameAndValueList
  : PropertyName ':' AssignmentExpression
  | PropertyNameAndValueList ',' PropertyName ':' AssignmentExpression
  ;

MemberExpression
  : PrimaryExpression
  | FunctionExpression
  | MemberExpression '[' Expression ']'
  | MemberExpression '.' IDENTIFIER
  | NEW MemberExpression Arguments
  ;

NewExpression
  : MemberExpression
  | NEW NewExpression
  ;

CallExpression
  : MemberExpression Arguments
  | CallExpression Arguments
  | CallExpression '[' Expression ']'
  | CallExpression '.' IDENTIFIER
  ;

Arguments
  : '(' ')'
  | '(' ArgumentList ')'
  ;

ArgumentList
  : AssignmentExpression
  | ArgumentList ',' AssignmentExpression
  ;

LeftHandSideExpression
  : NewExpression
  | CallExpression
  ;

PostfixExpression
  : LeftHandSideExpression
  | LeftHandSideExpression INCREMENT
  | LeftHandSideExpression DECREMENT
  ;

UnaryExpression
  : PostfixExpression
  | DELETE UnaryExpression
  | VOID UnaryExpression
  | TYPEOF UnaryExpression
  | INCREMENT UnaryExpression
  | DECREMENT UnaryExpression
  | '+' UnaryExpression
  | '-' UnaryExpression
  | '~' UnaryExpression
  | '!' UnaryExpression
  ;

MultiplicativeExpression
  : UnaryExpression
  | MultiplicativeExpression '*' UnaryExpression
  | MultiplicativeExpression '/' UnaryExpression
  | MultiplicativeExpression '%' UnaryExpression
  ;

AdditiveExpression
  : MultiplicativeExpression
  | AdditiveExpression '+' MultiplicativeExpression
  | AdditiveExpression '-' MultiplicativeExpression
  ;

ShiftExpression
  : AdditiveExpression
  | ShiftExpression SHIFT_LEFT AdditiveExpression
  | ShiftExpression SHIFT_RIGHT AdditiveExpression
  | ShiftExpression U_SHIFT_RIGHT AdditiveExpression
  ;

RelationalExpression
  : ShiftExpression
  | RelationalExpression '<' ShiftExpression
  | RelationalExpression '>' ShiftExpression
  | RelationalExpression LESS_EQUAL ShiftExpression
  | RelationalExpression GRATER_EQUAL ShiftExpression
  | RelationalExpression INSTANCEOF ShiftExpression
  | RelationalExpression IN ShiftExpression
  ;

RelationalExpressionNoIn
  : ShiftExpression
  | RelationalExpressionNoIn '<' ShiftExpression
  | RelationalExpressionNoIn '>' ShiftExpression
  | RelationalExpressionNoIn LESS_EQUAL ShiftExpression
  | RelationalExpressionNoIn GRATER_EQUAL ShiftExpression
  | RelationalExpressionNoIn INSTANCEOF ShiftExpression
  ;

EqualityExpression
  : RelationalExpression
  | EqualityExpression EQUAL RelationalExpression
  | EqualityExpression NOT_EQUAL RelationalExpression
  | EqualityExpression EQ RelationalExpression
  | EqualityExpression NOT_EQ RelationalExpression
  ;

EqualityExpressionNoIn
  : RelationalExpressionNoIn
  | EqualityExpressionNoIn EQUAL RelationalExpressionNoIn
  | EqualityExpressionNoIn NOT_EQUAL RelationalExpressionNoIn
  | EqualityExpressionNoIn EQ RelationalExpressionNoIn
  | EqualityExpressionNoIn NOT_EQ RelationalExpressionNoIn
  ;

BitwiseAndExpression
  : EqualityExpression
  | BitwiseAndExpression '&' EqualityExpression
  ;

BitwiseAndExpressionNoIn
  : EqualityExpressionNoIn
  | BitwiseAndExpressionNoIn '&' EqualityExpressionNoIn
  ;

BitwiseXorExpression
  : BitwiseAndExpression
  | BitwiseXorExpression '^' BitwiseAndExpression
  ;

BitwiseXorExpressionNoIn
  : BitwiseAndExpressionNoIn
  | BitwiseXorExpressionNoIn '^' BitwiseAndExpressionNoIn
  ;

BitwiseOrExpression
  : BitwiseXorExpression
  | BitwiseOrExpression '|' BitwiseXorExpression
  ;

BitwiseOrExpressionNoIn
  : BitwiseXorExpressionNoIn
  | BitwiseOrExpressionNoIn '|' BitwiseXorExpressionNoIn
  ;

LogicalAndExpression
  : BitwiseOrExpression
  | LogicalAndExpression LOGICAL_AND BitwiseOrExpression
  ;

LogicalAndExpressionNoIn
  : BitwiseOrExpressionNoIn
  | LogicalAndExpressionNoIn LOGICAL_AND BitwiseOrExpressionNoIn
  ;

LogicalOrExpression
  : LogicalAndExpression
  | LogicalOrExpression LOGICAL_OR LogicalAndExpression
  ;

LogicalOrExpressionNoIn
  : LogicalAndExpressionNoIn
  | LogicalOrExpressionNoIn LOGICAL_OR LogicalAndExpressionNoIn
  ;

ConditionalExpression
  : LogicalOrExpression
  | LogicalOrExpression '?' AssignmentExpression ':' AssignmentExpression
  ;

ConditionalExpressionNoIn
  : LogicalOrExpressionNoIn
  | LogicalOrExpressionNoIn '?' AssignmentExpressionNoIn ':' AssignmentExpressionNoIn
  ;

AssignmentExpression
  : ConditionalExpression
  | ArrowFunction
  | LeftHandSideExpression AssignmentOperator AssignmentExpression
  ;

AssignmentExpressionNoIn
  : ConditionalExpressionNoIn
  | LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
  ;

ArrowFunction
  : ArrowParameters ARROW ConciseBody
  ;

ArrowParameters
  : BindingIdentifier
  | CoverParenthesizedExpressionAndArrowParameterList
  ;

CoverParenthesizedExpressionAndArrowParameterList
  :
  | ARROWPAREN Expression ')'
  | ARROWPAREN ')'
  | ARROWPAREN DOT3 BindingIdentifier ')'
  | ARROWPAREN Expression ',' DOT3 BindingIdentifier ')'
  ;

BindingIdentifier
  : Identifier
  | YIELD
  ;

Identifier
  : IDENTIFIER_NAME
  ;

ConciseBody
  : AssignmentExpression
  | '{' FunctionBody '}'
  ;

AssignmentOperator
  : '='
  | MUL_LET
  | DIV_LET
  | MOD_LET
  | ADD_LET
  | SUB_LET
  | SHIFT_LEFT_LET
  | SHIFT_RIGHT_LET
  | U_SHIFT_RIGHT_LET
  | AND_LET
  | NOT_LET
  | OR_LET
  ;

Expression
  : AssignmentExpression
  | Expression ',' AssignmentExpression
  ;

ExpressionNoIn
  : AssignmentExpressionNoIn
  | ExpressionNoIn ',' AssignmentExpressionNoIn
  ;

ExpressionNoIn_opt
  :
  | ExpressionNoIn
  ;

Expression_opt
  :
  | Expression
  ;

Elision_opt
  :
  | Elision
  ;

%%

