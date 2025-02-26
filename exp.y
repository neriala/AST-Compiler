%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "lex.yy.c"
struct SymbolTable; 

typedef struct node {
    char *token;
    struct node **children;
    int num_children;
    int is_literal;
} node;

// Structures for Symbol Table
typedef struct Variable {
    char *name;            
    char *type;            
    union {
        int int_value;
        float float_value;
        double double_value;
        char char_value;
        struct {
            char *string_value;
            int size;      
        };
        char *bool_value;
    } value;
    int is_initialized;  
} Variable;


typedef struct Function {
    char *name;
    char *return_type;
    char *access_modifier;
    Variable **parameters;
    int param_count;
    struct SymbolTable *scope; 
    int is_static; 
} Function;


typedef struct SymbolTable {
    Variable **variables;
    int var_count;
    Function **functions;
    int func_count;
    struct SymbolTable *parent;
    Function *enclosingFunction; 
} SymbolTable;



node* mknode(char *token, int num_children, int is_literal, ...);
void add_child(node *parent, node *child);
void printtree(node *tree, int depth);
void printTabs(int n);
int yylex();
int yyerror(char *e);

/*------part 2------*/
SymbolTable *globalScope = NULL;
void checktree(node *tree, SymbolTable *currentScope);
void processVarDeclaration(node *declarationNode, SymbolTable *currentScope);
void processVarIdList(SymbolTable *currentScope,char *type,node *var_list);
SymbolTable* processFunction(node *functionNode, SymbolTable *currentScope);
void processBody(node *bodyNode, SymbolTable *currentScope);
void processReturn(node *returnNode, SymbolTable *currentScope);
void processIf(node *ifNode, SymbolTable *currentScope);
void processAssignment(node *assignmentNode, SymbolTable *currentScope);
void processAssignmentinVarDecl(node *assignmentNode, SymbolTable *currentScope, char *expectedType);
void processAssignmentinStringDecl(node *assignmentNode, SymbolTable *currentScope);
int findVariableInCurrentScope(SymbolTable *currentScope, const char *name);
Variable** parseParameters(node *args, int *paramCount);
void processVarIdListRecursive(node *varIdList, char *type, Variable ***totalParameters, int *totalParamCount);
char* findVariableType(SymbolTable *table, char *name);
char* findFunctionReturnType(SymbolTable *table, char *name);
Function* findFunction(SymbolTable *currentScope, const char *name);
char* exprType(node *tree, SymbolTable *currentScope);
Variable* findSymbol(SymbolTable *currentScope, const char *name);
void printSymbolTablesRecursively(SymbolTable *table, int level);
char* validateFunctionCall(node *tree, SymbolTable *currentScope);

/*------part 3------*/
char st[50][10];
int top = -1;
int i_l =0;
char s[4];
char temp[2] = "t";
int label[50];
int Lnum = 0, Ltop = 0;
int indent_level = 0; // משתנה לשמירה על רמת ההזחה
int flag = 0;
void check3AC(node *tree);
void processAssignment3AC(node *tree);
void processIfElse3AC(node *tree);
void processWhile3AC(node *tree);
void processDoWhile3AC(node *tree);
void processLogicalAnd3AC(node *tree);
void processLogicalOr3AC(node *tree);
void processLogicalANDInIf3AC(node* tree);
void processLogicalORInIf3AC(node* tree);
void processFor3AC(node *tree);
void processFunction3AC(node *tree);
void processMainFunction3AC(node *tree);
void processFunctionCall3AC(node *tree);
int getTypeSize(char *type);
int calculateFunctionMemory(Function *function);
void printIndented(const char *format, ...);



// List of essential nodes that should be printed
const char *essential_nodes[] = {
    "MAIN", "FUNC", "BODY", "if", "else", "while", "do", "for",
    "assignment", "return", "funcCall", "array_index", "length",
    "(+)", "(-)", "(*)", "(/)", "(>)", "(>=)", "(<)", "(<=)", "(==)", "(!=)","(!)","&&","||",
    "&", "*", "**","args","var_declaration","then","()","array_decl_assign","array_assign","size","array_decl","array_tail",
    "]","[","{}","{","}","funcCallVars"

};

int is_essential_node(const char *token) {
    int size = sizeof(essential_nodes) / sizeof(essential_nodes[0]);
    for (int i = 0; i < size; i++) {
        if (strcmp(token, essential_nodes[i]) == 0) {
            return 1;
        }
    }
    return 0;
}

SymbolTable* createSymbolTable(SymbolTable *parent) {
    SymbolTable *newTable = (SymbolTable*)malloc(sizeof(SymbolTable));
    newTable->variables = NULL;
    newTable->var_count = 0;
    newTable->functions = NULL;
    newTable->func_count = 0;
    newTable->parent = parent;
    newTable->enclosingFunction = NULL; 
    return newTable;
}



void addVariable(SymbolTable *table, char *name, char *type, int size) {
    for (int i = 0; i < table->var_count; i++) {
        if (strcmp(table->variables[i]->name, name) == 0) {
            printf("Error: Duplicate variable declaration '%s'\n", name);
            exit(1);
        }
    }
    table->variables = (Variable**)realloc(table->variables, sizeof(Variable*) * (table->var_count + 1));
    if (table->variables == NULL) {
        fprintf(stderr, "Out of memory while reallocating variables\n");
        exit(1);
    }
    table->variables[table->var_count] = (Variable*)malloc(sizeof(Variable));
    if (table->variables[table->var_count] == NULL) {
        fprintf(stderr, "Out of memory while allocating new variable\n");
        exit(1);
    }
    table->variables[table->var_count]->name = strdup(name);

    if (table->variables[table->var_count]->name == NULL) {
        fprintf(stderr, "Out of memory while duplicating variable name\n");
        exit(1);
    }
    table->variables[table->var_count]->type = strdup(type);
    if (table->variables[table->var_count]->type == NULL) {
        fprintf(stderr, "Out of memory while duplicating variable type\n");
        exit(1);
    }

    if (strcmp(type, "int") == 0) {
        table->variables[table->var_count]->value.int_value = 0;

    } else if (strcmp(type, "float") == 0) {
        table->variables[table->var_count]->value.float_value = 0.0f;

    } else if (strcmp(type, "double") == 0) {
        table->variables[table->var_count]->value.double_value = 0.0;

    } else if (strcmp(type, "char") == 0) {
        table->variables[table->var_count]->value.char_value = '\0';

    } else if (strcmp(type, "string") == 0) {
        table->variables[table->var_count]->value.string_value = (char*)malloc(sizeof(char) * size);
        if (table->variables[table->var_count]->value.string_value == NULL) {
            fprintf(stderr, "Out of memory while initializing string value\n");
            exit(1);
        }
        strcpy(table->variables[table->var_count]->value.string_value, "");
        table->variables[table->var_count]->value.size = size;

    } else if (strcmp(type, "bool") == 0) {
        table->variables[table->var_count]->value.bool_value = strdup("true");

    } else {
        printf("Error: Unsupported variable type '%s'\n", type);
        exit(1);
    }

    table->variables[table->var_count]->is_initialized = 0;
    table->var_count++;
}


// Function to add a function to the Symbol Table
void addFunction(SymbolTable *table, char *name, char *return_type, Variable **parameters, int param_count,int is_static,char *access_modifier) {
    for (int i = 0; i < table->func_count; i++) {
        if (strcmp(table->functions[i]->name, name) == 0) {
            printf("Error: Duplicate function declaration '%s'\n", name);
            exit(1);
        }
    }
    table->functions = (Function**)realloc(table->functions, sizeof(Function*) * (table->func_count + 1));
    table->functions[table->func_count] = (Function*)malloc(sizeof(Function));
    table->functions[table->func_count]->name = strdup(name);
    table->functions[table->func_count]->return_type = strdup(return_type);
    table->functions[table->func_count]->parameters = parameters;
    table->functions[table->func_count]->param_count = param_count;
    table->functions[table->func_count]->is_static = is_static;
    table->functions[table->func_count]->access_modifier = strdup(access_modifier);
    table->func_count++;
}


%}

%union {
    struct node *node;
    char *string;
}

%token <string> BOOL CHAR INT DOUBLE FLOAT STRING INTPTR CHARPTR DOUBLEPTR FLOATPTR VARIABLE NULL_TOKEN
%token <string> IF ELSE WHILE DO FOR
%token <string> ARGS PUBLIC PRIVATE STATIC RETURN VOID MAIN
%token <string> AND DIVISION ASSIGNMENT EQL GREATER GREATEREQL LESS LESSEQL MINUS NOT NOTEQL OR PLUS MULTI ADDRESS 
%token <string> LENGTH SEMICOLON COLON COMMA OPENBRACE CLOSEBRACE OPENPAREN CLOSEPAREN OPENBRACKET CLOSEBRACKET COMMENT
%token <string> BOOLTRUE BOOLFALSE IDENTIFIER NUM STRING_LTL CHAR_LTL HEX_LTL DOUBLE_LTL FLOAT_LTL

%left OR
%left AND
%left EQL NOTEQL
%left GREATER LESS GREATEREQL LESSEQL
%left PLUS MINUS
%left MULTI DIVISION
%left NOT
%left OPENPAREN CLOSEPAREN
%right ASSIGNMENT

%type <node> project statement_list statement expr function_decl return_stmt declears_list
%type <node> functype ret_type args is_stat body declears para_list var_id_list type_id funcs main_decl
%type <node> ret_ltrl funcCall funcCallVar array_declaration array_assign array_index array_tail var_declaration var_assignment
%type <node> if_statement if_else_statement while_statement do_while_statement for_statement
%%

project: statement_list { 
            //printf("(CODE\n");
            checktree($1, globalScope);
            //printtree($1, 1); 
            //printf(")\n");
            check3AC($1);
            printf("\n\n");

        }
        | funcs { 
            //printf("(CODE\n");
            checktree($1, globalScope);
            //printtree($1, 1); 
            //printf(")\n");
            check3AC($1);
            printf("\n\n");

        }
        ;

funcs :  funcs function_decl  { 
            add_child($1, $2);
            $$ = $1;
        }
        | function_decl { $$ = mknode("",1,0,$1); }
        | funcs main_decl  {
            add_child($1, $2);
            $$ = $1;
        }
        | main_decl { $$ = mknode("",1,0,$1); }
        ;

function_decl: functype ret_type IDENTIFIER OPENPAREN args CLOSEPAREN is_stat OPENBRACE body CLOSEBRACE {
                  $$ = mknode("FUNC", 6, 0, $1, $2, mknode($3, 0, 1), $5, $7, $9); 
              };

main_decl: functype ret_type MAIN OPENPAREN args CLOSEPAREN is_stat OPENBRACE body CLOSEBRACE {
                  $$ = mknode("MAIN", 6, 0, $1, $2, mknode("main", 0, 1), $5, $7, $9); 
              };

functype: PUBLIC { 
            $$ = mknode("public", 0, 1); 
          }
        | PRIVATE { 
            $$ = mknode("private", 0, 1); 
          }
        ;

ret_type: BOOL { $$ = mknode("bool", 0, 1); }
         | CHAR { $$ = mknode("char", 0, 1); }
         | INT { $$ = mknode("int", 0, 1); }
         | FLOAT { $$ = mknode("float", 0, 1); }
         | DOUBLE { $$ = mknode("double", 0, 1); }
         | INTPTR { $$ = mknode("int*", 0, 1); }
         | CHARPTR { $$ = mknode("char*", 0, 1); }
         | FLOATPTR { $$ = mknode("float*", 0, 1); }
         | DOUBLEPTR { $$ = mknode("double*", 0, 1); }
         | STRING { $$ = mknode("string", 0, 1); }
         | VOID { $$ = mknode("RET-void", 0, 1); }
         ;

args: ARGS declears_list { $$ = mknode("args", 1, 0, $2); }
     | { $$ = mknode("NON_ARGS", 0, 1); }
     ;



declears_list: declears SEMICOLON declears_list {
                  node *decl_node = mknode("decl_list", 1, 0, $1);
                  add_child(decl_node, $3);
                  $$ = decl_node;
              }
             | declears { $$ = $1; }
             ;

declears: para_list { $$ = $1; }
         ;

para_list: type_id COLON var_id_list { $$ = mknode("para_list", 2, 0, $1, $3); }
         ;

var_id_list: var_assignment COMMA var_id_list {
                $$=mknode("var_id_list",1,0,$1);
                add_child($$, $3);
            }
            | var_assignment { 
                $$ = $1; 
            }
            ;

type_id: BOOL { $$ = mknode("bool", 0, 1); }
       | STRING { $$ = mknode("string", 0, 1); }
       | CHAR { $$ = mknode("char", 0, 1); }
       | INT { $$ = mknode("int", 0, 1); }
       | FLOAT { $$ = mknode("float", 0, 1); }
       | DOUBLE { $$ = mknode("double", 0, 1); }
       | INTPTR { $$ = mknode("int*", 0, 1); }
       | CHARPTR { $$ = mknode("char*", 0, 1); }
       | FLOATPTR { $$ = mknode("float*", 0, 1); }
       | DOUBLEPTR { $$ = mknode("double*", 0, 1); }
       ;

return_stmt: RETURN ret_ltrl SEMICOLON { $$ = mknode("return", 1, 0, $2); }
           ;

ret_ltrl: HEX_LTL { $$ = mknode("hex",1,0,mknode($1, 0, 1)); }
         /*| DOUBLE_LTL { $$ = mknode($1, 0, 1); }
         | FLOAT_LTL { $$ = mknode($1, 0, 1); }
         | CHAR_LTL { $$ = mknode("char",1,0,mknode($1, 0, 1)); }*/
         | STRING_LTL { $$ = mknode("string",1,0,mknode($1, 0, 1)); }
         | expr { $$ = $1; }
         | NULL_TOKEN {$$ = mknode("NULL", 0, 1);}
         /*| DOUBLE_LTL { $$ = mknode("double",1,0,mknode($1, 0, 1)); }
         | FLOAT_LTL { $$ = mknode("float",1,0,mknode($1, 0, 1)); }*/

         ;

statement_list: statement_list statement { add_child($1, $2); $$ = $1; }
              | statement { $$ = mknode("",1,0,$1); }
              ;

statement: if_statement { $$ = $1; }
         | if_else_statement { $$ = $1; }
         | while_statement { $$ = $1; }
         | do_while_statement { $$ = $1; }
         | for_statement { $$ = $1; }
         | var_declaration SEMICOLON { $$ = $1; }
        /* | var_declaration { $$ = $1; }*/
         | var_assignment SEMICOLON { $$ = $1; }
         | var_assignment { $$ = $1; } 
         | funcCall { $$ = $1; }
         | return_stmt { $$ = $1; }
         | array_declaration { $$ = $1; }
         | array_assign { $$ = $1; }
         | expr SEMICOLON { $$ = $1; }
         | OPENBRACE body CLOSEBRACE {$$=mknode("{",2,0,$2,mknode("}",0,0));}
         | OPENBRACE CLOSEBRACE {$$=mknode("{}",0,0);}

         ;


if_statement: IF OPENPAREN expr CLOSEPAREN statement SEMICOLON { $$ = mknode("if", 2, 0, $3, mknode("then", 1, 0, $5)); }
             | IF OPENPAREN expr CLOSEPAREN OPENBRACE statement_list CLOSEBRACE { $$ = mknode("if", 2, 0, $3, mknode("then", 1, 0, $6)); }
             ;

if_else_statement: IF OPENPAREN expr CLOSEPAREN statement ELSE statement { $$ = mknode("if", 3, 0, $3, mknode("then", 1, 0, $5), mknode("else", 1, 0, $7)); }
                 | IF OPENPAREN expr CLOSEPAREN OPENBRACE statement_list CLOSEBRACE ELSE statement{ $$ = mknode("if", 3, 0, $3, mknode("then", 1, 0, $6), mknode("else", 1, 0, $9)); }
                 | IF OPENPAREN expr CLOSEPAREN OPENBRACE statement_list CLOSEBRACE ELSE OPENBRACE statement_list CLOSEBRACE { $$ = mknode("if", 3, 0, $3, mknode("then", 1, 0, $6), mknode("else", 1, 0, $10)); }
                 ;

while_statement: WHILE OPENPAREN expr CLOSEPAREN statement { $$ = mknode("while", 2, 0, $3, $5); }
               | WHILE OPENPAREN expr CLOSEPAREN OPENBRACE statement_list CLOSEBRACE { $$ = mknode("while", 2, 0, $3, $6); }
               ;

do_while_statement: DO OPENBRACE statement_list CLOSEBRACE WHILE OPENPAREN expr CLOSEPAREN SEMICOLON { $$ = mknode("do", 2, 0, $3, mknode("while", 1, 0, $7)); }
                  ;

for_statement: FOR OPENPAREN var_assignment SEMICOLON expr SEMICOLON var_assignment CLOSEPAREN statement { $$ = mknode("for", 4, 0, $3, mknode("for-term", 1, 0, $5), mknode("next-hop", 1, 0, $7), mknode("for-body", 1, 0, $9)); }
             | FOR OPENPAREN var_assignment SEMICOLON expr SEMICOLON var_assignment CLOSEPAREN OPENBRACE statement_list CLOSEBRACE { $$ = mknode("for", 4, 0, $3, mknode("for-term", 1, 0, $5), mknode("next-hop", 1, 0, $7), mknode("for-body", 1, 0, $10)); }
             ;


var_declaration: VARIABLE type_id COLON var_id_list {
                    $$ = mknode("var_declaration", 2, 0, $2, $4);
                 }
                 ;

var_assignment:  IDENTIFIER {
                    $$ = mknode($1, 0, 1);
                }
                |expr ASSIGNMENT ret_ltrl{
                    $$ = mknode("assignment",2,0,$1,$3);
                }
                |expr ASSIGNMENT funcCall{
                    $$ = mknode("assignment",2,0,$1,$3);
                }
                |expr ASSIGNMENT expr PLUS funcCall{
                    $$ = mknode("assignment",2,0,$1,mknode("(+)",2,0,$3,$5));
                }
                |expr ASSIGNMENT expr MINUS funcCall{
                    $$ = mknode("assignment",2,0,$1,mknode("(-)",2,0,$3,$5));
                }


                ;

funcCall : IDENTIFIER OPENPAREN funcCallVar CLOSEPAREN SEMICOLON { $$ = mknode("funcCall", 2, 0, mknode($1, 0, 1), $3); }
          ;

funcCallVar: funcCallVar COMMA ret_ltrl { 
                add_child($1, $3); 
                $$ = $1; 
             }
           | ret_ltrl { $$ = mknode("funcCallVars", 1, 0, $1); }
           | { $$ = mknode("funcCallVars", 0, 0); }
           ;


array_declaration: STRING IDENTIFIER OPENBRACKET expr CLOSEBRACKET array_tail SEMICOLON {
                      node *decl_node = mknode("array_decl", 3, 0, mknode($2, 0, 1), $4, $6);
                      $$ = decl_node;
                   }
                  | STRING IDENTIFIER OPENBRACKET expr CLOSEBRACKET ASSIGNMENT STRING_LTL array_tail SEMICOLON {
                      $$ = mknode("array_decl_assign", 4, 0, mknode($2, 0, 1), $4, mknode("string",1,0,mknode($7, 0, 1)),$8);
                   }

                  | STRING IDENTIFIER OPENBRACKET expr CLOSEBRACKET SEMICOLON {
                      node *decl_node = mknode("array_decl", 2, 0, mknode($2, 0, 1), $4);
                      $$ = decl_node;
                   }
                  | STRING IDENTIFIER OPENBRACKET expr CLOSEBRACKET ASSIGNMENT STRING_LTL SEMICOLON {
                      $$ = mknode("array_decl_assign", 3, 0, mknode($2, 0, 1), $4, mknode("string",1,0,mknode($7, 0, 1)));
                   }
                  ;

array_tail: COMMA IDENTIFIER OPENBRACKET expr CLOSEBRACKET ASSIGNMENT STRING_LTL array_tail {
               node *decl_node = mknode("array_decl_assign", 3, 0, mknode($2, 0, 1), $4, mknode("string",1,0,mknode($7, 0, 1)));
               add_child(decl_node, $8);
               $$ = decl_node;
            }
            | COMMA IDENTIFIER OPENBRACKET expr CLOSEBRACKET array_tail {
               node *decl_node = mknode("array_decl", 2, 0, mknode($2, 0, 1), $4);
               if ($6 != NULL) add_child(decl_node, $6);
               $$ = decl_node;
            }
            | COMMA IDENTIFIER OPENBRACKET expr CLOSEBRACKET ASSIGNMENT STRING_LTL {
               $$ = mknode("array_decl_assign", 3, 0, mknode($2, 0, 1), $4, mknode("string",1,0,mknode($7, 0, 1)));
            }
            | COMMA IDENTIFIER OPENBRACKET expr CLOSEBRACKET {
               $$ = mknode("array_tail", 2, 0, mknode($2, 0, 1), $4);
            } 
            ;

array_assign: IDENTIFIER OPENBRACKET expr CLOSEBRACKET ASSIGNMENT ret_ltrl SEMICOLON {
                $$ = mknode("array_assign", 3, 0, mknode($1, 0, 1),$3, $6);
           }
           ;


array_index: IDENTIFIER OPENBRACKET expr CLOSEBRACKET {
                $$ = mknode("array_index", 2, 0, mknode($1, 0, 1), $3);
           }
           ;

expr: OPENPAREN expr CLOSEPAREN { $$ = mknode("()", 1, 0, $2); }
    | LENGTH expr LENGTH { $$ = mknode("length", 1, 0, $2); }
    | expr AND expr { $$ = mknode("&&", 2, 0, $1 ,$3); }
    | expr OR expr {  $$ = mknode("||", 2, 0, $1 ,$3);}
    | expr GREATER expr { $$ = mknode("(>)", 2, 0, $1, $3); }
    | expr LESS expr { $$ = mknode("(<)", 2, 0, $1, $3); }
    | expr GREATEREQL expr { $$ = mknode("(>=)", 2, 0, $1, $3); }
    | expr LESSEQL expr { $$ = mknode("(<=)", 2, 0, $1, $3); }
    | expr EQL expr { $$ = mknode("(==)", 2, 0, $1, $3); }
    | expr NOTEQL expr { $$ = mknode("(!=)", 2, 0, $1, $3); }
    | NOT expr{ $$ = mknode("(!)",1,0,$2);}
    | expr MULTI expr { $$ = mknode("(*)", 2, 0, $1, $3); }
    | expr DIVISION expr { $$ = mknode("(/)", 2, 0, $1, $3); }
    | expr PLUS expr { $$ = mknode("(+)", 2, 0, $1, $3); }
    | expr MINUS expr { $$ = mknode("(-)", 2, 0, $1, $3); }
    | IDENTIFIER { $$ = mknode($1, 0, 1); }
    | NUM { $$ = mknode("int",1,0,mknode($1, 0, 1)); }
    | MULTI expr{ $$ = mknode ("*",1,0,$2);}
    | ADDRESS IDENTIFIER OPENBRACKET expr CLOSEBRACKET {$$ = mknode("&",2,0,mknode($2,0,1),$4);}
    | array_index { $$ = $1; }
    | BOOLTRUE {$$ = mknode("bool",1,0,mknode("true",0,1));}
    | BOOLFALSE {$$ = mknode("bool",1,0,mknode("false",0,1));}
    | CHAR_LTL { $$ = mknode("char",1,0,mknode($1, 0, 1)); }
    | DOUBLE_LTL { $$ = mknode("double",1,0,mknode($1, 0, 1)); }
    | FLOAT_LTL { $$ = mknode("float",1,0,mknode($1, 0, 1)); }


    ;

is_stat: COLON STATIC { $$ = mknode("STATIC", 0, 1); }
        | { $$ = mknode("NON_STATIC", 0, 1); }
        ;

body: statement_list { $$ = mknode("BODY", 1, 0, $1); } 
    | function_decl { $$ = mknode("BODY", 1, 0, $1); }
    | function_decl statement_list { $$ = mknode("BODY", 2, 0, $1, $2); }
    | statement_list function_decl statement_list { $$ = mknode("BODY", 3, 0, $1, $2, $3); }
    | { $$ = mknode("BODY", 0, 0); }
    ;

%%


/*----------------------------------------------------------------------------------*/

int main() {
    globalScope = createSymbolTable(NULL);
    int result = yyparse();
    if (result == 0) {
        printf("Parsing completed successfully.\n");
    } else {
        printf("Parsing failed.\n");
    }
    // Print the symbol table after parsing
    //printSymbolTablesRecursively(globalScope, 0);


    return result;    
}

node* mknode(char *token, int num_children, int is_literal, ...) {
    va_list args;
    va_start(args, num_children);

    node *newnode = (node*)malloc(sizeof(node));
    if (newnode == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }

    newnode->token = strdup(token);
    newnode->num_children = num_children;
    newnode->is_literal = is_literal;
    newnode->children = (node**)malloc(num_children * sizeof(node*));
    if (newnode->children == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }

    for (int i = 0; i < num_children; i++) {
        newnode->children[i] = va_arg(args, node*);
    }

    va_end(args);
    return newnode;
}

void add_child(node *parent, node *child) {
    parent->num_children++;
    parent->children = (node**)realloc(parent->children, parent->num_children * sizeof(node*));
    if (parent->children == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    parent->children[parent->num_children - 1] = child;
}

void printtree(node* tree, int depth) {
    if (tree == NULL) return;

    // Skip non-essential nodes
    if (!is_essential_node(tree->token) && !tree->is_literal) {
        for (int i = 0; i < tree->num_children; i++) {
            printtree(tree->children[i], depth);
        }
        return;
    }

    for (int i = 0; i < depth; i++) printf("   ");

    if (!tree->is_literal) {
        printf("(%s\n", tree->token);
        for (int i = 0; i < tree->num_children; i++) {
            printtree(tree->children[i], depth + 1);
        }
        for (int i = 0; i < depth; i++) printf("   ");
        printf(")\n");
    } else {
        printf("%s\n", tree->token);
    }
}

int yyerror(char *e) {
    fprintf(stderr, "Error: %s at line %d\n", e, yylineno);
    fprintf(stderr, "Token: '%s'\n", yytext);
    return 0;
}

/*--------------------------------------PART 2---------------------------------------------------------*/
void checktree(node *tree, SymbolTable *currentScope) {
    if (tree == NULL) return;


    if (strcmp(tree->token, "var_declaration") == 0) {
        processVarDeclaration(tree, currentScope);
    } 
    else if (strcmp(tree->token, "FUNC") == 0 || strcmp(tree->token, "MAIN") == 0) {
        SymbolTable *funcScope = processFunction(tree, currentScope);
        if (currentScope->enclosingFunction != NULL) {
            funcScope->enclosingFunction = currentScope->enclosingFunction; // שמירת הפונקציה העוטפת
        }
        processBody(tree->children[5], funcScope); 
        return; // להפסיק את הרקורסיה לאחר הטיפול ב-BODY
    } 
    else if (strcmp(tree->token, "if") == 0) {
        processIf(tree, currentScope);
    } 
    else if (strcmp(tree->token, "assignment") == 0) {
        processAssignment(tree, currentScope);
    } 
    else if (strcmp(tree->token, "array_decl") == 0 || strcmp(tree->token, "array_decl_assign") == 0) {
        processAssignmentinStringDecl(tree, currentScope);
    } 
    else if (strcmp(tree->token, "return") == 0) {
        processReturn(tree, currentScope);
        return; 
    }
    else if (strcmp(tree->token, "{") == 0) {
        SymbolTable *funcScope = createSymbolTable(currentScope);

        processBody(tree->children[0], funcScope); 
        return; 
    }
     else if (strcmp(tree->token, "funcCall") == 0) {
        char* funcCallType = validateFunctionCall(tree, currentScope);
    }

    for (int i = 0; i < tree->num_children; i++) {
        checktree(tree->children[i], currentScope);
    }
}




void processVarDeclaration(node *declarationNode, SymbolTable *currentScope) {
    char *type = declarationNode->children[0]->token;
    node *var_list = declarationNode->children[1];

    if (var_list->num_children == 0) {
        char *varName = var_list->token;
        if (findVariableInCurrentScope(currentScope, varName)) {
            printf("Error: Variable '%s' is already declared in the current scope.\n", varName);
            exit(1);
        }
        if (findSymbol(currentScope->parent, varName)) {
            printf("Warning: Variable '%s' is shadowing a variable from an outer scope.\n", varName);
        }
        addVariable(currentScope, varName, type, 0);  // pass size as 0 since this is not a string
    }

    else if (strcmp(var_list->token, "var_id_list") == 0) {
        processVarIdList(currentScope, type, var_list);
    }
    else if (strcmp(var_list->token, "assignment") == 0) {
        processAssignmentinVarDecl(var_list, currentScope, type); // Pass the type as an argument
    }
}


void processVarIdList(SymbolTable *currentScope, char *type, node *var_list) {
    for (int i = 0; i < var_list->num_children; i++) {
        char *varName = var_list->children[i]->token;
        
        if (strcmp(varName, "assignment") == 0) {
            processAssignmentinVarDecl(var_list->children[i], currentScope, type); // Pass the type as an argument
            continue;
        }

        if (strcmp(varName, "var_id_list") == 0) {
            processVarIdList(currentScope, type, var_list->children[i]);
            continue;
        }

        if (findVariableInCurrentScope(currentScope, varName)) {
            printf("Error: Variable '%s' is already declared in the current scope.\n", varName);
            exit(1);
        }

        if (findSymbol(currentScope->parent, varName)) {
            printf("Warning: Variable '%s' is shadowing a variable from an outer scope.\n", varName);
        }

        addVariable(currentScope, varName, type, 0);  
    }
}


int mainCounter = 0;
SymbolTable* processFunction(node *functionNode, SymbolTable *currentScope) {
    char *funcName = functionNode->children[2]->token;
    char *returnType = functionNode->children[1]->token;
    char *isPublicOrPrivate = functionNode->children[0]->token;
    char *isStatic = functionNode->children[4]->token;
    if (strcmp(funcName, "main") == 0) {
        if (mainCounter) {
            printf("Error: Multiple definitions of 'main' function.\n");
            exit(1);
        }
        mainCounter = 1;

        if (strcmp(returnType, "RET-void") != 0) {
            printf("Error: 'main' function must return void.\n");
            exit(1);
        }
        if (strcmp(isPublicOrPrivate, "public") != 0) {
            printf("Error: 'main' function must be public.\n");
            exit(1);
        }
        if (strcmp(isStatic, "STATIC") != 0) {
            printf("Error: 'main' function must be static.\n");
            exit(1);
        }
        if (functionNode->children[3]->num_children > 0) {
            printf("Error: 'main' function must not have arguments.\n");
            exit(1);
        }
    }

    node *args = functionNode->children[3];

    int paramCount;
    Variable **parameters = parseParameters(args, &paramCount);
    addFunction(currentScope, funcName, returnType, parameters, paramCount,strcmp(isStatic, "STATIC") == 0,isPublicOrPrivate);

    SymbolTable *funcScope = createSymbolTable(currentScope);
    funcScope->enclosingFunction = findFunction(currentScope, funcName); 

    // Associate the new scope with the function
    Function *currentFunction = findFunction(currentScope, funcName);
    if (currentFunction != NULL) {
        currentFunction->scope = funcScope;
    }

    // Add parameters to the new scope
    for (int i = 0; i < paramCount; i++) {
        addVariable(funcScope, parameters[i]->name, parameters[i]->type, 0);
    }

    return funcScope;
}

void processBody(node *bodyNode, SymbolTable *currentScope) {

    for (int i = 0; i < bodyNode->num_children; i++) {
        checktree(bodyNode->children[i], currentScope);
    }

}


void processReturn(node *returnNode, SymbolTable *currentScope) {

    SymbolTable *parentScope = currentScope->parent;
    Function *relevantFunction = NULL;

    // חיפוש הפונקציה הרלוונטית ב-Scope האב
    for (int i = 0; i < parentScope->func_count; i++) {
        if (parentScope->functions[i]->scope == currentScope) {
            relevantFunction = parentScope->functions[i];
            break;
        }
    }

    if (relevantFunction != NULL) {
        char *expectedReturnType = relevantFunction->return_type;
        char *actualReturnType = exprType(returnNode->children[0], currentScope);

        if (strcmp(expectedReturnType, actualReturnType) != 0) {
            printf("Error: Return type mismatch in function '%s'. Expected %s but got %s.\n",
                relevantFunction->name, expectedReturnType, actualReturnType);
            exit(1);
        }
    } else {
        printf("Error: 'return' statement outside of function scope.\n");
        exit(1);
    }


    // לאחר שהטיפול ב-return הסתיים, פשוט מחזירים את ה-control חזרה ל-scope הקודם:
    if (parentScope != NULL) {
        // המשך הניתוח נעשה על ידי חזרה לשכבת ה-scope הקודמת מבלי לקרוא לשדה parent ב-node
    }
}


void processIf(node *ifNode, SymbolTable *currentScope) {
    // Check condition type
    node *condition = ifNode->children[0];
    char *conditionType = exprType(condition, currentScope);
    if (strcmp(conditionType, "bool") != 0) {
        printf("Error: Condition in 'if' must be of type 'bool'.\n");
        exit(1);
    }

    // Process 'then' block
    node *thenBlock = ifNode->children[1];
    processBody(thenBlock, currentScope);

    // Check for 'else' block
    if (ifNode->num_children > 2) {
        node *elseBlock = ifNode->children[2];
        processBody(elseBlock, currentScope);
    }
}


void processAssignment(node *assignmentNode, SymbolTable *currentScope) {
    node *varNode = assignmentNode->children[0];
    node *exprNode = assignmentNode->children[1];
    char *exprTypeStr = exprType(exprNode, currentScope);

    Variable *varSymbol = findSymbol(currentScope, varNode->token);
    if (varSymbol == NULL) {
        printf("Error: Variable '%s' is not declared.\n", varNode->token);
        exit(1);
    }
    if (strcmp(varSymbol->type, exprTypeStr) != 0) {
        printf("Error: Type mismatch in assignment to '%s'. Expected %s but got %s.\n", varNode->token, varSymbol->type, exprTypeStr);
        exit(1);
    }


    varSymbol->is_initialized = 1;


}

void processAssignmentinVarDecl(node *assignmentNode, SymbolTable *currentScope, char *expectedType) {
    node *varNode = assignmentNode->children[0];
    node *exprNode = assignmentNode->children[1];
    char *exprTypeStr = exprType(exprNode, currentScope);

    if (strcmp(expectedType, exprTypeStr) != 0) {
        printf("Error: Type mismatch in assignment to '%s'. Expected %s but got %s.\n", varNode->token, expectedType, exprTypeStr);
        exit(1);
    }

    Variable *varSymbol = findSymbol(currentScope, varNode->token);
    if (varSymbol == NULL) {
        addVariable(currentScope, varNode->token, expectedType, 0);
    }

    varSymbol = findSymbol(currentScope, varNode->token);
    varSymbol->is_initialized = 1;
}



void processAssignmentinStringDecl(node *assignmentNode, SymbolTable *currentScope) {
    node *strNode = assignmentNode->children[0];
    node *sizeNode = assignmentNode->children[1];

    char* sizeTypeStr = exprType(sizeNode,currentScope);

    char *valTypeStr = NULL;

    if (strcmp(assignmentNode->token, "array_decl_assign") == 0) {
        node *valNode = assignmentNode->children[2];

        valTypeStr = exprType(valNode, currentScope);
        if (strcmp(valTypeStr,"string")!=0){
            printf("Error: Type mismatch in assignment to '%s'. Expected string but got %s.\n", assignmentNode->token, valTypeStr);
            exit(1);
        }
    }
    if (strcmp(sizeTypeStr,"int")!=0){
        printf("Error: Type mismatch in assignment to '%s'. Expected int but got %s.\n", assignmentNode->token, sizeTypeStr);
        exit(1);

    }

    Variable *varSymbol = findSymbol(currentScope, strNode->token);
    if (varSymbol == NULL) {
        int size = atoi(sizeNode->children[0] -> token);  // convert sizeNode token to integer
        addVariable(currentScope, strNode->token, "string", size);
    }
    varSymbol = findSymbol(currentScope, strNode->token);
    currentScope->variables[currentScope->var_count-1]->is_initialized = 1;
}



char* validateFunctionCall(node *tree, SymbolTable *currentScope) {
    // קבלת שם הפונקציה
    char *funcName = tree->children[0]->token;

    // בדיקת קיום הפונקציה בטבלת הסמלים
    Function *calledFunction = findFunction(currentScope, funcName);
    if (calledFunction == NULL) {
        printf("Error: Function '%s' is not declared.\n", funcName);
        exit(1);
    }

    // ניתוח כמות הארגומנטים בקריאה לפונקציה
    int numArgsInCall = (tree->children[1] == NULL) ? 0 : tree->children[1]->num_children;

    if (numArgsInCall != calledFunction->param_count) {
        printf("Error: Function '%s' expects %d arguments, but %d were provided.\n", 
               funcName, calledFunction->param_count, numArgsInCall);
        exit(1);
    }


    // מציאת הפונקציה שממנה בוצעה הקריאה
    Function *currentFunction = NULL;
    SymbolTable *parentScope = currentScope->parent;
    while (currentFunction == NULL && parentScope != NULL) {
        for (int i = 0; i < parentScope->func_count; i++) {
            if (parentScope->functions[i]->scope == currentScope) {
                currentFunction = parentScope->functions[i];
                break;
            }
        }
        parentScope = parentScope->parent;
    }

    // אם לא נמצאה פונקציה עוטפת
    if (currentFunction == NULL) {
        // בדיקת קריאה לפונקציה פרטית מתוך אותו ה-scope
        if (strcmp(calledFunction->access_modifier, "PRIVATE") == 0 && calledFunction->scope != currentScope) {
            printf("Error: Private function '%s' cannot be called from a different scope.\n", calledFunction->name);
            exit(1);
        }
    } else {

        // בדיקת סטטיות
        if (currentFunction->is_static && !calledFunction->is_static) {
            printf("Error: Static function '%s' cannot call non-static function '%s'.\n", currentFunction->name, funcName);
            exit(1);
        }

        // בדיקת גישת פרטיות וסקופים
        if (strcmp(currentFunction->access_modifier, "PUBLIC") == 0) {
            if (strcmp(calledFunction->access_modifier, "PRIVATE") == 0 && calledFunction->scope != currentFunction->scope) {
                printf("Error: Private function '%s' cannot be called from a different scope.\n", calledFunction->name);
                exit(1);
            }
        }
    }


    // בדיקת התאמה של טיפוסי הארגומנטים
    for (int i = 0; i < numArgsInCall; i++) {
        node *argNode = tree->children[1]->children[i]; // גישה לארגומנט הנכון

        char *argTypeInCall = NULL;

        // בדיקה אם הארגומנט הוא ביטוי מורכב
        if (strcmp(argNode->token, "(+)") == 0 || strcmp(argNode->token, "(-)") == 0 ||
            strcmp(argNode->token, "(*)") == 0 || strcmp(argNode->token, "(/)") == 0 ||
            strcmp(argNode->token, "(>)") == 0 || strcmp(argNode->token, "(<)") == 0 ||
            strcmp(argNode->token, "(>=)") == 0 || strcmp(argNode->token, "(<=)") == 0 ||
            strcmp(argNode->token, "(==)") == 0 || strcmp(argNode->token, "(!=)") == 0 ||
            strcmp(argNode->token, "&&") == 0 || strcmp(argNode->token, "||") == 0) {
            argTypeInCall = exprType(argNode, currentScope);  // זיהוי הסוג באמצעות הפונקציה exprType
        } else {
            // בדיקה אם הארגומנט הוא משתנה שהוגדר בעבר
            Variable *varSymbol = findSymbol(currentScope, argNode->token);
            if (varSymbol != NULL) {
                argTypeInCall = varSymbol->type;
            } else if (argNode->children[0]->is_literal) {
                argTypeInCall = exprType(argNode, currentScope); 
            } else {
                printf("Error: Argument '%s' in function call is not declared and is not a valid literal.\n", argNode->token);
                exit(1);
            }
        }

        char *expectedType = calledFunction->parameters[i]->type;

        if (strcmp(argTypeInCall, expectedType) != 0) {
            printf("Error: Argument %d in function call '%s' is of type '%s', but expected '%s'.\n",
                   i + 1, funcName, argTypeInCall, expectedType);
            exit(1);
        }
    }

    return calledFunction->return_type;
}


int findVariableInCurrentScope(SymbolTable *currentScope, const char *name) {
    for (int i = 0; i < currentScope->var_count; i++) {
        if (strcmp(currentScope->variables[i]->name, name) == 0) {
            return 1; // Found the variable in the current scope
        }
    }
    return 0; // Not found in the current scope
}



Variable** parseParameters(node *args, int *paramCount) {
    *paramCount = 0;

    if (strcmp(args->token, "NON_ARGS") == 0) {

        return NULL;
    }

    Variable **totalParameters = NULL;
    int totalParamCount = 0;

    if (strcmp(args->children[0]->token, "decl_list") == 0) {
        node *declLst = args->children[0];
        for (int i = 0; i < declLst->num_children; i++) {
            node *paraLst = declLst->children[i];
            char *type = paraLst->children[0]->token;

            // קריאה רקורסיבית לטיפול במבנה var_id_list
            processVarIdListRecursive(paraLst->children[1], type, &totalParameters, &totalParamCount);
        }
    }
    else if (strcmp(args->children[0]->token, "para_list") == 0){
        node *paraLst = args->children[0];
        char *type = paraLst->children[0]->token;
        processVarIdListRecursive(paraLst->children[1], type, &totalParameters, &totalParamCount);
    }

    *paramCount = totalParamCount;
    return totalParameters;
}




void processVarIdListRecursive(node *varIdList, char *type, Variable ***totalParameters, int *totalParamCount) {
    if (strcmp(varIdList->token, "var_id_list") == 0) {
        // קריאה רקורסיבית כאשר token הוא var_id_list
        processVarIdListRecursive(varIdList->children[0], type, totalParameters, totalParamCount);
        processVarIdListRecursive(varIdList->children[1], type, totalParameters, totalParamCount);
    } 
    else {
        // הרחבת המערך עבור משתנה חדש
        *totalParameters = (Variable**)realloc(*totalParameters, sizeof(Variable*) * (*totalParamCount + 1));
        if (*totalParameters == NULL) {
            fprintf(stderr, "Out of memory while reallocating totalParameters\n");
            exit(1);
        }

        // הקצאת זיכרון והגדרת השדות
        (*totalParameters)[*totalParamCount] = (Variable*)malloc(sizeof(Variable));
        if ((*totalParameters)[*totalParamCount] == NULL) {
            fprintf(stderr, "Out of memory while allocating new parameter\n");
            exit(1);
        }

        char *name = varIdList->token;
        (*totalParameters)[*totalParamCount]->name = strdup(name);
        (*totalParameters)[*totalParamCount]->type = strdup(type);
        (*totalParameters)[*totalParamCount]->is_initialized = 0;
        memset(&((*totalParameters)[*totalParamCount]->value), 0, sizeof((*totalParameters)[*totalParamCount]->value));
        (*totalParamCount)++;
    }
}



char* findVariableType(SymbolTable *table, char *name) {
    SymbolTable *current = table;
    while (current != NULL) {
        for (int i = 0; i < current->var_count; i++) {
            if (strcmp(current->variables[i]->name, name) == 0) {
                return current->variables[i]->type;
            }
        }
        current = current->parent;
    }
    printf("Error: Variable '%s' not declared\n", name);
    exit(1);
}


char* findFunctionReturnType(SymbolTable *table, char *name) {
    SymbolTable *current = table;
    while (current != NULL) {
        for (int i = 0; i < current->func_count; i++) {
            if (strcmp(current->functions[i]->name, name) == 0) {
                return current->functions[i]->return_type;
            }
        }
        current = current->parent;
    }
    printf("Error: Function '%s' not declared\n", name);
    exit(1);
}

Function* findFunction(SymbolTable *currentScope, const char *name) {
    SymbolTable *scope = currentScope;

    // Traverse through the scopes starting from the current one
    while (scope != NULL) {
        // Search for the function in the current scope
        for (int i = 0; i < scope->func_count; i++) {
            if (strcmp(scope->functions[i]->name, name) == 0) {
                return scope->functions[i]; // Found the function
            }
        }

        // Move to the parent scope if not found
        scope = scope->parent;
    }

    return NULL; // Function not found in any accessible scope
}


char* exprType(node *tree, SymbolTable *currentScope) {
    if (tree == NULL) {
        exit(1);
    }


    // Handle function calls
    if (strcmp(tree->token, "funcCall") == 0) {
        return validateFunctionCall(tree, currentScope);
    }

    // Handle array indexing
    if (strcmp(tree->token, "array_index") == 0) {
        Variable *arrayVar = findSymbol(currentScope, tree->children[0]->token);
        if (arrayVar == NULL) {
            printf("Error: Array '%s' is not declared.\n", tree->children[0]->token);
            exit(1);
        }
        if (strcmp(arrayVar->type, "string") != 0) {
            printf("Error: Attempted to index a non-array type '%s'.\n", arrayVar->type);
            exit(1);
        }
        char *indexType = exprType(tree->children[1], currentScope);
        if (strcmp(indexType, "int") != 0) {
            printf("Error: Array index must be of type 'int', got '%s'.\n", indexType);
            exit(1);
        }
        return "char";
    }

    // Handle simple identifiers
    if (tree->is_literal && tree->num_children == 0) {
        Variable *varSymbol = findSymbol(currentScope, tree->token);
        if (varSymbol == NULL) {
            printf("Error: Variable '%s' is not declared.\n", tree->token);
            exit(1);
        }
        return varSymbol->type;
    }

    // Handle literals
    if (strcmp(tree->token, "int") == 0) return "int";
    if (strcmp(tree->token, "float") == 0) return "float";
    if (strcmp(tree->token, "double") == 0) return "double";
    if (strcmp(tree->token, "char") == 0) return "char";
    if (strcmp(tree->token, "string") == 0) return "string";
    if (strcmp(tree->token, "bool") == 0) return "bool";

    // Handle expressions with parentheses
    if (strcmp(tree->token, "()") == 0) {
        return exprType(tree->children[0], currentScope);
    }

    // Handle binary expressions
    if (tree->num_children == 2) {
        char *leftType = exprType(tree->children[0], currentScope);
        char *rightType = exprType(tree->children[1], currentScope);

        if (strcmp(tree->token, "(+)") == 0 || strcmp(tree->token, "(-)") == 0 || strcmp(tree->token, "(*)") == 0 || strcmp(tree->token, "(/)") == 0) {
            if ((strcmp(leftType, "int") == 0 || strcmp(leftType, "float") == 0 || strcmp(leftType, "double") == 0) &&
                (strcmp(rightType, "int") == 0 || strcmp(rightType, "float") == 0 || strcmp(rightType, "double") == 0)) {
                if (strcmp(leftType, "double") == 0 || strcmp(rightType, "double") == 0) {
                    return "double";
                } else if (strcmp(leftType, "float") == 0 || strcmp(rightType, "float") == 0) {
                    return "float";
                } else {
                    return "int";
                }
            } else {
                printf("Error: Invalid operands for operator '%s'. Both operands must be int, float, or double.\n", tree->token);
                exit(1);
            }
        } else if (strcmp(tree->token, "&&") == 0 || strcmp(tree->token, "||") == 0) {
            if (strcmp(leftType, "bool") == 0 && strcmp(rightType, "bool") == 0) {
                return "bool";
            } else {
                printf("Error: Invalid operands for operator '%s'. Both operands must be bool.\n", tree->token);
                exit(1);
            }
        } else if (strcmp(tree->token, "(>)") == 0 || strcmp(tree->token, "(<)") == 0 || strcmp(tree->token, "(>=)") == 0 || strcmp(tree->token, "(<=)") == 0) {
            if ((strcmp(leftType, "int") == 0 || strcmp(leftType, "float") == 0 || strcmp(leftType, "double") == 0) &&
                (strcmp(rightType, "int") == 0 || strcmp(rightType, "float") == 0 || strcmp(rightType, "double") == 0)) {
                return "bool";
            } else {
                printf("Error: Invalid operands for operator '%s'. Both operands must be int, float, or double.\n", tree->token);
                exit(1);
            }
        } else if (strcmp(tree->token, "(==)") == 0 || strcmp(tree->token, "(!=)") == 0) {
            if (strcmp(leftType, rightType) == 0) {
                return "bool";
            } else {
                printf("Error: Invalid operands for operator '%s'. Both operands must be of the same type.\n", tree->token);
                exit(1);
            }
        }
    }

    // Handle complex binary expressions with mixed precedence
    if (tree->num_children == 3) {
        char *leftType = exprType(tree->children[0], currentScope);
        char *middleType = exprType(tree->children[1], currentScope);
        char *rightType = exprType(tree->children[2], currentScope);

        // Check for logical expressions involving multiple comparisons
        if (strcmp(tree->token, "&&") == 0 || strcmp(tree->token, "||") == 0) {
            if (strcmp(leftType, "bool") == 0 && strcmp(middleType, "bool") == 0 && strcmp(rightType, "bool") == 0) {
                return "bool";
            } else {
                printf("Error: Invalid operands for logical operator '%s'. All operands must be bool.\n", tree->token);
                exit(1);
            }
        }
    }

    // Handle unary expressions
    if (tree->num_children == 1) {
        char *childType = exprType(tree->children[0], currentScope);
        if (strcmp(tree->token, "!") == 0) {
            if (strcmp(childType, "bool") == 0) {
                return "bool";
            } else {
                printf("Error: Operator '!' can only be used with bool type.\n");
                exit(1);
            }
        } else if (strcmp(tree->token, "*") == 0) {
            if (strstr(childType, "*") != NULL) {
                childType[strlen(childType) - 1] = '\0'; // Remove one level of indirection
                return childType;
            } else {
                printf("Error: Operator '*' can only be used with pointer types.\n");
                exit(1);
            }
        }
    }

    printf("Error: Unhandled expression type for token: %s.\n", tree->token);
    exit(1);
}




Variable* findSymbol(SymbolTable *currentScope, const char *name) {
    SymbolTable *scope = currentScope;
    
    // Traverse through the scopes starting from the current one
    while (scope != NULL) {
        // Search for the variable in the current scope
        for (int i = 0; i < scope->var_count; i++) {
            if (strcmp(scope->variables[i]->name, name) == 0) {

                return scope->variables[i]; // Found the variable
            }
        }
        
        // Move to the parent scope if not found
        scope = scope->parent;
    }
    
    return NULL; // Not found in any accessible scope
}

void printSymbolTablesRecursively(SymbolTable *table, int level) {
    if (table == NULL) return;

    // הדפסת טבלת הסמלים הנוכחית
    printf("\nSymbol Table (Level %d) at address %p:\n", level, (void*)table);
    printf("Parent Table Address: %p\n", (void*)table->parent);
    printf("------------------------\n");

    // הדפסת משתנים
    printf("Variables:\n");
    for (int i = 0; i < table->var_count; i++) {
        printf("  Name: %s, Type: %s, Initialized: %s", 
               table->variables[i]->name, 
               table->variables[i]->type,
               table->variables[i]->is_initialized ? "Yes" : "No");

        if (strcmp(table->variables[i]->type, "int") == 0) {
            printf(", Value: %d\n", table->variables[i]->value.int_value);
        } else if (strcmp(table->variables[i]->type, "float") == 0) {
            printf(", Value: %f\n", table->variables[i]->value.float_value);
        } else if (strcmp(table->variables[i]->type, "double") == 0) {
            printf(", Value: %lf\n", table->variables[i]->value.double_value);
        } else if (strcmp(table->variables[i]->type, "char") == 0) {
            printf(", Value: %c\n", table->variables[i]->value.char_value);
        } else if (strcmp(table->variables[i]->type, "string") == 0) {
            printf(", Value: %s, Size: %d\n", 
                   table->variables[i]->value.string_value,
                   table->variables[i]->value.size);
        } else if (strcmp(table->variables[i]->type, "bool") == 0) {
            printf(", Value: %s\n", table->variables[i]->value.bool_value);
        } else {
            printf("\n");
        }
    }

    // הדפסת פונקציות
    printf("Functions:\n");
    for (int i = 0; i < table->func_count; i++) {
        printf("  Name: %s, Return Type: %s, Parameters: %d\n", 
               table->functions[i]->name, 
               table->functions[i]->return_type, 
               table->functions[i]->param_count);

        // הדפסת כל פרמטר
        for (int j = 0; j < table->functions[i]->param_count; j++) {
            printf("    Param %d - Name: %s, Type: %s\n", 
                   j + 1,
                   table->functions[i]->parameters[j]->name, 
                   table->functions[i]->parameters[j]->type);
        }

        // הדפסת ה-scope של הפונקציה, אם קיים
        if (table->functions[i]->scope != NULL) {
            printf("  Entering scope of function '%s' (Level %d) at address %p\n", 
                   table->functions[i]->name, level + 1, (void*)table->functions[i]->scope);
            printSymbolTablesRecursively(table->functions[i]->scope, level + 1);
        } else {
            printf("  No scope found for function '%s'\n", table->functions[i]->name);
        }
    }
}

/*---------------------------------------PART 3----------------------------------------------------------------------------*/

void check3AC(node *tree) {
    if (tree == NULL) return;

    if (strcmp(tree->token, "assignment") == 0) {
        processAssignment3AC(tree);
    } else if (strcmp(tree->token, "if") == 0) {
        processIfElse3AC(tree);
        return;
    }
    else if (strcmp(tree->token, "while") == 0) {
        processWhile3AC(tree);
        return;
    }
    else if (strcmp(tree->token, "do") == 0) {
        processDoWhile3AC(tree);
        return;
    }
     else if (strcmp(tree->token, "for") == 0) { 
        processFor3AC(tree);
        return;
    }
    else if (strcmp(tree->token, "FUNC") == 0) { 
        processFunction3AC(tree); 
        return;
    }
else if (strcmp(tree->token, "MAIN") == 0) { 
        processMainFunction3AC(tree);  
        return;
    }


    for (int i = 0; i < tree->num_children; i++) {
        check3AC(tree->children[i]);
    }
}

void processAssignment3AC(node *tree) {
    strcpy(temp, "t");
    sprintf(s, "%d", i_l);
    strcat(temp, s);
    //indent_level++;


    if (strcmp(tree->children[1]->token, "funcCall") == 0) {
        processFunctionCall3AC(tree->children[1]);
        printIndented("%s = %s\n", tree->children[0]->token, st[top]); // השימוש בתוצאה מהקריאה לפונקציה
        return;
    }

    // זיהוי ביטויים לוגיים כמו && ו- ||
    if (strcmp(tree->children[1]->token, "&&") == 0) {
        processLogicalAnd3AC(tree);
        return;
    } 
    else if (strcmp(tree->children[1]->token, "||") == 0) {
        processLogicalOr3AC(tree);
        return;
    }
    // זיהוי ביטוי length
    else if (strcmp(tree->children[1]->token, "length") == 0) {
        proccessExpr3AC(tree->children[1]);
    }
    // זיהוי ביטוי סוגריים ()
    else if (strcmp(tree->children[1]->token, "()") == 0) {
        proccessExpr3AC(tree->children[1]);
    }
    // זיהוי ביטוי של NOT (!)
    else if (strcmp(tree->children[1]->token, "(!)") == 0) {
        proccessExpr3AC(tree->children[1]);
    }
    // עיבוד ביטויים כלליים
    else if (tree->children[1]->num_children > 0) {
        flag = 1;
        proccessExpr3AC(tree->children[1]);

        if (tree->children[1]->num_children == 1) {
            printIndented("%s = %s\n", temp, st[top]); 
            flag = 1;
        }

        strcpy(st[top], temp);
        printIndented("%s = %s\n", tree->children[0]->token, st[top]); 
        if (flag == 0)
            i_l -= 1;
    } 
    else {
        strcpy(st[top], tree->children[1]->token);
        printIndented("%s = %s\n", temp, st[top]); 

        strcpy(st[top], temp);
        printIndented("%s = %s\n", tree->children[0]->token, st[top]); 
        i_l++;
    }
    i_l++;

}


void proccessExpr3AC(node *tree) {

    if (strcmp(tree->token, "&&") == 0) {
        processLogicalAnd3AC(tree);
        return;
    } 
    else if (strcmp(tree->token, "||") == 0) {
        processLogicalOr3AC(tree);
        return;
    }
    else if (strcmp(tree->token, "(!)") == 0) {
        proccessExpr3AC(tree->children[0]);  
        printIndented("%s = !%s\n", temp, st[top]); 
        strcpy(st[top], temp);
        i_l++;
        return;
    } 
    else if (strcmp(tree->token, "length") == 0) {
        proccessExpr3AC(tree->children[0]);  
        printIndented("%s = length(%s)\n", temp, st[top]); 
        strcpy(st[top], temp);
        i_l++;
        return;
    }
    else if (strcmp(tree->token, "()") == 0) {
        proccessExpr3AC(tree->children[0]);  
        return;
    }

    if (tree->num_children == 2) {
        if (tree->children[0]->num_children > 0) {
            proccessExpr3AC(tree->children[0]);
            
            strcpy(st[++top], tree->token);
            proccessExpr3AC(tree->children[1]);
        } else if (tree->children[0]->num_children == 0) {
            proccessExpr3AC(tree->children[0]);
            strcpy(st[++top], tree->token);
            proccessExpr3AC(tree->children[1]);
        }

        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);

        printIndented("%s = %s %s %s\n", temp, st[top-2], st[top-1], st[top]);
        top -= 2;
        strcpy(st[top], temp);
        i_l++;
    }
    else if (tree->num_children == 1) {
        if (strcmp(tree->token, "()") == 0) {
            proccessExpr3AC(tree->children[0]); 
        } else {
            strcpy(st[++top], tree->children[0]->token);
        }
        flag = 0;
    }
    else if (tree->num_children == 0) {
        strcpy(st[++top], tree->token); 
    }
}


void processIfElse3AC(node *tree) {
    Lnum++;
    label[Ltop++] = Lnum;
    int label_then = Lnum;

    int label_else = -1; 
    if (tree->num_children > 2) { 
        Lnum++;
        label[Ltop++] = Lnum;
        label_else = Lnum;
    }

    Lnum++;
    label[Ltop++] = Lnum;
    int label_end = Lnum;

    if (strcmp(tree->children[0]->token, "&&") == 0) {
        processLogicalANDInIf3AC(tree);
        return;
    } else if (strcmp(tree->children[0]->token, "||") == 0) {
        processLogicalORInIf3AC(tree);
        return;
    }

    if (tree->children[0]->num_children > 1 && tree->children[0]->children[1]->num_children == 2) {
        proccessExpr3AC(tree->children[0]->children[1]);
    } else {
        if (tree->children[0]->num_children > 1 && tree->children[0]->children[1]->num_children == 1) {
            strcpy(st[++top], tree->children[0]->children[1]->children[0]->token);
        } else if (tree->children[0]->num_children > 1 && tree->children[0]->children[1]->num_children == 0) {
            strcpy(st[++top], tree->children[0]->children[1]->token);
        } else if (tree->children[0]->num_children == 1) {
            strcpy(st[++top], tree->children[0]->children[0]->token);
        }

        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);
        printIndented("%s = %s \n", temp, st[top]);
        top--;
        strcpy(st[top], temp); 
        i_l++;
        top++;
    }


    if (tree->children[0]->children[0]->num_children == 2) {
        proccessExpr3AC(tree->children[0]->children[0]);
    } else {
        if (tree->children[0]->children[0]->num_children == 1) {
            strcpy(st[++top], tree->children[0]->children[0]->children[0]->token);
        } else if (tree->children[0]->children[0]->num_children == 0) {
            strcpy(st[++top], tree->children[0]->children[0]->token);
        }

        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);
        printIndented("%s : %s \n", temp, st[top]);

        top--;
        strcpy(st[top], temp); 
        i_l++;
    }

    printIndented("if %s %s %s goto L%d\n", st[top], tree->children[0]->token, st[top - 1], label_then);
    printIndented("goto L%d\n", label_else);

    indent_level--;
    printIndented("L%d:\n", label_then);
    indent_level++;
    check3AC(tree->children[1]);
    printIndented("goto L%d\n", label_end);

    if (tree->num_children > 2) {
        indent_level--;
        printIndented("L%d:\n", label_else);
        indent_level++;
        check3AC(tree->children[2]);
    }

    indent_level--;
    printIndented("L%d:\n", label_end);
    Lnum++;
    indent_level++; 

}

void processWhile3AC(node *tree) {
    int label_start = Lnum++;
    int label_body = Lnum++;
    int label_end = Lnum++;

    label[Ltop++] = label_start;
    label[Ltop++] = label_body;
    label[Ltop++] = label_end;

    indent_level--;
    printIndented("L%d:\n", label_start);
    indent_level++; 

    if (tree->children[0]->children[1]->num_children == 2) {
        proccessExpr3AC(tree->children[0]->children[1]);
    } else {
        if (tree->children[0]->children[1]->num_children == 1) {
            strcpy(st[++top], tree->children[0]->children[1]->children[0]->token);
        } else if (tree->children[0]->children[1]->num_children == 0) {
            strcpy(st[++top], tree->children[0]->children[1]->token);
        }

        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);
        printIndented("%s = %s\n", temp, st[top]);
        strcpy(st[top], temp);
        i_l++;
    }

    if(tree->children[0]->children[0]->num_children==2){
        proccessExpr3AC(tree->children[0]->children[0]);
    }
    else {
        if (tree->children[0]->children[0]->num_children == 1) {
            strcpy(st[++top], tree->children[0]->children[0]->children[0]->token);
        } else if (tree->children[0]->children[0]->num_children == 0) {
            strcpy(st[++top], tree->children[0]->children[0]->token);
        }

        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);
        printIndented("%s = %s\n", temp, st[top]);
        strcpy(st[top], temp);
        i_l++;
    }
    printIndented("if %s %s %s goto L%d\n", st[top], tree->children[0]->token, st[top-1], label_body);
    printIndented("goto L%d\n", label_end);
    
    indent_level--; 
    printIndented("L%d:\n", label_body);
    indent_level++;
    
    check3AC(tree->children[1]);

    printIndented("goto L%d\n", label_start);
    
    indent_level--;
    printIndented("L%d:\n", label_end);
    indent_level++; 

}



void processDoWhile3AC(node *tree) {
    int label_body = Lnum++;
    int label_start = Lnum++;
    int label_end = Lnum++;

    label[Ltop++] = label_body;
    label[Ltop++] = label_start;
    label[Ltop++] = label_end;

    indent_level--;
    printIndented("L%d:\n", label_body);
    indent_level++;

    check3AC(tree->children[0]);
    
    indent_level--;
    printIndented("L%d:\n", label_start);
    indent_level++;

    if (tree->children[1]->children[0]->children[0]->num_children == 2) {
        proccessExpr3AC(tree->children[1]->children[0]->children[0]);
    } else {
        if (tree->children[1]->children[0]->children[0]->num_children == 1) {
            strcpy(st[++top], tree->children[1]->children[0]->children[0]->children[0]->token);
        } else if (tree->children[1]->children[0]->children[0]->num_children == 0) {
            strcpy(st[++top], tree->children[1]->children[0]->children[0]->token);
        }

        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);
        printIndented("%s = %s\n", temp, st[top]);
        strcpy(st[top], temp);
        i_l++;
    }

    if (tree->children[1]->children[0]->children[1]->num_children == 2) {
        proccessExpr3AC(tree->children[1]->children[0]->children[1]);
    } else {
        if (tree->children[1]->children[0]->children[1]->num_children == 1) {
            strcpy(st[++top], tree->children[1]->children[0]->children[1]->children[0]->token);
        } else if (tree->children[1]->children[0]->children[1]->num_children == 0) {
            strcpy(st[++top], tree->children[1]->children[0]->children[1]->token);
        }

        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);
        printIndented("%s = %s\n", temp, st[top]);
        strcpy(st[top], temp);
        i_l++;
    }

    printIndented("if %s %s %s goto L%d\n", st[top-1], tree->children[1]->children[0]->token, st[top], label_body);
    printIndented("goto L%d\n", label_end);

    indent_level--;
    printIndented("L%d:\n", label_end);
    indent_level++; 

}


void processLogicalAnd3AC(node *tree) {
    int label_skip = Lnum++;
    int label_end = Lnum++;

    label[Ltop++] = label_skip;
    label[Ltop++] = label_end;
    proccessExpr3AC(tree->children[1]->children[0]);
    printIndented("if %s == 0 goto L%d\n", st[top], label_skip);

    proccessExpr3AC(tree->children[1]->children[1]);

    printIndented("if %s == 0 goto L%d\n", st[top], label_skip);

    printIndented("%s = 1\n", tree->children[0]->token); 

    printIndented("goto L%d\n", label_end);

    indent_level--;
    printIndented("L%d:\n", label_skip);
    indent_level++;

    printIndented("%s = 0\n",tree->children[0]->token);

    printIndented("L%d:\n", label_end);
    indent_level++; 

}

void processLogicalOr3AC(node *tree) {
    int label_true = Lnum++;
    int label_end = Lnum++;

    label[Ltop++] = label_true;
    label[Ltop++] = label_end;

    proccessExpr3AC(tree->children[1]->children[0]);

    printIndented("if %s != 0 goto L%d\n", st[top], label_true);

    proccessExpr3AC(tree->children[1]->children[1]);

    printIndented("if %s != 0 goto L%d\n", st[top], label_true);

    printIndented("%s = 0\n", tree->children[0]->token); 

    printIndented("goto L%d\n", label_end);

    indent_level--;
    printIndented("L%d:\n", label_true);
    indent_level++;

    printIndented("%s = 1\n",tree->children[0]->token); 

    indent_level--;
    printIndented("L%d:\n", label_end);
    indent_level++;
}


void processLogicalANDInIf3AC(node* tree){
    int label_skip = Lnum++;
    int label_end = Lnum++;

    label[Ltop++] = label_skip;
    label[Ltop++] = label_end;
    proccessExpr3AC(tree->children[0]->children[0]);

    printIndented("if %s == 0 goto L%d\n", st[top], label_skip);

    proccessExpr3AC(tree->children[0]->children[1]);

    printIndented("if %s == 0 goto L%d\n", st[top], label_skip);
    check3AC(tree->children[1]);
    printIndented("goto L%d\n", label_end);
    
    indent_level--;
    printIndented("L%d:\n", label_skip);
    indent_level++;
    if (tree->num_children > 2) {
        check3AC(tree->children[2]);

    }
    indent_level--;
    printIndented("L%d:\n", label_end);
    indent_level++;
}


void processLogicalORInIf3AC(node* tree){
    int label_true = Lnum++;
    int label_end = Lnum++;

    label[Ltop++] = label_true;
    label[Ltop++] = label_end;

    proccessExpr3AC(tree->children[0]->children[0]);

    printIndented("if %s != 0 goto L%d\n", st[top], label_true);

    proccessExpr3AC(tree->children[0]->children[1]);

    printIndented("if %s != 0 goto L%d\n", st[top], label_true);
    if (tree->num_children > 2) { // יש חלק של else
        check3AC(tree->children[2]);

    }

    printIndented("goto L%d\n", label_end);

    indent_level--;
    printIndented("L%d:\n", label_true);
    indent_level++;
    check3AC(tree->children[1]);

    indent_level--;
    printIndented("L%d:\n", label_end);
    indent_level++; 


}

void processFor3AC(node *tree) {
    int label_start = Lnum++; 
    int label_condition = Lnum++; 
    int label_body = Lnum++; 
    int label_end = Lnum++; 

    label[Ltop++] = label_start;
    label[Ltop++] = label_condition;
    label[Ltop++] = label_body;
    label[Ltop++] = label_end;

    check3AC(tree->children[0]); 
    indent_level--;
    printIndented("L%d:\n", label_condition);
    indent_level++;
    proccessExpr3AC(tree->children[1]->children[0]); 
    printIndented("if %s == 0 goto L%d\n", st[top], label_end); 
    indent_level--;
    printIndented("L%d:\n", label_body);
    indent_level++;
    check3AC(tree->children[3]); 

    check3AC(tree->children[2]->children[0]); 

    printIndented("goto L%d\n", label_condition);
    indent_level--;
    printIndented("L%d:\n", label_end);
    indent_level++;


}

void processFunction3AC(node *tree) {
    indent_level = 0;
    Function *function = findFunction(globalScope, tree->children[2]->token);
    if (function == NULL) {
        printf("Error: Function '%s' not found in the symbol table.\n", tree->children[2]->token);
        exit(1);
    }

    int memorySize = calculateFunctionMemory(function);
    printf("%s : \n",tree->children[2]->token);
    indent_level++; 
    indent_level++; 

    printIndented("BeginFunc %d\n", memorySize);
    check3AC(tree->children[3]);  

    check3AC(tree->children[5]);  
    
    if (tree->children[5]->num_children > 0) {
        proccessExpr3AC(tree->children[5]->children[0]->children[tree->children[5]->children[0]->num_children - 1]->children[0]); 
        printIndented("Return %s\n", temp);  
    }
    printIndented("EndFunc\n\n\n");
    Ltop--;
    indent_level--; 
}

void processMainFunction3AC(node *tree) {
    indent_level = 0;
    printIndented("main:\n");
    
    Function *main = findFunction(globalScope, tree->children[2]->token);
    if (main == NULL) {
        printf("Error: Function '%s' not found in the symbol table.\n", tree->children[2]->token);
        exit(1);
    }

    int memorySize = calculateFunctionMemory(main);
    indent_level++; 
    indent_level++; 
    printIndented("BeginFunc %d\n", memorySize);

    check3AC(tree->children[5]);  
    printIndented("EndFunc\n\n\n");

}

void processFunctionCall3AC(node *tree) {
    for (int i = 0; i < tree->children[1]->num_children; i++) {
        proccessExpr3AC(tree->children[1]->children[i]); 
        
        strcpy(temp, "t");
        sprintf(s, "%d", i_l);
        strcat(temp, s);

        printIndented("%s = %s\n", temp, st[top]); 
        strcpy(st[top], temp); 
        i_l++;
        printIndented("PushParam %s\n", st[top]);
    }

    printIndented("t%d = LCall %s\n", i_l, tree->children[0]->token);
    printIndented("PopParams %d\n", tree->children[1]->num_children * 4); 
    sprintf(temp, "t%d", i_l);
    strcpy(st[++top], temp);
    i_l++;

}

int getTypeSize(char *type) {
    if (strcmp(type, "int") == 0) return 4;
    if (strcmp(type, "float") == 0) return 4;
    if (strcmp(type, "double") == 0) return 8;
    if (strcmp(type, "char") == 0) return 1;
    if (strcmp(type, "string") == 0) return 4; 
    return 4; 
}


int calculateFunctionMemory(Function *function) {
    if (function == NULL || function->scope == NULL) return 0;

    SymbolTable *funcScope = function->scope;
    int totalMemory = 0;

    // חישוב זיכרון לכל המשתנים בטבלת הסמלים של הפונקציה
    for (int i = 0; i < funcScope->var_count; i++) {
        totalMemory += getTypeSize(funcScope->variables[i]->type);
    }

    // חישוב זיכרון לכל הפרמטרים של הפונקציה
    for (int i = 0; i < function->param_count; i++) {
        totalMemory += getTypeSize(function->parameters[i]->type);
    }

    return totalMemory;
}


void printIndented(const char *format, ...) {
    va_list args;
    va_start(args, format);

    // הדפסת ההזחה בהתאם לרמת ההזחה הנוכחית
    for (int i = 0; i < indent_level; i++) {
        printf("   ");  // ניתן גם להשתמש ברווחים במקום '\t'
    }

    // הדפסת הטקסט בפורמט הרגיל
    vprintf(format, args);

    va_end(args);
}
