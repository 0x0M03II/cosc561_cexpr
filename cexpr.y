/*
 * Author:  Maurice Green
 * Program: cexpr.y
 * Purpose: Programming assignment #2 
 *
 * Operator precedence can easily be defined by including at least one rule
 * in the production body for each non-terminal production, that points to 
 * the production with the next highest precedence. Because I listed my prod-
 * uctions in ascending order, this reference will point to the operator with
 * the next lowest precedence (i.e. O-10 with Precedence N=10 will point to 
 * O-9 with precedence N=9, where 1 is the highest precedence).
 *
 * Additionally, sentinal values guarantee that, once a token is derived using
 * the productions, the appropriate value will be printed exactly then. However,
 * if an error is detected (e.g. int overflow or DBZ), the appropriate error 
 * message will be printed instead of the value.
 *
 * Finally, it's worth mentioning that integer and variable values are can be 
 * derived from any production; because each production points to its successor
 * derivation for integer and variable values is possible.
 *
 */

%{
#include <stdio.h>
#include <string.h>
#include <limits.h>

#define ALPHABET_SIZE   26                      /* Should hold 26 lower-case characters of alph.   */


int yylex();                                    /* tokens should be <= INT_MAX or < LLONG_MIN      */
void yyerror(char *);                           /* handle  Lex & Yacc errors                       */
static void dump(void);                         /* dump the alphabet state ; show all computations */
static void reset(void);                        /* clear alphabet values; set to zero              */
long long int l_alphabet[ALPHABET_SIZE];        /* should hold long long for max int operations    */


/**
 * The following macros are used for calculating integer overflow and underflow
 * for assignment operations.
 */

#define GEN_OVERFLOW(a, b)                \
   ((((a)   >  INT_MAX)    || ((a)        <  INT_MIN))                  || \
   (((b)    >  INT_MAX)    || ((b)        <  INT_MIN)))


#define  ADD_OVERFLOW(a, b)               \
   ((((b)   >= 0) && ((a)  >  INT_MAX     -  (b)))                      || \
   (((b)    <  0) && ((a)  <  INT_MIN     -  (b))))


#define  SUB_OVERFLOW(a, b)               \
   ((((b)   <  0) && ((a)  >  INT_MAX     +  (b)))                      || \
   (((b)    >= 0) && ((a)  <  INT_MIN     +  (b))))


#define  MUL_OVERFLOW(a, b)               \
   (((((b)  >  0) && ((a)  >  0))         && ((a)  >  INT_MAX  /  (b))) || \
   ((((b)   <  0) && ((a)  <  0))         && ((a)  <  INT_MAX  /  (b))) || \
   ((((b)   >  0) && ((a)  <  0))         && ((a)  <  INT_MIN  /  (b))) || \
   ((((b)   <  0) && ((a)  >  0))         && ((a)  >  INT_MIN  /  (b))))


#define  DIV_OVERFLOW(a, b)               \
   (((a)   == INT_MIN)    && ((b)         == -1)   && ((a)     != 0))



#define DIV_ZERO(a, b)                    \
   ((b) == 0)


#define CHECK_OVERFLOW(op, a, b)          \
   op##_OVERFLOW((a),   (b))


#define CHECK_DIVIDEZERO(a, b)            \
   DIV_ZERO((a),  (b))


#define HANDLE_OVERFLOW(op)               \
   printf("overflow\n")


#define HANDLE_DIVIDEBYZERO()             \
   printf("dividebyzero\n")

%}

%union {
   char *string;
   long long int num;
}

   /* Tokens */
%token <string> DUMP
%token <string> CLEAR

%token <num> MODULO
%token <num> INTEGER
%token <num> VARIABLE
%token <num> NEGATION
%token <num> DIVISION
%token <num> ADDITION
%token <num> SEMICOLON
%token <num> SHIFT_LEFT
%token <num> BITWISE_OR 
%token <num> ASSIGNMENT
%token <num> SUBTRACTION
%token <num> SHIFT_RIGHT
%token <num> BITWISE_AND
%token <num> BITWISE_XOR
%token <num> BITWISE_NOT
%token <num> MULTIPLICATION
%token <num> OPEN_PARENTHESIS
%token <num> ASSIGNMENT_MODULO
%token <num> CLOSED_PARENTHESIS
%token <num> ASSIGNMENT_DIVISION
%token <num> ASSIGNMENT_ADDITION
%token <num> ASSIGNMENT_SHIFT_LEFT
%token <num> ASSIGNMENT_BITWISE_OR
%token <num> ASSIGNMENT_SHIFT_RIGHT
%token <num> ASSIGNMENT_BITWISE_AND
%token <num> ASSIGNMENT_BITWISE_XOR
%token <num> ASSIGNMENT_SUBTRACTION
%token <num> ASSIGNMENT_MULTIPLICATION

   /* Types */

%type <num> value
%type <num> negation
%type <num> shifting
%type <num> add_group
%type <num> mul_group
%type <num> bitwise_or
%type <num> expression
%type <num> parentheses
%type <num> bitwise_not
%type <num> bitwise_and
%type <num> bitwise_xor


%%

commands    :
	         |	commands command
	         ;

command     :  expression  SEMICOLON   {  if ( $1 != INT_MIN) { printf("%lld\n", $1); } } 
            |  DUMP        SEMICOLON   {  dump();              }
            |  CLEAR       SEMICOLON   {  reset();             }
            ;

expression  
            :  VARIABLE ASSIGNMENT expression {
                  if (CHECK_OVERFLOW(GEN, $3, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] = $3;
                     $$ = l_alphabet[$1];
                  }
               }
            
            |  VARIABLE ASSIGNMENT_ADDITION expression {
                  if (CHECK_OVERFLOW(ADD, $1, $3)) {
                     HANDLE_OVERFLOW(ADD);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] += $3;
                     $$ = l_alphabet[$1];
                  }
               }

            |  VARIABLE ASSIGNMENT_SUBTRACTION expression {
                  if (CHECK_OVERFLOW(SUB, $1, $3)) {
                     HANDLE_OVERFLOW(SUB);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] -= $3;
                     $$ = l_alphabet[$1];
                  }
               }
            |  VARIABLE ASSIGNMENT_MULTIPLICATION expression {
                  if (CHECK_OVERFLOW(MUL, $1, $3)) {
                     HANDLE_OVERFLOW(MUL);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] *= $3;
                     $$ = l_alphabet[$1];
                  }
               }
                         
            |  VARIABLE ASSIGNMENT_DIVISION expression {
                  if (CHECK_DIVIDEZERO($1, $3)) {
                     HANDLE_DIVIDEBYZERO();
                     $$ = INT_MIN;
                  }
                  else if (CHECK_OVERFLOW(DIV, $1, $3)) {
                     HANDLE_OVERFLOW(DIV);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] /= $3;
                     $$ = l_alphabet[$1];
                  }
               }

            |  VARIABLE ASSIGNMENT_MODULO expression {
                  if (CHECK_DIVIDEZERO($1, $3)) {
                     HANDLE_DIVIDEBYZERO();
                     $$ = INT_MIN;
                  }      
                  else if (CHECK_OVERFLOW(DIV, $1, $3)) {
                     HANDLE_OVERFLOW(DIV);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] %= $3;
                     $$ = l_alphabet[$1];
                  }                  
               }

            |  VARIABLE ASSIGNMENT_SHIFT_LEFT expression {
                  if (CHECK_OVERFLOW(GEN, $3, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] <<= $3;
                     $$ = l_alphabet[$1];
                  }
               }

            |  VARIABLE ASSIGNMENT_SHIFT_RIGHT expression {
                  if (CHECK_OVERFLOW(GEN, $3, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] >>= $3;
                     $$ = l_alphabet[$1];
                  }
               }

            |  VARIABLE ASSIGNMENT_BITWISE_AND expression {
                  if (CHECK_OVERFLOW(GEN, $3, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] &= $3;
                     $$ = l_alphabet[$1];
                  }
               }

            |  VARIABLE ASSIGNMENT_BITWISE_XOR expression {
                  if (CHECK_OVERFLOW(GEN, $3, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] ^= $3;
                     $$ = l_alphabet[$1];
                  }
               }

            |  VARIABLE ASSIGNMENT_BITWISE_OR expression {
                  if (CHECK_OVERFLOW(GEN, $3, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     l_alphabet[$1] |= $3;
                     $$ = l_alphabet[$1];
                  }                  
               }

            |  bitwise_or {
                  $$ = $1;
               }
            ;

bitwise_or 
            :  bitwise_xor

            |  bitwise_or BITWISE_OR bitwise_xor {
                  if (CHECK_OVERFLOW(GEN, $1, $3)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 | $3;
                  }
               }

            ;

bitwise_xor
            :  bitwise_and

            |  bitwise_xor BITWISE_XOR bitwise_and {
                  if (CHECK_OVERFLOW(GEN, $1, $3)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 ^ $3;
                  }
               }

            ;

bitwise_and
            :  shifting

            |  bitwise_and BITWISE_AND shifting { 
                  if (CHECK_OVERFLOW(GEN, $1, $3)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 & $3;
                  }
               }
            ;

shifting    
            :  add_group

            |  shifting SHIFT_LEFT add_group {
                  if (CHECK_OVERFLOW(GEN, $1, $3)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 << $3;
                  }
               }

            |  shifting SHIFT_RIGHT add_group {
                  if (CHECK_OVERFLOW(GEN, $1, $3)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 >> $3;
                  }
               }
            ;

add_group
            :  mul_group

            |  add_group ADDITION mul_group {
                  if (CHECK_OVERFLOW(GEN, $1, $3)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 + $3;
                  }
               }

            |  add_group SUBTRACTION mul_group {
                  if (CHECK_OVERFLOW(GEN, $1, $3)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 - $3;
                  }
               }

            ;

mul_group
            :  negation

            |  mul_group MULTIPLICATION negation {
                  if (CHECK_OVERFLOW(MUL, $1, $3)) {
                     HANDLE_OVERFLOW(MUL);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 * $3;
                  }  
               }
            
            |  mul_group DIVISION negation {  
                  if (CHECK_DIVIDEZERO($1, $3)) {
                     HANDLE_DIVIDEBYZERO();
                     $$ = INT_MIN;
                  }     
                  else if (CHECK_OVERFLOW(DIV, $1, $3)) {
                     HANDLE_OVERFLOW(DIV);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 / $3;
                  }
               }

            |  mul_group MODULO negation {
                  if (CHECK_DIVIDEZERO($1, $3)) {
                     HANDLE_DIVIDEBYZERO();
                     $$ = INT_MIN;
                  }
                  else if (CHECK_OVERFLOW(DIV, $1, $3)) {
                     HANDLE_OVERFLOW(DIV);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = $1 % $3;
                  }
               }

            ;

negation
            :  bitwise_not
            
            |  SUBTRACTION bitwise_not {
                  if (CHECK_OVERFLOW(GEN, $2, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = -$2;
                  }
               }
            
            ;

bitwise_not
            :  parentheses

            |  BITWISE_NOT parentheses {
                  if (CHECK_OVERFLOW(GEN, $2, 0)) {
                     HANDLE_OVERFLOW(GEN);
                     $$ = INT_MIN;
                  }
                  else {
                     $$ = ~$2;
                  }
               }

            ;

parentheses
            :  value

            |  OPEN_PARENTHESIS bitwise_or CLOSED_PARENTHESIS {
                  $$ = $2;
               }
            
            ;

value
            :  INTEGER {
                  $$ = $1;
               }
            
            |  VARIABLE {
                  $$ = l_alphabet[$1];
               }

            ;

%%

static void reset(void)
{  // Clear set all array values to zero
   memset(l_alphabet, 0, sizeof(l_alphabet));
}

static void dump(void)
{  // Dump the alphabet array values

   for (int k = 0; k < ALPHABET_SIZE; ++k)
   {
      printf("%c: %lld\n", 'a' + k, l_alphabet[k]);
   }
}

// print error to standard error 
void yyerror(char *s)
{
   fprintf(stderr, "%s\n", s);
}

int main(void)
{  // main function for parser

   // initialize array values to zero
   reset();

   // Begin Parsing
   if (yyparse())
      printf("\nInvalid expression.\n");
   else
      printf("\nCalculator off.\n");
}