%{
#include "y.tab.h"

%}

%{
            void yyerror(char *);
%}

%%

[0-9]+      { sscanf(yytext, "%lld", &yylval.num); return INTEGER;    }
[a-z]       { yylval.num = *yytext - 'a'; return VARIABLE;          }
"dump"      { yylval.string = strdup(yytext); return DUMP;          }
"clear"     { yylval.string = strdup(yytext); return CLEAR;         }
"="         { return ASSIGNMENT;                                    }
"+="        { return ASSIGNMENT_ADDITION;                           }
"-="        { return ASSIGNMENT_SUBTRACTION;                        }
"*="        { return ASSIGNMENT_MULTIPLICATION;                     }
"/="        { return ASSIGNMENT_DIVISION;                           }
"%="        { return ASSIGNMENT_MODULO;                             }
"<<="       { return ASSIGNMENT_SHIFT_LEFT;                         }
"=>>"       { return ASSIGNMENT_SHIFT_RIGHT;                        }
"&="        { return ASSIGNMENT_BITWISE_AND;                        }
"^="        { return ASSIGNMENT_BITWISE_XOR;                        }
"|="        { return ASSIGNMENT_BITWISE_OR;                         }
"|"         { return BITWISE_OR;                                    }
"^"         { return BITWISE_XOR;                                   }
"&"         { return BITWISE_AND;                                   }
"<<"        { return SHIFT_LEFT;                                    }
">>"        { return SHIFT_RIGHT;                                   }
"+"         { return ADDITION;                                      }
"-"         { return SUBTRACTION;                                   }
"*"         { return MULTIPLICATION;                                }
"/"         { return DIVISION;                                      }
"%"         { return MODULO;                                        }
"~"         { return BITWISE_NOT;                                   }
"("         { return OPEN_PARENTHESIS;                              }
")"         { return CLOSED_PARENTHESIS;                            }
";"         { return SEMICOLON;                                     }
[ \t]+      {                                                       }
[ \t\n\r]   ;
.           { return yytext[0];                                     }
