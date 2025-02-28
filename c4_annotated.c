// c4.c - C in four functions

// char, int, and pointer types
// if, while, return, and expression statements
// just enough features to allow self-compilation and a bit more

// Written by Robert Swierczek

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long // makes every int in the code long long 

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

void next()
{
  char *pp; // Pointer used to store the current position of the string during certain operations.

  // Loop through each character of the source code starting from current position in 'p'.
  while (tk = *p) {
    ++p; // Move to the next character in the source code.
    
    if (tk == '\n') { // If the current character is a newline,
      if (src) { // If the source code print flag is set,
        // Print the current line number and the source code from 'lp' to 'p'.
        printf("%d: %.*s", line, p - lp, lp);
        lp = p; // Update the 'lp' pointer to the new position.
        
        // Print the emitted assembly instructions, if any.
        while (le < e) {
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]);
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
        }
      }
      ++line; // Increment the line number after processing a newline character.
    }
    else if (tk == '#') { // If the current character is '#', skip the rest of the line (preprocessor directive).
      while (*p != 0 && *p != '\n') ++p; // Skip characters until the end of the line.
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') { // If the character is a letter or an underscore (start of an identifier),
      pp = p - 1; // Save the current position to start processing the identifier.
      // Process the identifier: Continue as long as the characters are valid for an identifier.
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_')
        tk = tk * 147 + *p++; // Hash the characters in the identifier to generate a unique token value.
      
      tk = (tk << 6) + (p - pp); // Finalize the token value based on the length of the identifier.
      id = sym; // Start checking for the identifier in the symbol table.
      while (id[Tk]) { // Loop through symbol table entries to find a match.
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; } // If a match is found, set the token and return.
        id = id + Idsz; // Move to the next entry in the symbol table.
      }
      id[Name] = (int)pp; // Store the name of the identifier.
      id[Hash] = tk; // Store the hash value of the identifier.
      tk = id[Tk] = Id; // Set the token type to 'Id' (identifier).
      return; // Return after processing the identifier.
    }
    else if (tk >= '0' && tk <= '9') { // If the character is a digit (start of a number),
      if (ival = tk - '0') { // If it's a decimal number, calculate its value.
        while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; // Process subsequent digits for the number.
      }
      else if (*p == 'x' || *p == 'X') { // If it's a hexadecimal number (starts with '0x'),
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0); // Process hexadecimal digits.
      }
      else { // If it's an octal number (starts with '0'),
        while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; // Process octal digits.
      }
      tk = Num; // Set token type to 'Num' (number).
      return; // Return after processing the number.
    }
    else if (tk == '/') { // If the character is a division operator,
      if (*p == '/') { // If it's a comment (starts with '//'),
        ++p; // Skip the '/' and continue to the next character.
        while (*p != 0 && *p != '\n') ++p; // Skip characters until the end of the line.
      }
      else {
        tk = Div; // Otherwise, it's a division operator.
        return; // Return after processing the division.
      }
    }
    else if (tk == '\'' || tk == '"') { // If the character is a quote (start of a string or character literal),
      pp = data; // Save the current position for storing the string/character.
      while (*p != 0 && *p != tk) { // Loop through the characters until the matching quote is found.
        if ((ival = *p++) == '\\') { // If an escape sequence is encountered,
          if ((ival = *p++) == 'n') ival = '\n'; // Convert '\n' to a newline character.
        }
        if (tk == '"') *data++ = ival; // Store the character in the data if it's a string.
      }
      ++p; // Move past the closing quote.
      if (tk == '"') ival = (int)pp; else tk = Num; // Set the value based on whether it's a string or character.
      return; // Return after processing the string or character literal.
    }
    else if (tk == '=') { // If the character is an equal sign,
      if (*p == '=') { // If it's a comparison operator ('=='),
        ++p; // Move past the second '='.
        tk = Eq; // Set the token to 'Eq' (equal).
      } else tk = Assign; // Otherwise, it's an assignment operator ('=').
      return; // Return after processing the operator.
    }
    else if (tk == '+') { // If the character is a plus sign,
      if (*p == '+') { // If it's an increment operator ('++'),
        ++p; // Move past the second '+'.
        tk = Inc; // Set the token to 'Inc' (increment).
      } else tk = Add; // Otherwise, it's a plus sign for addition.
      return; // Return after processing the operator.
    }
    else if (tk == '-') { // If the character is a minus sign,
      if (*p == '-') { // If it's a decrement operator ('--'),
        ++p; // Move past the second '-'.
        tk = Dec; // Set the token to 'Dec' (decrement).
      } else tk = Sub; // Otherwise, it's a minus sign for subtraction.
      return; // Return after processing the operator.
    }
    else if (tk == '!') { // If the character is an exclamation mark,
      if (*p == '=') { // If it's a not-equal comparison ('!='),
        ++p; // Move past the '='.
        tk = Ne; // Set the token to 'Ne' (not equal).
      }
      return; // Return after processing the operator.
    }
    else if (tk == '<') { // If the character is a less-than sign,
      if (*p == '=') { // If it's a less-than-or-equal comparison ('<='),
        ++p; // Move past the '='.
        tk = Le; // Set the token to 'Le' (less than or equal).
      } else if (*p == '<') { // If it's a left-shift operator ('<<'),
        ++p; // Move past the second '<'.
        tk = Shl; // Set the token to 'Shl' (shift left).
      } else tk = Lt; // Otherwise, it's a less-than comparison ('<').
      return; // Return after processing the operator.
    }
    else if (tk == '>') { // If the character is a greater-than sign,
      if (*p == '=') { // If it's a greater-than-or-equal comparison ('>='),
        ++p; // Move past the '='.
        tk = Ge; // Set the token to 'Ge' (greater than or equal).
      } else if (*p == '>') { // If it's a right-shift operator ('>>'),
        ++p; // Move past the second '>'.
        tk = Shr; // Set the token to 'Shr' (shift right).
      } else tk = Gt; // Otherwise, it's a greater-than comparison ('>').
      return; // Return after processing the operator.
    }
    else if (tk == '|') { // If the character is a pipe sign ('|'),
      if (*p == '|') { // If it's a logical OR operator ('||'),
        ++p; // Move past the second '|'.
        tk = Lor; // Set the token to 'Lor' (logical OR).
      } else tk = Or; // Otherwise, it's a bitwise OR operator ('|').
      return; // Return after processing the operator.
    }
    else if (tk == '&') { // If the character is an ampersand ('&'),
      if (*p == '&') { // If it's a logical AND operator ('&&'),
        ++p; // Move past the second '&'.
        tk = Lan; // Set the token to 'Lan' (logical AND).
      } else tk = And; // Otherwise, it's a bitwise AND operator ('&').
      return; // Return after processing the operator.
    }
    else if (tk == '^') { // If the character is a caret ('^'),
      tk = Xor; // Set the token to 'Xor' (bitwise XOR).
      return; // Return after processing the operator.
    }
    else if (tk == '%') { // If the character is a percent sign ('%'),
      tk = Mod; // Set the token to 'Mod' (modulus).
      return; // Return after processing the operator.
    }
    else if (tk == '*') { // If the character is an asterisk ('*'),
      tk = Mul; // Set the token to 'Mul' (multiplication).
      return; // Return after processing the operator.
    }
    else if (tk == '[') { // If the character is an opening square bracket ('['),
      tk = Brak; // Set the token to 'Brak' (array bracket).
      return; // Return after processing the operator.
    }
    else if (tk == '?') { // If the character is a question mark ('?'),
      tk = Cond; // Set the token to 'Cond' (conditional operator).
      return; // Return after processing the operator.
    }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
    // Skip characters that are not relevant for token generation (such as operators and punctuation).
  }
}

// expr function processes an expression, considering its syntax and precedence
void expr(int lev) 
{
    int t, *d;

    // Check for unexpected EOF in expression
    if (!tk) { 
        printf("%d: unexpected eof in expression\n", line); 
        exit(-1); 
    }
    // Process number literal
    else if (tk == Num) { 
        *++e = IMM; *++e = ival; next(); ty = INT; 
    }
    // Process string literal
    else if (tk == '"') {
        *++e = IMM; *++e = ival; next();
        while (tk == '"') next(); 
        data = (char *)((int)data + sizeof(int) & -sizeof(int)); 
        ty = PTR;
    }
    // Process sizeof operator
    else if (tk == Sizeof) { 
        next(); 
        if (tk == '(') next(); 
        else { 
            printf("%d: open paren expected in sizeof\n", line); 
            exit(-1); 
        }
        ty = INT; 
        if (tk == Int) next(); 
        else if (tk == Char) { 
            next(); 
            ty = CHAR; 
        }
        // Process pointer dereferencing in sizeof
        while (tk == Mul) { 
            next(); 
            ty = ty + PTR; 
        }
        if (tk == ')') next(); 
        else { 
            printf("%d: close paren expected in sizeof\n", line); 
            exit(-1); 
        }
        *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
        ty = INT;
    }
    // Process identifier (variable or function)
    else if (tk == Id) { 
        d = id; next();
        // If function call
        if (tk == '(') {
            next();
            t = 0;
            // Process function arguments
            while (tk != ')') { 
                expr(Assign); 
                *++e = PSH; 
                ++t; 
                if (tk == ',') next(); 
            }
            next();
            // Call appropriate function depending on symbol class
            if (d[Class] == Sys) 
                *++e = d[Val];
            else if (d[Class] == Fun) { 
                *++e = JSR; *++e = d[Val]; 
            }
            else { 
                printf("%d: bad function call\n", line); 
                exit(-1); 
            }
            if (t) { 
                *++e = ADJ; *++e = t; 
            }
            ty = d[Type];
        }
        // If variable reference
        else if (d[Class] == Num) { 
            *++e = IMM; *++e = d[Val]; ty = INT; 
        }
        else {
            // Handling undefined variables and pointers
            if (d[Class] == Loc) { 
                *++e = LEA; *++e = loc - d[Val]; 
            }
            else if (d[Class] == Glo) { 
                *++e = IMM; *++e = d[Val]; 
            }
            else { 
                printf("%d: undefined variable\n", line); 
                exit(-1); 
            }
            *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
        }
    }
    // Process parentheses, handle cast and subexpressions
    else if (tk == '(') {
        next();
        if (tk == Int || tk == Char) {
            t = (tk == Int) ? INT : CHAR; next();
            while (tk == Mul) { next(); t = t + PTR; }
            if (tk == ')') next(); else { 
                printf("%d: bad cast\n", line); 
                exit(-1); 
            }
            expr(Inc);
            ty = t;
        }
        else {
            expr(Assign);
            if (tk == ')') next(); else { 
                printf("%d: close paren expected\n", line); 
                exit(-1); 
            }
        }
    }
    // Process dereferencing (pointer dereference)
    else if (tk == Mul) {
        next(); 
        expr(Inc);
        if (ty > INT) ty = ty - PTR; 
        else { 
            printf("%d: bad dereference\n", line); 
            exit(-1); 
        }
        *++e = (ty == CHAR) ? LC : LI;
    }
    // Process address-of operator (&)
    else if (tk == And) {
        next(); 
        expr(Inc);
        if (*e == LC || *e == LI) --e; 
        else { 
            printf("%d: bad address-of\n", line); 
            exit(-1); 
        }
        ty = ty + PTR;
    }
    // Process logical negation
    else if (tk == '!') { 
        next(); 
        expr(Inc); 
        *++e = PSH; 
        *++e = IMM; *++e = 0; *++e = EQ; 
        ty = INT; 
    }
    // Process bitwise negation (~)
    else if (tk == '~') { 
        next(); 
        expr(Inc); 
        *++e = PSH; 
        *++e = IMM; *++e = -1; *++e = XOR; 
        ty = INT; 
    }
    // Process addition (+)
    else if (tk == Add) { 
        next(); 
        expr(Inc); 
        ty = INT; 
    }
    // Process subtraction (-)
    else if (tk == Sub) {
        next(); 
        *++e = IMM;
        if (tk == Num) { 
            *++e = -ival; 
            next(); 
        } 
        else { 
            *++e = -1; 
            *++e = PSH; 
            expr(Inc); 
            *++e = MUL; 
        }
        ty = INT;
    }
    // Process increment/decrement (pre)
    else if (tk == Inc || tk == Dec) {
        t = tk; next(); expr(Inc);
        if (*e == LC) { 
            *e = PSH; *++e = LC; 
        }
        else if (*e == LI) { 
            *e = PSH; *++e = LI; 
        }
        else { 
            printf("%d: bad lvalue in pre-increment\n", line); 
            exit(-1); 
        }
        *++e = PSH;
        *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (t == Inc) ? ADD : SUB;
        *++e = (ty == CHAR) ? SC : SI;
    }
    else { 
        printf("%d: bad expression\n", line); 
        exit(-1); 
    }

    // Top-down operator precedence handling
    while (tk >= lev) { 
        t = ty;
        // Assignment operator
        if (tk == Assign) {
            next();
            if (*e == LC || *e == LI) *e = PSH; 
            else { 
                printf("%d: bad lvalue in assignment\n", line); 
                exit(-1); 
            }
            expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
        }
        // Conditional operator (ternary)
        else if (tk == Cond) {
            next();
            *++e = BZ; d = ++e;
            expr(Assign);
            if (tk == ':') next(); 
            else { 
                printf("%d: conditional missing colon\n", line); 
                exit(-1); 
            }
            *d = (int)(e + 3); *++e = JMP; d = ++e;
            expr(Cond);
            *d = (int)(e + 1);
        }
        // Logical OR (||)
        else if (tk == Lor) { 
            next(); *++e = BNZ; d = ++e; 
            expr(Lan); *d = (int)(e + 1); ty = INT; 
        }
        // Logical AND (&&)
        else if (tk == Lan) { 
            next(); *++e = BZ; d = ++e; 
            expr(Or); *d = (int)(e + 1); ty = INT; 
        }
        // Handle other operators similarly...

    }
}
// stmt function processes different types of statements such as if, while, return, compound (block), and expression statements
void stmt()
{
  int *a, *b;

  // Handle 'if' statement
  if (tk == If) {
    next();  // Move to the next token
    if (tk == '(') next(); else { 
        printf("%d: open paren expected\n", line); 
        exit(-1); 
    } 
    expr(Assign);  // Process the condition expression for 'if'
    if (tk == ')') next(); else { 
        printf("%d: close paren expected\n", line); 
        exit(-1); 
    }
    *++e = BZ; b = ++e;  // Generate a branch if the condition is false
    stmt();  // Process the 'if' body
    // Handle optional 'else' block
    if (tk == Else) { 
        *b = (int)(e + 3); *++e = JMP; b = ++e;  // Jump over the 'else' block if needed
        next();  // Move to the next token after 'else'
        stmt();  // Process the 'else' body
    }
    *b = (int)(e + 1);  // Set the jump target after the 'if' block
  }
  // Handle 'while' loop
  else if (tk == While) {
    next();  // Move to the next token
    a = e + 1;  // Save the position for the jump back to the condition check
    if (tk == '(') next(); else { 
        printf("%d: open paren expected\n", line); 
        exit(-1); 
    }
    expr(Assign);  // Process the condition expression for 'while'
    if (tk == ')') next(); else { 
        printf("%d: close paren expected\n", line); 
        exit(-1); 
    }
    *++e = BZ; b = ++e;  // Generate a branch if the condition is false
    stmt();  // Process the body of the loop
    *++e = JMP; *++e = (int)a;  // Jump back to the condition check (loop)
    *b = (int)(e + 1);  // Set the branch target if the condition is false
  }
  // Handle 'return' statement
  else if (tk == Return) {
    next();  // Move to the next token
    if (tk != ';') expr(Assign);  // If there's an expression, process it
    *++e = LEV;  // Generate return from the current function
    if (tk == ';') next(); else { 
        printf("%d: semicolon expected\n", line); 
        exit(-1); 
    }
  }
  // Handle compound (block) statement
  else if (tk == '{') {
    next();  // Move to the next token
    // Process multiple statements inside the block
    while (tk != '}') stmt();
    next();  // Move past the closing brace
  }
  // Handle empty statement (semicolon only)
  else if (tk == ';') {
    next();  // Just move past the semicolon
  }
  // Handle expression statement (expression followed by semicolon)
  else {
    expr(Assign);  // Process the expression
    if (tk == ';') next(); else { 
        printf("%d: semicolon expected\n", line); 
        exit(-1); 
    }
  }
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // vm registers (pc = program counter, sp = stack pointer, bp = base pointer)
  int i, *t; // temporary variables

  --argc; ++argv; // adjust argc and argv
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; } // check for -s flag
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; } // check for -d flag
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; } // print usage if no file is provided

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; } // open the provided file

  poolsz = 256*1024; // arbitrary size for memory pool
  // allocate memory for different sections of the VM
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  // initialize memory sections to zero
  memset(sym,  0, poolsz);
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // add keywords to the symbol table
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add system library functions to symbol table
  next(); id[Tk] = Char; // handle void type
  next(); idmain = id; // keep track of main function

  // allocate memory for the source code area
  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
  // read the source code from the file
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
  p[i] = 0;
  close(fd);

  // parse declarations
  line = 1;
  next(); // start parsing
  while (tk) { // loop through tokens
    bt = INT; // default base type is int
    if (tk == Int) next(); // parse 'int' type
    else if (tk == Char) { next(); bt = CHAR; } // parse 'char' type
    else if (tk == Enum) { // handle enum type
      next();
      if (tk != '{') next(); // expect '{' for enum definition
      if (tk == '{') {
        next();
        i = 0;
        // parse enum identifiers and initializers
        while (tk != '}') {
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++; // add enum value to symbol table
          if (tk == ',') next();
        }
        next();
      }
    }
    // parse variable declarations
    while (tk != ';' && tk != '}') {
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; } // handle pointer type
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // function declaration
        id[Class] = Fun;
        id[Val] = (int)(e + 1); // store function address
        next(); i = 0;
        // parse function parameters
        while (tk != ')') {
          ty = INT; // default type for parameters is int
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; } // handle pointer type for parameters
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc; // set parameter as local
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        // parse local variable declarations within function
        while (tk == Int || tk == Char) {
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        // generate code for function entry
        *++e = ENT; *++e = i - loc;
        while (tk != '}') stmt(); // process function body
        *++e = LEV; // function exit
        id = sym; // unwind symbol table locals
        // restore local variables in the symbol table
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else { // global variable
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  // ensure main function is defined
  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; }
  if (src) return 0; // exit early if source flag is set

  // setup stack for program execution
  bp = sp = (int *)((int)sp + poolsz); // initialize stack pointer
  *--sp = EXIT; // call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

// Run the virtual machine cycle

cycle = 0;  // Initialize cycle count to 0
while (1) {  // Infinite loop for continuous execution
    i = *pc++; ++cycle;  // Fetch the next instruction and increment cycle count
    if (debug) {  // If debugging is enabled
        // Print out the current cycle and instruction name (based on index)
        printf("%d> %.4s", cycle,
            &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
             "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
             "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);  // Print the operation name
        if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");  // Print argument if relevant
    }

    // Instruction handling based on the value of i (instruction opcode)
    if      (i == LEA) a = (int)(bp + *pc++);  // LEA: Load effective address, compute address using base pointer
    else if (i == IMM) a = *pc++;  // IMM: Load immediate value or global address
    else if (i == JMP) pc = (int *)*pc;  // JMP: Jump to the address specified by the current value of pc
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }  // JSR: Jump to subroutine, save return address on the stack
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;  // BZ: Branch if zero, jump if condition is met
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;  // BNZ: Branch if not zero, jump if condition is met
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }  // ENT: Enter subroutine, adjust stack and base pointer
    else if (i == ADJ) sp = sp + *pc++;  // ADJ: Adjust the stack pointer (modify stack size)
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; }  // LEV: Leave subroutine, restore state
    else if (i == LI)  a = *(int *)a;  // LI: Load integer value from the address in a
    else if (i == LC)  a = *(char *)a;  // LC: Load character value from the address in a
    else if (i == SI)  *(int *)*sp++ = a;  // SI: Store integer value to the address pointed by sp
    else if (i == SC)  a = *(char *)*sp++ = a;  // SC: Store character value to the address pointed by sp
    else if (i == PSH) *--sp = a;  // PSH: Push value onto the stack

    // Perform bitwise and arithmetic operations
    else if (i == OR)  a = *sp++ |  a;  // OR: Bitwise OR
    else if (i == XOR) a = *sp++ ^  a;  // XOR: Bitwise XOR
    else if (i == AND) a = *sp++ &  a;  // AND: Bitwise AND
    else if (i == EQ)  a = *sp++ == a;  // EQ: Equal comparison
    else if (i == NE)  a = *sp++ != a;  // NE: Not equal comparison
    else if (i == LT)  a = *sp++ <  a;  // LT: Less than comparison
    else if (i == GT)  a = *sp++ >  a;  // GT: Greater than comparison
    else if (i == LE)  a = *sp++ <= a;  // LE: Less than or equal comparison
    else if (i == GE)  a = *sp++ >= a;  // GE: Greater than or equal comparison
    else if (i == SHL) a = *sp++ << a;  // SHL: Bitwise shift left
    else if (i == SHR) a = *sp++ >> a;  // SHR: Bitwise shift right
    else if (i == ADD) a = *sp++ +  a;  // ADD: Addition
    else if (i == SUB) a = *sp++ -  a;  // SUB: Subtraction
    else if (i == MUL) a = *sp++ *  a;  // MUL: Multiplication
    else if (i == DIV) a = *sp++ /  a;  // DIV: Division
    else if (i == MOD) a = *sp++ %  a;  // MOD: Modulo (remainder)

    // File and memory operations
    else if (i == OPEN) a = open((char *)sp[1], *sp);  // OPEN: Open a file
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);  // READ: Read from file
    else if (i == CLOS) a = close(*sp);  // CLOS: Close a file
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }  // PRTF: Print formatted output
    else if (i == MALC) a = (int)malloc(*sp);  // MALC: Memory allocation
    else if (i == FREE) free((void *)*sp);  // FREE: Free memory
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);  // MSET: Set memory block
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);  // MCMP: Compare memory blocks
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }  // EXIT: Exit the virtual machine, printing the result
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }  // Handle unknown instruction
}
