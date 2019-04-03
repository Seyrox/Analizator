#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include "analizator.h"
//functia conversie converteste din orice baza in INT

#define SAFEALLOC(var,Type) if((var=(Type*)malloc(sizeof(Type)))==NULL)err("not enough memory");

char inbuf[30001];
char *pCrtCh;

enum{ID, BREAK,CHAR,DOUBLE,ELSE,FOR,IF,INT,RETURN,STRUCT,VOID,WHILE,CT_INT,
    CT_REAL,CT_CHAR,CT_STRING,COMMA,SEMICOLON,LPAR,RPAR,LBRACKET,RBRACKET,LACC,RACC,
    ADD,SUB,MUL,DIV,DOT,AND,OR,NOT,ASSIGN,EQUAL,NOTEQ,LESS,LESSEQ,GR,GREQ,END};

int unit();
int declStruct();
int declVar();
int typeBase();
int arrayDecl();
int typeName();
int declFunc();
int funcArg();
int stm();
int stmCompound();
int expr();
int exprAssign();
int exprOr();
int exprAnd();
int exprEq();
int exprRel();
int exprAdd();
int exprMul();
int exprCast();
int exprUnary();
int exprPostFix();
int exprPrimary();

const char* atomName(int i){
  switch(i){
    case 0: return "ID"; break;
    case 1: return "BREAK";break;
    case 2: return "CHAR";break;
    case 3: return "DOUBLE";break;
    case 4: return "ELSE";break;
    case 5: return "FOR";break;
    case 6: return "IF";break;
    case 7: return "INT";break;
    case 8: return "RETURN";break;
    case 9: return "STRUCT";break;
    case 10:return "VOID";break;
    case 11:return "WHILE";break;
    case 12:return "CT_INT";break;
    case 13:return "CT_REAL";    break;
    case 14:return "CT_CHAR";break;
    case 15:return "CT_STRING";break;
    case 16:return "COMMA";break;
    case 17:return "SEMICOLON";break;
    case 18:return "LPAR";break;
    case 19:return "RPAR";break;
    case 20:return "LBRACKET";break;
    case 21:return "RBRACKET";    break;
    case 22:return "LACC";break;
    case 23:return "RACC";break;
    case 24:return "ADD";break;
    case 25:return "SUB";break;
    case 26:return "MUL";break;
    case 27:return "DIV";break;
    case 28:return "DOT";break;
    case 29:return "AND";break;
    case 30:return "OR";break;
    case 31:return "NOT";break;
    case 32:return "ASSIGN";break;
    case 33:return "EQUAL";break;
    case 34:return "NOTEQ";break;
    case 35:return "LESS";break;
    case 36:return "LESSEQ";break;
    case 37:return "GR";break;
    case 38:return "GREQ";break;
    case 39:return "END";break;
    default: return ""; break;
  }
}

typedef struct _Token{
  int code; // codul (numele)
  union{
  char *text; // folosit pentru ID, CT_STRING (alocat dinamic)
  long int i; // folosit pentru CT_INT, CT_CHAR
  double r; // folosit pentru CT_REAL
};
  int line; // linia din fisierul de intrare
  struct _Token *next; // inlantuire la urmatorul AL
}Token;

int line = 1;
Token *lastToken;
Token *tokens;

Token *addTk(int code){
  Token *tk;
  SAFEALLOC(tk,Token)
  tk->code=code;
  tk->line=line;
  tk->next=NULL;
  if(lastToken){
    lastToken->next=tk;
  }else{
    tokens = tk;
  }
  lastToken = tk;
  return tk;
}

void err(const char *fmt,...){
  va_list va;
  va_start(va,fmt);
  fprintf(stderr,"error: ");
  vfprintf(stderr,fmt,va);
  fputc('\n',stderr);
  va_end(va);
  exit(-1);
}

char* createString(char *pStart, char *pStop){
    char *str;
    //printf("Create String: %ld\n", pStop-pStart);
    if ( (str = (char*)malloc(sizeof(char)*(pStop-pStart+1)) ) == NULL)
      err("Memorie insuficientaa");
    memcpy(str,pStart,pStop-pStart);
    str[pStop-pStart]=0;
    return str;
}

char sequenceEsc(char ch){
    switch(ch){
      case 'a' :
        return '\a';
        break;
      case 'b' :
        return '\b';
        break;
      case 'f' :
        return '\f';
        break;
      case 'n' :
        return '\n';
        break;
      case 'r' :
        return '\r';
        break;
      case 't' :
        return '\t';
        break;
      case 'v' :
        return '\v';
        break;
      default:
        return ch;
        break;
    }
}

void tkerr(const Token *tk,const char *fmt,...){
  va_list va;
  va_start(va,fmt);
  fprintf(stderr,"error in line %d: ",tk->line);
  vfprintf(stderr,fmt,va);
  fputc('\n',stderr);
  va_end(va);
  exit(-1);
}

int getNextToken(){
  int state = 0;
  int n; //N CH
  char ch;
  char *pStartCh;
  char *pStartChINT;
  char *pStartChREAL;
  char *pStartChCHAR;
  char *pStartChSTRING;
  char *tmpCh;

  Token *tk;

  for (;;){

    ch = *pCrtCh;
    //if ((int)ch != 10 && (int)ch != 13)
    //  printf("starea %d cu %c--(%d) -- LINIA: %d\n",state,ch,ch, line);
    //printf("LINE: %d", line);
    switch(state){
      case 0:
        if (isalpha(ch) || ch == '_'){
          state = 1;
          pStartCh = pCrtCh;
          pCrtCh++;
        } else if (ch == ']'){
          state = 3;
          pCrtCh++;
        } else if (ch == '['){
          state = 4;
          pCrtCh++;
        } else if (ch == ')'){
          state = 5;
          pCrtCh++;
        } else if (ch == '('){
          state = 6;
          pCrtCh++;
        } else if (ch == '&'){
          state = 7;
          pCrtCh++;
        } else if (ch == '!'){
          state = 9;
          pCrtCh++;
        } else if (ch == ','){
          state = 11;
          pCrtCh++;
        } else if (ch == '{'){
          state = 12;
          pCrtCh++;
        } else if (ch == '}'){
          state = 13;
          pCrtCh++;
        } else if (ch == ';'){
          state = 14;
          pCrtCh++;
        } else if (ch == '+'){
          state = 15;
          pCrtCh++;
        } else if (ch == '-'){
          state = 16;
          pCrtCh++;
        } else if (ch == '*'){
          state = 17;
          pCrtCh++;
        } else if (ch == '.'){
          state = 18;
          pCrtCh++;
        } else if (ch == '|'){
          state = 19;
          pCrtCh++;
        } else if (ch == '='){
          state = 21;
          pCrtCh++;
        } else if (ch == '<'){
          state = 24;
          pCrtCh++;
        } else if (ch == '>'){
          state = 27;
          pCrtCh++;
        } else if (ch == '/'){
          state = 30;
          pCrtCh++;
        } else if (ch == '\''){
          state = 40;
          pStartChCHAR = pCrtCh;
          pCrtCh++;
        } else if (ch == '"'){
          state = 44;
          pStartChSTRING = pCrtCh;
          pCrtCh++;
        } else if(isdigit(ch) && ch != '0'){
          pStartChINT = pCrtCh;
          pStartChREAL = pCrtCh;
          state = 48;
          pCrtCh++;
        } else if (isdigit(ch) && ch == '0'){
          state = 49;
          pStartChINT = pCrtCh;
          pStartChREAL = pCrtCh;
          pCrtCh++;
        } else if (ch == ' ' || ch == '\r' || ch == '\t'){
          pCrtCh++;
        } else if (ch == '\0'){
          state = 999;
        } else if (ch == '\n'){
          pCrtCh++;
          line++;
          printf("\n");
          printf("LINE: %d", line);
        } else
            err ("caracter invalid %c-(%d) - state: %d", ch, ch, state);
        break;
      case 1:
        if (isalnum(ch) || ch == '_')
          pCrtCh++;
        else
          state = 2;
        break;
      case 2:
        n = pCrtCh - pStartCh;
        if (n == 5  && !(memcmp(pStartCh, "break", 5)) ){
          tk = addTk(BREAK);
          printf(" BREAK ");
        }else if (n == 4  && !(memcmp(pStartCh, "char", 4)) ){
          tk = addTk(CHAR);
          printf(" CHAR ");
        }else if (n == 6  && !(memcmp(pStartCh, "double", 6)) ){
          tk = addTk(DOUBLE);
          printf(" DOUBLE ");
        }else if (n == 4  && !(memcmp(pStartCh, "else", 4)) ){
          tk = addTk(ELSE);
          printf(" ELSE ");
        }else if (n == 3  && !(memcmp(pStartCh, "for", 3)) ){
          tk = addTk(FOR);
          printf(" FOR ");
        }else if (n == 2  && !(memcmp(pStartCh, "if", 2)) ){
          tk = addTk(IF);
          printf(" IF ");
        }else if (n == 3  && !(memcmp(pStartCh, "int", 3)) ){
          tk = addTk(INT);
          printf(" INT ");
        }else if (n == 6  && !(memcmp(pStartCh, "return", 6)) ){
          tk = addTk(RETURN);
          printf(" RETURN ");
        }else if (n == 6  && !(memcmp(pStartCh, "struct", 6)) ){
          tk = addTk(STRUCT);
          printf(" STRUCT ");
        }else if (n == 4  && !(memcmp(pStartCh, "void", 4)) ){
          tk = addTk(VOID);
          printf(" VOID ");
        }else if (n == 5  && !(memcmp(pStartCh, "while", 5)) ){
          tk = addTk(WHILE);
          printf(" WHILE ");
        }else{
          tk = addTk(ID);
          tk->text = createString(pStartCh, pCrtCh);
          printf("ID : %s", tk->text);
          return ID;
        }
        return tk->code;
      case 3:
        addTk(RBRACKET);
        printf(" ] ");
        return RBRACKET;
      case 4:
        addTk(LBRACKET);
        printf(" [ ");
        return LBRACKET;
      case 5:
        addTk(RPAR);
        printf(" ) ");
        return RPAR;
      case 6:
        addTk(LPAR);
        printf(" ( ");
        return LPAR;
      case 7:
        if (ch == '&'){
          pCrtCh++;
          state = 8;
        }
        break;
      case 8:
        addTk(AND);
        printf(" AND ");
        return AND;
      case 9:
        if (ch == '='){
          pCrtCh++;
          state = 61;
        }else
          state = 10;
        break;
      case 10:
        addTk(NOT);
        printf(" NOT ");
        return NOT;
      case 11:
        addTk(COMMA);
        printf(" COMMA ");
        return COMMA;
      case 12:
        addTk(LACC);
        printf(" LACC ");
        return LACC;
      case 13:
        addTk(RACC);
        printf(" RACC ");
        return RACC;
      case 14:
        addTk(SEMICOLON);
        printf(" SEMICOLON ");
        return SEMICOLON;
      case 15:
        addTk(ADD);
        printf(" PLUS ");
        return ADD;
      case 16:
        addTk(SUB);
        printf(" SUB ");
        return SUB;
      case 17:
        addTk(MUL);
        printf(" MUL ");
        return MUL;
      case 18:
        addTk(DOT);
        printf(" DOT ");
        return DOT;
      case 19:
        if (ch == '|'){
          pCrtCh++;
          state = 20;
        }
        break;
      case 20:
        addTk(OR);
        printf(" OR ");
        return OR;
      case 21:
        if (ch == '='){
          pCrtCh++;
          state = 23;
        }else
          state = 22;
        break;
      case 22:
        addTk(ASSIGN);
        printf(" ASSIGN ");
        return ASSIGN;
      case 23:
        addTk(EQUAL);
        printf(" EQUAL ");
        return EQUAL;
      case 24:
        if (ch == '='){
          pCrtCh++;
          state = 26;
        }else
          state = 25;
        break;
      case 25:
        addTk(LESS);
        printf(" LESS ");
        return LESS;
      case 26:
        addTk(LESSEQ);
        printf(" LESSEQ ");
        return LESSEQ;
      case 27:
        if (ch == '='){
          pCrtCh++;
          state = 29;
        }else
          state = 28;
        break;
      case 28:
        addTk(GR);
        printf(" GR ");
        return GR;
      case 29:
        addTk(GREQ);
        printf(" GREQ ");
        return GREQ;
      case 30:
        if (ch == '*'){
          state = 31;
          pCrtCh++;
        }else if (ch == '/'){
          state = 36;
          pCrtCh++;
        }else
          state = 34;
        break;
      case 31:
        if (ch == '*'){
          state = 32;
          pCrtCh++;
        }else if (ch == '\n' && ch != '*'){
          line++;
          printf("\n");
          printf("LINE: %d", line);
          pCrtCh++;
        }else if (ch != '*' && ch != '\n')
          pCrtCh++;
        break;
      case 32:
        if (ch == '*')
          pCrtCh++;
        else if (ch == '/'){
          state = 0;
          pCrtCh++;
        }else if (ch != '*' || ch != '/'){
          state = 31;
          pCrtCh++;
        }
        break;
      case 34:
        addTk(DIV);
        printf(" DIV ");
        return DIV;
      case 36:
        if (ch != '\n' && ch != '\r' && ch != '\0')
          pCrtCh++;
        else if (ch == '\n'){
          printf("LINE: %d", line);
          state = 0;
          line++;
        }else
          state = 0;
        break;
      case 40:
        if (ch == '\\'){
          tmpCh = pCrtCh;
          state = 41;
          pCrtCh++;
        }else if (ch != '\\' && ch != '\''){
          state = 42;
          pCrtCh++;
        }
        break;
      case 41:
        if (ch == 'a' || ch == 'b' || ch == 'f'  || ch == 'n' || ch == 't' ||
            ch == 'r' || ch == 'v' || ch == '\'' || ch == '?' || ch == '"' ||
            ch == '\\' || ch == '0'){
          state = 42;
          pCrtCh++;
        }
        break;
      case 42:
        if (ch == '\''){
          state = 43;
          pCrtCh++;
        }
        break;
      case 43:
        tk = addTk(CT_CHAR);
        char *tmpCh;
        tmpCh = createString(pStartChCHAR + 1, pCrtCh - 1);
        if (tmpCh[0] == '\\'){
          tmpCh[0] = sequenceEsc(tmpCh[1]);
          //tmpCh[1] = "";
        }
        tk->i = tmpCh[0];
        printf("CHAR : %c ", (char)tk->i);
        return tk->code;
      case 44:
        if (ch == '\\'){
          state = 45;
          pCrtCh++;
        }else if (ch != '"' || ch != '\\'){
          state = 46;
          pCrtCh++;
        }else if (ch == '"'){
          state = 46;
          pCrtCh++;
        }
        break;
      case 45:
        if (ch == 'a' || ch == 'b' || ch == 'f'  || ch == 'n' || ch == 't' ||
            ch == 'r' || ch == 'v' || ch == '\'' || ch == '?' || ch == '"' ||
            ch == '\\' || ch == '0'){
          state = 46;
          pCrtCh++;
        }
        break;
      case 46:
        if (ch == '"'){
          pCrtCh++;
          state = 47;
        }else{
          state = 44;
        }
        break;
      case 47:
        tk = addTk(CT_STRING);
        tk->text = createString(pStartChSTRING + 1, pCrtCh - 1);
        char tmpChString;
        int i, j;
        for(i = 0;i < strlen(tk->text);i++){
          if (tk->text[i] == '\\'){
            tmpChString = sequenceEsc(tk->text[i+1]);
            tk->text[i] = tmpChString;
            tk->text[i+1] = (char)255;
          }
        }
        printf("STRING: %s ", tk->text);
        return tk->code;
      case 48:
        if (isdigit(ch) && ch>='0' && ch<='9')
          pCrtCh++;
        else if (ch == '.'){
          pCrtCh++;
          state = 55;
        }else if (ch == 'e' || ch == 'E'){
          pCrtCh++;
          state = 57;
        }else
          state = 53;
        break;
      case 49:
        if (ch == 'x'){
          pCrtCh++;
          state = 51;
        }else
          state = 50;
        break;
      case 50:
        if (isdigit(ch) && (ch != '8' || ch != '9'))
          pCrtCh++;
        else if (ch == '.'){
          pCrtCh++;
          state = 55;
        }else if (ch == 'e' || ch == 'E'){
          pCrtCh++;
          state = 57;
        }else if (ch == '8' || ch == '9'){
          pCrtCh++;
          state = 54;
        }else
          state = 53;
        break;
      case 51:
        if ( (isalpha(ch) && ( (ch>='a' && ch<='f') || (ch>='A' && ch<='F') ) ) || isdigit(ch) ){
          pCrtCh++;
          state = 52;
        }
        break;
      case 52:
        if ( (isalpha(ch) && ( (ch>='a' && ch<='f') || (ch>='A' && ch<='F') ) ) || isdigit(ch) )
          pCrtCh++;
        else
          state = 53;
        break;
      case 53:
        tk = addTk(CT_INT);
        char *pc = createString(pStartChINT,pCrtCh);
        if(strchr(pc, 'x') == NULL && pc[0] != '0')
            tk->i=atoi(pc);
        else if(strchr(pc,'x') == NULL)
            tk->i=strtol(pc, NULL, 8);
        else
            tk->i=strtol(pc, NULL, 16);
        free(pc);
        printf("CT_INT : %ld ", tk->i);
        return CT_INT;
      case 54:
        if (isdigit(ch))
          pCrtCh++;
        else if (ch == '.'){
          pCrtCh++;
          state = 55;
        }else if (ch == 'e' || ch == 'E'){
          pCrtCh++;
          state = 57;
        }
        break;
      case 55:
        if (isdigit(ch)){
          pCrtCh++;
          state = 56;
        }
        break;
      case 56:
        if (isdigit(ch))
          pCrtCh++;
        else if (ch == 'e' || ch == 'E'){
          pCrtCh++;
          state = 57;
        }
        else
          state = 60;
        break;
      case 57:
        if (ch == '+' || ch == '-'){
          pCrtCh++;
          state = 58;
        }else
          state = 58;
        break;
      case 58:
        if (isdigit(ch)){
          pCrtCh++;
          state = 59;
        }
        break;
      case 59:
        if (isdigit(ch))
          pCrtCh++;
        else
          state = 60;
        break;
      case 60:
        tk = addTk(CT_REAL);
        char *pcReal;
        pcReal = createString(pStartChREAL, pCrtCh);
        tk->r = atof(pcReal);
        free(pcReal);
        printf("CT_REAL : %lf ", tk->r);
        return tk->code;
      case 61:
        addTk(NOTEQ);
        printf(" NOTEQ ");
        return NOTEQ;
      case 999:
        addTk(END);
        printf(" END ");
        return END;
      default : printf("Stare invalida %d(%c)", state, ch);
        break;
    }
  }
}

Token *consumedTk, *crtTk;

int consume(int code){
  if(crtTk->code==code){
    consumedTk=crtTk;
    crtTk=crtTk->next;
    printf("Consumat(%s-%d)\n", atomName(consumedTk->code), consumedTk->line);
    //printf("Curent(%s-%d)\n", atomName(crtTk->code), crtTk->line);
    return 1;
  }
  printf("Neconsumat(%s-%d)\n", atomName(consumedTk->code), consumedTk->line);
  //printf("Curent(%s-%d)\n", atomName(crtTk->code), crtTk->line);
  return 0;
}

int unit(){
  Token *startTk = crtTk;
  printf("###unit\n");
  int i = 1;
  for (;;){
    printf("UNIT:%d\n", i);
    if (declStruct()){
      printf("DECLSTRUCT\n");
    }else if (declFunc()){
      printf("DECLFUNC\n");
    }else if (declVar()){
      printf("DECLVAR\n");
    }else break;
    i++;
  }
  if (consume(END)){
    return 1;
  }
  crtTk = startTk;
  return 0;
}

int declStruct(){
  Token *startTk = crtTk;
  printf("###declStruct\n");
  if (consume(STRUCT)){
    if (consume(ID)){
      if (consume(LACC)){
        for (;;){
          if (declVar()){

          }else
            break;
        }
        if (consume(RACC)){
          if (consume(SEMICOLON)){
            return 1;
          }else
            tkerr(crtTk, "missing SEMICOLON in declStruct\n");
        }else
          tkerr(crtTk, "missing RACC in declStruct\n");
      }
    }else
      tkerr(crtTk, "missing ID in declStruct\n");
  }
  crtTk = startTk;
  return 0;
}

int declVar(){
  Token *startTk = crtTk;
  printf("###declVar\n");
  if (typeBase()){
    if (consume(ID)){
      if (arrayDecl()){

      }
      for (;;){
        if (consume(COMMA)){
          if (consume(ID)){
            if (arrayDecl()){
            }
          }else
            tkerr(crtTk, "missing ID in declVar\n");
        }else
          break;
      }
      if (consume(SEMICOLON)){
        return 1;
      }else
        tkerr(crtTk, "missing SEMICOLON in declVar\n");
    }else
      tkerr(crtTk, "missing ID in declVar\n");
  }
  crtTk = startTk;
  return 0;
}

int typeBase(){
  Token *startTk = crtTk;
  printf("###typeBase\n");
  if (consume(INT)){
    return 1;
  }else if (consume(DOUBLE) ){
    return 1;
  }else if (consume(CHAR) ){
    return 1;
  }else if (consume(STRUCT) ){
    if (consume(ID) ){
      return 1;
    }else
      tkerr(crtTk, "missing TYPE in typeBase\n");
  }
    crtTk = startTk;
    return 0;
}

int arrayDecl(){
  Token *startTk = crtTk;
  printf("###arrayDecl\n");
  if (consume(LBRACKET)){
    if (expr()){

    }
    if (consume(RBRACKET) ){
      return 1;
    }else
      tkerr(crtTk, "missing RBRACKET in arrayDecl\n");
  }
  crtTk = startTk;
  return 0;
}

int typeName(){
  Token *startTk = crtTk;
  printf("###typeName\n");
  if (!typeBase() ){
    crtTk = startTk;
    return 0;
  }
  arrayDecl();
  return 1;
}

int declFunc(){
  Token *startTk = crtTk;
  printf("###declFunc\n");
  if (typeBase() ){
    if (consume(MUL)){

    }
  }else if (consume(VOID)){

  }else
    return 0;
  if (consume(ID)){
    if (consume(LPAR)){
      if (funcArg()){
        for(;;){
          if (consume(COMMA)){
            if(funcArg()){

            }else
              tkerr(crtTk, "missing FUNCARG in DECLFUNC");
          }else
            break;
        }
      }
      if (consume(RPAR)){
        if (stmCompound()){
          return 1;
        }
      }else
        tkerr(crtTk, "missing RPAR in DECLFUNC");
    }
  }
  crtTk = startTk;
  return 0;
}

int funcArg(){
  Token *startTk = crtTk;
  printf("###funcArg\n");
  if (typeBase()){
    if (consume(ID)){
      if (arrayDecl()){
        return 1;
      }else{}
      return 1;
    }else
      return 0;
  }
  crtTk = startTk;
  return 0;
}

int stm(){
    Token *startTk = crtTk;
    printf("###stm\n");
    if(stmCompound()){
      return 1;
    }
    else if(consume(IF)){
      if(consume(LPAR)){
        if(expr()){
          if(consume(RPAR)){
            if(stm()){
              if(consume(ELSE)){
                if (stm()){

                }else
                  tkerr(crtTk, "missing STM from ELSE(STM)");
              }
              return 1;
            }else
              tkerr(crtTk,"missing STM in IF(STM)");
          }else
            tkerr(crtTk,"missing RPAR in IF(STM)");
        }else
          tkerr(crtTk,"missing EXPR in IF(STM)");
      }else
        tkerr(crtTk,"missing LPAR in IF(STM)");
    }else if(consume(WHILE)){
      if(consume(LPAR)){
        if(expr()){
          if(consume(RPAR)){
            if(stm()){
              return 1;
            }else
              tkerr(crtTk,"missing STM in WHILE(STM)");
          }else
            tkerr(crtTk,"missing RPAR in WHILE(STM)");
        }else
          tkerr(crtTk,"missing EXPR in WHILE(STM)");
      }else
        tkerr(crtTk,"missing LPAR in WHILE(STM)");
    }else if(consume(FOR)){
      if(consume(LPAR)){
        if(expr()){

        }
        if(consume(SEMICOLON)){
          if(expr()){

          }
          if(consume(SEMICOLON)){
            if(expr()){

            }
            if(consume(RPAR)){
              if(stm()){
                return 1;
              }else
                tkerr(crtTk,"missing STM in FOR(STM)");
            }else
              tkerr(crtTk,"missing RPAR in FOR(STM)");
          }else
            tkerr(crtTk,"missing SECOND SEMICOLON in FOR(STM)");
        }else
          tkerr(crtTk,"missing FIRST SEMICOLON in FOR(STM)");
      }else
        tkerr(crtTk,"missing LPAR in FOR(STM)");
    }else if(consume(BREAK)){
      if(consume(SEMICOLON)){
        return 1;
      }else
        tkerr(crtTk,"missing SEMICOLON in BREAK(STM)");
    }else if(consume(RETURN)){
      if(expr()){
        if(consume(SEMICOLON)){
          return 1;
        }else
          tkerr(crtTk,"missing SEMICOLON in RETURN(STM)");
      }else{
        if(consume(SEMICOLON)){
          return 1;
        }else
          tkerr(crtTk,"missing SEMICOLON in RETURN(STM)");
      }
    }else{
      if(expr()){

      }
      if(consume(SEMICOLON)){
        return 1;
      }
    }
    crtTk = startTk;
    return 0;
}

int stmCompound(){
  Token *startTk = crtTk;
  printf("###stmCompound\n");
  if (consume(LACC)){
    for (;;){
      if (declVar()){

      }else if (stm()){

      }else
        break;
    }
    if (consume(RACC)){
      return 1;
    }else
      tkerr(crtTk, "missing RACC in STMCOMPOUND");
  }
  crtTk = startTk;
  return 0;
}

int expr(){
  Token *startTk = crtTk;
  printf("###expr\n");
  if (exprAssign()){
    return 1;
  }
  crtTk = startTk;
  return 0;
}

int exprAssign(){
    Token *startTk = crtTk;
    printf("###exprAssign\n");
    if(exprUnary()){
      if(consume(ASSIGN)){
        if(exprAssign()){
          return 1;
        }
      }
    }
    crtTk = startTk;
    if(exprOr()){
      return 1;
    }
    crtTk = startTk;
    return 0;
}

int exprOrPrim(){
  //printf("###exprOrPrim\n");
  if (consume(OR)){
    if (exprAnd()){
      if (exprOrPrim()){
        return 1;
      }
    }else
      tkerr(crtTk, "error exprAnd in exprOrPrim");
  }
  return 1;
}

int exprOr(){
  Token *startTk = crtTk;
  printf("###exprOr\n");
  if (exprAnd()){
    if (exprOrPrim()){
      return 1;
    }
  }
  crtTk = startTk;
  return 0;
}

int exprAndPrim(){
  //printf("###exprAndPrim\n");
  if (consume(AND)){
    if (exprEq()){
      if (exprAndPrim()){
        return 1;
      }
    }else
      tkerr(crtTk, "error exprEq in exprAndPrim");
  }
  return 1;
}

int exprAnd(){
  printf("###exprAnd\n");
  Token *startTk;
  if (exprEq()){
    if (exprAndPrim()){
      return 1;
    }
  }
  crtTk = startTk;
  return 0;
}

int exprEqPrim(){
  //printf("###exprEqPrim\n");
  if (consume(EQUAL) || consume(NOTEQ)){
      if (exprRel()){
        if (exprEqPrim()){
          return 1;
        }
      }else
        tkerr(crtTk, "error exprRel in exprEqPrim");
  }
  return 1;
}


int exprEq(){
  Token *startTk = crtTk;
  printf("###exprEq\n");
  if (exprRel()){
    if (exprEqPrim()){
      return 1;
    }
  }
  crtTk = startTk;
  return 0;
}

int exprRelPrim(){
  //printf("###exprRelPrim\n");
  if (consume(LESS) || consume(LESSEQ) || consume(GR) || consume(GREQ)){
    if (exprAdd()){
      if (exprRelPrim()){
        return 1;
      }
    }else
      tkerr(crtTk, "error exprAdd in exprRelPrim");
  }
  return 1;
}

int exprRel(){
  Token *startTk = crtTk;
  printf("###exprRel\n");
  if (exprAdd()){
    if (exprRelPrim()){
      return 1;
    }
  }
  crtTk = startTk;
  return 0;
}

int exprAddPrim(){
  //printf("###exprAddPrim\n");
  if (consume(ADD) || consume(SUB)){
    if (exprMul()){
      if (exprAddPrim()){
        return 1;
      }
    }
  }
  return 1;
}

int exprAdd(){
  Token *startTk = crtTk;
  printf("###exprAdd\n");
  if (exprMul()){
    if (exprAddPrim()){
      return 1;
    }else
      tkerr(crtTk, "error exprAddPrim in exprAdd");
  }
  crtTk = startTk;
  return 0;
}

int exprMulPrim(){
  //printf("###exprMulPrim\n");
  if (consume(MUL) || consume(DIV)){
    if (exprCast()){
      if (exprMulPrim()){
        return 1;
      }
    }else
      tkerr(crtTk, "error exprCast in exprMulPrim");
  }
  return 1;
}

int exprMul(){
  Token *startTk = crtTk;
  printf("###exprMul\n");
  if (exprCast()){
    if (exprMulPrim()){
      return 1;
    }
  }
  crtTk = startTk;
  return 0;
}

int exprCast(){
    Token *startTk=crtTk;

    printf("###exprCast\n");
    if(consume(LPAR)){
      if(typeName()){
        if(consume(RPAR)){
          if(exprCast()){
            return 1;
          }else
            tkerr(crtTk,"error exprCast in exprCast");
        }else
          tkerr(crtTk,"missing )");
      }else
        tkerr(crtTk, "missing TYPENAME in EXPRCAST");
    }else{
      if(exprUnary()){
        return 1;
      }
    }
    crtTk = startTk;
    return 0;
}

int exprUnary(){
  Token *startTk = crtTk;
  printf("###exprUnary\n");
  if (consume(SUB) || consume(NOT)){
    if (exprUnary()){
      return 1;
    }else
      tkerr(crtTk, "Error exprUnary in exprUnary");
  }else{
    if (exprPostFix()){
      return 1;
    }
  }
  crtTk = startTk;
  return 0;
}

int exprPostFixPrim(){
  //printf("###exprPostFixPrim\n");
  if (consume(LBRACKET)){
    if (expr()){
      if (consume(RBRACKET)){
        if (exprPostFixPrim()){
          return 1;
        }
      }else
        tkerr(crtTk, "error RBRACKET in exprPostFixPrim");
    }else
      tkerr(crtTk, "error expr in exprPostFixPrim");
  }else if (consume(DOT)){
    if (consume(ID)){
      if (exprPostFixPrim()){
        return 1;
      }
    }else
      tkerr(crtTk, "error ID in exprPostFixPrim");
  }
  return 1;
}


int exprPostFix(){
  Token *startTk = crtTk;
  //printf("###exprPostFix\n");
  if (exprPrimary()){
    if (exprPostFixPrim()){
      return 1;
    }
  }
  crtTk = startTk;
  return 0;
}

int exprPrimary(){
  Token *startTk = crtTk;
  printf("###exprPrimary\n");
  if (consume(ID)){
    if (consume(LPAR)){
      if (expr()){
        for (;;){
          if (consume(COMMA)){
            if (expr()){

            }else
              tkerr(crtTk, "error expr in exprPrimary");
          }else
            break;
        }
      }
      if (consume(RPAR)){

      }
    }else{
        return 1;
    }
    return 1;
  }else if (consume(CT_INT)){
    return 1;
  }else if (consume(CT_REAL)){
    return 1;
  }else if (consume(CT_CHAR)){
    return 1;
  }else if (consume(CT_STRING)){
    return 1;
  }else if (consume(LPAR)){
    if (expr()){
      if (consume(RPAR)){
        return 1;
      }else
        tkerr(crtTk, "error RPAR in exprPrimary");
    }else
      tkerr(crtTk, "error expr() in exprPrimary");
  }
  crtTk = startTk;
  return 0;
}

int main(){
  FILE *fis;
  int i = 0;
  if ((fis = fopen("9.c", "r")) == NULL){
    err("invalid");
  }
  int n = fread(inbuf, 1, 30000, fis);
  inbuf[n] = '\0';
  for (i = 0;i < n;i++)
    printf("%c", inbuf[i]);
  printf("LINE: %d", line);
  pCrtCh = inbuf;
  fclose(fis);
  while (getNextToken() != END){}
  printf("\n");  //afisare
  crtTk = tokens;
  if (unit())
    printf("Syntax OK!");
  else
    printf("Syntax error!");

  printf("\n");
  return 0;
}
