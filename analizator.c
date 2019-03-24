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

enum{
  ID, END, CT_INT, CT_REAL, CT_CHAR, CT_STRING, COMMA, SEMICOLON,
  LPAR, RPAR, LBRACKET, RBRACKET, LACC, RACC, ADD, SUB, MUL, DIV,
  DOT, AND, OR, NOT, ASSIGN, EQUAL, NOTEQ, LESS, LESSEQ,
  GR, GREQ, SPACE, LINECOMMENT, COMMENT,
  BREAK, CHAR, DOUBLE, ELSE, FOR, IF, INT, RETURN, STRUCT, VOID, WHILE
};

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

  Token *tk;

  for (;;){

    ch = *pCrtCh;
    //if ((int)ch != 10 && (int)ch != 13)
      //printf("starea %d cu %c--(%d) -- LINIA: %d\n",state,ch,ch, line);
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
        }else if (n == 4  && !(memcmp(pStartCh, "char", 4)) ){
          tk = addTk(CHAR);
        }else if (n == 6  && !(memcmp(pStartCh, "double", 6)) ){
          tk = addTk(DOUBLE);
        }else if (n == 4  && !(memcmp(pStartCh, "else", 4)) ){
          tk = addTk(ELSE);
        }else if (n == 3  && !(memcmp(pStartCh, "for", 3)) ){
          tk = addTk(FOR);
        }else if (n == 2  && !(memcmp(pStartCh, "if", 2)) ){
          tk = addTk(IF);
        }else if (n == 3  && !(memcmp(pStartCh, "int", 3)) ){
          tk = addTk(INT);
        }else if (n == 6  && !(memcmp(pStartCh, "return", 6)) ){
          tk = addTk(RETURN);
        }else if (n == 6  && !(memcmp(pStartCh, "struct", 6)) ){
          tk = addTk(STRUCT);
        }else if (n == 4  && !(memcmp(pStartCh, "void", 4)) ){
          tk = addTk(VOID);
        }else if (n == 5  && !(memcmp(pStartCh, "while", 5)) ){
          tk = addTk(WHILE);
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
        printf(" { ");
        return LACC;
      case 13:
        addTk(RACC);
        printf(" } ");
        return RACC;
      case 14:
        addTk(SEMICOLON);
        printf(" ; ");
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
        }else
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
        return DIV;
      case 36:
        if (ch != '\n' && ch != '\r' && ch != '\0')
          pCrtCh++;
        else
          state = 0;
        break;
      case 40:
        if (ch == '\\'){
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
        tk->i = tmpCh[0];
        printf("CHAR : %c ", tk->i);
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
        printf("CT_INT : %d ", tk->i);
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
        return NOTEQ;
      case 999:
        addTk(END);
        return END;
      default : printf("Stare invalida %d(%c)", state, ch);
        break;
    }
  }
}

int main(){
  FILE *fis;
  int i = 0;
  if ((fis = fopen("8.c", "r")) == NULL){
    err("invalid");
  }
  int n = fread(inbuf, 1, 30000, fis);
  inbuf[n] = '\0';
  for (i = 0;i < n;i++)
    printf("%c", inbuf[i]);
  printf("\n");
  pCrtCh = inbuf;
  fclose(fis);
  while (getNextToken() != END){/*...*/}
  //afisare
}
