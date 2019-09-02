/*
Consider a sequence u where u is defined as follows:
    The number u(0) = 1 is the first one in u.
    For each x in u, then y = 2 * x + 1 and z = 3 * x + 1 must be in u too.
    There are no other numbers in u.
Ex: u = [1, 3, 4, 7, 9, 10, 13, 15, 19, 21, 22, 27, ...]
1 gives 3 and 4, then 3 gives 7 and 10, 4 gives 9 and 13, then 7 gives 15 and 22 and so on...
Task:
Given parameter n the function dbl_linear (or dblLinear...) returns the element u(n) of the ordered (with <) sequence u (so, there are no duplicates).
Example:
dbl_linear(10) should return 22
Note:
Focus attention on efficiency
*/
#include <stdio.h>
#include <stdlib.h>

typedef struct eldt{
  int info;
  struct elq *previous;
  struct elq *next;
} EldT;

typedef EldT* dTail;

typedef struct pdT{
  dTail start;
  dTail end;
} dT;

void push_dt(dT *t,int x){
    dTail aux = malloc(sizeof(dT));
    aux->info = x;
    aux->previous = t->end;
    aux->next = NULL;
    if(t->start == NULL){
      t->start = aux;
      t->end = aux;
      return;
      }
    t->end->next = aux;
    t->end = aux;
    return;
}

void pop_dt(dT *t){
  dTail aux;
  if(t->start == NULL) return;
  aux = t->start;
  t->start = t->start->next;
  free(aux);
  return;
}
int dblLinear(int n) {
  dT p,t;
  p.start = p.end = NULL;
  t.start = t.end = NULL;
  int k;
  push_dt(&p,1); 
  push_dt(&t,1);
  for(int i=0;i<n;i++){
    if(p.start->info<t.start->info){
      k = p.start->info;
      push_dt(&p,2*k+1);
      push_dt(&t,3*k+1);
      pop_dt(&p);
      }
    else if(p.start->info==t.start->info){
      k = t.start->info;
      push_dt(&p,2*k+1);
      push_dt(&t,3*k+1);
      pop_dt(&t);
      pop_dt(&p);
      }  
    else{
      k = t.start->info;
      push_dt(&p,2*k+1);
      push_dt(&t,3*k+1);
      pop_dt(&t);
      }
  }
  if(p.start->info<t.start->info) return p.start->info;
  else return t.start->info;
}
