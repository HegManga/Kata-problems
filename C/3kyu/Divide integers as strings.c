/*
Given non-negative integer a and positive integer b as C-strings, return a pointer "array" char ** of length 2 containing the quotient and remainder of the result in that order. The quotient and remainder should both be pointer-strings and not character arrays; furthermore, their memory (and the memory of the entire pointer "array") should be dynamically allocated as the test cases will free the memory allocated for your result which would throw an error otherwise.

Since the length of the pointer "array" is expected to be constant, there is no point in passing an int/size_t by reference and having your solution set it to the length of the output array.

    a and b can be very large (at the order of 10^150 to 10^200)
    As usual, your result should not have leading 0s
    require is disabled in JavaScript. Do it yourself ;-)
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void resize_char(char *a,int *sa){
	if(a==NULL || *sa<1){ 
		printf("\n Not possible in mylresize, debug print!");
		printf("\nAlt value sa=%d",*sa);
		exit(1);
	}	
	while(*sa>1 && a[(*sa)-1]=='0') (*sa)--; 
	return;
}

int compare_char(char *a,int sa,char *b,int sb){
	
	if(sa <= 0 || sb <= 0){
		printf("\n Not possible in lcompare, debug print!");
		printf("\nAlt value sa=%d sb=%d",sa,sb);
		exit(1);
	}
	if(sa==1 && sb==1) {
		if(a[0]==b[0]) return 0;
		else if(a[0]<b[0]) return -1;
		else return 1;
		}
	if(sa>sb) return 1;
	if(sa<sb) return -1;
	if(a[sa-1]<b[sb-1]) return -1;
	if(a[sa-1]>b[sb-1]) return 1;
	else return compare_char(a,sa-1,b,sb-1);
}

void inc_char(char *a, int sa, int index){
    if(a==NULL || index<0 || index>=sa){
		printf("\n Not possible in inc_char, debug print!");
		printf("\nAlt value sa=%d index=%d",sa,index);
		exit(1);
	}
	
	a[index]+=1;
	
	if(a[index]>'9'){
		a[index]='0';
		inc_char(a,sa,index+1);
	}
	return;	
}
void sub_char(char *a,int sa, char *b, int sb, int start){
  	//assuming a>=b
	if(sa < start+sb  || start<0){
		printf("\n Not possible in sub_char, debug print!");
		printf("\nAlt value sa=%d sb=%d start=%d",sa,sb,start);
		exit(1);
	}
	if((start+sb == sa) && (a[sa-1]<b[sb-1])){
		printf("\n Not possible in lsub, debug print!");
		printf("\nAlt value sa=%d sb=%d start=%d",sa,sb,start);
		printf("a<b, only natural subtraction");
		exit(1);
	}
	for(int  i = start; i< sb + start; i++){
		if(a[i]>=b[i-start]) a[i]-=b[i-start]-'0';
		else{
			a[i]= 10-(b[i-start]-a[i])+'0';
			int j;
			for(j=1; a[i+j]==0;j++) a[i+j] = '9';
			a[i+j]--;
		}
	}
	return;
}

void div_char(char *a,int *sa,char *b,int sb, char *q, int sq){
	if(a==NULL || b ==NULL){
		printf("\n Not possible in myldiv, debug print!");
		printf("\nAlt value sa=%d sb=%d sq=%d",*sa,sb,sq);
		exit(1);
	}	
	if(*sa<sb) return;
	if(((*sa)==sb) && (compare_char(a,*sa,b,sb)<0)) return;
	if(((*sa)==sb) && (compare_char(a,*sa,b,sb)==0)){
		a[0]='0';
		*sa=1;
		inc_char(q,sq,0);
		return;
	} 
	if((*sa>=sb+1)){
		
		while(*sa>=sb+1){
		    sub_char(a,*sa,b,sb,(*sa)-sb-1);
		    inc_char(q,sq,(*sa)-sb-1);
		    resize_char(a,sa);	    
		}    
		div_char(a,sa,b,sb,q,sq);
	}
	else{
		//here *sa==sb
		while(compare_char(a,*sa,b,sb)>=0){
			sub_char(a,*sa,b,sb,0);
			inc_char(q,sq,0);
			resize_char(a,sa);
			}	
		}
	return;
}
 
void reverse(char *x, int begin, int end){
   char c;
 
   if (begin >= end)
      return;
 
   c          = *(x+begin);
   *(x+begin) = *(x+end);
   *(x+end)   = c;
 
   reverse(x, ++begin, --end);
}

char **divide_strings(char *a, char *b) {
  
  char **result = malloc(2 * sizeof(char *));

  int d1s = (strlen(a) + 1);
  char * d1 = malloc(d1s * sizeof(char));
  for(int i=0;i<d1s-1;i++) d1[i]=a[d1s-2-i];
  d1[d1s-1]= '\0';
  
  int d2s = (strlen(b) + 1);
  char * d2 = malloc(d2s * sizeof(char));
  for(int i=0;i<d2s-1;i++) d2[i]=b[d2s-2-i];
  d2[d2s-1]= '\0';
  
  int dqs=(strlen(a) + 1);
  char *quotient = malloc(dqs * sizeof(char));
  for(int i=0;i<dqs-1;i++) quotient[i]='0';
  quotient[strlen(a)]= '\0';
  
  char *remainder;
  
  d1s--;d2s--;dqs--;
  div_char(d1,&d1s,d2,d2s,quotient,dqs);
  resize_char(d1,&d1s);
  resize_char(quotient,&dqs);
  d1s++;dqs++;
  
  remainder = malloc(d1s * sizeof(char));
  for(int i=0;i<d1s-1;i++) remainder[i]=d1[d1s-2-i];
  remainder[d1s-1]= '\0';
  
  char temp;

  quotient[dqs-1]= '\0';
  reverse(quotient,0,dqs-2);
  
  *result = malloc((strlen(quotient) + 1) * sizeof(char));
  *(result + 1) = malloc((strlen(remainder) + 1) * sizeof(char));
  for (int i = 0; i < strlen(quotient); i++) *(*result + i) = quotient[i];
  for (int i = 0; i < strlen(remainder); i++) *(*(result + 1) + i) = remainder[i];
  *(*result + strlen(quotient)) = *(*(result + 1) + strlen(remainder)) = '\0';
  
  free(d2);
  free(d1);
  free(quotient);
  free(remainder);
  return result;
}
