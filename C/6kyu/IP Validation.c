/*
Write an algorithm that will identify valid IPv4 addresses in dot-decimal format. 
IPs should be considered valid if they consist of four octets, with values between 0 and 255, inclusive.
Valid inputs examples:

Examples of valid inputs:
1.2.3.4
123.45.67.89

Invalid input examples:
1.2.3
1.2.3.4.5
123.456.78.90
123.045.067.089

Notes:

    Leading zeros (e.g. 01.02.03.04) are considered invalid
    Inputs are guaranteed to be a single string
*/

#include <string.h>
#include <stdio.h>

int is_valid_ip(const char * addr) {
  unsigned int l = strlen(addr);
  unsigned int j = 0;
  int id = 0;
  
  //preprocessing
  if(l>16 || l<8) return 0;
  if(addr[0]>'9' || addr[0]<'0') return 0;
  
  //
  for(int i = 0; i<4;i++){ //4 numbers
    id = 0;
    for(;j<l && addr[j]!='.';j++){
      if(addr[j]>'9' || addr[j]<'0') return 0;
      id = 10 * id + (addr[j] - '0');
      if(id > 255) return 0;
    }
    //non-zero starting with a zero
    if(id/10>0 && j>3 && addr[j-3]=='0') return 0;
    if(id/10==0 && id>0 && j>2 && addr[j-2]=='0') return 0;
    //addr[j] == '.'
    j++;
    //not enough numbers
    if(j>=l+1 && i<3) return 0;
  }
  if(j<l+1) return 0;
  return 1;  
}

/*
//Clever solution proposed by other kata user
#include <string.h>
#include <stdio.h>

int is_valid_ip(const char * addr) {
  unsigned char a,b,c,d;
  char test[30];
  sscanf(addr, "%hhu.%hhu.%hhu.%hhu", &a, &b, &c, &d);
  snprintf(test, 30, "%hhu.%hhu.%hhu.%hhu", a, b, c, d);
  return strcmp(addr, test)==0;
};
*/
