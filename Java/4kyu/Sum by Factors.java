/*
Given an array of positive or negative integers

I= [i1,..,in]

you have to produce a sorted array P of the form

[ [p, sum of all ij of I for which p is a prime factor (p positive) of ij] ...]

P will be sorted by increasing order of the prime numbers. The final result has to be given as a string in Java, C#, C, C++ and as an array of arrays in other languages.

Example:

I = {12, 15}; // result = "(2 12)(3 27)(5 15)"

[2, 3, 5] is the list of all prime factors of the elements of I, hence the result.

Notes:

    It can happen that a sum is 0 if some numbers are negative!

Example: I = [15, 30, -45] 5 divides 15, 30 and (-45) so 5 appears in the result, the sum of the numbers for which 5 is a factor is 0 so we have [5, 0] in the result amongst others.

    In Fortran - as in any other language - the returned string is not permitted to contain any redundant trailing whitespace: you can use dynamically allocated character strings.
*/

//ACHTUNG!!!! Rename file!
import java.util.*;

class MyPair{
  int p;
  int i;
  
  MyPair(int x,int y){
  p = x;
  i = y;
  }
  
  public String toString(){
    return "("+p+" "+i+")";
  }

}

public class SumOfDivided {
  private static boolean isones(int[] l){
      for(int i = 0; i<l.length; i++) if(l[i]!=1 && l[i]!=-1) return false;
      return true;
  }

  public static String sumOfDivided(int[] l) {
      Vector<MyPair> v = new Vector<MyPair>();
      String ret = "";
      int[] c = new int[l.length];
      for(int i = 0; i<l.length; i++) c[i]=l[i];
      
      // j = 2k
      int j = 2; 
      int temp = 0;
      int pass = 0;
      for(int i = 0; i<l.length; i++){
          if(c[i]%j == 0){
              pass = 1;
              temp += l[i];
              while(c[i]%j == 0) c[i] /= j;
          }
          if(i == l.length -1 && pass ==1){
              v.add(new MyPair(j,temp));
          }
      }
      
      if(isones(c)){
        for(MyPair x: v) ret += x.toString();
        return ret;
      }
      //j = 2k+1
      temp = 0;
      j = 3;
      pass = 0;
      while(!isones(c)){
          for(int i = 0; i<l.length; i++){
              if(c[i]%j == 0){
                  pass = 1;
                  temp += l[i];
                  c[i] /= j;
                  while(c[i]%j == 0) c[i] /= j;
              }
          }
          if(pass == 1) v.add(new MyPair(j,temp));
          pass = 0;
          j +=2;
          temp = 0; 
      }    
      for(MyPair x: v) ret += x.toString();
      return ret;
      
  }
}
