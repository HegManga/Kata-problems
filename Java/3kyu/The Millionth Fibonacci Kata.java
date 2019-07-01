/*
The year is 1214. One night, Pope Innocent III awakens to find the the archangel Gabriel floating before him. Gabriel thunders to the pope:

    Gather all of the learned men in Pisa, especially Leonardo Fibonacci. In order for the crusades in the holy lands to be successful, these men must calculate the millionth number in Fibonacci's recurrence. Fail to do this, and your armies will never reclaim the holy land. It is His will.

The angel then vanishes in an explosion of white light.

Pope Innocent III sits in his bed in awe. How much is a million? he thinks to himself. He never was very good at math.

He tries writing the number down, but because everyone in Europe is still using Roman numerals at this moment in history, he cannot represent this number. If he only knew about the invention of zero, it might make this sort of thing easier.

He decides to go back to bed. He consoles himself, The Lord would never challenge me thus; this must have been some deceit by the devil. A pretty horrendous nightmare, to be sure.

Pope Innocent III's armies would go on to conquer Constantinople (now Istanbul), but they would never reclaim the holy land as he desired.

In this kata you will have to calculate fib(n) where:

fib(0) := 0
fib(1) := 1
fin(n + 2) := fib(n + 1) + fib(n)

Write an algorithm that can handle n up to 2000000.

Your algorithm must output the exact integer answer, to full precision. Also, it must correctly handle negative numbers as input.

HINT I: Can you rearrange the equation fib(n + 2) = fib(n + 1) + fib(n) to find fib(n) if you already know fib(n + 1) and fib(n + 2)? Use this to reason what value fib has to have for negative values.

HINT II: See https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2.4

*/

//ACHTUNG!!!!! Rename file!

import java.math.BigInteger;
import java.util.*;

class My2x2Matrix{
  BigInteger a;
  BigInteger b;
  BigInteger c;
  BigInteger d;
  
  My2x2Matrix(BigInteger a1,BigInteger b1,BigInteger c1,BigInteger d1){
      a = a1;
      b = b1; 
      c = c1;
      d = d1;
  }
  
  public BigInteger get_a(){ return a;}
  public BigInteger get_b(){ return b;}
  public BigInteger get_c(){ return c;}
  public BigInteger get_d(){ return d;}
  
  private void mul(My2x2Matrix m){
      BigInteger at = a;
      BigInteger bt = b;
      BigInteger ct = c;
      BigInteger dt = d;
      a = at.multiply(m.a).add(bt.multiply(m.c));
      b = at.multiply(m.b).add(bt.multiply(m.d));
      c = ct.multiply(m.a).add(dt.multiply(m.c));
      d = ct.multiply(m.b).add(dt.multiply(m.d));
  }
  
  private void power2(){
      BigInteger at = a;
      BigInteger bt = b;
      BigInteger ct = c;
      BigInteger dt = d;
      a = at.multiply(at).add(bt.multiply(ct));
      b = bt.multiply(at.add(dt));
      c = ct.multiply(at.add(dt));
      d = ct.multiply(bt).add(dt.multiply(dt));
      return;
  }
  
  public void powern(BigInteger n){
      if(n.equals(new BigInteger("1"))) return;
   
      if(n.equals(new BigInteger("2"))){ 
          power2();
          return;
      }
      if(n.remainder(new BigInteger("2")).equals(new BigInteger("1"))){ 
          powern( n.subtract(new BigInteger("1")).divide(new BigInteger("2"))); 
          power2(); 
          mul(new My2x2Matrix(new BigInteger("1"),new BigInteger("1"),new BigInteger("1"),new BigInteger("0")));
      }
      else{
          powern(n.divide(new BigInteger("2"))); 
          power2();
      }    
      return;
    }
  public String toString(){
      return "| "+a+ " " +b+" |\n"+"| "+c+ " " +d+" |";
  }  
}

public class Fibonacci {

  public static BigInteger fib(BigInteger n) {
      int v = n.intValueExact();
      BigInteger i = n.abs();
      if(n.equals(new BigInteger("0"))) return new BigInteger("0");
      
      if(n.equals(new BigInteger("1"))) return v < 0 ? new BigInteger("-1") : new BigInteger("1");
      My2x2Matrix m = new My2x2Matrix(new BigInteger("1"),new BigInteger("1"),new BigInteger("1"),new BigInteger("0"));
      m.powern(i.subtract(new BigInteger("1")));
      return  v < 0 && (n.remainder(new BigInteger("2")).equals(new BigInteger("0")))? m.get_a().negate() : m.get_a();
  }
}  
