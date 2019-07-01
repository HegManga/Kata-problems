/*
Given a positive number n > 1 find the prime factor decomposition of n. The result will be a string with the following form :

 "(p1**n1)(p2**n2)...(pk**nk)"

with the p(i) in increasing order and n(i) empty if n(i) is 1.

Example: n = 86240 should return "(2**5)(5)(7**2)(11)"
*/

//ACHTUNG!!!!!!!!!!! Rename file! 

import java.util.*;
import java.lang.*;

class MyMath{
    public static boolean myisint(double x) {  
        return Math.floor(x) == x; 
    }
    public static long powerN(long number, long power) {
        return (long) Math.pow(number, power);
        }
    }

class MyFactorPair implements Cloneable{
    private int exponent; // >0 
    private long factor; // >0 "integer"
    
    MyFactorPair(){
        exponent = 1;
        factor = 1;
        return;
    }

    MyFactorPair(int e, long f){
    if(e < 1 || f < 1 || !(MyMath.myisint(f))){
      exponent = 1;
      factor = 1;
      return;
    }
    exponent = e;
    factor = f;
        return;
    }
    
    public boolean isOne(){
        if(factor == 1) return true;
        return false;
    }       
    
    private void One(){
        exponent = 1;
        factor = 1;
        return;
    }
    
    public void add(MyFactorPair x){
        if(x.isOne()) return;
        if(isOne()){
            exponent = x.get_exp();
            factor = x.get_fact();
            return;
            }
        if((x.get_fact()) == factor){ 
           exponent +=  x.get_exp();
        }
        return;
    }
    
    public void remove(MyFactorPair x){
        if(x.isOne()) return;
        if((x.get_fact()) == factor ){ 
            exponent -=  x.get_exp();
            //if(exponent < 0) 
            if(exponent == 0) One();
        }
        return;
    } 
    
    public int get_exp(){ return exponent;}
    public long get_fact(){ return factor;}
    
    @Override
    public Object clone(){
    try{
            return (MyFactorPair) super.clone();
            }
    catch(CloneNotSupportedException e){return null;} 
  }
    @Override
    public String toString() { 
         if(exponent == 1) return "("+factor + ")";
         else return "("+factor +"**"+exponent+ ")";
    }     
}

class FactorVector implements Cloneable{
    private Vector<MyFactorPair> v;
    private long num;
    
    FactorVector(){
        v = new Vector<MyFactorPair>();
        v.add(new MyFactorPair());
        num = (long) 1;
    }
    
    FactorVector(long n){
        v = new Vector<MyFactorPair>();
        MyFactorPair temp;
        
        v.add(new MyFactorPair());
        num = 1;
        if(n == 1) return;
        
        long f = 2;
        int  e = 0;
        while(n%2 == 0) {
            n /=  2;
            e++; 
        }
        
        temp = new MyFactorPair(e,f);
        add(temp);
        
        for (int i = 3; i <= n; i++) {
            if(n % i ==  0){
                e = 0;
                f = i;
                do{
                    n /= (long) i;
                    e++;
                }while (n % i == 0); 
                temp = new MyFactorPair(e,f); 
                add(temp);
            }
        }
        return;
    }
    
    public boolean isIn_factor(long fact){
        for(MyFactorPair p: v) 
            if(p.get_fact() == fact) return true;
        return false;    
    }
    
    private int indexOf_factor(long fact){

        for(MyFactorPair p: v) 
            if(p.get_fact() == fact) return v.indexOf(p);
        return -1;
    }
    
    public MyFactorPair get(long fact){
        if(isIn_factor(fact)) return v.get(indexOf_factor(fact));
        return null;
    }
    
    public MyFactorPair get_i(int index){
        if(index>=0 && index < v.size()) return v.get(index);
        return null;
    }
    
    public void add(MyFactorPair x){
        long fact = x.get_fact();
        int exp = x.get_exp();
        if(fact == 1) return;
        if(isIn_factor(fact)){
            get(fact).add(x);
            num *= MyMath.powerN(fact,exp);
        }
        else{
            if(num == 1){
                get(1).add(x);
                num *= MyMath.powerN(fact,exp);
            }
            else{
                int size = v.size();
                int i;
                
                for(i=0; i<size && (v.get(i).get_fact() <= fact);i++);
                v.add(i,x);
                num *= MyMath.powerN(fact,exp);
            }    
        }    
        return;    
    }
    
    public void remove(MyFactorPair x){
        long fact = x.get_fact();
        int exp = x.get_exp();
        if(fact == 1) return;
        if(isIn_factor(fact)){
            get(fact).remove(x);
            num /= MyMath.powerN(fact,exp); //Possible error if exp x > ....
        if(v.size() > 1 && isIn_factor(1)) v.remove(indexOf_factor(1));  
            
        }
        return;    
    }
    
    public long get_long(){return num;}
    
    
    @Override
    public Object clone(){//copia deep
    
        FactorVector ret = new FactorVector(num);
        int size = v.size();
        for(int i =0; i<size;i++)
            ret.v.add((MyFactorPair) v.get(i).clone());
        return ret;
     
  }
    @Override
    public String toString(){
        String ret = "";
        for(int i =0 ; i<v.size(); i++){
            ret += v.get(i).toString();
        }
        return ret;
    }
    
    private boolean isEmpty(){
        return v.isEmpty();
    }

}

public class PrimeDecomp {
   
    public static String factors(int n) {
        FactorVector ret = new FactorVector(n);
        return ret.toString();
    }
       
}
