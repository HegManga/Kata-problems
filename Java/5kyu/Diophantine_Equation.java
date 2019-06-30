/*
In mathematics, a Diophantine equation is a polynomial equation, usually with two or more unknowns, such that only the integer solutions are sought or studied.

In this kata we want to find all integers x, y (x >= 0, y >= 0) solutions of a diophantine equation of the form:
x2 - 4 * y2 = n

(where the unknowns are x and y, and n is a given positive number) in decreasing order of the positive xi.

If there is no solution return [] or "[]" or "". (See "RUN SAMPLE TESTS" for examples of returns).
Examples:

solEquaStr(90005) --> "[[45003, 22501], [9003, 4499], [981, 467], [309, 37]]"
solEquaStr(90002) --> "[]"

Hint:

x2 - 4 * y2 = (x - 2*y) * (x + 2*y)
*/

//ACHTUNG rename file in Dioph.java !!!!!!!!!!!!!!!!!!!!!!
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

class DPoint{
    private Long x;
    private Long y;
    
    DPoint(Long a,Long b){
        x = a;
        y = b;
        return;
    }
    
    public Long get_x(){return x;}
    
    public Long get_y(){return y;}
    
    public String toString(){
        return "["+x+", "+y+"]";
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
         return factor + "^" + exponent; 
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
        return "Fact("+num+") = "+v.toString();
    }
    
    private boolean isEmpty(){
        return v.isEmpty();
    }
    
    public void split_next(FactorVector w){
        MyFactorPair p1,p2;
        p1 = get_i(0);
        p2 = w.get_i( 0);  
        long f1 = p1.get_fact();
        long f2 = p2.get_fact(); 
        MyFactorPair temp,temp1,ptemp;
        
        if(f1 <= f2){ 
            temp = new MyFactorPair(1,f1);
            remove(temp);
            w.add(temp);
        }
        else if(f2 == 1){ 
			temp = new MyFactorPair(1,f1);
            remove(temp);
            w.add(temp);
		}
        else{ 
            temp = new MyFactorPair(1,f1);   
            int wsize = w.v.size();
            long f3 = 1;
            FactorVector fvtemp;
            
            for(int i = wsize-1; i>=0; i--){		
				ptemp = w.get_i(i);
				if(ptemp.get_fact() < f1){ 
					f3  *= MyMath.powerN(ptemp.get_fact(),ptemp.get_exp()); 	
				}
			}
			fvtemp = new FactorVector(f3);
			while(!(fvtemp.isEmpty()) && !(fvtemp.get_i(0).isOne())){ 
				ptemp=fvtemp.get_i(fvtemp.v.size()-1);	
				temp1 = new MyFactorPair(ptemp.get_exp(),ptemp.get_fact());
                add(temp1); 
				w.remove(temp1);
				fvtemp.remove(temp1);
			}
            w.add(temp); 
            remove(temp);
        }
        return;
    }
}


public class Dioph {
    //solves x^2-4*y^2=n
    
	public static String solEquaStr(long n) { System.out.println(n);
        Vector<DPoint> solEqua = new Vector<DPoint>();
    		FactorVector fv = new FactorVector(n);
    		FactorVector f1 = new FactorVector();
    		FactorVector f2 = new FactorVector(n);
        long n1 = f1.get_long(), n2 = f2.get_long();
        double x,y;
     
        do{ 
            n1 = f1.get_long();
            n2 = f2.get_long();
            x = (double) (n1+n2)/2; 
            y = (double)(n2-n1)/4; 
            if((x >= 0 && y>=0) && MyMath.myisint(x) && MyMath.myisint(y)){ 
                if(!(solEqua.isEmpty())){
                    int i;
                    int sizeE = solEqua.size();
                    for(i = 0; i<sizeE && solEqua.get(i).get_x()>=x; i++); 
                    if(i<sizeE && solEqua.get(i).get_x()<x){ 
                        solEqua.add(i,new DPoint((long)x,(long)y));
                        }
                    else if(i>=sizeE){
                        solEqua.add(i,new DPoint((long)x,(long)y)); 
                        }
                }
                else solEqua.add(new DPoint((long)x,(long)y));
            }    
            f2.split_next(f1);  
        } while(n1 < n);
    return solEqua.toString();
	}
}

/* 
Clever method but not object oriented

import java.util.*;

public class Dioph {
	
	public static String solEquaStr(long n) {
		StringJoiner sj = new StringJoiner(", ", "[", "]");

        System.out.println(Math.sqrt(n));
        for (long i = 1; i <= Math.sqrt(n) ; i++)
            if (n % i == 0) {
                long j = n / i;
                if ((i % 2 == j % 2) && ((j - i) % 4 == 0)) {
                    long x = (j + i) / 2;
                    long y = (j - i) / 4;
                    sj.add(String.format("[%d, %d]", x, y));
                }
            }

        return sj.toString();
	}
	
}
*/
