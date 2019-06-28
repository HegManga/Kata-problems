/*
You have a positive number n consisting of digits. You can do at most one operation: Choosing the index of a digit in the number, remove this digit at that index and insert it back to another or at the same place in the number in order to find the smallest number you can get.

#Task: Return an array or a tuple or a string depending on the language (see "Sample Tests") with

    1) the smallest number you got
    2) the index i of the digit d you took, i as small as possible
    3) the index j (as small as possible) where you insert this digit d to have the smallest number.

Example:

smallest(261235) --> [126235, 2, 0] or (126235, 2, 0) or "126235, 2, 0"

126235 is the smallest number gotten by taking 1 at index 2 and putting it at index 0

smallest(209917) --> [29917, 0, 1] or ...

[29917, 1, 0] could be a solution too but index `i` in [29917, 1, 0] is greater than 
index `i` in [29917, 0, 1].

29917 is the smallest number gotten by taking 2 at index 0 and putting it at index 1 which gave 029917 which is the number 29917.

smallest(1000000) --> [1, 0, 6] or ...
*/
/*ACHTUNG!!!!!
  rename file in "ToSmallest.java" /*
import java.util.*;

class ToSmallestVector{
    private Vector<Integer> v;
    final private int length_n;
    private int i;
    private int j;
    
    ToSmallestVector(long n){
        length_n = (int) (Math.log10(n) + 1);
        v = new Vector<Integer>();
        long digit;
        
        for(int i=0 ; i<length_n; i++){
            digit = n%10;
            v.add(0,(int) digit);
            n /= 10;
        }   
    }
    
    private long toLongaux(int i){
        //convert this in long from index i
        long ret = 0; 
        for( ; i<length_n; i++){
            ret += (long) v.get((int) i);
            ret *= 10;
        }
        ret /= 10;
        return ret;
    }
    
    public long toLong(){
        
        return toLongaux(0);
        
    }
    
    private int my_min_right(int i){
        //min from i to end, the """last""" (condition 3 (or 2,boh) of problem) in v 
        int ret = -1; // 0<= ret <=9 digits={0,1,2,3,4,5,6,7,8,9}
        int j = i;
        for(; i<length_n; ++i)
            if((ret== -1) || v.get(i) <= v.get(ret)) ret = i;
        for(i=ret; i>j; i--){
            if(v.get(ret).equals(v.get(i))) ret = i;
            else break;
        }    
        return ret;
    }
    
    private int my_min(int i){
        //min from i to end, the first min in v
        int ret = 10; // 0<= ret <=9 digits={0,1,2,3,4,5,6,7,8,9}
        
        for(; i<length_n; ++i)
            if((ret== 10) || v.get(i) < v.get(ret)) ret = i;
        return ret;
    }
    
    private long[] ToSmallestaux(int start){
        long ret[] = new long[3];
        if(start >= length_n){ //case: alredy best 
          
          return ret;
        }
        int index_min = my_min(start);
        if(index_min == start){
            ret=ToSmallestaux(start+1);
            if((ret[2] == start+1) && (v.get(start)).equals(v.get(start+1))) ret[2] = start;
            
            return ret;
        }
        if(index_min > start+1){
            index_min = my_min_right(start);
            int t = v.get(index_min);
            v.remove(index_min);
            v.add(start,t);
            ret[0] = toLong();
            ret[1] = index_min;
            ret[2] = start;  
            return ret;
        }
        //index_min == start+1 && array of two elements 
        if(index_min == length_n){
            int t = v.get(index_min);
            v.remove( index_min);
            v.add(start,t);
            ret[0] = toLong();
            ret[1] = index_min;
            ret[2] = start; 
            return ret;
        }
        //index_min == start+1
        if(v.get(start) <= v.get(start+2)){
            int old_index_min = index_min;
            index_min = my_min_right(start);
            int t = v.get(index_min);
            v.remove(index_min);
            v.add(start,t);
            if(old_index_min == index_min){
                ret[0] = toLong();
                ret[1] = start;
                ret[2] = index_min;  
            }
            else{
                ret[0] = toLong();
                ret[1] = index_min;
                ret[2] = start;  
            }
            return ret;
        }
        //
        int f = v.get(start);
        int c;
        for(c=start+2; c<length_n; c++){
            if( f < v.get(c)) break;
        }
        while(v.get(start) == v.get(c-1)) c--;
        int t = v.get(start);
        v.remove(start);
        v.add(c-1,t);
        ret[0] = toLong();
        ret[1] = start;
        ret[2] = c-1; 
        
        return ret;
    }
    
    public long[] ToSmallest(){
        long ret[];
        ret = ToSmallestaux(0);
        return ret;
    }
}


public class ToSmallest {
    
    public static long[] smallest(long n) {
        int length_n = (int) (Math.log10(n) + 1);
        long ret[] = new long[3];
        ToSmallestVector v = new ToSmallestVector(n);
        ret = v.ToSmallest();
    return ret;    
    }
}
