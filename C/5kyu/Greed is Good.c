/*
Greed is a dice game played with five six-sided dice. Your mission, should you choose to accept it, is to score a throw according to these rules. You will always be given an array with five six-sided dice values.

 Three 1's => 1000 points
 Three 6's =>  600 points
 Three 5's =>  500 points
 Three 4's =>  400 points
 Three 3's =>  300 points
 Three 2's =>  200 points
 One   1   =>  100 points
 One   5   =>   50 point

A single die can only be counted once in each roll. For example, a "5" can only count as part of a triplet (contributing to the 500 points) or as a single 50 points, but not both in the same roll.

Example scoring

 Throw       Score
 ---------   ------------------
 5 1 3 4 1   50 + 2 * 100 = 250
 1 1 1 3 1   1000 + 100 = 1100
 2 4 4 5 4   400 + 50 = 450

In some languages, it is possible to mutate the input to the function. This is something that you should never do. If you mutate the input, you will not be able to pass all the tests.

*/
int tpoints(int x){
  switch(x){
   	  case 1:
   	    		return 1000;
   	  case 6:
   	    		return 600;
      case 5:
   	    		return 500;
      case 4:
   	    		return 400;
      case 3:
   	    		return 300;
      case 2:
   	    		return 200;
	    default:
	     		return 0;
	    }
   return 0;   
}

int opoints(int x){
  switch(x){
   	  case 1:
   	    		return 100;
   	  case 5:
   	    		return 50;
	    default:
	     		return 0;
	    }
   return 0;   
}
int score(const int dice[5]) {
    int ret=0;
    int face[6];
    for(int i = 0; i<6; i++) face[i]=0;
    for(int i = 0; i<5; i++) face[dice[i]-1]++;
    for(int i = 0; i<6; i++){
      if(face[i]>=3){
        face[i]-=3;
        ret+=tpoints(i+1);
        i--;
      }
      else if(face[i]>0){
        face[i]--;
        ret+=opoints(i+1);
        i--;
      }
    }
    return ret;
}
