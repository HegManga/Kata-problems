/*
In this finite version of Conway's Game of Life (here is an excerpt of the rules) ...

The universe of the Game of Life is an infinite two-dimensional orthogonal grid of square cells, 
each of which is in one of two possible states, alive or dead. Every cell interacts with its eight neighbours, 
which are the cells that are horizontally, vertically, or diagonally adjacent. 
At each step in time, the following transitions occur:

    Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any live cell with more than three live neighbours dies, as if by overcrowding.
    Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

The initial pattern constitutes the seed of the system. 
The first generation is created by applying the above rules simultaneously to every cell in the seed—births and deaths occur simultaneously, 
and the discrete moment at which this happens is sometimes called a tick (in other words, each generation is a pure function of the preceding one)

...implement your own method which will take the initial state as an NxM array of 0's (dead cell) and 1's (living cell) and
return an equally sized array representing the next generation. 
Cells outside the array must be considered dead. 
Cells that would born out of the array boundaries should be ignored (universe never grows beyond the initial NxM grid).
N.B.: for illustration purposes, 0 and 1 will be represented as ░ and ▓ blocks (PHP: basic black and white squares) respectively.
You can take advantage of the 'htmlize' function to get a text representation of the universe:
eg:

console.log(htmlize(cells));

*/

#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>

unsigned int neighbours_num(size_t rows, size_t cols, const bool cur_gen[rows][cols], size_t i, size_t j){
  unsigned int ret = 0;
  cols--;
  rows--;
  if(i>0 && j>0 && cur_gen[i-1][j-1]==1 ) ret++;
  if(i>0 && cur_gen[i-1][j]==1) ret++;
  if(i>0 && j<cols && cur_gen[i-1][j+1]==1) ret++;
  if(j>0 && cur_gen[i][j-1]==1) ret++;
  if(j<cols && cur_gen[i][j+1]==1) ret++;
  if(i<rows && j>0 && cur_gen[i+1][j-1]==1) ret++;
  if(i<rows && cur_gen[i+1][j]==1) ret++;
  if(i<rows && j<cols && cur_gen[i+1][j+1]==1) ret++;
  
  return ret;
}

void evolve (size_t rows, size_t cols, const bool cur_gen[rows][cols], bool next_gen[rows][cols]){

  int n;
	for(size_t i=0;i<rows;i++){
    printf("\n");
    for(size_t j=0;j<cols;j++){]
      n = neighbours_num(rows,cols,cur_gen,i,j);
      printf("%d ",n);
      switch (n){
          case 2:  if(cur_gen[i][j]==1) next_gen[i][j] = 1;
                   break; 
          case 3:  next_gen[i][j = 1;
                   break;
          default: next_gen[i][j] = 0;
                   break;   
      } 
    }
  }
	return;
}
