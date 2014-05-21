#include "BuiltinsIO.h"

int potencia(int a, int b){
	if(b>=0){
		int c = b;
		int r = 1;
		while(c>0){
			r *= a;
			c--;
		}
		return r;
	}else{
		return -1;
	}
}

int factorial(int a){
	if(a>=0){
		int fact = 1;
		for(int i=2;i<=a;i++){
			fact *=i;
		}
		return fact;
	}else{
		return -1;
	}
}

int fibonacci(int a){
	if(a<0) return -1;
	if(a==0 || a==1) return 1;
	int fibo1 = 1;
	int fibo2 = 1;
	int auxf;
	for(int i=2; i<a; i++){
		auxf = fibo1 + fibo2;
		fibo1 = fibo2;
		fibo2 = auxf;
	}
	return fibo1 + fibo2;
}

int introduceEntero(){
	return ConsoleIn_Int();
}

int introduceEntero(int a){
	ConsoleOut_Int(a);
	return 0;
}