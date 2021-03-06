/*library of built-in functions for programs generated by lispccompiler.lisp*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// arithmetic: int int
void inline plus_int_int(const int a, const int b, int* r) {
	*r = a+b;
}
void inline multiply_int_int(const int a, const int b, int* r) {
	*r = a*b;
}
void inline minus_int_int(const int a, const int b, int* r) {
	*r = a-b;
}
void inline divide_int_int(const int a, const int b, int* r) {
	*r = a/b;
}
void inline max_int_int(const int a, const int b, int* r) {
	if (a >= b) { 
		*r = a;
	} else {
		*r = b;
	}
}
void inline min_int_int(const int a, const int b, int* r) {
	if (a >= b) { 
		*r = b;
	} else {
		*r = a;
	}
}
// arithmetic: float float
void inline plus_float_float(const float a, const float b, float* r) {
	*r = a+b;
}
void inline multiply_float_float(const float a, const float b, float* r) {
	*r = a*b;
}
void inline minus_float_float(const float a, const float b, float* r) {
	*r = a-b;
}
void inline divide_float_float(const float a, const float b, float* r) {
	*r = a/b;
}
void inline max_float_float(const float a, const float b, float* r) {
	if (a >= b) { 
		*r = a;
	} else {
		*r = b;
	}
}
void inline min_float_float(const float a, const float b, float* r) {
	if (a >= b) { 
		*r = b;
	} else {
		*r = a;
	}
}
// arithmetic: float int
void inline plus_float_int(const float a, const int b, float* r) {
	*r = a+b;
}
void inline multiply_float_int(const float a, const int b, float* r) {
	*r = a*b;
}
void inline minus_float_int(const float a, const int b, float* r) {
	*r = a-b;
}
void inline divide_float_int(const float a, const int b, float* r) {
	*r = a/b;
}
void inline max_float_int(const float a, const int b, float* r) {
	if (a >= b) { 
		*r = a;
	} else {
		*r = b;
	}
}
void inline min_float_int(const float a, const int b, float* r) {
	if (a >= b) { 
		*r = b;
	} else {
		*r = a;
	}
}
// arithmetic: int float
void inline plus_int_float(const int a, const float b, float* r) {
	*r = a+b;
}
void inline multiply_int_float(const int a, const float b, float* r) {
	*r = a*b;
}
void inline minus_int_float(const int a, const float b, float* r) {
	*r = a-b;
}
void inline divide_int_float(const int a, const float b, float* r) {
	*r = a/b;
}
void inline max_int_float(const int a, const float b, float* r) {
	if (a >= b) { 
		*r = a;
	} else {
		*r = b;
	}
}
void inline min_int_float(const int a, const float b, float* r) {
	if (a >= b) { 
		*r = b;
	} else {
		*r = a;
	}
}
// arithmetic: int
void inline plusone_int (const int a, int* r) {
	*r = a+1;
}
void inline minusone_int (const int a, int* r) {
	*r = a-1;
}
void inline abs_int(const int a, int* r) {
	if (a < 0) {
		*r = -a;
	} else {
		*r = a;
	}
}
void inline signum_int(const int a, int* r) {
	if (a > 0) {
		*r = 1;
	} else if (a < 0) {
		*r = -1;
	} else {
		*r = 0;
	}
}
// arithmetic: float
void inline plusone_float (const float a, float* r) {
	*r = a+1;
}
void inline minusone_float (const float a, float* r) {
	*r = a-1;
}
void inline abs_float(const float a, float* r) {
	*r = fabs(a);
}
void inline signum_float(const float a, float* r) {
	if (a > 0) {
		*r = 1;
	} else if (a < 0) {
		*r = -1;
	} else {
		*r = 0;
	}
}
// misc
void inline print_int(const int a, int* r) {
	printf("%i",a);
	*r = a;
}
void inline eq_int_int(const int a, const int b, int* r) {
	*r = a==b;
}
// comparison: int int
void inline less_int_int(const int a, const int b, int* r) {
	*r = a<b;
}
void inline lessequal_int_int(const int a, const int b, int* r) {
	*r = a<=b;
}
void inline greater_int_int(const int a, const int b, int* r) {
	*r = a>b;
}
void inline greaterequal_int_int(const int a, const int b, int* r) {
	*r = a>=b;
}
void inline equal_int_int(const int a, const int b, int* r) {
	*r = a==b;
}
void inline notequal_int_int(const int a, const int b, int* r) {
	*r = a!=b;
}
// comparison: float float
void inline less_float_float(const float a, const float b, int* r) {
	*r = a<b;
}
void inline lessequal_float_float(const float a, const float b, int* r) {
	*r = a<=b;
}
void inline greater_float_float(const float a, const float b, int* r) {
	*r = a>b;
}
void inline greaterequal_float_float(const float a, const float b, int* r) {
	*r = a>=b;
}
void inline equal_float_float(const float a, const float b, int* r) {
	*r = a==b;
}
void inline notequal_float_float(const float a, const float b, int* r) {
	*r = a!=b;
}
// comparison: float int
void inline less_float_int(const float a, const int b, int* r) {
	*r = a<b;
}
void inline lessequal_float_int(const float a, const int b, int* r) {
	*r = a<=b;
}
void inline greater_float_int(const float a, const int b, int* r) {
	*r = a>b;
}
void inline greaterequal_float_int(const float a, const int b, int* r) {
	*r = a>=b;
}
void inline equal_float_int(const float a, const int b, int* r) {
	*r = a==b;
}
void inline notequal_float_int(const float a, const int b, int* r) {
	*r = a!=b;
}
// comparison: int float
void inline less_int_float(const int a, const float b, int* r) {
	*r = a<b;
}
void inline lessequal_int_float(const int a, const float b, int* r) {
	*r = a<=b;
}
void inline greater_int_float(const int a, const float b, int* r) {
	*r = a>b;
}
void inline greaterequal_int_float(const int a, const float b, int* r) {
	*r = a>=b;
}
void inline equal_int_float(const int a, const float b, int* r) {
	*r = a==b;
}
void inline notequal_int_float(const int a, const float b, int* r) {
	*r = a!=b;
}
// logical
void inline and_f_int_int(const int a, const int b, int* r) {
	*r = a && b;
}
void inline or_f_int_int(const int a, const int b, int* r) {
	*r = a || b;
}
void inline not_int(const int a, int* r) {
	*r = !a;
}
void fail_with_message_str(const char* message) {
	fprintf(stderr, "%s\n", message);
	exit(1);
}
// type conversion
void inline float_int (const int a, float *r) {
	*r = (float)a;
}
void inline floor_float(const float a, int* i, float* r) {
	float j = floorf(a);
	*i = (int)j;
	*r = a - j;
}
