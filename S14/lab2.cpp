#define __MINMAX_DEFINED
#include <list>
#include <iostream>
#include <stdlib.h>
#include <vector>
#include <algorithm>
#include <iterator>

using namespace std;


/* STL Tutorial
Exercise: 4.1.2

Write a STL program that takes an arbitrary sequence of binary digits (integer
values 0 and 1) from cin and stores them into a container. When receiving a
value different from 0 or 1 from cin stop reading. Now, you should have a
container storing a sequence of 0’s and 1’s. After finishing the read-process,
apply a "bit-stuffing" algorithm to the container. Bit-stuffing is used to
transmit data from a sender to a receiver. To avoid bit sequences in the data,
which would erroneously be interpreted as the stop flag (here: 01111110), it is
necessary to ensure that six consecutive 1’s in the data are splitted by
inserting a 0 after each consecutive five 1’s. Hint: Complexity considerations
(inserting in the middle of a vector takes linear time!) and the fact, that
inserting into a vector can make all iterators to elements invalid should make
you choose the STL container list. A list of integers is defined like a vector
by list<int> l; All operations explained in the vector section are provided for
the list, too. Get an iterator to the first list element. As long as this
iterator is different from the end() iterator increment the iterator and
dereference it to get the appropriate binary value. 
*/

/* 
Exercise 4.1.3

Refine Exercise 4.1.2 and print the original bit sequence and the "bit-stuffed"
bit sequence to cout. Use the hint from Exercise 4.1.2 to form a loop for the
output procedure.
*/

/*
Exercise 4.1.4

Refine Exercise 4.1.3 and print out the absolute and relative expansion of the
bit sequence. The absolute expansion is the expasion measured in bits (e.g. the
bitstuffed sequence has increased by 5 bits), the relative expansion is the
percentage of the expansion (e.g. the relative expansion between the "new" and "old" sequence is 5.12%).
*/

/*
Exercise 4.1.5

Refine Exercise 4.1.4 and write the inverse algorithm to the one in Exercise
4.1.2 that the receiver has to perform to get the initial binary data representation. After the bit-stuffing and bit-unstuffing compare your list with the original one using the equality operator==. If the lists are equal, you did a fine job. Note: It is advisable to include a plausibility test in your unstuff algorithm. After a sequence of five consecutive ones there must be a zero, otherwise something went wrong in the stuffing algorithm
*/

template <class InputIterator>
void copy2cout (InputIterator first, InputIterator last) {
	while (first != last) {
		cout << *first++;
	}
	cout <<endl;
}

void func1(void) {
	list<int> bitSeq;  // list of ints
 	int input = 0;      // values read from cin
  	int count1 = 0;    // counter for number of 1

  cout << "Input 0 and 1 values (one at a time), enter another value to stop input..." << endl;

  	while (cin >> input) {
	 	if (!(input == 0 || input == 1)) 
			break;
	 	bitSeq.push_back (input);
  	}

  	// output loop
  	cout << "Original bit sequence:" << endl;
	copy2cout(bitSeq.begin(), bitSeq.end());

  	//create a new list for bit_stuffing
  	list<int> bitStuff(bitSeq);

  	//define loop iterators
  	list<int>::iterator first = bitStuff.begin();
  	list<int>::iterator last  = bitStuff.end();

  	//bit stuff loop

  	while (first != last) {
		if (*first == 0)
			count1 = 0;    	// reset 1's-counter
		else if (*first == 1)
			count1++;      	// increment number of consecutive 1's
		first++;            // go to the next list element
		if (count1 == 5) {
         // insert a 0 after the fifth consecutive 1
			bitStuff.insert (first, 0);
			count1 = 0;				// reset counter
		}
  	}

  	// output loop
  	cout << "Bit-stuffed bit sequence:" << endl;
	copy2cout(bitStuff.begin(), bitStuff.end());

	double relExp; //relative expansion
	relExp = (double) bitStuff.size() / bitSeq.size();
	relExp = (relExp - 1) * 100;
	cout.precision(4);

	cout << "Relative expansion: " << relExp << "%" << endl;
	cout << "Absolute expansion: " << (bitStuff.size() - bitSeq.size());
	cout << " bit" << endl;

	//bit unstuff loop
	first = bitStuff.begin();
	last = bitStuff.end();
	list<int>::iterator erase_it;

	count1 = 0;

	while (first != last) {
		if (*first == 0) count1 = 0; else count1++;
		if (count1 == 5) {
			erase_it = first;
			if(*(++erase_it) != 0) {
				cout << "not a valid bit-stffed sequence!" << endl;
				exit(0);
			}
			bitStuff.erase(erase_it);
			count1 = 0;
		}
		++first;
	}

	cout << "After bit-unstuffing: ";
	
	//check if valid
	if (bitStuff == bitSeq) {
		cout << "sequences are equal";
	} else {
		cout << "sequences are not equal";
	}
	cout << endl;
}
/*
Excerise 4.3.1
 
Fill two containers with the same number of integer values. Create a new
container, whose elements are the sum of the appropriate elements in the
original container. Hint: The library provides an algorithm and a function
object to do the exercise.
*/

void func2(void) {
	vector<int> a;
  	vector<int> b;

  	for (int i = 0; i < 4; i++) {
		a.push_back(i); 
		b.push_back(i); 
	}

  	vector<int> c(4);	// allocate memory for 4 int values!!

  // use the algo "transform" and the function object "plus"
  // transfrom takes the elements of vectors a and b, adds them using
  // plus and writes the results to c

  transform(a.begin(), a.end(), b.begin(), c.begin(), plus<int>() );

  copy(c.begin(), c.end(), ostream_iterator<int> (cout, " ") );
  cout << endl;
}
/*
Excercise 4.3.2

Write a generator object which can be used with the STL algorithm generate
(group 2) to fill containers with certain values. It should be possible to
specify a starting value and a step size, so that the first element in the
container is the starting value and every further element is the sum of the
preceding element and the step size. 
*/

template <class value_type>
class gen {
	value_type start, step;
	public:
		gen() : start(0), step(1) {}
        gen(value_type sta, value_type ste) : start(sta), step(ste) {}

        value_type operator() (void){ 
			value_type tmp = start; 
			start += step; 
			return tmp; 
		}
};

void func3(void) {
	vector<int> v(10);

  	generate(v.begin(), v.end(), gen<vector<int>::value_type>(1,2));
	copy(v.begin(), v.end(), ostream_iterator<int>(cout," "));
	cout << endl;
}


int main(void) {
	//func1();
	//func2();
	func3();
	return 0;
}
