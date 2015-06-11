#include <iostream>
#include <cstdlib>
#include <algorithm>
#include <vector>
#include <iterator>
#include <functional>
#include <ext/functional>

using namespace __gnu_cxx;
using namespace std;

/* Create a vector of 100 ints. Fill it with 100 small, random integers between 1 and 100, inclusive, by using the appropriate mutating algorithm. Generate these numbers by passing the mutating algorithm a function object (or perhaps a simple function) that models a Generator. (Your function object should use rand() and the modulo operator, at least.) */

class randNumGen {
	int min, max;
	public:
		randNumGen(int a, int b) : min(a), max(b) {}
		int operator() () { return rand() % max + min; }
};

int func1(void)
{
	randNumGen rnum(1,10);
	vector<int> v(10);
	cout << "size of v = " << v.size() << endl;
	
	generate(v.begin(), v.end(), rnum);
	copy(v.begin(), v.end(), ostream_iterator<int>(cout, "\n"));
	return 0;
}

/* Create a new vector to hold 100 strings. Use another Generator that you write to fill it with 100 random alphabetic strings of random lengths between 5 and 15 characters inclusive.

Sort the vector you've made using the version of sort that applies operator< to the vector. Display this sorted vector in a similar fashion to the way you displayed exercise 1's result.

Then, re-sort the vector by length of string (ascending). You'll want to use a comparator that conforms to the Strict Weak Ordering model. Output your re-sorted vector to stdout. */

class randStrGen {
	int min, max;
	public:
		randStrGen(int a, int b) : min(a), max(b) {}
		string operator() () {
			int randLength = rand() % max + min;
			string str;
			for (int i=0; i<randLength; i++) {
				str += rand() % ('z'-'a'+1) + 'a';
			}
			return str;
		}
};

class myObject {
public:
   bool operator() (string s1, string s2) { return s1.length() < s2.length(); }
};

int func2(void) {
    vector<string> strlist(10);
    randStrGen rnum(5,15);
    myObject mo;
    generate(strlist.begin(), strlist.end(), rnum);
    cout << "AFTER GENERATION:\n";
    copy(strlist.begin(), strlist.end(), ostream_iterator<string>(cout, "\n"));
    sort(strlist.begin(), strlist.end());
    cout << "\nAFTER FIRST SORT:\n";
    copy(strlist.begin(), strlist.end(), ostream_iterator<string>(cout, "\n"));
    sort(strlist.begin(), strlist.end(), mo);
    cout << "\nAFTER SECOND SORT:\n";
    copy(strlist.begin(), strlist.end(), ostream_iterator<string>(cout, "\n"));
    return 0;
}

/* Generate a vector of 100 random integers between 1 and 100, as before, then write some code to count how many of these numbers are odd and how many are even, in a single pass over the integers. Do this by creating a simple function object that implements the unary_function model, and that maintains some internal state. You should then be able to apply it with the for_each algorithm and then extract the counts at the end. */

class countOE {
	int odds, evens;
	public:
    	countOE() : odds(0), evens(0) {}
    	void operator() (int a) {
        	odds = odds + (a % 2);
        	evens = evens + ((a+1)%2);
    	}
    	int getOdds() { return odds; }
    	int getEvens() { return evens; }
};

int func3(void) {
    vector<int> v(10);
    randNumGen rnum(1,10);
    countOE coe;
    generate(v.begin(), v.end(), rnum);
    cout << "AFTER GENERATION\n";
    copy(v.begin(), v.end(), ostream_iterator<int>(cout, "\n"));
    coe = for_each(v.begin(), v.end(), coe);
    cout << " count_odd = " << coe.getOdds() << "\ncount_even = " << coe.getEvens() << "\n";
    return 0;
}

// Have a look at this code:

int func4(void) {
	vector<int> v;
    v.push_back(1);
    v.push_back(4);
    v.push_back(2);
    v.push_back(8);
    v.push_back(5);
    v.push_back(7);

    copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
    cout << endl;

    vector<int>::iterator new_end = 
              remove_if(v.begin(), v.end(), 
                        compose1(bind2nd(equal_to<int>(), 0),
                                 bind2nd(modulus<int>(), 2)));

    copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
    cout << endl;
}

/* (The compose1 function can be difficult to track down in the g++ compiler. It should be in the __gnu_cxx namespace, and you will need to #include <ext/functional> to get the definition.)
Paste it into exercise4.cc, and use a comment block at the top to explain:

1. what you think the author of this code intended it to do,
2. why this code is broken (what does it do?),
3. how you've fixed it.

Of course, actually fix the code, compile, and verify that it works the way it was probably meant to work.

Use the STL docs to help you understand what bind2nd and compose1 do.

(Hint 1: They're easy!)
(Hint 2: Finding them in the STL documentation isn't easy! See the Introduction section for Function Objects.)
*/

/* Now you can try your hand at writing a simple STL algorithm. You will create your own version of the reverse() algorithm in a file exercise5.cc. This algorithm must be implemented as a function template (which we will discuss in a future class), declared like this: */

template <typename BidirectionalIterator>
void my_reverse(BidirectionalIterator first, BidirectionalIterator last)
{
    BidirectionalIterator it1 = first;
    BidirectionalIterator it2 = last;
    while (it1 < it2)
    {
        it2--;
        swap(*it1,*it2);
        it1++;
    }
}

int func5(int size) {
    vector<int> v(size);
    randNumGen rnum(1,100);
    generate(v.begin(), v.end(), rnum);
    cout << "AFTER GENERATE\n";
    copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
    cout << "\n";
    my_reverse(v.begin(), v.end());
    cout << "AFTER REVERSE\n";
    copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
    cout << "\n";
    return 0;
}

int main(void) {
	//func1();
	//func2();
    //func3();
    //func4();
    func5(10);
	return 0;
}
