/*
Testing the header class defintion
*/


#ifndef _ADDNUMBERS_H
#define _ADDNUMBERS_H

class AddNumbers
{
        private:
        int _a;
        int _b;

        public:
        AddNumbers (); // The default constructor
        ~AddNumbers (); // The destructor

        void setA (int a);
        void setB (int b);

        int getA () const;
        int getB () const;

        int getSum () const;

}; // AddNumbers

#endif // _ADDNUMBERS_H
