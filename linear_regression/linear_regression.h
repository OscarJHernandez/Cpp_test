//  This is the linear regression header files
//
//


#ifndef LINEAR_REGRESSION
#define LINEAR_REGRESSION

class linear_regression{
    private:
    int Ndata;
    
    public:
    
    // The constructor for the class
    linear_regression(int Ndata);
    
    // Helper function
    void SetData(int Ndata);
    
    // Returns the Data set size
    int GetNdata();
    
    // This function should read x,y data from a file store it
    void readData();
    
    };





#endif
