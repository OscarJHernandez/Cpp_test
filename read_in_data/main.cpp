
// Include all of the Preprocessor directives
#include <iostream>
#include <fstream>
#include <string>

using namespace std;


int main(){
    
    // Pointers to store dynamically allocated arrays,
    double * x;
    double * y;
    double xi;
    double yi;
    int Ndata;
    int i;
    string line;
    string s1;
    string s2;
    string::size_type sz;


    // Open the Data File
    ifstream input("test_data1.txt");
    
    Ndata = 0;
    
    
    if(input.is_open()){
        
        
        // First we count the number of data points
        while(!input.eof()){
            
            
            getline(input,line); // Read in the line from input, store in the string line
            
            // Print out each line
            cout << line << endl;
            
            Ndata++;
            
            }
        
        }
        
    Ndata--; // Decrease the value of the Ndata
    
    // Print out the Number of Data points in text file
    cout << "Number of data points: " << Ndata << "\n";
        
    // Clear
    input.clear();
    input.seekg(0); // Set the startin position of the Stream back to 0
    
    // Allocate Memory for the arrays 
    x = new double[Ndata];
    y = new double[Ndata];
    i=0;
    
    
    if(input.is_open()){
        
        
        // First we count the number of data points
        while(!input.eof()){
            
            getline(input,line); // Read in the line from input, store in the string line
            
            //cout << line.length();
            
            // Split the line string, by white space
            // s1 = [0 ... index of white space]
            s1 = line.substr(0,line.find(" ")) ;
            
            if(s1.length() != 0)
            {
                s1 = line.substr(0,line.find(" "));
                s2 = line.substr(line.find(" "));
                
                // Convert the Strings into 
                xi = std::stod(s1,&sz);
                yi = std::stod(s2,&sz);
                
                // Store the elements into an array 
                x[i] = xi;
                y[i] = yi;
                
                // increment the index
                i++;
                }
            
            }
        
        }
    
    
    // Now that all data has been read in, we can print off the arrays
    for(int i=0; i<Ndata; i++){
        xi = x[i];
        yi = y[i];
        cout << xi << " , " << yi << endl;
        }
    

    cout << "Hello World" << endl;
    
    return 0;
    }
