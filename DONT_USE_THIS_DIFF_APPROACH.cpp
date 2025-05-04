#include <iostream>
#include <ostream>
#include <vector>
using namespace std;

struct Komplex{

     vector<double> p;
     Komplex(double r = 0.0, double i = 0.0) : p{r,i} {}
};

template<typename Derived> class Operations{
     public:

          Derived operator+(const Derived& other) const {
          
               Derived result = static_cast<const Derived&>(*this);

               for (int i = 0; i < other.rows; i++) {
                    for (int j = 0; j < other.cols; j++) {
                         result.mat[i][j].p[0] += other.mat[i][j].p[0];
                         result.mat[i][j].p[1] += other.mat[i][j].p[1];
                    }
               }
               return result;
          }

          Derived operator+(double scalar) const {
               Derived result = static_cast<const Derived&>(*this);
               
               result.mat[0][0].p[0] += scalar;

               return result;

          }

          Derived operator+(const Komplex& scalar) const {

               Derived result = static_cast<const Derived&>(*this);

               result.mat[0][0].p[0] += scalar.p[0];
               result.mat[0][0].p[1] += scalar.p[1];


               return result;
          }



          Derived operator-(const Derived& other) const {
               Derived result = static_cast<const Derived&>(*this);

               for (int i = 0; i < other.rows; i++) {
                    for (int j = 0; j < other.cols; j++) {
                         result.mat[i][j].p[0] -= other.mat[i][j].p[0];
                         result.mat[i][j].p[1] -= other.mat[i][j].p[1];
                    }
               }
               return result;
          }

          Derived operator-() const{

               Derived result = static_cast<const Derived&>(*this);

               for (int i = 0; i < result.rows; i++) {
                    for (int j = 0; j < result.cols; j++) {
                         result.mat[i][j].p[0] = -result.mat[i][j].p[0];
                         result.mat[i][j].p[1] = -result.mat[i][j].p[1];
                    }
               }

               return result;

          }


          Derived& operator+=(const Derived& other) {

              Derived& result = static_cast<Derived&>(*this);
              
               for (int i = 0; i < other.rows; i++) {
                    for (int j = 0; j < other.cols; j++) {
                         result.mat[i][j].p[0] += other.mat[i][j].p[0];
                         result.mat[i][j].p[1] += other.mat[i][j].p[1];
                    }
               }

               return result;

          }

          Derived& operator+=(double scalar){

               Derived& result = static_cast<Derived&>(*this);

               for (int i = 0; i < result.rows; i++) {
                    for (int j = 0; j < result.cols; j++) {
                         result.mat[i][j].p[0] += scalar;
                         result.mat[i][j].p[1] += scalar;
                    }
               }

               return result;

          }

          Derived operator-=(const Derived& other) {
               Derived& result = static_cast<Derived&>(*this);

               for (int i = 0; i < other.rows; i++) {
                    for (int j = 0; j < other.cols; j++) {
                         result.mat[i][j].p[0] -= other.mat[i][j].p[0];
                         result.mat[i][j].p[1] -= other.mat[i][j].p[1];
                    }
               }

               return result;

          }

          Derived& operator-=(double scalar){

               Derived& result = static_cast<Derived&>(*this);

               for (int i = 0; i < result.rows; i++) {
                    for (int j = 0; j < result.cols; j++) {
                         result.mat[i][j].p[0] -= scalar;
                         result.mat[i][j].p[1] -= scalar;
                    }
               }

               return result;

          }

          friend Derived operator*(const Derived& m,double scalar){

               Derived result = m;


               for (int i = 0; i < result.rows; i++) {
                    for (int j = 0; j < result.cols; j++) {
                         result.mat[i][j].p[0] *= scalar;
                         result.mat[i][j].p[1] *= scalar;
                    }
               }

               return result;

          }

          Derived& operator=(const Derived& other) {
               Derived& result = static_cast<Derived&>(*this);
                    for (int i = 0; i < result.rows; i++) {
                         for (int j = 0; j < result.cols; j++) {
                              result.mat[i][j].p[0] = other.mat[i][j].p[0];
                              result.mat[i][j].p[1] = other.mat[i][j].p[1];
                         }
                    }
               return static_cast<Derived&>(*this);
          }

          bool operator==(const Derived& other) {
               Derived& result = static_cast<Derived&>(*this);
               for (int i = 0; i < result.rows; i++) {
                    for (int j = 0; j < result.cols; j++) {
                         if (result.mat[i][j].p[0] != other.mat[i][j].p[0] || result.mat[i][j].p[1] != other.mat[i][j].p[1]) {
                              return false;
                         }
                    }
               }
               return true;
          }

          bool operator!=(const Derived& other) {
               Derived& result = static_cast<Derived&>(*this);
               for (int i = 0; i < result.rows; i++) {
                    for (int j = 0; j < result.cols; j++) {
                         if (result.mat[i][j].p[0] != other.mat[i][j].p[0] || result.mat[i][j].p[1] != other.mat[i][j].p[1]) {
                              return true;
                         }
                    }
               }
               return false;
          }


          friend ostream& operator<<(ostream& os, const Derived& obj) {
               for (int i = 0; i < obj.rows; i++) {
                    for (int j = 0; j < obj.cols; j++) {
                         os << obj.mat[i][j].p[0] << " + " << obj.mat[i][j].p[1] << "i" << " ";
                    }
                    os << endl;
               }
               return os;
          }

};

template<typename T>class Matrix : public Operations<Matrix<T>> {
     friend class Operations<Matrix<T>>;
     
     vector<vector<Komplex>> mat;
     size_t rows;
     size_t cols;

     public:

     Matrix() : rows(0), cols(0) {};

     Matrix(size_t rows, size_t cols) : rows(rows), cols(cols) ,mat(rows, vector<Komplex> (cols)){

          cout <<"Inicializacia matice" << endl;

          for (int i = 0; i < rows; i++) {
               for (int j = 0; j < cols; j++) {
                    for(int k = 0; k < 2; k++){
                         mat[i][j].p[k] = rand() % 10;
                    }
                    print();
               }
          }
     };

     void print(){

          for (int i = 0; i < rows; i++) {
               for (int j = 0; j < cols; j++) {
                    cout << mat[i][j].p[0] << " + " << mat[i][j].p[1] << "i" << " ";
               }
               cout << endl;
          }
     }


     ~Matrix() {
          for (int i = 0; i < rows; i++) {
               for (int j = 0; j < cols; j++) {
                    mat[i][j].p.clear();
               }
          }
          mat.clear();
     };

};



int main (int argc, char *argv[]) {


     Matrix<Komplex> A(2,2);
     Matrix<Komplex> B(2,2);
     Matrix<Komplex> C(2,2);
     Matrix<Komplex> X;


     cout << "A" << endl;

     cout << A << endl;

     cout << "B" << endl;

     cout << B << endl;

     cout << "C" << endl;

     cout << C << endl;

     cout << "(-C)" << endl;

     cout << (-C) << endl;

     cout << "(-C) * 2" << endl;
     cout << (-C) * 2 << endl;

     cout << "X" << endl;

     X = A + 3;

     cout << X << endl;

}
