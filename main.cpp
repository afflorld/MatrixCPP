#include <ostream>
#include <stdexcept>
#include <vector>
#include <iostream>
#include <cmath>
#include <cstdlib>

using namespace std;

template <typename T> struct Komplex{

     T re;
     T im;

     Komplex(T r = T {}, T i = T {}) : re(r), im(i) {}

     Komplex operator+(const Komplex& other) const {
          return Komplex(re + other.re, im + other.im);
     }

     Komplex operator+(const double scalar) const {
          return Komplex(re + scalar, im);
     }

     Komplex operator-(const Komplex& other) const {
          return Komplex(re - other.re, im - other.im);
     }

     Komplex operator-() const {
          return Komplex(-re, -im);
     }

     Komplex operator-(const double scalar) const{

          return Komplex(re - scalar, im);

     }

     Komplex operator*(double scalar) const {

          return Komplex(re * scalar, im * scalar);

     }

     Komplex operator/(double scalar) const {

          if (scalar == 0) {
               throw::invalid_argument("Delenie nulou");
          }
          return Komplex(re / scalar, im / scalar);

     }

     Komplex operator/(const Komplex& other) const {
          if (other.re == 0 && other.im == 0) {
               throw::invalid_argument("Delenie nulou");
          }
          T denom = other.re * other.re + other.im * other.im;
          return Komplex((re * other.re + im * other.im) / denom,
                    (im * other.re - re * other.im) / denom);
     }


     Komplex operator*(const Komplex& other) const {
          return Komplex(re * other.re - im * other.im, re * other.im + im * other.re);
     }

     Komplex operator+=(const Komplex& other) {
          re += other.re;
          im += other.im;
          return *this;
     }

     Komplex operator-=(const Komplex& other) {
          re -= other.re;
          im -= other.im;
          return *this;
     }

     Komplex operator+=(const double scalar) {
          re += scalar;
          return *this;
     }
     Komplex operator-=(const double scalar) {
          re -= scalar;
          return *this;
     }

     Komplex operator*=(const double scalar) {
          re *= scalar;
          im *= scalar;
          return *this;
     }

     Komplex operator*=(const Komplex& other) {
          T temp_re = re * other.re - im * other.im;
          im = re * other.im + im * other.re;
          re = temp_re;
          return *this;
     }

     Komplex operator/=(const double scalar) {
          if (scalar == 0) {
               throw::invalid_argument("Delenie nulou");
          }
          re /= scalar;
          im /= scalar;
          return *this;
     }

     Komplex operator=(const Komplex& other) {
          if (this != &other) {
               re = other.re;
               im = other.im;
          }
          return *this;
     }

     bool operator==(const Komplex& other) const {
          return (re == other.re && im == other.im);
     }

     bool operator!=(const Komplex& other) const {
          return !(re == other.re && im == other.im);
     }

     Komplex determinant() const {
          if (re == 0 && im == 0) {
               throw std::invalid_argument("Determinant je 0");
          }
          return Komplex(re, im);
     }

     Komplex transpose() const {
          return Komplex(re, -im);
     }

     Komplex conjugate() const {
          return Komplex(re, -im);
     }

     Komplex inverse() const {
          if (re == 0 && im == 0) {
               throw std::invalid_argument("Inverz je neplatny");
          }
          T denom = re * re + im * im;
          return Komplex(re / denom, -im / denom);
     }


     friend ostream& operator<<(ostream& os, const Komplex<T>& k) {
          os << k.re << " + " << k.im << "i";
          return os;
     }

};

template <typename T> class Matrix{

     vector<vector<T>> mat;
     size_t rows;
     size_t cols;

     public: 

     Matrix() : rows(0), cols(0) {}

     Matrix(size_t r, size_t c) : rows(r), cols(c), mat(r, vector<T>(c)) {

          for(auto& row : mat) {
               for(auto& elem : row) {
                    elem = T
                    {(double)1 + rand() % 5,(double)1 + rand() % 5};
               }
          }
     }

     Matrix(const Matrix& other) : rows(other.rows), cols(other.cols), mat(other.mat) {}

     template <typename U>

          Matrix(const Matrix<U>& other) : rows(other.rows), cols(other.cols), mat(other.rows, vector<T>(other.cols)) {
               for (size_t i = 0; i < rows; ++i) {
                    for (size_t j = 0; j < cols; ++j) {
                         mat[i][j] = static_cast<T>(other.mat[i][j]);
                    }
               }
          }

     void print() const {
          for(const auto& row : mat) {
               for (const auto& elem : row) {
                    cout << elem << " ";
               }
               cout << endl;
          }
     }

     Matrix operator+(const Matrix& other) const {
          if (rows != other.rows || cols != other.cols) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = mat[i][j] + other.mat[i][j];
               }
          }
          return result;
     }

     Matrix operator+(const double scalar) const {
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = mat[i][j] + scalar;
               }
          }
          return result;
     }

     Matrix operator-(const Matrix& other) const {
          if (rows != other.rows || cols != other.cols) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = mat[i][j] - other.mat[i][j];
               }
          }
          return result;
     }

     Matrix operator-(const double scalar) const {
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = mat[i][j] - scalar;
               }
          }
          return result;
     }

     Matrix operator-() const {

          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = -mat[i][j];
               }
          }
          return result;

     }

     Matrix operator*(const Matrix& other) const {
          if (cols != other.rows) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          Matrix result(rows, other.cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < other.cols; ++j) {
                    T sum{};  
                    for (size_t k = 0; k < cols; ++k) {
                         sum += mat[i][k] * other.mat[k][j];
                    }
                    result.mat[i][j] = sum;
               }
          }
          return result;
     }

     Matrix operator*(const double scalar) const {
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = mat[i][j] * scalar;
               }
          }
          return result;
     }

     Matrix operator/(const double scalar) const {
          if (scalar == 0) {
               throw::invalid_argument("Delenie nulou");
          }
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = mat[i][j] / scalar;
               }
          }
          return result;
     }

     Matrix& operator+=(const Matrix& other) {
          if (rows != other.rows || cols != other.cols) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    mat[i][j] += other.mat[i][j];
               }
          }
          return *this;
     }

     Matrix& operator+=(const double scalar) {
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    mat[i][j] += scalar;
               }
          }
          return *this;
     }

     Matrix& operator-=(const Matrix& other) {
          if (rows != other.rows || cols != other.cols) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    mat[i][j] -= other.mat[i][j];
               }
          }
          return *this;
     }

     Matrix& operator-=(const double scalar) {
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    mat[i][j] -= scalar;
               }
          }
          return *this;
     }

     Matrix& operator*=(const double scalar) {
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    mat[i][j] *= scalar;
               }
          }
          return *this;
     }

     Matrix& operator*=(const Matrix& other) {
          if (cols != other.rows) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          Matrix result(rows, other.cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < other.cols; ++j) {
                    T sum{};  
                    for (size_t k = 0; k < cols; ++k) {
                         sum += mat[i][k] * other.mat[k][j];
                    }
                    result.mat[i][j] = sum;
               }
          }
          mat = result.mat;
          cols = other.cols;  
          return *this;
     }

     Matrix& operator/=(const double scalar) {
          if (scalar == 0) {
               throw::invalid_argument("Delenie nulou");
          }
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    mat[i][j] /= scalar;
               }
          }
          return *this;
     }

     Matrix& operator=(const Matrix& other) {
          if (this != &other) {
               rows = other.rows;
               cols = other.cols;
               mat = other.mat;
          }
          return *this;
     }

     bool operator==(const Matrix& other) const{
          if (rows != other.rows || cols != other.cols) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    if (mat[i][j] != other.mat[i][j]) {
                         return false;
                    }
               }
          }
          return true;
     }

     bool operator!=(const Matrix& other) const{
          if (rows != other.rows || cols != other.cols) {
               throw::invalid_argument("Nespravne rozmery matice");
          }
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    if (mat[i][j] != other.mat[i][j]) {
                         return true;
                    }
               }
          }
          return false;
     }

     vector<T>& operator[](size_t index) {
          if (index >= rows) {
               throw::out_of_range("Nespravny rozmer indexu");
          }
          return mat[index];
     }

     const vector<T>& operator[](size_t index) const {
          if (index >= rows) {
               throw::out_of_range("Nespravny rozmer indexu");
          }
          return mat[index];
     }

     T determinant() const {
          if (rows != cols) {
               throw invalid_argument("Matica musi byt typu stvorec");
          }

          if (rows == 1) {
               return mat[0][0]; 
          }
          else if (rows == 2) {
               return mat[0][0] * mat[1][1] - mat[0][1] * mat[1][0];
          }
          else {
               T det{};
               for (size_t i = 0; i < rows; ++i) {
                    Matrix<T> submatrix(rows - 1, cols - 1);
                    for (size_t j = 1; j < rows; ++j) {
                         size_t sub_col = 0;
                         for (size_t k = 0; k < cols; ++k) {
                              if (k == i) continue; 
                              submatrix.mat[j-1][sub_col++] = mat[j][k];
                         }
                    }
                    T sign = (i % 2 == 0) ? T(1) : T(-1);
                    det = det + (mat[0][i] * sign) * submatrix.determinant();
               }
               return det;
          }
     }

     Matrix transpose() const {
          Matrix result(cols, rows);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[j][i] = mat[i][j];
               }
          }
          return  result;
     }

     Matrix conjugate() const {
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    result.mat[i][j] = mat[i][j].conjugate();
               }
          }
          return  result;
     }

     Matrix inverse() const {
          if (rows != cols) {
               throw::invalid_argument("Matica musi byt typu stvorec");
          }
          T det = determinant();
          if (det.re == 0 && det.im == 0) {
               throw::invalid_argument("Matica nema inverznu hodnotu");
          }
          Matrix result(rows, cols);
          for (size_t i = 0; i < rows; ++i) {
               for (size_t j = 0; j < cols; ++j) {
                    Matrix submatrix(rows - 1, cols - 1);
                    for (size_t k = 0; k < rows; ++k) {
                         for (size_t l = 0; l < cols; ++l) {
                              if (k == i || l == j) continue;
                              submatrix.mat[k < i ? k : k - 1][l < j ? l : l - 1] = mat[k][l];
                         }
                    }
                    T sign = ((i + j) % 2 == 0) ? T(1) : T(-1);
                    result.mat[j][i] = (sign * submatrix.determinant()) / det;
               }
          }
          return result;
     }

     friend ostream& operator<<(ostream& os, const Matrix<T>& m) {
          for (const auto& row : m.mat) {
               for (const auto& elem : row) {
                    os << elem << " ";
               }
               os << endl;
          }
          return os;
     }

     ~Matrix() {
          mat.clear();
     }

};

int main (int argc, char *argv[]) {

     srand(time(0));


     Matrix<Komplex<double>> A(2,2);
     Matrix<Komplex<double>> B(2,2);
     Matrix<Komplex<double>> C(2,2);
     Matrix<Komplex<double>> X;

     try{
          cout << "A" << endl;

          cout << A << endl;

          cout << "A[0][0] = 3 + 2" << endl;

          A[0][0] = Komplex<double>(3,2);

          cout << A << endl;

          cout << "determinant" << endl;

          cout << A.determinant() << endl;

          cout << "A.transpose()" << endl;

          cout << A.transpose() << endl;

          cout << "A.conjugate()" << endl;

          cout << A.conjugate() << endl;

          cout << "A.inverse()" << endl;

          cout << A << endl;

          cout << A.inverse() << endl;

          cout << "B" << endl;

          cout << B << endl;

          cout <<"C" << endl;

          cout << C << endl;

          cout << "(-C)" << endl;

          cout << (-C) << endl;

          cout << "A" << endl;

          cout << A << endl;

          cout << "B" << endl;

          cout << B << endl;

          cout << "C" << endl;

          cout << C << endl;

          cout << "X " << endl;

          X = A + 3 - B + (-C) * 2;

          cout << X << endl;

          cout << "A" << endl;

          cout << A << endl;

          cout << "B" << endl;

          cout << B << endl;

          A = B;

          cout << "A = B" << endl;

          cout << A << endl;

          cout << "A == B" << endl;

          if(A == B) {
               cout << "true" << endl;
          }else{
               cout << "false" << endl;
          }

          cout << "A != B" << endl;

          if(A != B) {
               cout << "true" << endl;
          }else{
               cout << "false" << endl;
          }

          cout << "A" << endl;

          cout << A << endl;

          cout << "A[][]" << endl;

          cout << A[0][0] << endl;

          cout << "A" << endl;

          cout << A << endl;

          cout << "B" << endl;

          cout << B << endl;

          cout << "A*B" << endl;

          cout << A * B << endl;

          cout << "A" << endl;

          cout << A << endl;

          cout << "B" << endl;

          cout << B << endl;

          cout << "A*=B" << endl;

          A *= B;

          cout << A << endl;

          cout << "A/=2" << endl;

          A /= 2;

          cout << A << endl;

     }catch(const exception& error) {

          cout << "Chyba " << error.what() << endl;

     }

     return 0;
}
