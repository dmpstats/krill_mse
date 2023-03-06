#ifndef HAVE_AD_BLAS_HPP
#define HAVE_AD_BLAS_HPP
// Autogenerated - do not edit by hand !
#include <Eigen/Dense>
#include "global.hpp"

namespace TMBad {

/** \brief Request a contiguous block on the tape.

    1. Check if `x` already is on the tape and satisfies the storage
   requirement.
    2. If **no** invoke a deep copy of `x` to the tape **and** *update* `x` with
   the new tape addresses.

    \return A reference to `x` as a contiguous block on the tape.

    \note The update step is critical as it ensures that a given
    matrix can be used several times without invoking a deep copy more
    than once.
*/
template <class Matrix>
global::ad_segment contiguousBlock(const Matrix &x) {
  bool yes = true;
  Index j_previous = -1;
  for (size_t i = 0; i < (size_t)x.size(); i++) {
    if (!x(i).ontape()) {
      yes = false;
      break;
    }
    Index j = ad_plain(x(i)).index;
    if (i > 0) {
      if (j != j_previous + 1) {
        yes = false;
        break;
      }
    }
    j_previous = j;
  }
  if (yes) {
    return global::ad_segment(ad_plain(x(0)), x.rows(), x.cols());
  }

  ad_plain ans;
  for (size_t i = 0; i < (size_t)x.size(); i++) {
    ad_plain xi_cpy = x(i).copy();

    x(i).override_by(xi_cpy);
    if (i == 0) ans = xi_cpy;
  }
  return global::ad_segment(ans, x.rows(), x.cols());
}

using Eigen::Dynamic;
using Eigen::Map;
using Eigen::Matrix;
typedef Matrix<double, Dynamic, Dynamic> dmatrix;
typedef Matrix<global::Replay, Dynamic, Dynamic> vmatrix;

template <class Target>
void fill(Target &y, const global::ad_segment x) {
  TMBAD_ASSERT((size_t)y.size() == (size_t)x.size());
  for (size_t i = 0; i < (size_t)y.size(); i++) {
    y(i) = x[i];
  }
}

template <bool XT, bool YT, bool ZT, bool UP>
struct MatMul;
template <bool XT, bool YT, bool ZT, bool UP>
void matmul(const vmatrix &x, const vmatrix &y, Map<vmatrix> z) {
  global::ad_segment xc = contiguousBlock(x);
  global::ad_segment yc = contiguousBlock(y);
  if (!UP) {
    global::ad_segment out =
        get_glob()->add_to_stack<MatMul<XT, YT, ZT, UP> >(xc, yc);
    fill(z, out);
  } else {
    global::ad_segment zc = contiguousBlock(z);
    get_glob()->add_to_stack<MatMul<XT, YT, ZT, UP> >(xc, yc, zc);
  }
}

/** \brief Multiply two matrices of ad variables */
vmatrix matmul(const vmatrix &x, const vmatrix &y);

/** \brief Multiply two matrices of scalar types */
dmatrix matmul(const dmatrix &x, const dmatrix &y);

/** Expand all 16 combinations */
template <bool XT, bool YT, bool ZT, bool UP>
void matmul(Map<const dmatrix> x, Map<const dmatrix> y, Map<dmatrix> z) {
  if (!UP) {
    if (XT && YT && ZT) z.transpose() = x.transpose() * y.transpose();
    if (!XT && YT && ZT) z.transpose() = x * y.transpose();
    if (XT && !YT && ZT) z.transpose() = x.transpose() * y;
    if (XT && YT && !ZT) z = x.transpose() * y.transpose();
    if (!XT && !YT && ZT) z.transpose() = x * y;
    if (XT && !YT && !ZT) z = x.transpose() * y;
    if (!XT && YT && !ZT) z = x * y.transpose();
    if (!XT && !YT && !ZT) z = x * y;
  }
  if (UP) {
    if (XT && YT && ZT) z.transpose() += x.transpose() * y.transpose();
    if (!XT && YT && ZT) z.transpose() += x * y.transpose();
    if (XT && !YT && ZT) z.transpose() += x.transpose() * y;
    if (XT && YT && !ZT) z += x.transpose() * y.transpose();
    if (!XT && !YT && ZT) z.transpose() += x * y;
    if (XT && !YT && !ZT) z += x.transpose() * y;
    if (!XT && YT && !ZT) z += x * y.transpose();
    if (!XT && !YT && !ZT) z += x * y;
  }
}

template <bool XT, bool YT, bool ZT, bool UP>
struct MatMul : global::Operator<2 + UP, -1> {
  static const bool dynamic = true;
  static const int max_fuse_depth = 0;
  int n1, n2, n3;
  static const int ninput = 2 + UP;
  MatMul(global::ad_segment X, global::ad_segment Y) {
    set_dim(X.rows(), X.cols(), Y.rows(), Y.cols());
  }
  MatMul(int n1, int n2, int n3) : n1(n1), n2(n2), n3(n3) {}
  Index input_size() const { return 2 + UP; }
  Index output_size() const {
    if (UP) return 0;
    int Xrows, Xcols, Yrows, Ycols, Zrows, Zcols;
    get_dim(Xrows, Xcols, Yrows, Ycols, Zrows, Zcols);
    return Zrows * Zcols;
  }
  static const bool have_input_size_output_size = true;
  void set_dim(int Xrows, int Xcols, int Yrows, int Ycols) {
    n1 = Xrows;
    n2 = Xcols;
    n3 = (YT ? Yrows : Ycols);
  }
  void get_dim(int &Xrows, int &Xcols, int &Yrows, int &Ycols, int &Zrows,
               int &Zcols) const {
    Xrows = n1;
    Xcols = n2;

    int Xop_rows = Xrows, Xop_cols = Xcols;
    if (XT) std::swap(Xop_rows, Xop_cols);

    int Yop_rows = Xop_cols, Yop_cols = n3;

    Yrows = Yop_rows;
    Ycols = Yop_cols;
    if (YT) std::swap(Yrows, Ycols);

    int Zop_rows = Xop_rows, Zop_cols = Yop_cols;

    Zrows = Zop_rows;
    Zcols = Zop_cols;
    if (ZT) std::swap(Zrows, Zcols);
  }
  template <class Type>
  void forward(ForwardArgs<Type> &args) {
    int Xrows, Xcols, Yrows, Ycols, Zrows, Zcols;
    get_dim(Xrows, Xcols, Yrows, Ycols, Zrows, Zcols);
    typedef Map<Matrix<Type, Dynamic, Dynamic> > MapMatrix;
    typedef Map<const Matrix<Type, Dynamic, Dynamic> > ConstMapMatrix;
    Type *zp = (UP ? args.x_ptr(2) : args.y_ptr(0));
    ConstMapMatrix X(args.x_ptr(0), Xrows, Xcols);
    ConstMapMatrix Y(args.x_ptr(1), Yrows, Ycols);
    MapMatrix Z(zp, Zrows, Zcols);
    matmul<XT, YT, ZT, UP>(X, Y, Z);
  }
  template <class Type>
  void reverse(ReverseArgs<Type> &args) {
    int Xrows, Xcols, Yrows, Ycols, Zrows, Zcols;
    get_dim(Xrows, Xcols, Yrows, Ycols, Zrows, Zcols);
    typedef Map<Matrix<Type, Dynamic, Dynamic> > MapMatrix;
    typedef Map<const Matrix<Type, Dynamic, Dynamic> > ConstMapMatrix;
    Type *dzp = (UP ? args.dx_ptr(2) : args.dy_ptr(0));
    ConstMapMatrix X(args.x_ptr(0), Xrows, Xcols);
    ConstMapMatrix Y(args.x_ptr(1), Yrows, Ycols);
    ConstMapMatrix W(dzp, Zrows, Zcols);
    MapMatrix DX(args.dx_ptr(0), Xrows, Xcols);
    MapMatrix DY(args.dx_ptr(1), Yrows, Ycols);

    matmul<ZT, !YT, XT, true>(W, Y, DX);
    matmul<!XT, ZT, YT, true>(X, W, DY);
  }

  void dependencies(Args<> &args, Dependencies &dep) const {
    int Xrows, Xcols, Yrows, Ycols, Zrows, Zcols;
    get_dim(Xrows, Xcols, Yrows, Ycols, Zrows, Zcols);
    dep.add_segment(args.input(0), Xrows * Xcols);
    dep.add_segment(args.input(1), Yrows * Ycols);
  }

  void dependencies_updating(Args<> &args, Dependencies &dep) const {
    int Xrows, Xcols, Yrows, Ycols, Zrows, Zcols;
    get_dim(Xrows, Xcols, Yrows, Ycols, Zrows, Zcols);
    if (UP) {
      dep.add_segment(args.input(2), Zrows * Zcols);
    }
  }
  static const bool have_dependencies = true;
  /** \brief This operator **has** implicit dependencies */
  static const bool implicit_dependencies = true;
  /** \brief It is **not* safe to remap the inputs of this operator */
  static const bool allow_remap = false;
  /** \brief This operator may update existing variables on the tape */
  static const bool updating = true;

  void forward(ForwardArgs<Writer> &args) { TMBAD_ASSERT(false); }
  void reverse(ReverseArgs<Writer> &args) { TMBAD_ASSERT(false); }
  const char *op_name() { return "MatMul"; }
};

}  // namespace TMBad
#endif  // HAVE_AD_BLAS_HPP
