#include "R.h"
#include "Rinternals.h"

#define REXPORT extern "C"

class TraceVector {
public:
    /** Returns the size of the trace vector. 
     */
    unsigned size() const {
        return size_;
    }
  
    /** Returns the capacity of the trace vector. When the capacity is reached, the vector will grow to twice the capacity.
     */
    unsigned capacity() const {
        return capacity_;
    }

    /** Adds the given trace to the end of the trace vector, grows the vector if necessary. Returns the index of the trace added. C style indexing (from 0) is used.
     */  
    unsigned push_back(SEXP trace) {
        PROTECT(trace);
        if (size_ == capacity_)
            grow();
        UNPROTECT(1);
        SET_VECTOR_ELT(vector_, size_, trace);
        return size_++;
    }
  
    /** Returns the index'th trace. C style indexing (from 0) is used. 
     */
    SEXP operator [] (unsigned index) const {
        if (index >= size_)
          Rf_error("Index to TraceVector[] out of bounds");
        return VECTOR_ELT(vector_, index);
    }
  
    /** Updates trace with given index. We can't use [] operator because SET_VECTOR_ELT has to be used for assignments or bad things may happen. 
     */ 
    void update_trace(unsigned index, SEXP value) {
        if (index >= size_)
            Rf_error("Index to TraceVector.update_trace out of bounds");
        SET_VECTOR_ELT(vector_, index, value);
    }

    /** Clears the traces so that they can be garbage collected, but keeps the current capacity reserved. 
     */  
    void clear() {
        // to force R to delete the traces, we must set them to null
        for (unsigned i = 0; i < size_; ++i)
            SET_VECTOR_ELT(vector_, i, R_NilValue);
        size_ = 0;
    }

    /** Creates a trace vector with given initial size. 
     */
    TraceVector(unsigned initialSize = 1024):
        capacity_(initialSize),
        size_(0),
        vector_(Rf_allocVector(VECSXP, initialSize)) {
        R_PreserveObject(vector_);
    }
  
    /** Releases the GC protection on the trace vector when deleting the object.
     */
    ~TraceVector() {
        if (vector_ != nullptr)
            R_ReleaseObject(vector_);
    }
  
    /** Creates a copy of the trace vector of the exact size. 
     */
    SEXP copy() const {
        SEXP result = PROTECT(Rf_allocVector(VECSXP, size_));
        for (unsigned i = 0; i < size_; ++i)
            SET_VECTOR_ELT(result, i, VECTOR_ELT(vector_, i));
        UNPROTECT(1);
        return result;
    }

private:
  
    /** Grows the trace vector by doubling its capacity and copying the existing traces to the newly allocated vector. 
     */ 
    void grow() {
        // create the larger vector
        capacity_ *= 2;
        SEXP newVector = PROTECT(Rf_allocVector(VECSXP, capacity_));
        // copy the existing contents from the old vector to the new one
        for (unsigned i = 0; i != size_; ++i)
          SET_VECTOR_ELT(newVector, i, VECTOR_ELT(vector_, i));
        // remove protection of the old vector, swap the vectors and protect the new one
        R_ReleaseObject(vector_);
        vector_ = newVector;
        R_PreserveObject(vector_);
        // unprotect the new object, we have created it
        UNPROTECT(1);
    }  

    SEXP vector_;  
    unsigned capacity_;
    unsigned size_;
};

/** Access to the trace vector to circumvent C++'s module initialization issues.
 */
TraceVector & tv() {
    static TraceVector tv;
    return tv;
}

/** Helper function for creating R integers from C++ values. 
 */
inline SEXP CREATE_INTEGER(int value) {
    SEXP result = Rf_allocVector(INTSXP, 1);
    INTEGER(result)[0] = value;
    return result;
}

/** Returns the size of the trace as an R integer. 
 */
REXPORT SEXP trace_size() {
    return CREATE_INTEGER(tv().size());
}

/** Returns the current trace vector capacity as an R integer.
 */
REXPORT SEXP trace_capacity() {
    return CREATE_INTEGER(tv().capacity());
}

/** Resets the trace vector, clearing all its contents, but keeping the capacity reserved. 
 */
REXPORT void reset_traces() {
  tv().clear();
}

/** Pushes the given trace to the trace vector and returns the index at which the trace is stored. Uses C-style indexing 
 */
REXPORT SEXP push_trace(SEXP trace) {
    return CREATE_INTEGER(tv().push_back(trace));
}  

/** Returns the index-th trace. Uses C style indexing (from 0). 
 */
REXPORT SEXP get_trace(SEXP index) {
    if (TYPEOF(index) != INTSXP || LENGTH(index) != 1)
        Rf_error("get_trace expects integer scalar as index");
    return tv()[INTEGER(index)[0]];
}

/** Updates the index-th trace. Uses C style indexing (from 0).
 */
REXPORT void update_trace(SEXP index, SEXP trace) {
    if (TYPEOF(index) != INTSXP || LENGTH(index) != 1)
        Rf_error("get_trace expects integer scalar as index");
    tv().update_trace(INTEGER(index)[0], trace);
}

/** Returns a copy of the trace vector with the exact size. 
 */
REXPORT SEXP copy_traces() {
    return tv().copy();
}
