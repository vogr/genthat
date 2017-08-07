#include <R.h>
#include <Rcpp.h>

class SequenceTracer {
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
          Rf_error("Index to SequenceTracer[] out of bounds");
        return VECTOR_ELT(vector_, index);
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
    SequenceTracer(unsigned initialSize = 1024):
        capacity_(initialSize),
        size_(0),
        vector_(Rf_allocVector(VECSXP, initialSize)) {
        R_PreserveObject(vector_);
    }

    /** Releases the GC protection on the trace vector when deleting the object.
     */
    ~SequenceTracer() {
        if (vector_ != nullptr) {
            R_ReleaseObject(vector_);
        }
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

// [[Rcpp::export]]
SEXP sequence_tracer_create() {
    auto tracer = Rcpp::XPtr<SequenceTracer>(new SequenceTracer(), true);
    tracer.attr("class") = "sequence_tracer";

    return tracer;
}

/** Resets the trace vector, clearing all its contents, but keeping the capacity reserved.
 */
// [[Rcpp::export]]
SEXP sequence_tracer_reset_traces(SEXP tracer_xp) {
    Rcpp::XPtr<SequenceTracer> tracer(tracer_xp);
    tracer->clear();

    return R_NilValue;
}

/** Pushes the given trace to the trace vector and returns the index at which the trace is stored. Uses C-style indexing
 */
// [[Rcpp::export]]
SEXP sequence_tracer_store_trace(SEXP tracer_xp, SEXP trace) {
    Rcpp::XPtr<SequenceTracer> tracer(tracer_xp);
    tracer->push_back(trace);

    return trace;
}

/** Returns a copy of the trace vector with the exact size.
 */
// [[Rcpp::export]]
SEXP sequence_tracer_copy_traces(SEXP tracer_xp) {
    Rcpp::XPtr<SequenceTracer> tracer(tracer_xp);

    return tracer->copy();
}
