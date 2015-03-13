#ifndef _ZBAR_EXCEPTION_H_
#define _ZBAR_EXCEPTION_H_


#ifndef _ZBAR_H_
# error "include zbar.h in your application, **not** zbar/Exception.h"
#endif

#include <exception>
#include <new>

namespace zbar {

class Exception : public std::exception {

public:
    Exception (const void *obj = NULL) : std::exception(), _obj(obj) {
    }

    ~Exception () throw() {
    }

    virtual const char* what () const throw() {
        if(!_obj)
            return("zbar library unspecified generic error");
        return(_zbar_error_string(_obj, 0));
    }

private:
    const void *_obj;
};

class InternalError : public Exception {
public:
    InternalError (const void *obj) : Exception(obj) {
    }
};

class UnsupportedError : public Exception {
public:

    UnsupportedError (const void *obj) : Exception(obj) {
    }
};

class InvalidError : public Exception {
public:
    InvalidError (const void *obj) : Exception(obj) {
    }
};

class SystemError : public Exception {
public:
    SystemError (const void *obj) : Exception(obj) {
    }
};

class LockingError : public Exception {
public:
    LockingError (const void *obj) : Exception(obj) {
    }
};

class BusyError : public Exception {
public:
    BusyError (const void *obj) : Exception(obj) {
    }
};

class XDisplayError : public Exception {
public:
    XDisplayError (const void *obj) : Exception(obj) {
    }
};

class XProtoError : public Exception {
public:
    XProtoError (const void *obj) : Exception(obj) {
    }
};

class ClosedError : public Exception {
public:
    ClosedError (const void *obj) : Exception(obj) {
    }
};

class FormatError : public Exception {
    virtual const char* what () const throw() {
        return("unsupported format");
    }
};

static inline std::exception throw_exception (const void *obj) {
    switch(_zbar_get_error_code(obj)) {
    case ZBAR_ERR_NOMEM:
        throw std::bad_alloc();
    case ZBAR_ERR_INTERNAL:
        throw InternalError(obj);
    case ZBAR_ERR_UNSUPPORTED:
        throw UnsupportedError(obj);
    case ZBAR_ERR_INVALID:
        throw InvalidError(obj);
    case ZBAR_ERR_SYSTEM:
        throw SystemError(obj);
    case ZBAR_ERR_LOCKING:
        throw LockingError(obj);
    case ZBAR_ERR_BUSY:
        throw BusyError(obj);
    case ZBAR_ERR_XDISPLAY:
        throw XDisplayError(obj);
    case ZBAR_ERR_XPROTO:
        throw XProtoError(obj);
    case ZBAR_ERR_CLOSED:
        throw ClosedError(obj);
    default:
        throw Exception(obj);
    }
}

}

#endif
