#import <Foundation/Foundation.h>
#import "sqlite3.h"
#import "FMResultSet.h"
#import "FMDatabasePool.h"

#if ! __has_feature(objc_arc)
    #define FMDBAutorelease(__v) ([__v autorelease]);
    #define FMDBReturnAutoreleased FMDBAutorelease
    #define FMDBRetain(__v) ([__v retain]);
    #define FMDBReturnRetained FMDBRetain
    #define FMDBRelease(__v) ([__v release]);
    #define FMDBDispatchQueueRelease(__v) (dispatch_release(__v));
#else
    #define FMDBAutorelease(__v)
    #define FMDBReturnAutoreleased(__v) (__v)
    #define FMDBRetain(__v)
    #define FMDBReturnRetained(__v) (__v)
    #define FMDBRelease(__v)
    #if OS_OBJECT_USE_OBJC
        #define FMDBDispatchQueueRelease(__v)
    #else
        #define FMDBDispatchQueueRelease(__v) (dispatch_release(__v));
    #endif
#endif

#if !__has_feature(objc_instancetype)
    #define instancetype id
#endif


typedef int(^FMDBExecuteStatementsCallbackBlock)(NSDictionary *resultsDictionary);

@interface FMDatabase : NSObject  {
    
    sqlite3*            _db;
    NSString*           _databasePath;
    BOOL                _logsErrors;
    BOOL                _crashOnErrors;
    BOOL                _traceExecution;
    BOOL                _checkedOut;
    BOOL                _shouldCacheStatements;
    BOOL                _isExecutingStatement;
    BOOL                _inTransaction;
    NSTimeInterval      _maxBusyRetryTimeInterval;
    NSTimeInterval      _startBusyRetryTime;
    NSMutableDictionary *_cachedStatements;
    NSMutableSet        *_openResultSets;
    NSMutableSet        *_openFunctions;
    NSDateFormatter     *_dateFormat;
}

@property (atomic, assign) BOOL traceExecution;
@property (atomic, assign) BOOL checkedOut;
@property (atomic, assign) BOOL crashOnErrors;
@property (atomic, assign) BOOL logsErrors;
@property (atomic, retain) NSMutableDictionary *cachedStatements;

+ (instancetype)databaseWithPath:(NSString*)inPath;
- (instancetype)initWithPath:(NSString*)inPath;
- (BOOL)open;

#if SQLITE_VERSION_NUMBER >= 3005000
- (BOOL)openWithFlags:(int)flags;
#endif

- (BOOL)close;
- (BOOL)goodConnection;
- (BOOL)executeUpdate:(NSString*)sql withErrorAndBindings:(NSError**)outErr, ...;
- (BOOL)update:(NSString*)sql withErrorAndBindings:(NSError**)outErr, ... __attribute__ ((deprecated));
- (BOOL)executeUpdate:(NSString*)sql, ...;
- (BOOL)executeUpdateWithFormat:(NSString *)format, ... NS_FORMAT_FUNCTION(1,2);
- (BOOL)executeUpdate:(NSString*)sql withArgumentsInArray:(NSArray *)arguments;
- (BOOL)executeUpdate:(NSString*)sql withParameterDictionary:(NSDictionary *)arguments;
- (BOOL)executeUpdate:(NSString*)sql withVAList: (va_list)args;
- (BOOL)executeStatements:(NSString *)sql;
- (BOOL)executeStatements:(NSString *)sql withResultBlock:(FMDBExecuteStatementsCallbackBlock)block;
- (sqlite_int64)lastInsertRowId;
- (int)changes;
- (FMResultSet *)executeQuery:(NSString*)sql, ...;
- (FMResultSet *)executeQueryWithFormat:(NSString*)format, ... NS_FORMAT_FUNCTION(1,2);
- (FMResultSet *)executeQuery:(NSString *)sql withArgumentsInArray:(NSArray *)arguments;
- (FMResultSet *)executeQuery:(NSString *)sql withParameterDictionary:(NSDictionary *)arguments;
- (FMResultSet *)executeQuery:(NSString*)sql withVAList: (va_list)args;
- (BOOL)beginTransaction;
- (BOOL)beginDeferredTransaction;
- (BOOL)commit;
- (BOOL)rollback;
- (BOOL)inTransaction;
- (void)clearCachedStatements;
- (void)closeOpenResultSets;
- (BOOL)hasOpenResultSets;
- (BOOL)shouldCacheStatements;
- (void)setShouldCacheStatements:(BOOL)value;
- (BOOL)setKey:(NSString*)key;
- (BOOL)rekey:(NSString*)key;
- (BOOL)setKeyWithData:(NSData *)keyData;
- (BOOL)rekeyWithData:(NSData *)keyData;
- (NSString *)databasePath;
- (sqlite3*)sqliteHandle;
- (NSString*)lastErrorMessage;
- (int)lastErrorCode;
- (BOOL)hadError;
- (NSError*)lastError;
- (void)setMaxBusyRetryTimeInterval:(NSTimeInterval)timeoutInSeconds;
- (NSTimeInterval)maxBusyRetryTimeInterval;

#if SQLITE_VERSION_NUMBER >= 3007000
- (BOOL)startSavePointWithName:(NSString*)name error:(NSError**)outErr;
- (BOOL)releaseSavePointWithName:(NSString*)name error:(NSError**)outErr;
- (BOOL)rollbackToSavePointWithName:(NSString*)name error:(NSError**)outErr;
- (NSError*)inSavePoint:(void (^)(BOOL *rollback))block;
#endif

+ (BOOL)isSQLiteThreadSafe;
+ (NSString*)sqliteLibVersion;
+ (NSString*)FMDBUserVersion;
+ (SInt32)FMDBVersion;
- (void)makeFunctionNamed:(NSString*)name maximumArguments:(int)count withBlock:(void (^)(sqlite3_context *context, int argc, sqlite3_value **argv))block;
+ (NSDateFormatter *)storeableDateFormat:(NSString *)format;
- (BOOL)hasDateFormatter;
- (void)setDateFormat:(NSDateFormatter *)format;
- (NSDate *)dateFromString:(NSString *)s;
- (NSString *)stringFromDate:(NSDate *)date;

@end

@interface FMStatement : NSObject {
    sqlite3_stmt *_statement;
    NSString *_query;
    long _useCount;
    BOOL _inUse;
}

@property (atomic, assign) long useCount;
@property (atomic, retain) NSString *query;
@property (atomic, assign) sqlite3_stmt *statement;
@property (atomic, assign) BOOL inUse;

- (void)close;
- (void)reset;

@end

