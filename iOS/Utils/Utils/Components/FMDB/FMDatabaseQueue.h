#import <Foundation/Foundation.h>
#import "sqlite3.h"

@class FMDatabase;

@interface FMDatabaseQueue : NSObject {
    NSString            *_path;
    dispatch_queue_t    _queue;
    FMDatabase          *_db;
    int                 _openFlags;
}

@property (atomic, retain) NSString *path;
@property (atomic, readonly) int openFlags;

+ (instancetype)databaseQueueWithPath:(NSString*)aPath;
+ (instancetype)databaseQueueWithPath:(NSString*)aPath flags:(int)openFlags;
- (instancetype)initWithPath:(NSString*)aPath;
- (instancetype)initWithPath:(NSString*)aPath flags:(int)openFlags;
+ (Class)databaseClass;
- (void)close;

- (void)inDatabase:(void (^)(FMDatabase *db))block;
- (void)inTransaction:(void (^)(FMDatabase *db, BOOL *rollback))block;
- (void)inDeferredTransaction:(void (^)(FMDatabase *db, BOOL *rollback))block;

#if SQLITE_VERSION_NUMBER >= 3007000
- (NSError*)inSavePoint:(void (^)(FMDatabase *db, BOOL *rollback))block;
#endif

@end

